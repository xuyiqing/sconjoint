## Proper subgroup inference on the saved forced-mean refit.
## Gates are set to large FINITE values (the user's directive: do not let the
## diagnostic gates withhold the subgroup analysis). The estimates themselves
## are unaffected by the tolerances; only the release gating is.
suppressPackageStartupMessages(pkgload::load_all("~/GitHub/sconjoint", quiet = TRUE))
OUT <- Sys.getenv("SWREFIT_OUT", unset = "/tmp/swrefit")
log <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n"); flush.console() }
fit <- readRDS(file.path(OUT, "fit_forced_mean.rds"))
.nm <- load("~/GitHub/sconjoint/data/sw2022.rda"); d <- get(.nm[1])

GEN <- "cand_genderMale"; j <- match(GEN, fit$attr_names)
contrast <- rep(0, length(fit$attr_names)); contrast[j] <- 1
rid <- as.character(fit$respondent_id); keep <- !duplicated(rid)
resp <- rid[keep]; M <- as.matrix(fit$mu_hat)[keep, , drop = FALSE]
pr <- d[!duplicated(as.character(d$respondent)), ]
pr <- pr[match(resp, as.character(pr$respondent)), ]
party <- ifelse(pr$party_Republican == 1, "Republican",
         ifelse(pr$party_Independent == 1, "Independent", "Democrat"))

BIG <- list(riesz_equation_tolerance = 1e6, ridge_sensitivity_tolerance = 1e6)
dml <- function(tgts) do.call(scmix_dml, c(list(fit = fit, targets = character(0),
                                                plugin_targets = tgts), BIG))
getse <- function(inf, nm) {
  cv <- inf$diagnostic_covariance
  if (is.null(cv)) return(NA_real_)
  cv <- as.matrix(cv); k <- match(nm, rownames(cv))
  if (is.na(k)) NA_real_ else sqrt(cv[k, k])
}
res <- data.frame()

ov <- tryCatch({
  inf <- dml(list(tau = sconjoint:::scmix_inference_target("tau", contrast = contrast)))
  nm <- names(inf$estimate)[1]
  log("overall targets:", paste(names(inf$estimate), collapse = ", "))
  data.frame(party = "Overall", estimate = as.numeric(inf$estimate)[1],
             diagnostic_se = getse(inf, nm), method = "dml_tau")
}, error = function(e) { log("overall DML failed:", conditionMessage(e)); NULL })
if (!is.null(ov)) { res <- rbind(res, ov); log("Overall:", round(ov$estimate,4), "se", round(ov$diagnostic_se,4)) }

for (lbl in c("Democrat", "Independent", "Republican")) {
  r <- tryCatch({
    g <- as.numeric(party == lbl); names(g) <- resp
    inf <- dml(list(sg = sconjoint:::scmix_inference_target(
      "subgroup_tau_primitives", contrast = contrast, subgroup = g)))
    nms <- names(inf$estimate)
    wn <- grep("weighted", nms, value = TRUE)[1]; pn <- grep("probability", nms, value = TRUE)[1]
    tr <- sconjoint:::scmix_delta_transform(inf, "subgroup_ratio", c(wn, pn),
                                            denominator_margin = 0.05)
    se <- NA_real_
    for (f in c("diagnostic_variance", "diagnostic_covariance", "variance")) {
      if (!is.null(tr[[f]])) { se <- sqrt(as.numeric(tr[[f]])[1]); break }
    }
    data.frame(party = lbl, estimate = as.numeric(tr$estimate)[1],
               diagnostic_se = se, method = "dml_subgroup_ratio")
  }, error = function(e) { log("DML failed for", lbl, "-", conditionMessage(e)); NULL })
  if (!is.null(r) && is.finite(r$estimate)) {
    res <- rbind(res, r); log(lbl, ":", round(r$estimate,4), "se", round(r$diagnostic_se,4))
  }
}
if (nrow(res)) write.csv(res, file.path(OUT, "party_dml.csv"), row.names = FALSE)
log("rows:", nrow(res)); log("DONE")
