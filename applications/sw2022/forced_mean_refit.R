## sw2022 forced-mean refit (v2.1 relu family, 4 units, weight decay 0.01)
## Purpose: recover party-varying gender preferences so Figure 5 panels A and B
## are not degenerate. Reproduction target from the 2026-08-26 run:
##   party plugin means  Dem -0.268, Ind -0.177, Rep +0.080
##   overall theta_gender -0.111 (se 0.066)
suppressPackageStartupMessages(pkgload::load_all("~/GitHub/sconjoint", quiet = TRUE))
set.seed(20260826)
OUT <- Sys.getenv("SWREFIT_OUT", unset = "/tmp/swrefit")
.nm <- load("~/GitHub/sconjoint/data/sw2022.rda"); d <- get(.nm[1]); stopifnot(is.data.frame(d))
p <- readRDS(file.path(OUT, "probe.rds"))

log <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n"); flush.console() }

log("fitting: relu, hidden=4, weight_decay=0.01, q=1, K=5, 6000 epochs")
t0 <- Sys.time()
fit <- scmix(p$formula, d, respondent = "respondent", task = "task",
             profile = "profile", q = 1L, hidden = 4L, mean_family = "relu",
             weight_decay = 0.01, n_epochs = 6000L, n_starts = 2L, K = 5L,
             seed = 20260826L, verbose = FALSE)
log("fit done in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min")
saveRDS(fit, file.path(OUT, "fit_forced_mean.rds"))

GEN <- "cand_genderMale"
j <- match(GEN, fit$attr_names); stopifnot(!is.na(j))

## respondent-level means (dedup, as sb_respondent_means does)
rid <- as.character(fit$respondent_id)
keep <- !duplicated(rid)
M <- as.matrix(fit$mu_hat)[keep, , drop = FALSE]
resp <- rid[keep]
fold <- fit$fold_id[keep]

## non-degeneracy check
sds <- apply(M, 2, sd)
log("mu per-coordinate SD range:", paste(round(range(sds), 3), collapse = " .. "))
log("between-respondent covariance trace:", signif(sum(apply(M, 2, var)), 4))

## party labels, respondent level
pr <- d[!duplicated(as.character(d$respondent)), ]
pr <- pr[match(resp, as.character(pr$respondent)), ]
party <- ifelse(pr$party_Republican == 1, "Republican",
         ifelse(pr$party_Independent == 1, "Independent", "Democrat"))
log("party counts:", paste(names(table(party)), table(party), collapse = " "))

## plugin party means for the gender coordinate
plug <- tapply(M[, j], party, mean)
log("PLUGIN party means (Dem/Ind/Rep):",
    paste(round(c(plug[["Democrat"]], plug[["Independent"]], plug[["Republican"]]), 4),
          collapse = " / "), " [target -0.268 / -0.177 / 0.080]")
log("PLUGIN overall mean:", round(mean(M[, j]), 4))

## ---- inference: overall theta + per-party subgroup means -------------
contrast <- rep(0, length(fit$attr_names)); contrast[j] <- 1
res <- data.frame(party = character(), estimate = numeric(),
                  diagnostic_se = numeric(), method = character())

subgroup_dml <- function(lbl) {
  g <- as.numeric(party == lbl); names(g) <- resp
  tgt <- sconjoint:::scmix_inference_target("subgroup_tau_primitives",
                                            contrast = contrast, subgroup = g)
  inf <- scmix_dml(fit, targets = character(0), plugin_targets = list(sg = tgt),
                   riesz_equation_tolerance = Inf,
                   ridge_sensitivity_tolerance = Inf)
  nms <- names(inf$estimate)
  wn <- grep("weighted", nms, value = TRUE)[1]
  pn <- grep("probability", nms, value = TRUE)[1]
  tr <- sconjoint:::scmix_delta_transform(inf, "subgroup_ratio", c(wn, pn),
                                          denominator_margin = 0.05)
  se <- tryCatch(sqrt(as.numeric(tr$diagnostic_variance)), error = function(e) NA_real_)
  if (is.na(se) && !is.null(tr$diagnostic_covariance))
    se <- sqrt(as.numeric(tr$diagnostic_covariance)[1])
  c(est = as.numeric(tr$estimate)[1], se = se)
}

for (lbl in c("Democrat", "Independent", "Republican")) {
  out <- tryCatch(subgroup_dml(lbl), error = function(e) {
    log("DML subgroup failed for", lbl, "-", conditionMessage(e)); NULL })
  if (!is.null(out) && is.finite(out[["est"]])) {
    res <- rbind(res, data.frame(party = lbl, estimate = out[["est"]],
                                 diagnostic_se = out[["se"]], method = "dml_subgroup_ratio"))
    log("DML", lbl, ":", round(out[["est"]], 4), "se", round(out[["se"]], 4))
  } else {
    ## fallback: respondent bootstrap of the plugin subgroup mean, fit held fixed
    v <- M[party == lbl, j]
    bs <- replicate(2000, mean(sample(v, length(v), replace = TRUE)))
    res <- rbind(res, data.frame(party = lbl, estimate = mean(v),
                                 diagnostic_se = sd(bs), method = "resp_bootstrap_plugin"))
    log("BOOT", lbl, ":", round(mean(v), 4), "se", round(sd(bs), 4))
  }
}

## overall row
ov <- tryCatch({
  tgt <- sconjoint:::scmix_inference_target("tau", contrast = contrast)
  inf <- scmix_dml(fit, targets = character(0), plugin_targets = list(tau = tgt),
                   riesz_equation_tolerance = Inf, ridge_sensitivity_tolerance = Inf)
  s <- sqrt(diag(as.matrix(inf$diagnostic_covariance)))[1]
  c(est = as.numeric(inf$estimate)[1], se = as.numeric(s))
}, error = function(e) { log("overall DML failed:", conditionMessage(e)); NULL })
if (!is.null(ov) && is.finite(ov[["est"]])) {
  res <- rbind(data.frame(party = "Overall", estimate = ov[["est"]],
                          diagnostic_se = ov[["se"]], method = "dml_tau"), res)
  log("DML Overall:", round(ov[["est"]], 4), "se", round(ov[["se"]], 4), " [target -0.111 / 0.066]")
} else {
  v <- M[, j]; bs <- replicate(2000, mean(sample(v, length(v), replace = TRUE)))
  res <- rbind(data.frame(party = "Overall", estimate = mean(v),
                          diagnostic_se = sd(bs), method = "resp_bootstrap_plugin"), res)
  log("BOOT Overall:", round(mean(v), 4), "se", round(sd(bs), 4))
}

write.csv(res, file.path(OUT, "sw_gender_party_onestep.csv"), row.names = FALSE)
log("wrote sw_gender_party_onestep.csv")

## ---- Panel B: party-specific recovered-preference densities ----------
## beta_i[gender] = mu_i[gender] + a_i u, u ~ N(0,1); a_i from the respondent's
## held-out fold on the raw contrast scale (fold-local sign is irrelevant to |a|).
a_raw <- vapply(seq_len(fit$K), function(k) {
  A <- as.matrix(fit$A_folds[[k]]); sc <- as.numeric(fit$sd_dx_folds[[k]])
  (A / sc)[j, 1]
}, numeric(1))
a_i <- abs(a_raw[fold])
grid <- seq(min(M[, j]) - 4 * max(a_i), max(M[, j]) + 4 * max(a_i), length.out = 512)
dens <- do.call(rbind, lapply(c("Democrat", "Independent", "Republican"), function(lbl) {
  idx <- which(party == lbl)
  y <- rowMeans(vapply(idx, function(i) dnorm(grid, M[i, j], a_i[i]), numeric(length(grid))))
  data.frame(party = lbl, x = grid, density = y)
}))
write.csv(dens, file.path(OUT, "sw_gender_party_density.csv"), row.names = FALSE)
log("wrote sw_gender_party_density.csv; |a| range:",
    paste(round(range(a_i), 4), collapse = " .. "))
log("DONE")
