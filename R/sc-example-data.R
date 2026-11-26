#' Model inputs for the paper's two applications
#'
#' Builds, from the bundled data, the exact inputs the paper's fits used for
#' the Graham and Svolik (2020) candidate conjoint (`"gs"`) or the
#' Ballard-Rosa, Martin and Scheve (2017) tax-plan conjoint (`"br"`): the
#' choices and attribute contrasts, the respondent covariates fed to the
#' network, the five cross-fitting folds, the respondents held out for early
#' stopping in each fold, and the subgroup variables the reported quantities
#' use.
#'
#' For `"gs"`, the left-displayed candidate comes first: `y = 1` means the
#' respondent chose the candidate shown on the left, and `d` is left minus
#' right. Matchups whose candidate 1 was shown on the right (`on_left == 0`
#' in [gs2020]) are flipped. The covariates are the 16 columns of the main
#' fit.
#'
#' For `"br"`, Plan A comes first, and the sample is the 1,991 respondents
#' with an income answer (`valid_income` in [br2017]). Each plan is coded as
#' a linear and a quadratic term per bracket rate and four Helmert contrasts
#' for the five revenue labels (16 features). The covariates are raw codes:
#' age in years, sex, education dummies, white, the income code, and party
#' dummies.
#'
#' @param app `"gs"` (Graham and Svolik 2020) or `"br"` (Ballard-Rosa, Martin
#'   and Scheve 2017).
#'
#' @return A list with
#' \describe{
#'   \item{data}{A `rum_data` object: one record per
#'     respondent, with choices `y` and contrasts `d`.}
#'   \item{Z}{Respondent covariates, one row per respondent (row names are
#'     the respondent ids), as the paper passed them to the network (it
#'     centres and scales them on each fold's training respondents).}
#'   \item{ids}{Respondent ids, in the order of `data` and `Z`.}
#'   \item{fold}{Cross-fitting fold of each respondent (1 to 5).}
#'   \item{validation}{A list with one integer vector per fold: positions in
#'     `ids` of the training respondents held out to choose the number of
#'     training epochs.}
#'   \item{pid7}{Party ID. GS: -3 (strong Democrat) to 3 (strong
#'     Republican), 0 = pure independent. BR: 1 (strong Democrat) to 7
#'     (strong Republican), 4 = independent, 8 = not sure.}
#'   \item{party}{`"Dem"`, `"Ind"` or `"Rep"`. GS: the sign of `pid7`. BR:
#'     `pid7` 1-3, 5-7, and the rest; BR `"Ind"` includes the 74 "not sure"
#'     respondents, while the paper's independents are `pid7 == 4`.}
#'   \item{ideology}{GS only (`NULL` for BR): `"lib"` (ideology 1-3),
#'     `"mod"` (4) or `"con"` (5-7).}
#'   \item{PL}{GS only (`NULL` for BR): 1 for partisans and leaners
#'     (`pid7 != 0`), 0 for pure independents.}
#'   \item{app}{The application, `"gs"` or `"br"`.}
#' }
#'
#' @seealso [gs2020], [br2017].
#' @examples
#' ex <- sc_example_data("br")
#' ex$data
#' dim(ex$Z)
#' table(ex$fold)
#' @export
sc_example_data <- function(app = c("gs", "br")) {
  app <- match.arg(app)
  splits <- .sc_paper_splits[[app]]
  out <- if (app == "gs") .sc_example_gs() else .sc_example_br()
  if (!identical(out$ids, splits$ids))
    stop("The bundled data do not match the paper's respondent list.")
  out$fold <- splits$fold
  out$validation <- splits$validation
  out[c("data", "Z", "ids", "fold", "validation", "pid7", "party",
        "ideology", "PL", "app")]
}

# Split task rows into one block per respondent, keeping row order.
.sc_by_respondent <- function(x, resp) {
  f <- factor(resp, levels = unique(resp))
  if (is.matrix(x)) lapply(split(seq_len(nrow(x)), f), function(i) x[i, , drop = FALSE])
  else unname(split(x, f))
}

.sc_example_gs <- function() {
  gs <- sconjoint::gs2020
  p1 <- gs[gs$profile == 1L, , drop = FALSE]
  feat <- grep("^diff_", names(p1), value = TRUE)
  d <- as.matrix(p1[, feat])
  y <- as.numeric(p1$choice)
  flip <- p1$on_left == 0L
  d[flip, ] <- -d[flip, , drop = FALSE]
  y[flip] <- 1 - y[flip]
  ids <- unique(p1$respondent)
  data <- rum_data(.sc_by_respondent(y, p1$respondent),
                   unname(.sc_by_respondent(d, p1$respondent)), ids = ids,
                   feature_names = feat,
                   metadata = list(
                     orientation = "y=1 selects the candidate displayed on the left; d = left minus right (gs2020 reoriented with on_left).",
                     application = "Graham-Svolik (gs2020)"))
  r1 <- p1[!duplicated(p1$respondent), , drop = FALSE]
  zc <- c("z_ideo", "z_pid7", "z_trump", "z_age", "z_educ", "z_hhi", "z_auth",
          "z_knowl", "z_female", "z_race_black", "z_race_asian", "z_race_other",
          "E_ideal", "I_ideal", "M_ideal", "T_ideal")
  Z <- as.matrix(r1[, zc])
  dimnames(Z) <- list(ids, zc)
  pid7 <- as.numeric(r1$pid7)
  list(data = data, Z = Z, ids = ids, pid7 = pid7,
       party = ifelse(pid7 < 0, "Dem", ifelse(pid7 > 0, "Rep", "Ind")),
       ideology = ifelse(r1$ideo7 <= 3, "lib", ifelse(r1$ideo7 >= 5, "con", "mod")),
       PL = as.numeric(pid7 != 0), app = "gs")
}

.sc_example_br <- function() {
  br <- sconjoint::br2017
  br <- br[br$valid_income, , drop = FALSE]
  A <- br[br$profile == 1L, , drop = FALSE]
  B <- br[br$profile == 2L, , drop = FALSE]
  if (!identical(A$respondent, B$respondent) || !identical(A$task, B$task))
    stop("br2017 must hold Plan A and Plan B rows for every task.")
  rates <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
             "rate_175_375", "rate_375P")
  prof <- function(p) {
    out <- stats::setNames(as.data.frame(p[, rates]), paste0("r", 1:6))
    out$taxrev <- 3 - p$revenue_score
    out
  }
  d <- .rum_design_code_profiles(.rum_design_spec("br"), prof(A), prof(B))
  y <- as.numeric(A$choice)
  ids <- unique(A$respondent)
  data <- rum_data(.sc_by_respondent(y, A$respondent),
                   unname(.sc_by_respondent(d, A$respondent)), ids = ids,
                   feature_names = colnames(d),
                   metadata = list(
                     orientation = "y=1 selects Plan A (profile 1); d = Plan A minus Plan B.",
                     application = "Ballard-Rosa, Martin and Scheve (br2017), valid-income respondents"))
  r <- A[!duplicated(A$respondent), , drop = FALSE]
  dummies <- function(x, lv, nm) {
    m <- vapply(lv, function(k) as.numeric(x == k), numeric(length(x)))
    m <- matrix(m, nrow = length(x))
    colnames(m) <- paste0(nm, "_", lv)
    m
  }
  Z <- cbind(age = r$resp_age, female = r$female, dummies(r$resp_educ, 2:6, "educ"),
             race_white = r$race_white, income = r$resp_income,
             dummies(r$resp_pid7, 2:8, "pid7"))
  rownames(Z) <- ids
  pid7 <- as.numeric(r$resp_pid7)
  list(data = data, Z = Z, ids = ids, pid7 = pid7,
       party = ifelse(pid7 <= 3, "Dem", ifelse(pid7 %in% 5:7, "Rep", "Ind")),
       ideology = NULL, PL = NULL, app = "br")
}
