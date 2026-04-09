#' Per-dummy heterogeneity test of Var(beta_j(Z)) > 0
#'
#' Implements the one-sided test from
#' `07_quantities_of_interest.R` lines 215--251: for each dummy
#' column \eqn{j}, let \eqn{\hat v_j = \mathrm{var}(B_{\cdot j})} and
#' \eqn{\widehat{\mathrm{se}}(\hat v_j)} be the respondent-clustered
#' standard error of the variance estimator; report the
#' one-sided z-statistic \eqn{t_j = \hat v_j/\widehat{\mathrm{se}}}
#' and p-value \eqn{p_j = \Phi(-t_j)}.  Multiplicity adjustment via
#' `stats::p.adjust()` with `"holm"` or `"bh"` (mapped to `"BH"`).
#'
#' @param object An `sc_fit`.
#' @param adjust Either `"none"`, `"holm"`, or `"bh"`.
#' @return An `sc_quantity` whose `estimate` is a data.frame with
#'   columns `dummy_name`, `var_beta`, `se_var`, `t_stat`, `p_value`,
#'   `p_adjusted`, `sig`.
#' @export
sc_heterogeneity_test <- function(object,
                                  adjust = c("none", "holm", "bh")) {
  stopifnot(inherits(object, "sc_fit"))
  adjust <- match.arg(adjust)
  B <- object$beta_hat
  resp <- object$respondent_id
  N <- nrow(B); p <- ncol(B)
  M <- length(unique(resp))
  resp_f <- as.factor(resp)
  var_beta <- numeric(p)
  se_var   <- numeric(p)
  for (j in seq_len(p)) {
    bj <- B[, j]
    bbar <- mean(bj)
    dev2 <- (bj - bbar)^2
    var_beta[j] <- sum(dev2) / (N - 1L)
    cm <- tapply(dev2 - var_beta[j], resp_f, sum)
    cm[is.na(cm)] <- 0
    se_var[j] <- sqrt((M / (M - 1)) * sum(as.numeric(cm)^2) / N^2)
  }
  t_stat <- ifelse(se_var > 0, var_beta / se_var, NA_real_)
  p_value <- stats::pnorm(-t_stat)
  method <- switch(adjust, none = "none", holm = "holm", bh = "BH")
  p_adj <- if (method == "none") p_value else stats::p.adjust(p_value, method = method)
  sig <- ifelse(p_adj < 0.001, "***",
         ifelse(p_adj < 0.01,  "**",
         ifelse(p_adj < 0.05,  "*",
         ifelse(p_adj < 0.1,   ".", ""))))
  df <- data.frame(
    dummy_name = object$attr_names,
    var_beta   = var_beta,
    se_var     = se_var,
    t_stat     = t_stat,
    p_value    = p_value,
    p_adjusted = p_adj,
    sig        = sig,
    stringsAsFactors = FALSE,
    row.names  = NULL
  )
  .sc_quantity(
    name = "heterogeneity_test",
    estimate = df,
    se = NA_real_,
    details = list(adjust = adjust,
                   method = "one-sided var>0, clustered on respondent"),
    call = match.call()
  )
}
