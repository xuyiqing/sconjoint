#' Average marginal effects on the logit or probability scale
#'
#' With `scale = "logit"` (default), returns the DML coefficient
#' \eqn{\hat\theta} with its clustered SE — equivalent to
#' `coef(object)` but wrapped in an `sc_quantity`.
#'
#' With `scale = "probability"`, computes average marginal effects:
#' \deqn{\mathrm{AME}_k = \frac{1}{N_S}\sum_{i\in S}
#'   \hat\beta_k(Z_i)\,G'(\Delta X_i^\top \hat\beta(Z_i))}
#' where \eqn{G'(x) = \mathrm{plogis}(x)(1 - \mathrm{plogis}(x))}.
#' This requires the stored `deltaX` matrix.
#'
#' When the fit carries an attribute-interaction term
#' (`scfit(..., interactions != "none")`), the \eqn{G'} weights in the
#' `"probability"` scaling are computed from the main-effect linear
#' predictor \eqn{\Delta X^\top \hat\theta} only -- the per-task
#' interaction offset is omitted from the curvature evaluation -- and
#' the scaled coefficients remain main-effect quantities at the
#' no-interaction baseline.
#'
#' @param object An `sc_fit`.
#' @param scale One of `"logit"` or `"probability"`.
#' @param subgroup Optional row selector.  With `scale = "logit"`, a
#'   non-`NULL` subgroup returns the influence-function subgroup estimate:
#'   the mean of the task-level orthogonal-score contributions within the
#'   subgroup, with a respondent-clustered standard error.  This is the
#'   debiased subgroup average of \eqn{f(Z)} (the construction used for the
#'   party-level estimates in the paper's candidate application), not the
#'   subgroup mean of the MAP \eqn{\hat\beta_i}.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#'   Only affects `scale = "probability"`; the `"logit"` scale uses
#'   `object$theta` from the DML fit (or its influence-function subgroup
#'   version when `subgroup` is supplied).
#' @return An `sc_quantity` with a data.frame estimate containing
#'   per-attribute AMEs and clustered SEs.
#' @export
sc_average <- function(object, scale = c("logit", "probability"),
                       subgroup = NULL,
                       which_beta = c("hybrid", "dnn")) {
  stopifnot(inherits(object, "sc_fit"))
  scale <- match.arg(scale)
  which_beta <- match.arg(which_beta)
  S <- .sc_resolve_subgroup(object, subgroup)
  Bs <- .sc_pick_beta(object, which_beta)[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  w_s <- .sc_weights_for_rows(object, S)
  p <- ncol(Bs)
  if (scale == "logit") {
    ci_q <- stats::qnorm(0.975)
    if (is.null(subgroup)) {
      ## Full population: wrap theta + clustered SE from the DML fit.
      est_vec <- object$theta
      se_vec  <- sqrt(diag(object$vcov))
      se_method <- "DML, respondent-clustered"
    } else {
      ## Influence-function subgroup estimate: mean of the task-level
      ## orthogonal-score contributions within the subgroup, with a
      ## respondent-clustered SE (cluster sums of the centered influence,
      ## M_S/(M_S - 1) * sum_m s_m^2 / n_S^2).
      inf <- object$influence_raw
      if (is.null(inf)) {
        stop("sc_average(scale = 'logit', subgroup = ...): ",
             "`object$influence_raw` is not stored; refit with a current ",
             "version of scfit().", call. = FALSE)
      }
      inf_s  <- inf[S, , drop = FALSE]
      resp_s <- object$respondent_id[S]
      if (is.null(w_s)) {
        est_vec <- colMeans(inf_s)
        centered <- sweep(inf_s, 2L, est_vec)
        sums <- rowsum(centered, group = as.character(resp_s))
        M_s  <- nrow(sums)
        se_vec <- sqrt(M_s / (M_s - 1) * colSums(sums^2) / length(S)^2)
      } else {
        w_obj <- .sc_respondent_weight_object(resp_s, w_s)
        phi_s <- rowsum(inf_s, group = as.character(resp_s), reorder = TRUE) / w_obj$count
        st <- .sc_weighted_cluster_stats(phi_s, w_obj$w)
        est_vec <- st$estimate
        se_vec <- st$se
      }
      se_method <- "influence-function subgroup mean, respondent-clustered"
    }
    df <- data.frame(
      dummy_name = object$attr_names,
      estimate   = unname(est_vec),
      se         = unname(se_vec),
      ci_lo      = unname(est_vec - ci_q * se_vec),
      ci_hi      = unname(est_vec + ci_q * se_vec),
      stringsAsFactors = FALSE,
      row.names  = NULL
    )
    return(.sc_quantity(
      name = "average_logit",
      estimate = df,
      se = NA_real_,
      details = list(scale = "logit", subgroup_size = length(S),
                     se_method = se_method),
      call = match.call()
    ))
  }
  ## scale == "probability"
  dX <- object$deltaX
  if (is.null(dX)) {
    stop("sc_average(scale='probability'): object$deltaX not stored.")
  }
  dX_s <- dX[S, , drop = FALSE]
  ## Average G'(linear predictor) across the subgroup -- the delta-
  ## method scaling factor that takes logit-scale theta to
  ## probability-scale AME.  Use the POPULATION-AVERAGE theta_hat
  ## (not the per-respondent beta_hat matrix) for the linear
  ## predictor: this is what the standard delta-method specifies, and
  ## it avoids a perverse interaction on continuous-attribute designs
  ## (e.g. BR's tax rates) where v0.2 MAP betas can have extreme
  ## per-respondent tails.  Plugging those tails into `deltaX %*% beta`
  ## yields huge |linear predictor|, pushes G' toward zero, and
  ## shrinks the AME by ~50x relative to the LPM AMCE.
  theta_vec <- as.numeric(object$theta)
  lin    <- as.numeric(dX_s %*% theta_vec)
  gprime <- stats::plogis(lin) * (1 - stats::plogis(lin))
  gprime_avg <- .sc_weighted_task_mean(gprime, object$respondent_id[S], w_s)
  ## Point estimate: scale theta_hat by mean(G').
  est_vec <- theta_vec * gprime_avg
  ## SE via delta-method: Var(AME_k) = gprime_avg^2 * Var(theta_k).
  V <- object$vcov
  se_vec <- if (!is.null(V) && nrow(V) == p) {
    gprime_avg * sqrt(pmax(diag(V), 0))
  } else {
    rep(NA_real_, p)
  }
  ci_q <- stats::qnorm(0.975)
  df <- data.frame(
    dummy_name = object$attr_names,
    estimate   = est_vec,
    se         = se_vec,
    ci_lo      = est_vec - ci_q * se_vec,
    ci_hi      = est_vec + ci_q * se_vec,
    stringsAsFactors = FALSE,
    row.names  = NULL
  )
  .sc_quantity(
    name = "average_probability",
    estimate = df,
    se = NA_real_,
    details = list(
      scale         = "probability",
      subgroup_size = length(S),
      se_method     = "respondent-clustered"
    ),
    call = match.call()
  )
}
