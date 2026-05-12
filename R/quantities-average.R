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
#' @param object An `sc_fit`.
#' @param scale One of `"logit"` or `"probability"`.
#' @param subgroup Optional row selector.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#'   Only affects `scale = "probability"`; the `"logit"` scale uses
#'   `object$theta` from the DML fit.
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
  p <- ncol(Bs)
  if (scale == "logit") {
    ## Just wrap theta + clustered SE
    est_vec <- object$theta
    se_vec  <- sqrt(diag(object$vcov))
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
    return(.sc_quantity(
      name = "average_logit",
      estimate = df,
      se = NA_real_,
      details = list(scale = "logit", subgroup_size = length(S)),
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
  gprime_avg <- mean(gprime)
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
