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
#' @return An `sc_quantity` with a data.frame estimate containing
#'   per-attribute AMEs and clustered SEs.
#' @export
sc_average <- function(object, scale = c("logit", "probability"),
                       subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  scale <- match.arg(scale)
  S <- .sc_resolve_subgroup(object, subgroup)
  Bs <- object$beta_hat[S, , drop = FALSE]
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
  ## logit index per row
  lin <- rowSums(dX_s * Bs)
  gprime <- stats::plogis(lin) * (1 - stats::plogis(lin))
  ## per-row AME contribution for each attribute
  ame_mat <- Bs * gprime  # |S| x p, each row scaled by G'
  est_vec <- colMeans(ame_mat)
  se_vec <- numeric(p)
  for (j in seq_len(p)) {
    se_vec[j] <- .sc_cluster_se(ame_mat[, j], resp_s)
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
