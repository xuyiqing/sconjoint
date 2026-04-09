#' Preference polarization index per dummy
#'
#' For every dummy column \eqn{j}, computes
#' \eqn{1 - |\mathrm{frac}^+_j - \mathrm{frac}^-_j|} where
#' \eqn{\mathrm{frac}^\pm_j} is the subgroup-averaged indicator of
#' \eqn{\hat\beta_j(Z_i) > 0} (resp. `< 0`).  Values near 1 indicate
#' a population that is evenly split in direction (highly polarized);
#' values near 0 indicate near-uniform agreement.
#'
#' Point estimate is a direct port of `07b_structural_quantities.R`
#' lines 478--497.  No standard error is defined at v0.1 — the
#' polarization index is a bounded nonlinear transform of two
#' proportions and does not have a clean clustered-sd expression.
#'
#' @param object An `sc_fit`.
#' @param subgroup Row selector.
#' @return An `sc_quantity` with `estimate` a data.frame of one row
#'   per dummy (`dummy_name`, `frac_positive`, `frac_negative`,
#'   `polarization_idx`, plus `NA` SE/CI columns).
#' @export
sc_polarization <- function(object, subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  B <- object$beta_hat
  S <- .sc_resolve_subgroup(object, subgroup)
  Bs <- B[S, , drop = FALSE]
  fp <- colMeans(Bs > 0)
  fn <- colMeans(Bs < 0)
  poli <- 1 - abs(fp - fn)
  df <- data.frame(
    dummy_name       = object$attr_names,
    frac_positive    = fp,
    frac_negative    = fn,
    polarization_idx = poli,
    se               = NA_real_,
    ci_lo            = NA_real_,
    ci_hi            = NA_real_,
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
  .sc_quantity(
    name = "polarization",
    estimate = df,
    se = NA_real_,
    details = list(subgroup_size = length(S),
                   se_method = "none"),
    call = match.call()
  )
}
