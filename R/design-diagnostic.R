## sc_design_diagnostic(): estimate R²_Z per coefficient + recovery
## tier hint per paper §6 heuristics.
##
## Algorithm reference: see
## `statsclaw-workspace/sconjoint/ref/design-diagnostic.md`.

#' Estimate per-coefficient R^2_Z and a recovery-tier hint
#'
#' For each coefficient \eqn{k}, estimate
#' \deqn{\hat R^2_{Z,k} = \frac{\mathrm{Var}_i(\hat\beta_{\text{ens,resp},i,k})}
#'                            {\mathrm{Var}_i(\hat\beta_{\text{ens,resp},i,k}) +
#'                             \overline{\sigma^2_{\text{post,diag},k}}}}
#' where the numerator is the cross-respondent variance of the
#' Stage-2 DNN ensemble mean (the conditional mean
#' \eqn{f(\mathbf{Z}_i)}), and the denominator adds the average
#' posterior variance read off the MAP Hessian.  Under correct
#' specification this approximates the share of preference
#' heterogeneity explained by observed respondent covariates.
#'
#' The function then maps the cross-coefficient mean
#' \eqn{\overline{\hat R^2_Z}} and the average tasks-per-respondent
#' \eqn{T} to four recovery tiers per paper \eqn{\S}6 prose, and
#' prints a banner indicating which tiers the design supports.
#'
#' Requires a fit produced with `stage2 = "map_c5"` or `"varref"`
#' (we need the posterior-variance diagonal from the MAP Hessian).
#'
#' @param object An `sc_fit` produced by `scfit()` with `stage2`
#'   in c("map_c5", "varref").
#' @param tier_thresholds Optional named list overriding the paper
#'   \eqn{\S}6 default thresholds.  Names: `T_distributional`,
#'   `R2_distributional`, `T_individual`, `R2_individual`,
#'   `T_ratio`, `R2_ratio`, `N_ratio`.  See default in
#'   `.sc_default_tier_thresholds()`.
#' @param experimental Logical; if `TRUE` the print method shows
#'   an "experimental" banner.  Default `TRUE` for v0.2.1 because
#'   the estimator has not yet been validated against the paper's
#'   5,760-cell simulation grid.
#' @return An `sc_quantity` of subclass `sc_quantity_design_diagnostic`
#'   with:
#'   * `estimate`: data.frame, one row per coefficient
#'     (`dummy_name`, `var_ens`, `mean_post_var`, `R2_Z`).
#'   * `summary`: named list with `R2Z_mean`, `T_mean`, `N_resp`,
#'     `N_tasks`, `tier_passes` (named logical), and the threshold
#'     list used.
#' @examples
#' \dontrun{
#' fit <- scfit(..., stage2 = "map_c5")
#' diag <- sc_design_diagnostic(fit)
#' print(diag)
#' }
#' @export
sc_design_diagnostic <- function(object,
                                 tier_thresholds = NULL,
                                 experimental = TRUE) {
  stopifnot(inherits(object, "sc_fit"))
  if (is.null(object$stage2_method) ||
      identical(object$stage2_method, "none") ||
      is.null(object$sigma_post_diag) ||
      is.null(object$beta_hat_ens)) {
    stop("sc_design_diagnostic() requires an sc_fit produced with ",
         "stage2 = \"map_c5\" or \"varref\" -- it needs the MAP ",
         "posterior-variance diagonal and the Stage-2 ensemble matrix.",
         call. = FALSE)
  }

  ## 1. Collapse the task-level ensemble to respondent level so we
  ##    have one row per unique respondent, then take per-column
  ##    cross-respondent variance.
  resp <- object$respondent_id
  first_row <- !duplicated(resp)
  ## Per-respondent ensemble means (one row per unique respondent;
  ## the ensemble is constant within respondent because it is a
  ## function of Z_i alone).
  ens_resp <- object$beta_hat_ens[first_row, , drop = FALSE]
  var_ens  <- apply(ens_resp, 2L, stats::var)

  ## 2. Average posterior-variance diagonal across respondents.
  ##    Already stored on the fit as a length-P vector.
  mean_post_var <- as.numeric(object$sigma_post_diag)

  ## 3. R^2_Z per coefficient, floored at 0 and capped at 1.
  denom <- var_ens + pmax(mean_post_var, 0)
  R2_Z  <- ifelse(denom > 0,
                  pmin(pmax(var_ens / denom, 0), 1),
                  NA_real_)
  names(R2_Z) <- object$attr_names

  est_df <- data.frame(
    dummy_name    = object$attr_names,
    var_ens       = unname(var_ens),
    mean_post_var = unname(mean_post_var),
    R2_Z          = unname(R2_Z),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  ## 4. Tier-hint summary
  thr <- .sc_default_tier_thresholds()
  if (!is.null(tier_thresholds)) {
    for (nm in names(tier_thresholds)) thr[[nm]] <- tier_thresholds[[nm]]
  }
  N_resp  <- length(unique(resp))
  N_tasks <- length(resp)
  T_mean  <- if (N_resp > 0) N_tasks / N_resp else NA_real_
  R2Z_mean <- mean(R2_Z, na.rm = TRUE)

  tier_passes <- c(
    mean = TRUE,  # always
    distributional = T_mean >= thr$T_distributional &&
                     R2Z_mean >= thr$R2_distributional,
    individual     = T_mean >= thr$T_individual &&
                     R2Z_mean >= thr$R2_individual,
    ratio          = T_mean >= thr$T_ratio &&
                     R2Z_mean >= thr$R2_ratio &&
                     N_resp >= thr$N_ratio
  )

  summary_list <- list(
    R2Z_mean       = R2Z_mean,
    T_mean         = T_mean,
    N_resp         = N_resp,
    N_tasks        = N_tasks,
    tier_passes    = tier_passes,
    thresholds     = thr,
    experimental   = isTRUE(experimental)
  )

  out <- .sc_quantity(
    name     = "design_diagnostic",
    estimate = list(estimate = est_df, summary = summary_list),
    se       = NA_real_,
    details  = list(stage2_method = object$stage2_method),
    call     = match.call()
  )
  class(out) <- c("sc_quantity_design_diagnostic", class(out))
  out
}

#' Default tier thresholds (paper §6 heuristics)
#' @keywords internal
#' @noRd
.sc_default_tier_thresholds <- function() {
  list(
    T_distributional = 5,
    R2_distributional = 0.35,
    T_individual    = 8,
    R2_individual   = 0.55,
    T_ratio         = 10,
    R2_ratio        = 0.55,
    N_ratio         = 5000
  )
}

#' Print method for `sc_quantity_design_diagnostic`
#' @param x An `sc_quantity_design_diagnostic` object.
#' @param digits Significant digits.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.sc_quantity_design_diagnostic <- function(x, digits = 3L, ...) {
  s <- x$estimate$summary
  cat("sc_design_diagnostic --- recovery-tier hint\n")
  if (isTRUE(s$experimental)) {
    cat("[experimental: estimator not yet validated against paper sim grid;\n",
        " interpret as guidance, not a hard cutoff]\n", sep = "")
  }
  cat(sprintf("Stage 2: %s\n",
              if (is.null(x$details$stage2_method)) "(unknown)"
              else x$details$stage2_method))
  cat(sprintf("Respondents: %d | Tasks: %d | T_mean: %s | mean R^2_Z: %s\n",
              s$N_resp, s$N_tasks,
              format(s$T_mean, digits = digits),
              format(s$R2Z_mean, digits = digits)))

  thr <- s$thresholds
  tp  <- s$tier_passes
  marks <- c("Y" = "[YES]", "N" = "[NO] ")
  fmt_tier <- function(name, condition_text, pass) {
    cat(sprintf("  %s %-22s  %s\n",
                if (pass) marks[["Y"]] else marks[["N"]],
                name, condition_text))
  }
  cat("\nRecovery tiers:\n")
  fmt_tier("mean & aggregate",
           "(any reasonable design)", TRUE)
  fmt_tier("distributional",
           sprintf("(T >= %g and R^2_Z >= %g)",
                   thr$T_distributional, thr$R2_distributional),
           tp[["distributional"]])
  fmt_tier("individual-level",
           sprintf("(T >= %g and R^2_Z >= %g)",
                   thr$T_individual, thr$R2_individual),
           tp[["individual"]])
  fmt_tier("ratio (MRS / WTP)",
           sprintf("(T >= %g and R^2_Z >= %g and N >= %g)",
                   thr$T_ratio, thr$R2_ratio, thr$N_ratio),
           tp[["ratio"]])

  ## Top / bottom R^2_Z coefficients for guidance on which
  ## attributes are best-pinned by Z and which need more T.
  df <- x$estimate$estimate
  df_sorted <- df[order(-df$R2_Z), ]
  n_show <- min(5L, nrow(df_sorted))
  cat("\nTop coefficients by R^2_Z (best-pinned by Z):\n")
  for (i in seq_len(n_show)) {
    cat(sprintf("  %-30s  %s\n",
                df_sorted$dummy_name[i],
                format(df_sorted$R2_Z[i], digits = digits)))
  }
  if (nrow(df_sorted) > n_show) {
    cat(sprintf("Bottom coefficients (rely most on T for recovery):\n"))
    for (i in seq(nrow(df_sorted) - n_show + 1L, nrow(df_sorted))) {
      cat(sprintf("  %-30s  %s\n",
                  df_sorted$dummy_name[i],
                  format(df_sorted$R2_Z[i], digits = digits)))
    }
  }

  invisible(x)
}
