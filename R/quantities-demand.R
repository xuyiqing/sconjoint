#' Aggregate demand curve as a function of a numeric cost attribute
#'
#' Sweeps a grid of values for a numeric attribute (typically cost or
#' price) while holding other profile attributes fixed, computing the
#' average choice probability at each grid point.  The reference
#' profile is the all-reference-level alternative.
#'
#' For each cost value \eqn{c} and respondent \eqn{i}:
#' \deqn{V_i(c) = \hat\beta_{\mathrm{cost}}(Z_i) \cdot c +
#'   \sum_{k \neq \mathrm{cost}} \hat\beta_k(Z_i) \cdot
#'   \mathrm{profile}_k}
#' \deqn{\mathrm{demand}(c) = \frac{1}{N_S}\sum_{i\in S}
#'   \mathrm{plogis}(V_i(c))}
#'
#' @param object An `sc_fit`.
#' @param cost_attr Character name of the numeric cost attribute.
#'   Must correspond to a single column in `object$attr_names`.
#' @param cost_grid Numeric vector of cost values to evaluate.
#'   Default: 20-point regular grid over the observed range of
#'   `cost_attr` in `object$deltaX`.
#' @param profile Optional named list for non-cost attributes
#'   (defaults to reference levels, i.e. all zeros).
#' @param subgroup Optional row selector.
#' @return An `sc_quantity` whose `estimate` is a data.frame with
#'   columns `cost`, `demand`, and `se`.
#' @export
sc_demand_curve <- function(object, cost_attr,
                            cost_grid = NULL,
                            profile = NULL,
                            subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  ## Resolve cost attribute to column index
  cost_idx <- .sc_parse_dummy_name(object, cost_attr)
  ## Build the non-cost profile dummies
  p <- length(object$attr_names)
  if (is.null(profile)) {
    base_dx <- numeric(p)
  } else {
    base_dx <- .sc_profile_to_dummies(object, profile)
  }
  ## Zero out the cost column — we'll fill it per grid point
  base_dx[cost_idx] <- 0
  ## Default grid from observed deltaX
  if (is.null(cost_grid)) {
    dX <- object$deltaX
    if (is.null(dX)) {
      stop("sc_demand_curve(): cost_grid not supplied and object$deltaX not stored.")
    }
    cvals <- dX[, cost_idx]
    cost_grid <- seq(min(cvals), max(cvals), length.out = 20L)
  }
  Bm <- object$beta_hat
  S  <- .sc_resolve_subgroup(object, subgroup)
  Bs <- Bm[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  ## Base linear predictor (non-cost part)
  lin_base <- as.numeric(Bs %*% base_dx)
  beta_cost <- Bs[, cost_idx]
  n_grid <- length(cost_grid)
  demand_vec <- numeric(n_grid)
  se_vec     <- numeric(n_grid)
  for (g in seq_len(n_grid)) {
    lin_g <- lin_base + beta_cost * cost_grid[g]
    p_i <- stats::plogis(lin_g)
    demand_vec[g] <- mean(p_i)
    se_vec[g]     <- .sc_cluster_se(p_i, resp_s)
  }
  ci_q <- stats::qnorm(0.975)
  df <- data.frame(
    cost   = cost_grid,
    demand = demand_vec,
    se     = se_vec,
    ci_lo  = demand_vec - ci_q * se_vec,
    ci_hi  = demand_vec + ci_q * se_vec,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  .sc_quantity(
    name = "demand_curve",
    estimate = df,
    se = NA_real_,
    details = list(
      cost_attr     = cost_attr,
      n_grid        = n_grid,
      subgroup_size = length(S),
      se_method     = "respondent-clustered"
    ),
    call = match.call()
  )
}
