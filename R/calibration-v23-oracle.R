## Design-matched statistical oracle for the protocol-v2.3 Step-3 study.
##
## The production oracle is a respondent-clustered local-Gaussian experiment
## derived from a task-level rank-one mixed-logit model.  It never invents a
## diagnostic by multiplying a default by random noise.  The sensitivity
## (bread), respondent score covariance (meat), target derivatives, boundary
## distances, and inverse-problem diagnostics all come from the frozen
## response-free design and DGP.  A separate task-level generator is retained
## so that the approximation can be face-validated with genuine Bernoulli
## response sequences before calibration results are used.

.scmix_v23_oracle_names <- c(
  "subgroup_interior_margin",
  "directional_heterogeneity_regularity_margin",
  "mean_tradeoff_denominator_margin",
  "chart_discrepancy_cutoff", "nonlinear_remainder_cutoff",
  "interior_distance_cutoff", "active_eigenvalue_standardized_cutoff",
  "generalized_information_margin",
  "directional_variance_standardized_cutoff",
  "total_heterogeneity_standardized_cutoff",
  "denominator_standardized_cutoff", "null_space_materiality_cutoff",
  "defect_cutoff", "ridge_cutoff")

.scmix_v23_oracle_targets <- c(
  "unrestricted_mean_direction", "probability", "subgroup_share",
  "preference_direction_share", "compensating_benefit_share",
  "directional_heterogeneity", "mean_tradeoff_mrs_ratio")

.scmix_v23_oracle_target_keys <- c(
  unrestricted_mean_direction = "unrestricted_mean_direction",
  probability = "probability",
  subgroup_share__z_high = "subgroup_share",
  subgroup_share__z_low = "subgroup_share",
  preference_direction_share = "preference_direction_share",
  compensating_benefit_share = "compensating_benefit_share",
  directional_heterogeneity = "directional_heterogeneity",
  mean_tradeoff_mrs_ratio = "mean_tradeoff_mrs_ratio")

.scmix_v23_oracle_variants <- c(
  unrestricted_mean_direction = "all_respondents",
  probability = "all_respondents",
  subgroup_share__z_high = "z_positive_after_standardization",
  subgroup_share__z_low = "z_nonpositive_after_standardization",
  preference_direction_share = "r_positive",
  compensating_benefit_share = "r_plus_0.5_times_d_positive",
  directional_heterogeneity = "r_explained_share",
  mean_tradeoff_mrs_ratio = "negative_s_over_d")

.scmix_v23_oracle_number <- function(row, name, default) {
  value <- if (name %in% names(row)) row[[name]][[1L]] else default
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value)) {
    stop("The oracle DGP field `", name, "` must be finite.", call. = FALSE)
  }
  value
}

.scmix_v23_oracle_flag <- function(row, name, default = FALSE) {
  value <- if (name %in% names(row)) row[[name]][[1L]] else default
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(value)
  value <- tolower(trimws(as.character(value)))
  if (!value %in% c("true", "false")) {
    stop("The oracle DGP field `", name, "` must be true or false.",
         call. = FALSE)
  }
  identical(value, "true")
}

.scmix_v23_oracle_text <- function(row, name, default) {
  value <- if (name %in% names(row)) row[[name]][[1L]] else default
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("The oracle DGP field `", name, "` must be nonempty.",
         call. = FALSE)
  }
  value
}

.scmix_v23_oracle_config <- function(plan, regime_id, severity_id = NULL) {
  row <- plan$dgp[plan$dgp$regime_id == regime_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("Exactly one frozen DGP row is required for `", regime_id, "`.",
         call. = FALSE)
  }
  severity <- NULL
  if (!is.null(plan$severity)) {
    candidate <- plan$severity[plan$severity$regime_id == regime_id, ,
                               drop = FALSE]
    if (is.null(severity_id)) {
      if (nrow(candidate) == 1L) severity <- candidate else {
        preferred <- which(candidate$severity_id %in% c("reference", "severe"))
        severity <- candidate[if (length(preferred)) preferred[[length(preferred)]]
                              else nrow(candidate), , drop = FALSE]
      }
    } else {
      severity <- candidate[candidate$severity_id == severity_id, , drop = FALSE]
      if (nrow(severity) != 1L) {
        stop("Exactly one severity row is required for `", regime_id, "/",
             severity_id, "`.", call. = FALSE)
      }
    }
  }
  value <- function(name, default) {
    if (!is.null(severity) && name %in% names(severity) &&
        !is.na(severity[[name]][[1L]]) && nzchar(as.character(
          severity[[name]][[1L]]))) severity[[name]][[1L]] else
      if (name %in% names(row)) row[[name]][[1L]] else default
  }
  config_row <- as.data.frame(as.list(stats::setNames(
    vapply(names(row), function(name) as.character(value(name, "")),
           character(1L)), names(row))), stringsAsFactors = FALSE)
  for (name in setdiff(names(severity), names(config_row))) {
    config_row[[name]] <- severity[[name]][[1L]]
  }
  out <- list(
    regime_id = regime_id,
    severity_id = if (is.null(severity)) "registry_default" else
      as.character(severity$severity_id[[1L]]),
    certification_truth = if (!is.null(severity) &&
        any(c("severity_role", "certification_truth") %in% names(severity))) {
      role_name <- intersect(c("severity_role", "certification_truth"),
                             names(severity))[[1L]]
      as.character(severity[[role_name]][[1L]])
    } else {
      sid <- if (is.null(severity)) "reference" else
        as.character(severity$severity_id[[1L]])
      if (sid %in% c("reference", "mild")) "safe" else if (
        identical(sid, "moderate")) "transition" else "unsafe"
    },
    d_weak_weight = .scmix_v23_oracle_number(
      config_row, "d_projection_weakest_weight", 0),
    kappa = .scmix_v23_oracle_number(config_row, "intercept", 0),
    loading = .scmix_v23_oracle_number(config_row, "loading_sd", .55),
    direction_intercept = .scmix_v23_oracle_number(
      config_row, "direction_intercept", .12),
    direction_moderator_slope = .scmix_v23_oracle_number(
      config_row, "direction_moderator_slope", .22),
    numerator_intercept = .scmix_v23_oracle_number(
      config_row, "numerator_intercept", .75),
    numerator_moderator_slope = .scmix_v23_oracle_number(
      config_row, "numerator_moderator_slope", .10),
    denominator_intercept = .scmix_v23_oracle_number(
      config_row, "mrs_denominator_intercept", .60),
    denominator_moderator_slope = .scmix_v23_oracle_number(
      config_row, "denominator_moderator_slope", .08),
    residual = .scmix_v23_oracle_text(
      config_row, "residual_distribution", "normal"),
    serial_rho = .scmix_v23_oracle_number(config_row, "serial_rho", 0),
    serial_sd = .scmix_v23_oracle_number(
      config_row, "serial_innovation_sd", .35),
    informative_completion = .scmix_v23_oracle_flag(
      config_row, "informative_completion", FALSE),
    completion_intercept = .scmix_v23_oracle_number(
      config_row, "completion_intercept", .8),
    completion_latent = .scmix_v23_oracle_number(
      config_row, "completion_latent_coefficient", -1),
    completion_difficulty = .scmix_v23_oracle_number(
      config_row, "completion_difficulty_coefficient", -.45),
    weak_direction = .scmix_v23_oracle_flag(
      config_row, "weak_target_direction", FALSE),
    model_nodes = as.integer(.scmix_v23_oracle_number(
      config_row, "oracle_model_nodes", 31)),
    truth_normal_nodes = as.integer(.scmix_v23_oracle_number(
      config_row, "truth_normal_nodes", 257)),
    truth_nonnormal_quantiles = as.integer(.scmix_v23_oracle_number(
      config_row, "truth_nonnormal_quantiles", 4097)))
  if (isTRUE(out$weak_direction) && out$d_weak_weight == 0) {
    out$d_weak_weight <- 1
  }
  out$severity_role <- out$certification_truth
  out
}

.scmix_v23_oracle_check_template <- function(template, require_frozen = TRUE) {
  if (inherits(template, "scmix_v23_design_template") &&
      exists(".scmix_v23_step3_check_design", mode = "function",
             inherits = TRUE)) {
    get(".scmix_v23_step3_check_design", mode = "function", inherits = TRUE)(
      template, require_frozen = require_frozen)
    return(invisible(TRUE))
  }
  needed <- c("design_id", "deltaX", "Z", "respondent_id", "task_id")
  if (!is.list(template) || any(!needed %in% names(template))) {
    stop("A response-free v2.3 design template is required.", call. = FALSE)
  }
  if (isTRUE(require_frozen)) {
    stop("An unsigned pilot template cannot be used in a frozen oracle run.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.scmix_v23_oracle_sign <- function(v) {
  j <- which.max(abs(v))
  if (length(j) && is.finite(v[[j]]) && v[[j]] < 0) -v else v
}

.scmix_v23_oracle_standardize <- function(x) {
  x <- as.matrix(x)
  center <- colMeans(x)
  scale <- apply(x, 2L, stats::sd)
  active <- is.finite(scale) & scale > sqrt(.Machine$double.eps)
  if (sum(active) < 3L) {
    stop("The oracle requires at least three varying design columns.",
         call. = FALSE)
  }
  out <- sweep(x[, active, drop = FALSE], 2L, center[active], `-`)
  out <- sweep(out, 2L, scale[active], `/`)
  list(x = out, center = center, scale = scale, active = active)
}

.scmix_v23_oracle_prepare <- function(template, require_frozen = TRUE) {
  .scmix_v23_oracle_check_template(template, require_frozen)
  dx <- .scmix_v23_oracle_standardize(template$deltaX)
  respondent_id <- as.character(template$respondent_id)
  id <- unique(respondent_id)
  group <- match(respondent_id, id)
  n_i <- tabulate(group, nbins = length(id))
  gram <- crossprod(dx$x) / nrow(dx$x)
  eig <- eigen(gram, symmetric = TRUE)
  vectors <- apply(eig$vectors, 2L, .scmix_v23_oracle_sign)
  if (is.null(dim(vectors))) vectors <- matrix(vectors, ncol = 1L)
  score <- dx$x %*% vectors
  unit_score <- function(j) {
    ans <- score[, j]
    s <- stats::sd(ans)
    if (!is.finite(s) || s <= sqrt(.Machine$double.eps)) {
      stop("A required oracle design direction has zero variation.",
           call. = FALSE)
    }
    as.numeric(ans / s)
  }
  r <- unit_score(1L)
  s <- unit_score(2L)
  d_regular <- unit_score(3L)
  ## The weak score deliberately retains its empirical weakness relative to
  ## the leading score.  Renormalizing it would erase the design feature that
  ## the weak-inverse regime is meant to probe.
  d_weak <- as.numeric(score[, ncol(score)] / stats::sd(score[, 1L]))

  Z <- as.matrix(template$Z)
  storage.mode(Z) <- "double"
  z_resp <- rowsum(Z, group, reorder = FALSE) / n_i
  varying <- which(apply(z_resp, 2L, stats::sd) >
                     sqrt(.Machine$double.eps))
  if (!length(varying)) {
    stop("The oracle requires at least one varying response-free moderator.",
         call. = FALSE)
  }
  z_raw <- z_resp[, varying[[1L]]]
  z <- as.numeric((z_raw - mean(z_raw)) / stats::sd(z_raw))
  ## The first respondent-varying moderator is centered at its empirical
  ## respondent mean before scaling.  Thus z > 0 is the fixed above-mean
  ## subgroup and z <= 0 is its complement.  This remains meaningful for a
  ## binary first moderator, unlike a strict above-median split.
  subgroup <- z > 0
  if (!any(subgroup) || all(subgroup)) {
    stop("The frozen above-mean subgroup is empty or universal.",
         call. = FALSE)
  }
  order_in_respondent <- integer(length(group))
  split_rows <- split(seq_along(group), group)
  for (rows in split_rows) order_in_respondent[rows] <- seq_along(rows)
  list(
    design_id = as.character(template$design_id), n_respondents = length(id),
    n_task_rows = length(group), respondent_id = id, group = group,
    task_count = n_i, split_rows = split_rows,
    task_id = as.character(template$task_id),
    order_in_respondent = order_in_respondent,
    z_respondent = z, z_task = z[group], subgroup = subgroup,
    r = r, s = s, d_regular = d_regular, d_weak = d_weak,
    eigenvalues = pmax(eig$values, 0), active_columns = which(dx$active),
    response_free_hash = if (!is.null(template$hashes))
      unname(template$hashes[["response_free_payload_hash"]]) else
        NA_character_)
}

.scmix_v23_oracle_theta <- function(config) {
  c(kappa = config$kappa,
    direction_intercept = config$direction_intercept,
    direction_moderator_slope = config$direction_moderator_slope,
    loading = config$loading,
    numerator_intercept = config$numerator_intercept,
    numerator_moderator_slope = config$numerator_moderator_slope,
    denominator_intercept = config$denominator_intercept,
    denominator_moderator_slope = config$denominator_moderator_slope)
}

.scmix_v23_oracle_rows <- function(prep, config) {
  weak_weight <- min(max(config$d_weak_weight, 0), 1)
  d <- (1 - weak_weight) * prep$d_regular + weak_weight * prep$d_weak
  list(r = prep$r, s = prep$s, d = d, z = prep$z_task)
}

.scmix_v23_oracle_index <- function(theta, rows, u) {
  fixed <- theta[["kappa"]] +
    rows$r * (theta[["direction_intercept"]] +
                theta[["direction_moderator_slope"]] * rows$z) +
    rows$s * (theta[["numerator_intercept"]] +
                theta[["numerator_moderator_slope"]] * rows$z) +
    rows$d * (theta[["denominator_intercept"]] +
                theta[["denominator_moderator_slope"]] * rows$z)
  outer(fixed, u, function(a, b) a + rows$r * theta[["loading"]] * b)
}

.scmix_v23_oracle_grid <- function(config, truth = FALSE) {
  if (!isTRUE(truth) || identical(config$residual, "normal")) {
    n <- if (isTRUE(truth)) config$truth_normal_nodes else config$model_nodes
    grid <- .sc_gh_grid(1L, n_nodes = n)
    return(list(u = grid$U[, 1L], w = grid$w, kind = "normal_gh"))
  }
  n <- config$truth_nonnormal_quantiles
  probability <- (seq_len(n) - .5) / n
  if (identical(config$residual, "student_t5_unit_variance")) {
    df <- 5
    return(list(u = stats::qt(probability, df = df) * sqrt((df - 2) / df),
                w = rep(1 / n, n), kind = paste0("student_t", df,
                                                  "_midquantile")))
  }
  if (identical(config$residual, "centered_chisq3_unit_variance")) {
    return(list(u = (stats::qchisq(probability, df = 3) - 3) / sqrt(6),
                w = rep(1 / n, n), kind = "centered_chisq3_midquantile"))
  }
  if (identical(config$residual,
                "symmetric_bimodal_mu0p85_unit_variance")) {
    ## Pairing antithetically preserves the equal mixture and exact finite-grid
    ## symmetry.  For an odd registry count, the last zero node has zero weight.
    half <- floor(n / 2)
    base <- stats::qnorm((seq_len(half) - .5) / half)
    u <- c(-.85 + sqrt(.2775) * base,
           .85 - sqrt(.2775) * base)
    w <- rep(.5 / half, 2L * half)
    if (length(u) < n) {
      u <- c(u, 0)
      w <- c(w, 0)
    }
    return(list(u = u, w = w,
                kind = "symmetric_bimodal_mu0p85_midquantile"))
  }
  stop("No frozen truth grid exists for residual distribution `",
       config$residual, "`.", call. = FALSE)
}

.scmix_v23_oracle_row_probabilities <- function(
    prep, config, theta = .scmix_v23_oracle_theta(config), truth = FALSE,
    chunk_size = 512L) {
  rows <- .scmix_v23_oracle_rows(prep, config)
  grid <- .scmix_v23_oracle_grid(config, truth = truth)
  n <- prep$n_task_rows
  out <- numeric(n)
  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  for (idx in chunks) {
    rr <- lapply(rows, `[`, idx)
    eta <- .scmix_v23_oracle_index(theta, rr, grid$u)
    out[idx] <- as.numeric(stats::plogis(eta) %*% grid$w)
  }
  out
}

.scmix_v23_oracle_respondent_mean <- function(x, prep, subgroup = NULL) {
  means <- rowsum(x, prep$group, reorder = FALSE) / prep$task_count
  means <- as.numeric(means)
  if (!is.null(subgroup)) means <- means[subgroup]
  mean(means)
}

.scmix_v23_oracle_target_values <- function(
    prep, config, theta = .scmix_v23_oracle_theta(config),
    truth_distribution = FALSE) {
  probability <- .scmix_v23_oracle_row_probabilities(
    prep, config, theta, truth = truth_distribution)
  z <- prep$z_respondent
  mean_direction <- theta[["direction_intercept"]] +
    theta[["direction_moderator_slope"]] * z
  mean_denominator <- theta[["denominator_intercept"]] +
    theta[["denominator_moderator_slope"]] * z
  mean_compensating <- mean_direction + .5 * mean_denominator
  if (isTRUE(truth_distribution) && !identical(config$residual, "normal")) {
    grid <- .scmix_v23_oracle_grid(config, truth = TRUE)
    direction <- mean(vapply(mean_direction, function(value)
      sum(grid$w * (value + theta[["loading"]] * grid$u > 0)), numeric(1L)))
    compensating <- mean(vapply(mean_compensating, function(value)
      sum(grid$w * (value + theta[["loading"]] * grid$u > 0)), numeric(1L)))
    residual_variance <- sum(grid$w *
      (grid$u - sum(grid$w * grid$u))^2)
  } else {
    loading <- abs(theta[["loading"]])
    direction <- if (loading <= .Machine$double.eps) {
      mean(mean_direction > 0)
    } else mean(stats::pnorm(mean_direction / loading))
    compensating <- if (loading <= .Machine$double.eps) {
      mean(mean_compensating > 0)
    } else mean(stats::pnorm(mean_compensating / loading))
    residual_variance <- 1
  }
  explained <- mean((theta[["direction_moderator_slope"]] *
                       (z - mean(z)))^2)
  residual <- theta[["loading"]]^2 * residual_variance
  numerator <- mean(theta[["numerator_intercept"]] +
                      theta[["numerator_moderator_slope"]] * z)
  denominator <- mean(theta[["denominator_intercept"]] +
                        theta[["denominator_moderator_slope"]] * z)
  c(
    unrestricted_mean_direction = mean(mean_direction),
    probability = .scmix_v23_oracle_respondent_mean(probability, prep),
    subgroup_share__z_high = .scmix_v23_oracle_respondent_mean(
      probability, prep, prep$subgroup),
    subgroup_share__z_low = .scmix_v23_oracle_respondent_mean(
      probability, prep, !prep$subgroup),
    preference_direction_share = direction,
    compensating_benefit_share = compensating,
    directional_heterogeneity = explained / (explained + residual),
    mean_tradeoff_mrs_ratio = -numerator / denominator)
}

#' Compute outcome-free Step-3 oracle truths
#'
#' Truths are finite-population functionals of the signed response-free design
#' and the frozen task-level DGP.  Calling this function neither generates nor
#' reads an application outcome.
#'
#' @param designs One signed v2.3 template or a list of templates.
#' @param regime_ids Frozen regime identifiers.
#' @param plan The signed prospective v2.3 plan.
#' @param require_frozen Whether every template must match the design manifest.
#' @return A data frame with one row per design, regime, and target.
#' @export
scmix_v23_oracle_truths <- function(
    designs, regime_ids = NULL, plan = scmix_v23_calibration_plan(),
    require_frozen = TRUE) {
  if (inherits(designs, "scmix_v23_design_template") ||
      (is.list(designs) && all(c("deltaX", "Z", "respondent_id", "task_id") %in%
                                names(designs)))) designs <- list(designs)
  if (!is.list(designs) || !length(designs)) {
    stop("`designs` must contain at least one v2.3 template.", call. = FALSE)
  }
  if (is.null(regime_ids)) regime_ids <- as.character(plan$regimes$regime_id)
  if (any(!regime_ids %in% plan$regimes$regime_id) ||
      anyDuplicated(regime_ids)) {
    stop("`regime_ids` must be unique IDs in the frozen registry.",
         call. = FALSE)
  }
  prepared <- lapply(designs, .scmix_v23_oracle_prepare,
                     require_frozen = require_frozen)
  cells <- if (!is.null(plan$severity)) {
    plan$severity[plan$severity$regime_id %in% regime_ids, , drop = FALSE]
  } else data.frame(regime_id = regime_ids, severity_id = "registry_default",
                    stringsAsFactors = FALSE)
  result <- list()
  for (d in seq_along(prepared)) for (cell in seq_len(nrow(cells))) {
    regime_id <- as.character(cells$regime_id[[cell]])
    severity_id <- as.character(cells$severity_id[[cell]])
    config <- .scmix_v23_oracle_config(plan, regime_id, severity_id)
    value <- .scmix_v23_oracle_target_values(
      prepared[[d]], config, truth_distribution = TRUE)
    entry <- data.frame(
      design_id = prepared[[d]]$design_id, regime_id = regime_id,
      severity_id = severity_id,
      certification_truth = config$certification_truth,
      severity_role = config$severity_role,
      target_key = names(value),
      target_id = unname(.scmix_v23_oracle_target_keys[names(value)]),
      variant_id = unname(.scmix_v23_oracle_variants[names(value)]),
      truth = unname(value),
      truth_conditioning = "signed_response_free_empirical_design",
      application_outcomes_used = FALSE, stringsAsFactors = FALSE)
    entry$primitive_a_truth <- NA_real_
    entry$primitive_b_truth <- NA_real_
    for (key in c("subgroup_share__z_high", "subgroup_share__z_low")) {
      j <- match(key, entry$target_key)
      b_value <- if (identical(key, "subgroup_share__z_high"))
        mean(prepared[[d]]$subgroup) else mean(!prepared[[d]]$subgroup)
      entry$primitive_b_truth[[j]] <- b_value
      entry$primitive_a_truth[[j]] <- b_value * entry$truth[[j]]
    }
    result[[length(result) + 1L]] <- entry
  }
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out
}

## Actual task-level generator used by the numerical layer and by the oracle
## face-validity audit.  Its only response variable is generated after the
## response-free template has passed its signature check.
.scmix_v23_oracle_generate <- function(
    template, regime_id, seed, plan = scmix_v23_calibration_plan(),
    severity_id = NULL, require_frozen = TRUE) {
  prep <- .scmix_v23_oracle_prepare(template, require_frozen)
  config <- .scmix_v23_oracle_config(plan, regime_id, severity_id)
  theta <- .scmix_v23_oracle_theta(config)
  rows <- .scmix_v23_oracle_rows(prep, config)
  set.seed(as.integer(seed))
  u <- switch(
    config$residual,
    normal = stats::rnorm(prep$n_respondents),
    student_t5_unit_variance = stats::rt(
      prep$n_respondents, df = 5) * sqrt(3 / 5),
    centered_chisq3_unit_variance =
      (stats::rchisq(prep$n_respondents, df = 3) - 3) / sqrt(6),
    symmetric_bimodal_mu0p85_unit_variance =
      sample(c(-.85, .85), prep$n_respondents, replace = TRUE) +
        stats::rnorm(prep$n_respondents, sd = sqrt(.2775)),
    stop("Unsupported frozen residual distribution `", config$residual,
         "`.", call. = FALSE))
  fixed <- theta[["kappa"]] +
    rows$r * (theta[["direction_intercept"]] +
                theta[["direction_moderator_slope"]] * rows$z) +
    rows$s * (theta[["numerator_intercept"]] +
                theta[["numerator_moderator_slope"]] * rows$z) +
    rows$d * (theta[["denominator_intercept"]] +
                theta[["denominator_moderator_slope"]] * rows$z)
  eta <- fixed + rows$r * theta[["loading"]] * u[prep$group]
  copula_normal <- rep(NA_real_, prep$n_task_rows)
  if (config$serial_rho != 0) {
    for (idx in prep$split_rows) {
      innovation <- stats::rnorm(length(idx))
      copula_normal[idx[[1L]]] <- innovation[[1L]]
      if (length(idx) > 1L) for (j in 2:length(idx)) {
        copula_normal[idx[[j]]] <-
          config$serial_rho * copula_normal[idx[[j - 1L]]] +
          sqrt(1 - config$serial_rho^2) * innovation[[j]]
      }
    }
  }
  probability <- stats::plogis(eta)
  y <- if (config$serial_rho == 0) {
    stats::rbinom(length(probability), 1L, probability)
  } else as.integer(stats::pnorm(copula_normal) <= probability)
  row_index <- seq_along(y)
  selected_source <- seq_len(prep$n_respondents)
  if (isTRUE(config$informative_completion)) {
    difficulty <- vapply(prep$split_rows, function(idx) mean(abs(eta[idx])),
                         numeric(1L))
    log_weight <- config$completion_intercept + config$completion_latent * u +
      config$completion_difficulty * difficulty
    selected_source <- integer(0)
    for (tasks in sort(unique(prep$task_count))) {
      members <- which(prep$task_count == tasks)
      weight <- exp(log_weight[members] - max(log_weight[members]))
      selected_source <- c(selected_source, sample(
        members, length(members), replace = TRUE, prob = weight))
    }
    ## Stable order makes the exact T_i histogram and duplicate provenance
    ## auditable while leaving the PPS sample itself random.
    selected_source <- selected_source[order(
      prep$task_count[selected_source], seq_along(selected_source))]
    row_index <- unlist(prep$split_rows[selected_source], use.names = FALSE)
  }
  selected_group <- if (isTRUE(config$informative_completion)) {
    rep(seq_along(selected_source), prep$task_count[selected_source])
  } else prep$group[row_index]
  list(
    design_id = prep$design_id, regime_id = regime_id,
    severity_id = config$severity_id,
    certification_truth = config$certification_truth,
    seed = as.integer(seed),
    respondent_id = sprintf("synthetic_%07d", selected_group),
    source_respondent_id = prep$respondent_id[prep$group[row_index]],
    selected_source_respondent = selected_source,
    source_row_index = row_index, task_id = prep$task_id[row_index],
    y = y[row_index], observed = rep(TRUE, length(row_index)),
    synthetic_probability = probability[row_index], latent_u = u,
    serial_copula_normal = copula_normal[row_index], theta = theta,
    truth = .scmix_v23_oracle_target_values(
      prep, config, theta, truth_distribution = TRUE),
    response_free_hash = prep$response_free_hash,
    application_outcomes_used = FALSE,
    completion_mechanism = if (isTRUE(config$informative_completion))
      "finite_population_PPS_with_replacement_within_exact_Ti_strata" else
        "complete_frozen_design",
    serial_mechanism = if (config$serial_rho != 0)
      "stationary_Gaussian_AR1_copula_preserving_task_marginals" else
        "conditionally_independent_Bernoulli_logit",
    generator = "task_level_q1_mixed_logit_v2.3")
}

## One observed-sequence score calculation under the maintained normal model.
## This is intentionally an audit helper rather than the production fast
## engine.  It lets tests and calibration provenance compare empirical
## task-level Bernoulli experiments with the local-Gaussian approximation.
.scmix_v23_oracle_sequence_step <- function(problem, generated) {
  prep <- problem$prep
  if (!identical(generated$source_row_index, seq_len(prep$n_task_rows))) {
    stop("The direct sequence-score audit currently requires the complete, ",
         "unduplicated design. PPS completion is checked by its dedicated ",
         "generator invariants.", call. = FALSE)
  }
  config <- problem$config
  theta <- problem$theta
  rows_all <- .scmix_v23_oracle_rows(prep, config)
  grid <- .scmix_v23_oracle_grid(config, truth = FALSE)
  score <- matrix(0, prep$n_respondents, length(theta),
                  dimnames = list(NULL, names(theta)))
  used <- logical(prep$n_respondents)
  for (i in seq_along(prep$split_rows)) {
    idx <- prep$split_rows[[i]]
    idx <- idx[generated$observed[idx]]
    if (!length(idx)) next
    used[[i]] <- TRUE
    rows <- lapply(rows_all, `[`, idx)
    eta <- .scmix_v23_oracle_index(theta, rows, grid$u)
    p <- stats::plogis(eta)
    y <- generated$y[idx]
    softplus <- pmax(eta, 0) + log1p(exp(-abs(eta)))
    log_likelihood <- colSums(y * eta - softplus)
    log_weight <- log(grid$w) + log_likelihood
    posterior <- exp(log_weight - max(log_weight))
    posterior <- posterior / sum(posterior)
    for (g in seq_along(grid$u)) {
      derivative <- cbind(
        1, rows$r, rows$r * rows$z, rows$r * grid$u[[g]],
        rows$s, rows$s * rows$z, rows$d, rows$d * rows$z)
      score[i, ] <- score[i, ] + posterior[[g]] *
        colSums((y - p[, g]) * derivative)
    }
  }
  mean_score <- colMeans(score[used, , drop = FALSE])
  theta_hat <- theta + as.numeric(problem$Hplus %*% mean_score)
  names(theta_hat) <- names(theta)
  list(
    theta_hat = theta_hat, mean_score = mean_score,
    respondent_scores = score[used, , drop = FALSE],
    respondents_used = sum(used), task_rows_used = sum(generated$observed),
    application_outcomes_used = FALSE,
    estimator = "one_integrated_sequence_score_step_at_frozen_reference")
}

.scmix_v23_oracle_model_moments <- function(prep, config, theta) {
  rows <- .scmix_v23_oracle_rows(prep, config)
  grid <- .scmix_v23_oracle_grid(config, truth = FALSE)
  eta <- .scmix_v23_oracle_index(theta, rows, grid$u)
  probability <- stats::plogis(eta)
  slope <- probability * (1 - probability)
  m <- as.numeric(probability %*% grid$w)
  w0 <- as.numeric(slope %*% grid$w)
  w1 <- as.numeric((slope * rep(grid$u, each = nrow(slope))) %*% grid$w)
  derivative <- cbind(
    kappa = w0,
    direction_intercept = rows$r * w0,
    direction_moderator_slope = rows$r * rows$z * w0,
    loading = rows$r * w1,
    numerator_intercept = rows$s * w0,
    numerator_moderator_slope = rows$s * rows$z * w0,
    denominator_intercept = rows$d * w0,
    denominator_moderator_slope = rows$d * rows$z * w0)
  list(m = m, derivative = derivative, probability = probability,
       grid = grid, rows = rows)
}

.scmix_v23_oracle_true_probability_matrix <- function(
    prep, config, theta, idx, max_nodes = 257L) {
  rows <- lapply(.scmix_v23_oracle_rows(prep, config), `[`, idx)
  grid <- .scmix_v23_oracle_grid(config, truth = TRUE)
  if (length(grid$u) > max_nodes) {
    take <- unique(round(seq(1, length(grid$u), length.out = max_nodes)))
    grid$u <- grid$u[take]
    grid$w <- grid$w[take] / sum(grid$w[take])
  }
  list(P = stats::plogis(.scmix_v23_oracle_index(theta, rows, grid$u)),
       u = grid$u, w = grid$w, rows = rows)
}

.scmix_v23_oracle_psd <- function(x, tolerance = 0) {
  x <- (x + t(x)) / 2
  eig <- eigen(x, symmetric = TRUE)
  value <- pmax(eig$values, tolerance)
  tcrossprod(sweep(eig$vectors, 2L, sqrt(value), `*`))
}

.scmix_v23_oracle_problem <- function(
    template, regime_id, plan = scmix_v23_calibration_plan(),
    severity_id = NULL, require_frozen = TRUE) {
  prep <- .scmix_v23_oracle_prepare(template, require_frozen)
  config <- .scmix_v23_oracle_config(plan, regime_id, severity_id)
  theta <- .scmix_v23_oracle_theta(config)
  model <- .scmix_v23_oracle_model_moments(prep, config, theta)
  k <- length(theta)
  H <- matrix(0, k, k, dimnames = list(names(theta), names(theta)))
  B <- H
  score_bias <- stats::setNames(numeric(k), names(theta))
  true_marginal <- if (!identical(config$residual, "normal")) {
    .scmix_v23_oracle_row_probabilities(
      prep, config, theta, truth = TRUE)
  } else NULL

  ## Expected PPS tilting used only for the fast local approximation.  The
  ## task generator below performs the frozen finite-population sampling
  ## exactly.  Here the random stratum denominator is replaced by its
  ## design-stratum expectation, which is recorded and never treated as an
  ## exact finite-sample calculation.
  pps_factor <- rep(1, prep$n_respondents)
  pps_node_weight <- vector("list", prep$n_respondents)
  if (isTRUE(config$informative_completion)) {
    propensity <- numeric(prep$n_respondents)
    for (i in seq_along(prep$split_rows)) {
      idx <- prep$split_rows[[i]]
      truth <- .scmix_v23_oracle_true_probability_matrix(
        prep, config, theta, idx)
      index <- .scmix_v23_oracle_index(theta, truth$rows, truth$u)
      log_tilt <- config$completion_latent * truth$u +
        config$completion_difficulty * colMeans(abs(index))
      tilt <- truth$w * exp(log_tilt - max(log_tilt))
      propensity[[i]] <- sum(tilt) * exp(max(log_tilt))
      pps_node_weight[[i]] <- tilt / sum(tilt)
    }
    for (tasks in unique(prep$task_count)) {
      members <- which(prep$task_count == tasks)
      pps_factor[members] <- propensity[members] / mean(propensity[members])
    }
  }

  for (i in seq_along(prep$split_rows)) {
    idx <- prep$split_rows[[i]]
    m <- model$m[idx]
    derivative <- model$derivative[idx, , drop = FALSE]
    variance <- pmax(m * (1 - m), 1e-10)
    A <- sweep(derivative, 1L, variance, `/`)
    truth <- .scmix_v23_oracle_true_probability_matrix(
      prep, config, theta, idx)
    P <- truth$P
    w <- truth$w
    m_true <- if (is.null(true_marginal)) as.numeric(P %*% w) else
      true_marginal[idx]

    if (isTRUE(config$informative_completion)) {
      w <- pps_node_weight[[i]]
      m_true <- as.numeric(P %*% w)
      factor <- pps_factor[[i]]
      H <- H + factor *
        crossprod(sweep(derivative, 1L, 1 / sqrt(variance), `*`))
      centered <- sweep(P, 1L, m, `-`)
      weighted_centered <- sweep(centered, 2L, sqrt(w), `*`)
      off <- tcrossprod(weighted_centered)
      diag_second <- as.numeric((P * (1 - P) + centered^2) %*% w)
      diag(off) <- diag_second
      mu <- as.numeric(centered %*% w)
      covariance <- off - tcrossprod(mu)
      mean_score <- factor * colSums(A * mu)
    } else {
      H <- H + crossprod(sweep(derivative, 1L, 1 / sqrt(variance), `*`))
      weighted <- sweep(P, 2L, sqrt(w), `*`)
      m_joint <- as.numeric(P %*% w)
      covariance <- tcrossprod(weighted) - tcrossprod(m_joint)
      diag(covariance) <- m_true * (1 - m_true)
      mean_score <- colSums(A * (m_true - m))
      if (config$serial_rho != 0 && length(idx) > 1L) {
        lag <- abs(outer(seq_along(idx), seq_along(idx), `-`))
        copula_correlation <- config$serial_rho^lag
        threshold_density <- stats::dnorm(stats::qnorm(
          pmin(pmax(P, 1e-10), 1 - 1e-10)))
        weighted_density <- sweep(threshold_density, 2L, sqrt(w), `*`)
        covariance <- covariance +
          tcrossprod(weighted_density) * copula_correlation
        diag(covariance) <- m_true * (1 - m_true)
      }
    }
    covariance <- .scmix_v23_oracle_psd(covariance)
    B <- B + if (isTRUE(config$informative_completion))
      pps_factor[[i]] * crossprod(A, covariance %*% A) else
        crossprod(A, covariance %*% A)
    score_bias <- score_bias + mean_score
  }
  N <- prep$n_respondents
  H <- H / N
  B <- B / N
  score_bias <- score_bias / N
  eig <- eigen((H + t(H)) / 2, symmetric = TRUE)
  tolerance <- max(dim(H)) * .Machine$double.eps * max(eig$values)
  keep <- eig$values > tolerance
  Hplus <- if (any(keep)) {
    eig$vectors[, keep, drop = FALSE] %*%
      diag(1 / eig$values[keep], nrow = sum(keep)) %*%
      t(eig$vectors[, keep, drop = FALSE])
  } else matrix(0, k, k)
  sandwich <- .scmix_v23_oracle_psd(Hplus %*% B %*% Hplus)
  model_truth <- .scmix_v23_oracle_target_values(
    prep, config, theta, truth_distribution = FALSE)
  dgp_truth <- .scmix_v23_oracle_target_values(
    prep, config, theta, truth_distribution = TRUE)
  list(
    prep = prep, config = config, theta = theta, H = H, B = B,
    Hplus = Hplus, sandwich = sandwich, score_bias = score_bias,
    center = theta + as.numeric(Hplus %*% score_bias),
    eigenvalues = eig$values, rank_tolerance = tolerance,
    numerical_rank = sum(keep), null_vectors = eig$vectors[, !keep, drop = FALSE],
    model_truth = model_truth, truth = dgp_truth,
    engine = "respondent_clustered_marginal_composite_Godambe",
    local_gaussian = TRUE,
    sequence_dependence_in_meat = TRUE,
    serial_covariance_uses_first_order_logit_normal_augmentation =
      FALSE,
    serial_covariance_uses_first_order_gaussian_copula_expansion =
      config$serial_rho != 0,
    completion_local_approximation = if (isTRUE(config$informative_completion))
      "expected_stratum_denominator_PPS_tilt" else "not_applicable")
}

.scmix_v23_oracle_target_map <- function(problem, theta) {
  ## The local experiment uses a second-order target map.  The derivatives are
  ## calculated once from the exact response-free mixed-logit functional, not
  ## re-estimated from synthetic responses.
  .scmix_v23_oracle_target_values(
    problem$prep, problem$config, theta, truth_distribution = FALSE)
}

.scmix_v23_oracle_derivatives <- function(problem) {
  theta <- problem$theta
  k <- length(theta)
  h <- 2e-4 * pmax(1, abs(theta))
  base <- .scmix_v23_oracle_target_map(problem, theta)
  gradient <- matrix(0, length(base), k,
                     dimnames = list(names(base), names(theta)))
  hessian <- array(0, c(length(base), k, k),
                   dimnames = list(names(base), names(theta), names(theta)))
  plus <- minus <- vector("list", k)
  for (j in seq_len(k)) {
    tp <- tm <- theta
    tp[j] <- tp[j] + h[j]
    tm[j] <- tm[j] - h[j]
    plus[[j]] <- .scmix_v23_oracle_target_map(problem, tp)
    minus[[j]] <- .scmix_v23_oracle_target_map(problem, tm)
    gradient[, j] <- (plus[[j]] - minus[[j]]) / (2 * h[j])
    hessian[, j, j] <- (plus[[j]] - 2 * base + minus[[j]]) / h[j]^2
  }
  for (j in seq_len(k - 1L)) for (l in (j + 1L):k) {
    pp <- pm <- mp <- mm <- theta
    pp[c(j, l)] <- pp[c(j, l)] + h[c(j, l)]
    pm[j] <- pm[j] + h[j]; pm[l] <- pm[l] - h[l]
    mp[j] <- mp[j] - h[j]; mp[l] <- mp[l] + h[l]
    mm[c(j, l)] <- mm[c(j, l)] - h[c(j, l)]
    cross <- (.scmix_v23_oracle_target_map(problem, pp) -
                .scmix_v23_oracle_target_map(problem, pm) -
                .scmix_v23_oracle_target_map(problem, mp) +
                .scmix_v23_oracle_target_map(problem, mm)) /
      (4 * h[j] * h[l])
    hessian[, j, l] <- hessian[, l, j] <- cross
  }
  list(value = base, gradient = gradient, hessian = hessian)
}

.scmix_v23_oracle_mvn <- function(covariance) {
  eig <- eigen((covariance + t(covariance)) / 2, symmetric = TRUE)
  value <- pmax(eig$values, 0)
  as.numeric(eig$vectors %*% (sqrt(value) * stats::rnorm(length(value))))
}

.scmix_v23_oracle_fieller <- function(numerator, denominator, covariance,
                                      z) {
  vnn <- covariance[1L, 1L]
  vdd <- covariance[2L, 2L]
  vnd <- covariance[1L, 2L]
  A <- denominator^2 - z^2 * vdd
  B <- -2 * (numerator * denominator - z^2 * vnd)
  C <- numerator^2 - z^2 * vnn
  discriminant <- B^2 - 4 * A * C
  if (!is.finite(discriminant) || discriminant < 0 || A <= 0) {
    return(c(lower = -Inf, upper = Inf, empty = 0, uninformative = 1))
  }
  roots <- sort(c((-B - sqrt(discriminant)) / (2 * A),
                  (-B + sqrt(discriminant)) / (2 * A)))
  c(lower = roots[[1L]], upper = roots[[2L]], empty = 0, uninformative = 0)
}

.scmix_v23_oracle_gate_map <- list(
  unrestricted_mean_direction = c(
    "active_eigenvalue_standardized_cutoff",
    "generalized_information_margin", "null_space_materiality_cutoff",
    "defect_cutoff", "ridge_cutoff"),
  probability = c("chart_discrepancy_cutoff", "interior_distance_cutoff",
                  "active_eigenvalue_standardized_cutoff",
                  "generalized_information_margin",
                  "null_space_materiality_cutoff", "defect_cutoff",
                  "ridge_cutoff"),
  subgroup_share = c("subgroup_interior_margin",
                     "nonlinear_remainder_cutoff",
                     "active_eigenvalue_standardized_cutoff",
                     "generalized_information_margin",
                     "null_space_materiality_cutoff", "defect_cutoff",
                     "ridge_cutoff"),
  preference_direction_share = c(
    "chart_discrepancy_cutoff", "interior_distance_cutoff",
    "active_eigenvalue_standardized_cutoff",
    "generalized_information_margin", "null_space_materiality_cutoff",
    "defect_cutoff", "ridge_cutoff"),
  compensating_benefit_share = c(
    "chart_discrepancy_cutoff", "interior_distance_cutoff",
    "active_eigenvalue_standardized_cutoff",
    "generalized_information_margin", "null_space_materiality_cutoff",
    "defect_cutoff", "ridge_cutoff"),
  directional_heterogeneity = c(
    "nonlinear_remainder_cutoff",
    "directional_heterogeneity_regularity_margin",
    "active_eigenvalue_standardized_cutoff",
    "directional_variance_standardized_cutoff",
    "total_heterogeneity_standardized_cutoff",
    "generalized_information_margin", "null_space_materiality_cutoff",
    "defect_cutoff", "ridge_cutoff"),
  mean_tradeoff_mrs_ratio = c(
    "mean_tradeoff_denominator_margin", "nonlinear_remainder_cutoff",
    "denominator_standardized_cutoff",
    "active_eigenvalue_standardized_cutoff",
    "generalized_information_margin", "null_space_materiality_cutoff",
    "defect_cutoff", "ridge_cutoff"))

## A regime-level severity label is not automatically an unsafe label for
## every target that happens to be reported in that regime.  These bindings
## state which target (or fit) the frozen perturbation was designed to stress.
## In particular, shrinking the residual loading does not shrink total
## directional heterogeneity while the moderator component remains nonzero,
## so the present DGP supplies no valid unsafe cell for the total-
## heterogeneity cutoff.  That constant must remain uncalibrated until a
## prospectively amended DGP supplies one.
.scmix_v23_oracle_targeted_failure_targets <- list(
  subgroup_interior_margin = "subgroup_share",
  directional_heterogeneity_regularity_margin = "directional_heterogeneity",
  mean_tradeoff_denominator_margin = "mean_tradeoff_mrs_ratio",
  chart_discrepancy_cutoff = "probability",
  nonlinear_remainder_cutoff = "subgroup_share",
  interior_distance_cutoff = "probability",
  active_eigenvalue_standardized_cutoff = .scmix_v23_oracle_targets,
  generalized_information_margin = "mean_tradeoff_mrs_ratio",
  directional_variance_standardized_cutoff = "directional_heterogeneity",
  total_heterogeneity_standardized_cutoff = character(),
  denominator_standardized_cutoff = "mean_tradeoff_mrs_ratio",
  null_space_materiality_cutoff = "mean_tradeoff_mrs_ratio",
  defect_cutoff = "mean_tradeoff_mrs_ratio",
  ridge_cutoff = "mean_tradeoff_mrs_ratio")

.scmix_v23_oracle_calibration_unit <- function(constant, target_key,
                                                target_id) {
  if (identical(constant, "active_eigenvalue_standardized_cutoff")) {
    return("fit_q1")
  }
  if (identical(target_id, "subgroup_share")) return("subgroup_share_joint")
  as.character(target_key)
}

.scmix_v23_oracle_gate_certification <- function(
    constant, target_id, regime_id, cell_certification_truth, registry) {
  adverse <- registry$targeted_failure_regime[
    match(constant, registry$constant)]
  targets <- .scmix_v23_oracle_targeted_failure_targets[[constant]]
  targeted <- length(adverse) == 1L && !is.na(adverse) &&
    identical(as.character(regime_id), as.character(adverse)) &&
    as.character(target_id) %in% targets
  if (identical(regime_id, "well_informed_interior")) return("safe")
  if (!targeted) return("not_targeted")
  as.character(cell_certification_truth)
}

.scmix_v23_oracle_target_certification <- function(
    target_id, regime_id, cell_certification_truth, registry) {
  constants <- names(Filter(function(targets)
    as.character(target_id) %in% targets,
    .scmix_v23_oracle_targeted_failure_targets))
  targeted <- any(registry$constant %in% constants &
                    registry$targeted_failure_regime == regime_id)
  if (identical(regime_id, "well_informed_interior")) return("safe")
  if (!targeted) return("not_targeted")
  as.character(cell_certification_truth)
}

.scmix_v23_oracle_active_eigen_statistic <- function(
    loading, loading_variance) {
  if (length(loading) != 1L || length(loading_variance) != 1L ||
      !is.finite(loading) || !is.finite(loading_variance) ||
      loading_variance < 0) {
    stop("Loading and loading variance must be finite, with nonnegative variance.",
         call. = FALSE)
  }
  eigenvalue <- loading^2
  standard_error <- sqrt(max(
    4 * loading^2 * loading_variance + 2 * loading_variance^2, 0))
  standardized <- if (standard_error <= 0) Inf else
    eigenvalue / standard_error
  c(eigenvalue = eigenvalue, standard_error = standard_error,
    standardized = standardized)
}

.scmix_v23_oracle_one <- function(problem, derivatives, seed, level) {
  set.seed(seed)
  N <- problem$prep$n_respondents
  theta_error <- .scmix_v23_oracle_mvn(problem$sandwich / N)
  theta_hat <- problem$center + theta_error
  delta <- theta_hat - problem$theta
  target_keys <- names(derivatives$value)
  estimate <- truth <- se <- stats::setNames(
    numeric(length(target_keys)), target_keys)
  lower <- upper <- estimate
  oracle_benchmark_length <- estimate
  empty <- uninformative <- covered <- rep(FALSE, length(estimate))
  names(empty) <- names(uninformative) <- names(covered) <- names(estimate)
  linear <- plugin <- chart <- estimate
  zcrit <- stats::qnorm(1 - (1 - level) / 2)
  Vtheta <- problem$sandwich / N

  for (target in names(estimate)) {
    gradient <- derivatives$gradient[target, ]
    curvature <- derivatives$hessian[target, , ]
    linear[[target]] <- derivatives$value[[target]] + sum(gradient * delta)
    plugin[[target]] <- linear[[target]] +
      .5 * as.numeric(crossprod(delta, curvature %*% delta))
    se[[target]] <- sqrt(max(as.numeric(
      crossprod(gradient, Vtheta %*% gradient)), 0))
    oracle_benchmark_length[[target]] <- 2 * zcrit * se[[target]]
    truth[[target]] <- problem$truth[[target]]
    if (identical(target, "directional_heterogeneity")) {
      z_variance <- mean((problem$prep$z_respondent -
                            mean(problem$prep$z_respondent))^2)
      hz <- theta_hat[["direction_moderator_slope"]]^2 * z_variance
      hr <- theta_hat[["loading"]]^2
      ghz_local <- stats::setNames(rep(0, length(theta_hat)), names(theta_hat))
      ghr_local <- ghz_local
      ghz_local[["direction_moderator_slope"]] <-
        2 * theta_hat[["direction_moderator_slope"]] * z_variance
      ghr_local[["loading"]] <- 2 * theta_hat[["loading"]]
      primitive_gradient <- rbind(ghz_local, ghr_local)
      primitive_covariance <- primitive_gradient %*% Vtheta %*%
        t(primitive_gradient)
      primitive <- pmax(c(hz, hr), sqrt(.Machine$double.eps))
      log_se <- sqrt(pmax(diag(primitive_covariance), 0)) / primitive
      joint_z <- stats::qnorm(1 - (1 - level) / 4)
      primitive_lower <- primitive * exp(-joint_z * log_se)
      primitive_upper <- primitive * exp(joint_z * log_se)
      estimate[[target]] <- hz / max(hz + hr, .Machine$double.eps)
      chart[[target]] <- estimate[[target]]
      lower[[target]] <- primitive_lower[[1L]] /
        (primitive_lower[[1L]] + primitive_upper[[2L]])
      upper[[target]] <- primitive_upper[[1L]] /
        (primitive_upper[[1L]] + primitive_lower[[2L]])
      uninformative[[target]] <- lower[[target]] <= 1e-8 &&
        upper[[target]] >= 1 - 1e-8
    } else if (!target %in% c("mean_tradeoff_mrs_ratio",
                              "unrestricted_mean_direction")) {
      base <- min(max(derivatives$value[[target]], 1e-10), 1 - 1e-10)
      correction <- (linear[[target]] - base) / (base * (1 - base))
      chart[[target]] <- stats::plogis(stats::qlogis(base) + correction)
      se_chart <- se[[target]] / (base * (1 - base))
      center_chart <- stats::qlogis(base) + correction
      lower[[target]] <- stats::plogis(center_chart - zcrit * se_chart)
      upper[[target]] <- stats::plogis(center_chart + zcrit * se_chart)
      estimate[[target]] <- chart[[target]]
      uninformative[[target]] <- lower[[target]] <= 1e-8 &&
        upper[[target]] >= 1 - 1e-8
    } else if (identical(target, "mean_tradeoff_mrs_ratio")) {
      zi <- problem$prep$z_respondent
      gnum <- stats::setNames(rep(0, length(problem$theta)),
                             names(problem$theta))
      gden <- gnum
      gnum[c("numerator_intercept", "numerator_moderator_slope")] <-
        c(1, mean(zi))
      gden[c("denominator_intercept", "denominator_moderator_slope")] <-
        c(1, mean(zi))
      num <- mean(theta_hat[["numerator_intercept"]] +
                    theta_hat[["numerator_moderator_slope"]] * zi)
      den <- mean(theta_hat[["denominator_intercept"]] +
                    theta_hat[["denominator_moderator_slope"]] * zi)
      joint <- rbind(gnum, gden) %*% Vtheta %*% t(rbind(gnum, gden))
      interval <- .scmix_v23_oracle_fieller(num, den, joint, zcrit)
      truth_num <- mean(problem$theta[["numerator_intercept"]] +
                          problem$theta[["numerator_moderator_slope"]] * zi)
      truth_den <- mean(problem$theta[["denominator_intercept"]] +
                          problem$theta[["denominator_moderator_slope"]] * zi)
      oracle_interval <- .scmix_v23_oracle_fieller(
        truth_num, truth_den, joint, zcrit)
      estimate[[target]] <- -num / den
      chart[[target]] <- linear[[target]]
      lower[[target]] <- -interval[["upper"]]
      upper[[target]] <- -interval[["lower"]]
      oracle_benchmark_length[[target]] <-
        oracle_interval[["upper"]] - oracle_interval[["lower"]]
      empty[[target]] <- as.logical(interval[["empty"]])
      uninformative[[target]] <- as.logical(interval[["uninformative"]])
    } else {
      estimate[[target]] <- linear[[target]]
      chart[[target]] <- linear[[target]]
      lower[[target]] <- estimate[[target]] - zcrit * se[[target]]
      upper[[target]] <- estimate[[target]] + zcrit * se[[target]]
    }
    covered[[target]] <- !empty[[target]] &&
      truth[[target]] >= lower[[target]] && truth[[target]] <= upper[[target]]
  }

  targets <- data.frame(
    target_key = names(estimate),
    target_id = unname(.scmix_v23_oracle_target_keys[names(estimate)]),
    variant_id = unname(.scmix_v23_oracle_variants[names(estimate)]),
    truth = unname(truth),
    estimate = unname(estimate), se = unname(se),
    lower = unname(lower), upper = unname(upper), covered = unname(covered),
    empty_constraint_set = unname(empty),
    uninformative_constraint_set = unname(uninformative),
    linear_estimate = unname(linear), quadratic_plugin = unname(plugin),
    oracle_benchmark_length = unname(oracle_benchmark_length),
    stringsAsFactors = FALSE)
  targets$reported_interval_length <- targets$upper - targets$lower
  targets$interval_length_ratio_to_oracle <-
    targets$reported_interval_length / targets$oracle_benchmark_length
  targets$primitive_a_truth <- targets$primitive_b_truth <-
    targets$primitive_a_estimate <- targets$primitive_b_estimate <- NA_real_
  targets$subgroup_constraint_scale <-
    targets$subgroup_constraint_margin_standardized <- NA_real_
  targets$subgroup_constraint_margin_raw <- NA_real_
  for (key in c("subgroup_share__z_high", "subgroup_share__z_low")) {
    j <- match(key, targets$target_key)
    b_value <- if (identical(key, "subgroup_share__z_high"))
      mean(problem$prep$subgroup) else mean(!problem$prep$subgroup)
    scale <- max(b_value * se[[key]], sqrt(.Machine$double.eps))
    targets$primitive_b_truth[[j]] <- b_value
    targets$primitive_a_truth[[j]] <- b_value * truth[[key]]
    targets$primitive_b_estimate[[j]] <- b_value
    targets$primitive_a_estimate[[j]] <- b_value * estimate[[key]]
    targets$subgroup_constraint_scale[[j]] <- 1
    targets$subgroup_constraint_margin_raw[[j]] <- min(
      targets$primitive_a_estimate[[j]], 1 - b_value,
      b_value - targets$primitive_a_estimate[[j]])
    targets$subgroup_constraint_margin_standardized[[j]] <- min(
      targets$primitive_a_estimate[[j]], 1 - b_value,
      b_value - targets$primitive_a_estimate[[j]]) / scale
  }
  targets$primitive_hz_truth <- targets$primitive_hr_truth <-
    targets$primitive_hz_estimate <- targets$primitive_hr_estimate <- NA_real_
  heterogeneity_row <- match("directional_heterogeneity", targets$target_key)
  z_variance <- mean((problem$prep$z_respondent -
                        mean(problem$prep$z_respondent))^2)
  truth_grid <- .scmix_v23_oracle_grid(problem$config, truth = TRUE)
  truth_u_mean <- sum(truth_grid$w * truth_grid$u)
  truth_u_variance <- sum(truth_grid$w *
    (truth_grid$u - truth_u_mean)^2)
  targets$primitive_hz_truth[[heterogeneity_row]] <-
    problem$theta[["direction_moderator_slope"]]^2 * z_variance
  targets$primitive_hr_truth[[heterogeneity_row]] <-
    problem$theta[["loading"]]^2 * truth_u_variance
  targets$primitive_hz_estimate[[heterogeneity_row]] <-
    theta_hat[["direction_moderator_slope"]]^2 * z_variance
  targets$primitive_hr_estimate[[heterogeneity_row]] <-
    theta_hat[["loading"]]^2
  targets$coverage_scope <- "local_Gaussian_scalar_or_joint_mapping"
  targets$coverage_scope[targets$target_id == "subgroup_share"] <-
    paste0("local_Gaussian_joint_a_b_mapping_with_conditional_design_",
           "b_fixed")
  targets$coverage_scope[
    targets$target_id == "directional_heterogeneity"] <-
    "local_Gaussian_joint_log_HZ_HR_rectangle_mapping"

  H <- problem$H
  B <- problem$B
  eig <- eigen((H + t(H)) / 2, symmetric = TRUE)
  machine_null <- eig$values <= problem$rank_tolerance
  rho <- sqrt(.Machine$double.eps) * max(eig$values)
  score_bar <- as.numeric(H %*% delta)
  names(score_bar) <- names(problem$theta)
  zresp <- problem$prep$z_respondent
  explained <- theta_hat[["direction_moderator_slope"]]^2 *
    mean((zresp - mean(zresp))^2)
  residual <- theta_hat[["loading"]]^2
  ghz <- stats::setNames(rep(0, length(problem$theta)), names(problem$theta))
  ghr <- ghz
  ghz[["direction_moderator_slope"]] <-
    2 * theta_hat[["direction_moderator_slope"]] *
    mean((zresp - mean(zresp))^2)
  ghr[["loading"]] <- 2 * theta_hat[["loading"]]
  se_hz <- sqrt(max(as.numeric(crossprod(ghz, Vtheta %*% ghz)), 0))
  se_hr <- sqrt(max(as.numeric(crossprod(ghr, Vtheta %*% ghr)), 0))
  se_total <- sqrt(max(as.numeric(
    crossprod(ghz + ghr, Vtheta %*% (ghz + ghr))), 0))
  ## G004 is a fit-level rank-separation diagnostic.  In the rank-one local
  ## experiment the active covariance eigenvalue is loading^2.  Its Gaussian
  ## sampling variance retains the second-order term, which is essential near
  ## zero and keeps this diagnostic distinct from the target-directional
  ## first-order H_R / se(H_R) diagnostic used by G006.
  loading_index <- match("loading", names(problem$theta))
  active <- .scmix_v23_oracle_active_eigen_statistic(
    theta_hat[["loading"]], Vtheta[loading_index, loading_index])
  gden <- stats::setNames(rep(0, length(problem$theta)), names(problem$theta))
  gden[c("denominator_intercept", "denominator_moderator_slope")] <-
    c(1, mean(zresp))
  denominator <- theta_hat[["denominator_intercept"]] +
    theta_hat[["denominator_moderator_slope"]] * mean(zresp)
  se_denominator <- sqrt(max(as.numeric(
    crossprod(gden, Vtheta %*% gden)), 0))
  ratio <- function(value, standard_error) {
    if (!is.finite(standard_error) || standard_error <= 0) Inf else
      abs(value) / standard_error
  }

  gate_rows <- list()
  for (target in target_keys) {
    target_id <- .scmix_v23_oracle_target_keys[[target]]
    gradient <- derivatives$gradient[target, ]
    norm_gradient <- sqrt(sum(gradient^2))
    variance_direction <- as.numeric(
      crossprod(gradient, problem$Hplus %*% gradient))
    information <- if (norm_gradient <= 0 || variance_direction <= 0) 0 else
      norm_gradient^2 / variance_direction
    null_materiality <- if (!any(machine_null) || norm_gradient <= 0) 0 else
      sqrt(sum((crossprod(eig$vectors[, machine_null, drop = FALSE],
                          gradient))^2)) / norm_gradient
    alpha <- solve(H + diag(rho, nrow(H)), gradient)
    alpha10 <- solve(H + diag(10 * rho, nrow(H)), gradient)
    defect_sd <- sqrt(max(as.numeric(crossprod(alpha, B %*% alpha)),
                          .Machine$double.eps))
    defect <- sqrt(sum((H %*% alpha - gradient)^2)) / defect_sd
    ridge <- abs(sum((alpha - alpha10) * score_bar)) /
      max(se[[target]], sqrt(.Machine$double.eps))
    subgroup_margin <- if (target %in% c(
        "subgroup_share__z_high", "subgroup_share__z_low")) {
      targets$subgroup_constraint_margin_raw[
        match(target, targets$target_key)]
    } else NA_real_
    metric <- c(
      subgroup_interior_margin = subgroup_margin,
      directional_heterogeneity_regularity_margin = min(
        explained, residual),
      mean_tradeoff_denominator_margin = abs(denominator),
      chart_discrepancy_cutoff = ratio(
        chart[[target]] - linear[[target]], se[[target]]),
      nonlinear_remainder_cutoff = ratio(
        plugin[[target]] - linear[[target]], se[[target]]),
      interior_distance_cutoff = ratio(
        min(estimate[[target]], 1 - estimate[[target]]), se[[target]]),
      active_eigenvalue_standardized_cutoff = active[["standardized"]],
      generalized_information_margin = information,
      directional_variance_standardized_cutoff = ratio(residual, se_hr),
      total_heterogeneity_standardized_cutoff = ratio(
        explained + residual, se_total),
      denominator_standardized_cutoff = ratio(denominator, se_denominator),
      null_space_materiality_cutoff = null_materiality,
      defect_cutoff = defect, ridge_cutoff = ridge)
    ids <- .scmix_v23_oracle_gate_map[[target_id]]
    gate_rows[[length(gate_rows) + 1L]] <- data.frame(
      target_key = target, target_id = target_id,
      variant_id = .scmix_v23_oracle_variants[[target]],
      constant = ids, value = unname(metric[ids]),
      stringsAsFactors = FALSE)
  }
  gates <- do.call(rbind, gate_rows)
  gates$calibration_unit_id <- vapply(seq_len(nrow(gates)), function(j)
    .scmix_v23_oracle_calibration_unit(
      gates$constant[[j]], gates$target_key[[j]], gates$target_id[[j]]),
    character(1L))
  gates$metric_scope <- ifelse(
    gates$constant == "active_eigenvalue_standardized_cutoff",
    "fit_q1", "target")
  gates$statistical_authority <- ifelse(
    gates$constant %in% c(
      "null_space_materiality_cutoff", "defect_cutoff", "ridge_cutoff"),
    "population_information_surrogate_requires_end_to_end_Riesz_validation",
    "marginal_local_Gaussian_diagnostic_only")
  list(targets = targets, gates = gates, theta_hat = theta_hat,
       finite = all(is.finite(theta_hat)) &&
         all(is.finite(gates$value) | is.infinite(gates$value)))
}

.scmix_v23_oracle_aggregate <- function(gates, registry) {
  out <- stats::setNames(rep(NA_real_, length(.scmix_v23_oracle_names)),
                         .scmix_v23_oracle_names)
  for (constant in names(out)) {
    value <- gates$value[gates$constant == constant]
    comparator <- registry$comparator[registry$constant == constant]
    if (!length(value) || length(comparator) != 1L) next
    out[[constant]] <- if (identical(comparator, "less_than_or_equal")) {
      max(value, na.rm = TRUE)
    } else min(value, na.rm = TRUE)
  }
  out
}

.scmix_v23_oracle_seed <- function(plan, stream, design_index, regime_index,
                                   severity_index, replication) {
  field <- paste0("oracle_", stream, "_seed")
  master <- plan$counts[[field]]
  if (is.null(master)) {
    raw_field <- if (identical(stream, "calibration"))
      "Oracle-Calibration-Master-Seed" else "Oracle-Validation-Master-Seed"
    master <- as.numeric(plan$raw[[raw_field]])
  }
  as.integer(master + (design_index - 1L) * 30000000 +
               (regime_index - 1L) * 2000000 +
               (severity_index - 1L) * 500000 + replication)
}

#' Run the design-matched local-Gaussian statistical oracle
#'
#' The experiment is derived from marginal task probabilities and the exact
#' respondent-cluster covariance induced by the persistent rank-one draw.
#' It is an oracle calibration of statistical geometry, not a substitute for
#' the separately required full-fit numerical and transport layers.
#'
#' @param designs Signed v2.3 response-free templates.
#' @param stream `"calibration"` or untouched `"validation"`.
#' @param regime_ids Frozen regimes to run.
#' @param replications `NULL` uses the prospective count. Any override requires
#'   `pilot = TRUE` and is ineligible for threshold selection.
#' @param plan Signed prospective v2.3 plan.
#' @param workers Fork workers, bounded by the plan.
#' @param pilot Mark a non-production smoke or face-validity run.
#' @return A signed run with wide replication metrics, long target results,
#'   and long target-specific gate metrics.
#' @export
scmix_v23_run_oracle_calibration <- function(
    designs, stream = c("calibration", "validation"), regime_ids = NULL,
    replications = NULL, plan = scmix_v23_calibration_plan(), workers = 1L,
    pilot = FALSE) {
  stream <- match.arg(stream)
  if (!isTRUE(pilot) && exists(
      ".scmix_v23_step3_intact", mode = "function", inherits = TRUE) &&
      !get(".scmix_v23_step3_intact", mode = "function", inherits = TRUE)(
        plan, "scmix_v23_calibration_plan")) {
    stop("A non-pilot oracle run requires the intact frozen plan.",
         call. = FALSE)
  }
  if (inherits(designs, "scmix_v23_design_template") ||
      (is.list(designs) && all(c("deltaX", "Z", "respondent_id", "task_id") %in%
                                names(designs)))) designs <- list(designs)
  if (!is.list(designs) || !length(designs)) {
    stop("`designs` must contain signed v2.3 templates.", call. = FALSE)
  }
  if (is.null(regime_ids)) regime_ids <- as.character(plan$regimes$regime_id)
  if (any(!regime_ids %in% plan$regimes$regime_id) ||
      anyDuplicated(regime_ids)) {
    stop("The requested regimes are not the frozen registry.", call. = FALSE)
  }
  maximum_workers <- if (!is.null(plan$counts[["maximum_workers"]]))
    plan$counts[["maximum_workers"]] else as.integer(plan$raw[["Maximum-Workers"]])
  workers <- as.integer(workers)
  if (!is.finite(workers) || workers < 1L || workers > maximum_workers) {
    stop("`workers` exceeds the frozen prospective bound.", call. = FALSE)
  }
  frozen_reps <- if (identical(stream, "calibration")) {
    if (!is.null(plan$counts[["oracle_calibration_reps"]]))
      plan$counts[["oracle_calibration_reps"]] else
        as.integer(plan$raw[["Oracle-Calibration-Replications-Per-Cell"]])
  } else {
    if (!is.null(plan$counts[["oracle_validation_reps"]]))
      plan$counts[["oracle_validation_reps"]] else
        as.integer(plan$raw[["Oracle-Validation-Replications-Per-Cell"]])
  }
  B <- if (is.null(replications)) frozen_reps else as.integer(replications)
  if (length(B) != 1L || !is.finite(B) || B < 1L ||
      (!isTRUE(pilot) && B != frozen_reps)) {
    stop("Replication overrides require `pilot = TRUE`.", call. = FALSE)
  }
  lapply(designs, .scmix_v23_oracle_check_template,
         require_frozen = !isTRUE(pilot))
  design_order <- match(vapply(designs, `[[`, character(1L), "design_id"),
                        plan$designs$design_id)
  if (anyNA(design_order)) stop("A design is absent from the frozen manifest.",
                                call. = FALSE)
  severity <- if (!is.null(plan$severity)) {
    plan$severity[plan$severity$regime_id %in% regime_ids, , drop = FALSE]
  } else data.frame(regime_id = regime_ids, severity_index = 1L,
                    severity_id = "registry_default",
                    severity_role = NA_character_, stringsAsFactors = FALSE)
  jobs <- merge(
    data.frame(design = seq_along(designs), .order = seq_along(designs)),
    transform(severity, .severity_order = seq_len(nrow(severity))),
    by = NULL, sort = FALSE)
  jobs <- jobs[order(jobs$.order, jobs$.severity_order), , drop = FALSE]
  run_cell <- function(j) {
    design <- designs[[jobs$design[[j]]]]
    regime_id <- jobs$regime_id[[j]]
    severity_id <- as.character(jobs$severity_id[[j]])
    severity_index <- as.integer(jobs$severity_index[[j]])
    d_index <- design_order[[jobs$design[[j]]]]
    r_index <- match(regime_id, plan$regimes$regime_id)
    problem <- .scmix_v23_oracle_problem(
      design, regime_id, plan, severity_id = severity_id,
      require_frozen = !isTRUE(pilot))
    derivatives <- .scmix_v23_oracle_derivatives(problem)
    reps <- targets <- gates <- vector("list", B)
    for (b in seq_len(B)) {
      seed <- .scmix_v23_oracle_seed(
        plan, stream, d_index, r_index, severity_index, b)
      one <- .scmix_v23_oracle_one(
        problem, derivatives, seed,
        level = if (!is.null(plan$rates[["nominal_coverage"]]))
          plan$rates[["nominal_coverage"]] else .95)
      aggregate <- .scmix_v23_oracle_aggregate(one$gates, plan$constants)
      reps[[b]] <- data.frame(
        design_id = problem$prep$design_id, regime_id = regime_id,
        severity_id = severity_id, severity_index = severity_index,
        certification_truth = problem$config$certification_truth,
        severity_role = problem$config$severity_role,
        replication = b, seed = seed, finite = one$finite,
        as.list(aggregate), check.names = FALSE, stringsAsFactors = FALSE)
      one$targets$design_id <- problem$prep$design_id
      one$targets$regime_id <- regime_id
      one$targets$severity_id <- severity_id
      one$targets$severity_index <- severity_index
      one$targets$cell_certification_truth <-
        problem$config$certification_truth
      one$targets$target_certification_truth <- vapply(
        one$targets$target_id, .scmix_v23_oracle_target_certification,
        character(1L), regime_id = regime_id,
        cell_certification_truth = problem$config$certification_truth,
        registry = plan$constants)
      ## Retain the historical field as the target-specific label.  The
      ## original severity-grid label is available explicitly above.
      one$targets$certification_truth <-
        one$targets$target_certification_truth
      one$targets$severity_role <- problem$config$severity_role
      one$targets$replication <- b
      one$targets$seed <- seed
      targets[[b]] <- one$targets
      one$gates$design_id <- problem$prep$design_id
      one$gates$regime_id <- regime_id
      one$gates$severity_id <- severity_id
      one$gates$severity_index <- severity_index
      one$gates$cell_certification_truth <- problem$config$certification_truth
      one$gates$target_certification_truth <- vapply(
        seq_len(nrow(one$gates)), function(k)
          .scmix_v23_oracle_gate_certification(
            one$gates$constant[[k]], one$gates$target_id[[k]], regime_id,
            problem$config$certification_truth, plan$constants),
        character(1L))
      one$gates$certification_truth <-
        one$gates$target_certification_truth
      one$gates$severity_role <- problem$config$severity_role
      one$gates$replication <- b
      one$gates$seed <- seed
      gates[[b]] <- one$gates
    }
    list(replications = do.call(rbind, reps), targets = do.call(rbind, targets),
         gates = do.call(rbind, gates),
         problem = list(
           design_id = problem$prep$design_id, regime_id = regime_id,
           severity_id = severity_id, severity_index = severity_index,
           certification_truth = problem$config$certification_truth,
           severity_role = problem$config$severity_role,
           information_eigenvalues = problem$eigenvalues,
           numerical_rank = problem$numerical_rank,
           rank_tolerance = problem$rank_tolerance,
           score_bias = problem$score_bias,
           engine = problem$engine,
           serial_covariance_approximation =
             problem$serial_covariance_uses_first_order_gaussian_copula_expansion,
           completion_local_approximation =
             problem$completion_local_approximation))
  }
  apply_fun <- function(X, FUN) {
    if (workers <= 1L || length(X) <= 1L || .Platform$OS.type == "windows")
      return(lapply(X, FUN))
    parallel::mclapply(X, FUN, mc.cores = min(workers, length(X)),
                       mc.preschedule = FALSE, mc.set.seed = FALSE)
  }
  cells <- apply_fun(seq_len(nrow(jobs)), run_cell)
  out <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    stream = stream, layer = "oracle_local_gaussian", pilot = isTRUE(pilot),
    engine = "respondent_clustered_marginal_composite_Godambe",
    design_ids = vapply(designs, `[[`, character(1L), "design_id"),
    regime_ids = regime_ids, replications_per_cell = B,
    frozen_replications_per_cell = frozen_reps,
    replications = do.call(rbind, lapply(cells, `[[`, "replications")),
    targets = do.call(rbind, lapply(cells, `[[`, "targets")),
    gates = do.call(rbind, lapply(cells, `[[`, "gates")),
    problems = lapply(cells, `[[`, "problem"),
    oracle_primary_constants = .scmix_v23_oracle_names,
    numerical_proxy_metrics_used = FALSE,
    application_outcomes_used = FALSE,
    production_manifest_created = FALSE,
    statistical_engine_production_authority = FALSE,
    production_promotion_permitted = FALSE,
    production_promotion_blocker = paste0(
      "marginal_local_Gaussian_oracle_requires_end_to_end_sequence_",
      "likelihood_and_Riesz_validation"),
    eligible_for_threshold_selection = FALSE,
    eligible_for_diagnostic_threshold_selection =
      identical(stream, "calibration") && !isTRUE(pilot) && B == frozen_reps)
  rownames(out$replications) <- rownames(out$targets) <-
    rownames(out$gates) <- NULL
  if (exists(".scmix_v23_step3_sign", mode = "function", inherits = TRUE)) {
    return(get(".scmix_v23_step3_sign", mode = "function", inherits = TRUE)(
      out, "scmix_v23_oracle_calibration_run"))
  }
  class(out) <- c("scmix_v23_oracle_calibration_run", "list")
  out
}

.scmix_v23_oracle_pass <- function(value, threshold, comparator) {
  ifelse(comparator == "less_than_or_equal", value <= threshold,
         value >= threshold)
}

.scmix_v23_oracle_wilson <- function(success, total, level = .95) {
  if (!is.finite(total) || total <= 0) return(c(lower = NA, upper = NA))
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- success / total
  center <- (p + z^2 / (2 * total)) / (1 + z^2 / total)
  half <- z * sqrt(p * (1 - p) / total + z^2 / (4 * total^2)) /
    (1 + z^2 / total)
  c(lower = max(0, center - half), upper = min(1, center + half))
}

.scmix_v23_oracle_validation_assessment <- function(targets, run, plan) {
  columns <- c(
    "criterion", "design_id", "regime_id", "severity_id", "target_id",
    "variant_id", "estimate", "lower", "upper", "numerator",
    "denominator", "passed", "detail")
  empty_result <- function() {
    out <- as.data.frame(stats::setNames(replicate(
      length(columns), logical(0L), simplify = FALSE), columns),
      stringsAsFactors = FALSE)
    out[c("criterion", "design_id", "regime_id", "severity_id",
          "target_id", "variant_id", "detail")] <- lapply(
            out[c("criterion", "design_id", "regime_id", "severity_id",
                  "target_id", "variant_id", "detail")], as.character)
    out[c("estimate", "lower", "upper", "numerator", "denominator")] <-
      lapply(out[c("estimate", "lower", "upper", "numerator",
                   "denominator")], as.numeric)
    out
  }
  if (!identical(run$stream, "validation") || isTRUE(run$pilot)) {
    return(list(criteria = empty_result(), all_statistical_criteria_pass = FALSE,
                assessed = FALSE,
                reason = "requires_full_untouched_validation_stream"))
  }
  rows <- list()
  add <- function(criterion, dat, estimate, ci = c(lower = NA, upper = NA),
                  numerator = NA_real_, denominator = NA_real_, passed,
                  detail) {
    rows[[length(rows) + 1L]] <<- data.frame(
      criterion = criterion, design_id = dat$design_id[[1L]],
      regime_id = dat$regime_id[[1L]],
      severity_id = dat$severity_id[[1L]],
      target_id = dat$target_id[[1L]], variant_id = dat$variant_id[[1L]],
      estimate = as.numeric(estimate), lower = as.numeric(ci[["lower"]]),
      upper = as.numeric(ci[["upper"]]), numerator = as.numeric(numerator),
      denominator = as.numeric(denominator), passed = isTRUE(passed),
      detail = detail, stringsAsFactors = FALSE)
  }
  binomial <- function(criterion, dat, event, denominator, rule, detail) {
    keep <- denominator %in% TRUE
    success <- sum(event %in% TRUE & keep)
    total <- sum(keep)
    ci <- .scmix_v23_oracle_wilson(success, total)
    estimate <- if (total) success / total else NA_real_
    add(criterion, dat, estimate, ci, success, total,
        rule(estimate, ci, total), detail)
  }

  ## Coverage and constraint geometry are assessed target by target in the
  ## well-informed cell.  The two subgroup variants carry the same joint
  ## coverage/release event, so retain only one copy for these criteria.
  well <- targets[targets$regime_id == "well_informed_interior", , drop = FALSE]
  well$validation_variant <- ifelse(
    well$target_id == "subgroup_share", "high_and_low_joint", well$variant_id)
  well_groups <- split(well, interaction(
    well$design_id, well$target_id, well$validation_variant, drop = TRUE))
  for (dat in well_groups) {
    if (identical(dat$target_id[[1L]], "subgroup_share")) {
      dat <- dat[!duplicated(dat$replication), , drop = FALSE]
      dat$variant_id <- "high_and_low_joint"
    }
    coverage_rule <- function(p, ci, n)
      n > 0 && is.finite(p) && p >= .94 && p <= .96 &&
      is.finite(ci[["lower"]]) && ci[["lower"]] <= .95 &&
      is.finite(ci[["upper"]]) && ci[["upper"]] >= .95
    binomial("well_informed_unconditional_coverage", dat,
             dat$coverage_assessment_event, rep(TRUE, nrow(dat)),
             coverage_rule,
             "point in [0.94,0.96] and Wilson interval contains 0.95")
    conditional_rule <- function(p, ci, n)
      n >= plan$counts[["minimum_released_for_conditional_coverage"]] &&
      coverage_rule(p, ci, n)
    binomial("well_informed_conditional_coverage", dat,
             dat$coverage_assessment_event, dat$released,
             conditional_rule,
             "coverage rule plus at least the frozen released-count minimum")
    rare_rule <- function(p, ci, n)
      n > 0 && is.finite(p) && p <= .01 && is.finite(ci[["upper"]]) &&
      ci[["upper"]] <= .02
    binomial("well_informed_empty_set_rate", dat,
             dat$empty_assessment_event, rep(TRUE, nrow(dat)), rare_rule,
             "point at most 0.01 and Wilson upper bound at most 0.02")
    binomial("well_informed_uninformative_set_rate", dat,
             dat$uninformative_assessment_event, rep(TRUE, nrow(dat)),
             rare_rule,
             "point at most 0.01 and Wilson upper bound at most 0.02")

    ## Efficiency remains variant-specific even when subgroup coverage is
    ## joint.  It uses only intervals that would actually be reported.
    efficiency_parts <- if (identical(dat$target_id[[1L]], "subgroup_share")) {
      split(well[well$design_id == dat$design_id[[1L]] &
                   well$target_id == "subgroup_share", , drop = FALSE],
            well$variant_id[well$design_id == dat$design_id[[1L]] &
                              well$target_id == "subgroup_share"],
            drop = TRUE)
    } else list(dat)
    for (eff in efficiency_parts) {
      ratio <- eff$interval_length_ratio_to_oracle[
        eff$released & !eff$empty_constraint_set &
          is.finite(eff$interval_length_ratio_to_oracle) &
          eff$interval_length_ratio_to_oracle >= 0]
      n <- length(ratio)
      median_ratio <- if (n) as.numeric(stats::quantile(
        ratio, .5, names = FALSE, type = 8)) else NA_real_
      p90_ratio <- if (n) as.numeric(stats::quantile(
        ratio, .9, names = FALSE, type = 8)) else NA_real_
      minimum <- plan$counts[["minimum_released_for_conditional_coverage"]]
      add("well_informed_interval_median_ratio", eff, median_ratio,
          denominator = n,
          passed = n >= minimum && is.finite(median_ratio) &&
            median_ratio <=
              plan$rates[["maximum_interval_median_ratio_to_oracle"]],
          detail = "reported natural length / frozen oracle benchmark <= 1.25")
      add("well_informed_interval_p90_ratio_type8", eff, p90_ratio,
          denominator = n,
          passed = n >= minimum && is.finite(p90_ratio) &&
            p90_ratio <= plan$rates[["maximum_interval_p90_ratio_to_oracle"]],
          detail = "type-8 p90 reported length / oracle benchmark <= 2.0")
    }
  }

  ## Passage checks are cell-specific.  Pooling the well-informed and mild
  ## cells could conceal a failure in either one, so every safe target cell is
  ## evaluated separately.  Only explicitly target-specific unsafe rows enter
  ## false certification.
  passage <- targets[targets$target_certification_truth %in% c("safe", "unsafe"),
                     , drop = FALSE]
  passage$validation_variant <- ifelse(
    passage$target_id == "subgroup_share", "high_and_low_joint",
    passage$variant_id)
  passage_groups <- split(passage, interaction(
    passage$design_id, passage$regime_id, passage$severity_id,
    passage$target_id, passage$validation_variant, drop = TRUE))
  for (dat in passage_groups) {
    if (identical(dat$target_id[[1L]], "subgroup_share")) {
      dat <- dat[!duplicated(dat$replication), , drop = FALSE]
      dat$variant_id <- "high_and_low_joint"
    }
    if (identical(dat$target_certification_truth[[1L]], "safe")) {
      safe_rule <- function(p, ci, n)
        n > 0 && is.finite(p) && p >= .90 &&
        is.finite(ci[["lower"]]) && ci[["lower"]] >= .88
      binomial("target_specific_benign_passage", dat, dat$released,
               rep(TRUE, nrow(dat)), safe_rule,
               "point at least 0.90 and Wilson lower bound at least 0.88")
    } else {
      unsafe_rule <- function(p, ci, n)
        n > 0 && is.finite(p) && p <= .01 &&
        is.finite(ci[["upper"]]) && ci[["upper"]] <= .02
      binomial("target_specific_false_certification", dat, dat$released,
               rep(TRUE, nrow(dat)), unsafe_rule,
               "point at most 0.01 and Wilson upper bound at most 0.02")
    }
  }
  criteria <- if (length(rows)) do.call(rbind, rows) else empty_result()
  rownames(criteria) <- NULL
  expected <- plan$counts[["oracle_validation_reps"]]
  cell_sizes <- table(targets$design_id, targets$regime_id,
                      targets$severity_id, targets$target_key)
  full_count <- length(cell_sizes) > 0L && all(cell_sizes == expected)
  list(
    criteria = criteria,
    all_statistical_criteria_pass = full_count && nrow(criteria) > 0L &&
      all(criteria$passed),
    assessed = TRUE, frozen_cell_counts_used = full_count,
    reason = if (full_count) "all_registered_statistical_criteria_evaluated"
      else "validation_cell_counts_do_not_match_frozen_2000_replications")
}

#' Summarize the frozen Step-3 oracle operating characteristics
#'
#' @param run A calibration or untouched-validation oracle run.
#' @param thresholds Named calibrated candidates for all oracle-primary
#'   constants. This function evaluates but never selects or retunes them.
#' @param plan Signed v2.3 plan.
#' @return A list containing target release decisions, gate decisions, and a
#'   table with the seven registered operating-characteristic names.
#' @export
scmix_v23_oracle_operating_characteristics <- function(
    run, thresholds, plan = scmix_v23_calibration_plan()) {
  if (!inherits(run, "scmix_v23_oracle_calibration_run") ||
      (exists(".scmix_v23_step3_intact", mode = "function", inherits = TRUE) &&
       !get(".scmix_v23_step3_intact", mode = "function", inherits = TRUE)(
         run, "scmix_v23_oracle_calibration_run"))) {
    stop("`run` must be an intact v2.3 oracle run.", call. = FALSE)
  }
  if (!identical(run$plan_id, plan$plan_id) ||
      !identical(run$protocol_id, plan$protocol_id) ||
      (!isTRUE(run$pilot) &&
       !identical(run$plan_fingerprint, plan$fingerprint))) {
    stop("The oracle run and prospective plan do not match.", call. = FALSE)
  }
  if (!is.numeric(thresholds) || is.null(names(thresholds)) ||
      anyDuplicated(names(thresholds)) ||
      !setequal(names(thresholds), .scmix_v23_oracle_names)) {
    stop("Oracle evaluation requires exactly the 14 named oracle constants.",
         call. = FALSE)
  }
  thresholds <- thresholds[.scmix_v23_oracle_names]
  registry_threshold <- plan$constants[
    match(names(thresholds), plan$constants$constant), , drop = FALSE]
  positive <- registry_threshold$comparator == "greater_than_or_equal"
  if (any(!is.finite(thresholds)) || any(thresholds[!positive] < 0) ||
      any(thresholds[positive] <= 0)) {
    stop("Oracle cutoffs must be finite and in their frozen nonnegative or strictly positive domains.",
         call. = FALSE)
  }
  gates <- run$gates
  registry <- plan$constants[match(gates$constant, plan$constants$constant), ]
  gates$threshold <- as.numeric(thresholds[gates$constant])
  gates$comparator <- registry$comparator
  gates$pass <- .scmix_v23_oracle_pass(
    gates$value, gates$threshold, gates$comparator)
  gates$state <- ifelse(gates$pass, "pass", "fail")
  gates$release_unit_id <- ifelse(
    gates$target_id == "subgroup_share", "subgroup_share_joint",
    gates$target_key)
  key <- paste(gates$design_id, gates$regime_id, gates$severity_id,
               gates$replication, gates$release_unit_id, sep = "\r")
  release <- tapply(gates$pass, key, function(x) all(x %in% TRUE))
  targets <- run$targets
  targets$release_unit_id <- ifelse(
    targets$target_id == "subgroup_share", "subgroup_share_joint",
    targets$target_key)
  target_key <- paste(
    targets$design_id, targets$regime_id, targets$severity_id,
    targets$replication, targets$release_unit_id, sep = "\r")
  targets$released <- as.logical(release[target_key])
  targets$released[is.na(targets$released)] <- FALSE
  targets$coverage_assessment_event <- targets$covered
  targets$empty_assessment_event <- targets$empty_constraint_set
  targets$uninformative_assessment_event <-
    targets$uninformative_constraint_set
  subgroup <- targets$target_id == "subgroup_share"
  if (any(subgroup)) {
    subgroup_key <- paste(
      targets$design_id[subgroup], targets$regime_id[subgroup],
      targets$severity_id[subgroup], targets$replication[subgroup], sep = "\r")
    joint_coverage <- tapply(targets$covered[subgroup], subgroup_key,
                             function(x) all(x %in% TRUE))
    joint_empty <- tapply(targets$empty_constraint_set[subgroup], subgroup_key,
                          function(x) any(x %in% TRUE))
    joint_uninformative <- tapply(
      targets$uninformative_constraint_set[subgroup], subgroup_key,
      function(x) any(x %in% TRUE))
    targets$coverage_assessment_event[subgroup] <-
      as.logical(joint_coverage[subgroup_key])
    targets$empty_assessment_event[subgroup] <-
      as.logical(joint_empty[subgroup_key])
    targets$uninformative_assessment_event[subgroup] <-
      as.logical(joint_uninformative[subgroup_key])
  }

  group_cols <- c("design_id", "regime_id", "severity_id",
                  "certification_truth", "target_id", "variant_id")
  split_target <- split(targets, targets[group_cols], drop = TRUE)
  rows <- list()
  add_binomial <- function(dat, characteristic, event, denominator) {
    denominator <- denominator %in% TRUE
    event <- event %in% TRUE
    n <- sum(denominator)
    s <- sum(event & denominator)
    ci <- .scmix_v23_oracle_wilson(s, n)
    data.frame(
      design_id = dat$design_id[[1L]], regime_id = dat$regime_id[[1L]],
      severity_id = dat$severity_id[[1L]],
      certification_truth = dat$certification_truth[[1L]],
      severity_role = dat$severity_role[[1L]],
      target_id = dat$target_id[[1L]], variant_id = dat$variant_id[[1L]],
      characteristic = characteristic,
      simulation_scope = "oracle_primary_simulated_only",
      estimate = if (n > 0) s / n else NA_real_, lower = ci[["lower"]],
      upper = ci[["upper"]], numerator = s, denominator = n,
      stringsAsFactors = FALSE)
  }
  for (dat in split_target) {
    rows[[length(rows) + 1L]] <- add_binomial(
      dat, "unconditional_coverage", dat$coverage_assessment_event,
      rep(TRUE, nrow(dat)))
    rows[[length(rows) + 1L]] <- add_binomial(
      dat, "coverage_conditional_on_release",
      dat$coverage_assessment_event, dat$released)
    target_constants <- .scmix_v23_oracle_gate_map[[dat$target_id[[1L]]]]
    target_registry <- plan$constants[
      plan$constants$constant %in% target_constants, , drop = FALSE]
    targeted <- identical(dat$target_certification_truth[[1L]], "unsafe") &&
      any(target_registry$targeted_failure_regime == dat$regime_id[[1L]])
    rows[[length(rows) + 1L]] <- add_binomial(
      dat, "false_certification_rate", dat$released,
      rep(targeted, nrow(dat)))
    finite_length <- dat$released & !dat$empty_constraint_set &
      is.finite(dat$reported_interval_length) &
      dat$reported_interval_length >= 0
    length_value <- dat$reported_interval_length[finite_length]
    standardized_length <- length_value / dat$se[finite_length]
    summarize_length <- function(characteristic, value, probability) {
      data.frame(
        design_id = dat$design_id[[1L]], regime_id = dat$regime_id[[1L]],
        severity_id = dat$severity_id[[1L]],
        certification_truth = dat$certification_truth[[1L]],
        severity_role = dat$severity_role[[1L]],
        target_id = dat$target_id[[1L]], variant_id = dat$variant_id[[1L]],
        characteristic = characteristic,
        simulation_scope = "oracle_primary_simulated_only",
        estimate = if (length(value)) as.numeric(stats::quantile(
          value, probability, names = FALSE, type = 8)) else NA_real_,
        lower = NA_real_, upper = NA_real_, numerator = NA_real_,
        denominator = length(value), stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1L]] <- summarize_length(
      "interval_length_natural_median", length_value, .5)
    rows[[length(rows) + 1L]] <- summarize_length(
      "interval_length_natural_p90", length_value, .9)
    standardized_length <- standardized_length[
      is.finite(standardized_length) & standardized_length >= 0]
    rows[[length(rows) + 1L]] <- summarize_length(
      "interval_length_se_units_median", standardized_length, .5)
    rows[[length(rows) + 1L]] <- summarize_length(
      "interval_length_se_units_p90", standardized_length, .9)
    rows[[length(rows) + 1L]] <- add_binomial(
      dat, "empty_constraint_set_rate", dat$empty_constraint_set,
      rep(TRUE, nrow(dat)))
    rows[[length(rows) + 1L]] <- add_binomial(
      dat, "uninformative_constraint_set_rate",
      dat$uninformative_constraint_set, rep(TRUE, nrow(dat)))
  }
  split_gate <- split(gates, interaction(
    gates$design_id, gates$regime_id, gates$severity_id, gates$target_key,
    gates$constant, drop = TRUE))
  for (dat in split_gate) {
    ci <- .scmix_v23_oracle_wilson(sum(dat$pass), nrow(dat))
    rows[[length(rows) + 1L]] <- data.frame(
      design_id = dat$design_id[[1L]], regime_id = dat$regime_id[[1L]],
      severity_id = dat$severity_id[[1L]],
      certification_truth = dat$certification_truth[[1L]],
      severity_role = dat$severity_role[[1L]],
      target_id = paste0(dat$target_id[[1L]], ":", dat$constant[[1L]]),
      variant_id = dat$variant_id[[1L]],
      characteristic = "gate_passage_frequency",
      simulation_scope = "oracle_primary_simulated_only",
      estimate = mean(dat$pass), lower = ci[["lower"]], upper = ci[["upper"]],
      numerator = sum(dat$pass), denominator = nrow(dat),
      stringsAsFactors = FALSE)
    for (state in c("pass", "warn", "fail", "not_run", "not_applicable")) {
      success <- sum(dat$state == state)
      ci_state <- .scmix_v23_oracle_wilson(success, nrow(dat))
      rows[[length(rows) + 1L]] <- data.frame(
        design_id = dat$design_id[[1L]], regime_id = dat$regime_id[[1L]],
        severity_id = dat$severity_id[[1L]],
        certification_truth = dat$certification_truth[[1L]],
        severity_role = dat$severity_role[[1L]],
        target_id = paste0(dat$target_id[[1L]], ":", dat$constant[[1L]],
                           ":", state),
        variant_id = dat$variant_id[[1L]],
        characteristic = "gate_state_frequency",
        simulation_scope = "oracle_primary_simulated_only",
        estimate = success / nrow(dat), lower = ci_state[["lower"]],
        upper = ci_state[["upper"]], numerator = success,
        denominator = nrow(dat), stringsAsFactors = FALSE)
    }
  }
  gate_order <- stats::setNames(seq_len(nrow(plan$constants)),
                                plan$constants$constant)
  by_replication <- split(gates, paste(
    gates$design_id, gates$regime_id, gates$severity_id, gates$replication,
    gates$target_key, sep = "\r"))
  first_failure <- do.call(rbind, lapply(by_replication, function(dat) {
    failed <- dat[dat$state == "fail", , drop = FALSE]
    first <- if (!nrow(failed)) "none" else failed$constant[[
      which.min(gate_order[failed$constant])]]
    data.frame(
      design_id = dat$design_id[[1L]], regime_id = dat$regime_id[[1L]],
      severity_id = dat$severity_id[[1L]],
      certification_truth = dat$certification_truth[[1L]],
      severity_role = dat$severity_role[[1L]], target_id = dat$target_id[[1L]],
      variant_id = dat$variant_id[[1L]], first_failure = first,
      stringsAsFactors = FALSE)
  }))
  first_groups <- split(first_failure, first_failure[c(
    "design_id", "regime_id", "severity_id", "certification_truth",
    "target_id", "variant_id")], drop = TRUE)
  for (dat in first_groups) for (failure in unique(dat$first_failure)) {
    success <- sum(dat$first_failure == failure)
    ci <- .scmix_v23_oracle_wilson(success, nrow(dat))
    rows[[length(rows) + 1L]] <- data.frame(
      design_id = dat$design_id[[1L]], regime_id = dat$regime_id[[1L]],
      severity_id = dat$severity_id[[1L]],
      certification_truth = dat$certification_truth[[1L]],
      severity_role = dat$severity_role[[1L]],
      target_id = paste0(dat$target_id[[1L]], ":first=", failure),
      variant_id = dat$variant_id[[1L]],
      characteristic = "first_failure_frequency",
      simulation_scope = "oracle_primary_simulated_only",
      estimate = success / nrow(dat), lower = ci[["lower"]],
      upper = ci[["upper"]], numerator = success, denominator = nrow(dat),
      stringsAsFactors = FALSE)
  }
  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL
  observed <- unique(summary$characteristic)
  expected <- as.character(plan$operating_characteristics$characteristic)
  if (!identical(sort(observed), sort(expected))) {
    stop("The oracle summary does not contain every frozen operating ",
         "characteristics.", call. = FALSE)
  }
  validation <- .scmix_v23_oracle_validation_assessment(targets, run, plan)
  list(summary = summary, targets = targets, gates = gates,
       simulated_gate_scope = "oracle_primary_simulated_only",
       unsimulated_constants = setdiff(plan$constants$constant,
                                        .scmix_v23_oracle_names),
       thresholds_selected_here = FALSE, validation_used_for_tuning = FALSE,
       validation = validation,
       production_promotion_permitted = FALSE,
       production_promotion_blocker = paste0(
         "marginal_local_Gaussian_oracle_cannot_authorize_end_to_end_",
         "sequence_likelihood_or_sample_Riesz_behavior"))
}
