## Protocol-v2.5 target and family-finalization utilities.
##
## This file deliberately contains no outcome generator and no application
## runner.  It supplies the estimand callbacks and deterministic reporting
## operations needed by the prospective v2.5 evaluator.  All functions are
## internal until the evaluator interface is hash-frozen.

.scmix_v25_target_stop <- function(...) {
  stop(..., call. = FALSE)
}

.scmix_v25_target_text <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .scmix_v25_target_stop(name, " must be one nonempty string.")
  }
  x
}

.scmix_v25_target_probability <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 && x < 1
}

.scmix_v25_gh_normal <- function(n_nodes) {
  if (!is.numeric(n_nodes) || length(n_nodes) != 1L ||
      !is.finite(n_nodes) || n_nodes < 3L ||
      n_nodes != as.integer(n_nodes)) {
    .scmix_v25_target_stop("n_nodes must be an integer at least 3.")
  }
  n <- as.integer(n_nodes)
  jacobi <- matrix(0, n, n)
  off <- sqrt(seq_len(n - 1L))
  jacobi[cbind(seq_len(n - 1L), 2L:n)] <- off
  jacobi[cbind(2L:n, seq_len(n - 1L))] <- off
  decomposition <- eigen(jacobi, symmetric = TRUE)
  order <- order(decomposition$values)
  list(x = decomposition$values[order],
       w = decomposition$vectors[1L, order]^2)
}

## Construct the v2.5 choice-probability callback.  The target first averages
## integrated choice probabilities equally over each respondent's frozen task
## sequence and then averages respondents equally.  `task_respondent_id` may
## contain records outside the current DML fold; the callback selects the
## records belonging to the respondent rows supplied by scmix_dml().
.scmix_v25_frozen_choice_target <- function(
    task_x, task_respondent_id, kappa_sign = 1, n_nodes = 31L,
    label = "choice_probability") {
  task_x <- as.matrix(task_x)
  storage.mode(task_x) <- "double"
  task_respondent_id <- enc2utf8(as.character(task_respondent_id))
  label <- .scmix_v25_target_text(label, "label")
  if (!nrow(task_x) || !ncol(task_x) || any(!is.finite(task_x))) {
    .scmix_v25_target_stop("task_x must be a nonempty finite numeric matrix.")
  }
  if (length(task_respondent_id) != nrow(task_x) ||
      anyNA(task_respondent_id) || any(!nzchar(task_respondent_id))) {
    .scmix_v25_target_stop(
      "task_respondent_id must identify every row of task_x.")
  }
  if (!is.numeric(kappa_sign) || any(!is.finite(kappa_sign)) ||
      !(length(kappa_sign) %in% c(1L, nrow(task_x)))) {
    .scmix_v25_target_stop(
      "kappa_sign must be one finite value or one value per task.")
  }
  kappa_sign <- rep(as.numeric(kappa_sign), length.out = nrow(task_x))
  if (!is.null(colnames(task_x)) &&
      (anyNA(colnames(task_x)) || any(!nzchar(colnames(task_x))) ||
       anyDuplicated(colnames(task_x)))) {
    .scmix_v25_target_stop(
      "Non-null task_x column names must be unique and nonempty.")
  }
  gh <- .scmix_v25_gh_normal(n_nodes)
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold,
                       attr_names) {
    mu <- as.matrix(mu)
    Sigma <- as.matrix(Sigma)
    respondent_id <- enc2utf8(as.character(respondent_id))
    N <- nrow(mu)
    p <- ncol(mu)
    if (length(respondent_id) != N || anyNA(respondent_id) ||
        any(!nzchar(respondent_id)) || anyDuplicated(respondent_id)) {
      .scmix_v25_target_stop(
        "The DML callback respondent_id must be unique and match mu rows.")
    }
    if (length(attr_names) != p || ncol(task_x) != p ||
        !identical(dim(Sigma), c(p, p)) ||
        any(!is.finite(c(mu, kappa, Sigma)))) {
      .scmix_v25_target_stop(
        "Choice callback dimensions or nuisance values do not match.")
    }
    X <- task_x
    if (!is.null(colnames(X))) {
      column <- match(attr_names, colnames(X))
      if (anyNA(column) || length(column) != ncol(X)) {
        .scmix_v25_target_stop(
          "Named task_x columns must match attr_names exactly.")
      }
      X <- X[, column, drop = FALSE]
    }
    value <- matrix(0, N, 1L)
    d_mu <- array(0, c(N, 1L, p))
    d_kappa <- matrix(0, N, 1L)
    d_Sigma <- array(0, c(N, 1L, p, p))
    for (i in seq_len(N)) {
      rows <- which(task_respondent_id == respondent_id[[i]])
      if (!length(rows)) {
        .scmix_v25_target_stop(
          "No frozen tasks were found for callback respondent `",
          respondent_id[[i]], "`.")
      }
      Xi <- X[rows, , drop = FALSE]
      si <- kappa_sign[rows]
      task_value <- numeric(length(rows))
      task_dk <- numeric(length(rows))
      task_dm <- matrix(0, length(rows), p)
      task_ds <- array(0, c(length(rows), p, p))
      for (t in seq_along(rows)) {
        x <- Xi[t, ]
        variance <- as.numeric(crossprod(x, Sigma %*% x))
        tolerance <- 100 * .Machine$double.eps *
          max(1, sum(abs(Sigma)) * sum(x^2))
        if (!is.finite(variance) || variance < -tolerance) {
          .scmix_v25_target_stop(
            "Choice callback received a negative directional variance.")
        }
        variance <- max(variance, 0)
        fixed <- si[[t]] * kappa + sum(x * mu[i, ])
        if (variance == 0) {
          probability <- stats::plogis(fixed)
          slope <- probability * (1 - probability)
          integrated_probability <- probability
          integrated_slope <- slope
          derivative_variance <- 0.5 * slope * (1 - 2 * probability)
        } else {
          index <- fixed + sqrt(variance) * gh$x
          probability <- stats::plogis(index)
          slope <- probability * (1 - probability)
          integrated_probability <- sum(gh$w * probability)
          integrated_slope <- sum(gh$w * slope)
          ## This differentiates the fixed quadrature rule itself.  The
          ## variance-zero branch above is its continuous limiting derivative.
          derivative_variance <-
            sum(gh$w * slope * gh$x) / (2 * sqrt(variance))
        }
        task_value[[t]] <- integrated_probability
        task_dk[[t]] <- si[[t]] * integrated_slope
        task_dm[t, ] <- integrated_slope * x
        task_ds[t, , ] <- derivative_variance * tcrossprod(x)
      }
      value[i, 1L] <- mean(task_value)
      d_kappa[i, 1L] <- mean(task_dk)
      d_mu[i, 1L, ] <- colMeans(task_dm)
      d_Sigma[i, 1L, , ] <- apply(task_ds, c(2L, 3L), mean)
    }
    list(
      target_type = "rowwise_expectation", value = value,
      d_mu = d_mu, d_kappa = d_kappa, d_Sigma = d_Sigma,
      labels = label)
  }
  attr(callback, "scmix_v25_target") <- list(
    protocol_id = "paperps-inference-2026-09-07-v2.5",
    type = "frozen_sequence_choice_probability",
    reference_weight_rule = "respondent_equal_outer_task_equal_inner",
    n_nodes = as.integer(n_nodes), label = label)
  class(callback) <- c("scmix_v25_rowwise_target",
                       "scmix_rowwise_target", "function")
  callback
}

## Construct a directional-share target that remains evaluable at the
## rank-zero boundary.  The second rowwise primitive is the common directional
## variance itself.  It gives the v2.5 gate an explicit regularity margin, so
## target evaluation never needs an artificial positive variance floor.
.scmix_v25_directional_share_target <- function(
    contrast, label = "preference_direction_share", include_ties = FALSE) {
  label <- .scmix_v25_target_text(label, "label")
  if (!is.logical(include_ties) || length(include_ties) != 1L ||
      is.na(include_ties)) {
    .scmix_v25_target_stop("include_ties must be one nonmissing logical.")
  }
  contrast_names <- names(contrast)
  contrast <- as.numeric(contrast)
  names(contrast) <- contrast_names
  if (!length(contrast) || any(!is.finite(contrast)) ||
      !any(contrast != 0)) {
    .scmix_v25_target_stop(
      "contrast must be a finite, nonzero directional vector.")
  }
  if (!is.null(contrast_names) &&
      (anyNA(contrast_names) || any(!nzchar(contrast_names)) ||
       anyDuplicated(contrast_names))) {
    .scmix_v25_target_stop(
      "Named directional contrasts must have unique nonempty names.")
  }
  variance_label <- paste0(label, ":directional_variance")
  finite_exp <- function(log_value) {
    if (is.na(log_value)) return(NA_real_)
    if (log_value == -Inf || log_value < log(.Machine$double.xmin)) return(0)
    if (log_value > log(.Machine$double.xmax)) return(.Machine$double.xmax)
    exp(log_value)
  }
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold,
                       attr_names) {
    mu <- as.matrix(mu)
    Sigma <- as.matrix(Sigma)
    N <- nrow(mu)
    p <- ncol(mu)
    if (!N || !p || length(attr_names) != p ||
        !identical(dim(Sigma), c(p, p)) ||
        any(!is.finite(c(mu, Sigma)))) {
      .scmix_v25_target_stop(
        "Directional-share callback dimensions or nuisance values do not match.")
    }
    direction <- contrast
    if (!is.null(contrast_names)) {
      index <- match(attr_names, contrast_names)
      if (anyNA(index) || length(index) != length(contrast) ||
          !setequal(attr_names, contrast_names)) {
        .scmix_v25_target_stop(
          "Named directional contrast must match attr_names exactly.")
      }
      direction <- contrast[index]
    } else if (length(direction) != p) {
      .scmix_v25_target_stop(
        "Unnamed directional contrast must match the coefficient dimension.")
    }
    mean_direction <- as.numeric(mu %*% direction)
    cc <- tcrossprod(direction)
    variance <- as.numeric(crossprod(direction, Sigma %*% direction))
    roundoff <- 128 * .Machine$double.eps *
      max(1, sum(abs(Sigma)) * sum(abs(cc)))
    if (!is.finite(variance) || variance < -roundoff) {
      .scmix_v25_target_stop(
        "Directional-share callback received a negative directional variance.")
    }
    variance <- max(variance, 0)
    value <- matrix(0, N, 2L, dimnames = list(
      NULL, c(label, variance_label)))
    d_mu <- array(0, c(N, 2L, p))
    d_kappa <- matrix(0, N, 2L)
    d_Sigma <- array(0, c(N, 2L, p, p))
    for (i in seq_len(N)) d_Sigma[i, 2L, , ] <- cc
    value[, 2L] <- variance
    if (variance == 0) {
      value[, 1L] <- ifelse(
        mean_direction > 0, 1,
        ifelse(mean_direction < 0, 0, if (include_ties) 1 else 0))
    } else {
      standard_deviation <- sqrt(variance)
      z <- mean_direction / standard_deviation
      value[, 1L] <- stats::pnorm(z)
      log_density <- stats::dnorm(z, log = TRUE)
      d_mean <- vapply(log_density - 0.5 * log(variance),
                       finite_exp, numeric(1L))
      d_variance <- numeric(N)
      nonzero <- mean_direction != 0
      if (any(nonzero)) {
        log_absolute <- log(0.5) + log(abs(mean_direction[nonzero])) +
          log_density[nonzero] - 1.5 * log(variance)
        d_variance[nonzero] <- -sign(mean_direction[nonzero]) *
          vapply(log_absolute, finite_exp, numeric(1L))
      }
      for (i in seq_len(N)) {
        d_mu[i, 1L, ] <- d_mean[[i]] * direction
        d_Sigma[i, 1L, , ] <- d_variance[[i]] * cc
      }
    }
    if (any(!is.finite(c(value, d_mu, d_kappa, d_Sigma)))) {
      .scmix_v25_target_stop(
        "Directional-share callback produced a nonfinite analytic result.")
    }
    list(
      target_type = "rowwise_expectation", value = value,
      d_mu = d_mu, d_kappa = d_kappa, d_Sigma = d_Sigma,
      labels = c(label, variance_label))
  }
  attr(callback, "scmix_v25_target") <- list(
    protocol_id = "paperps-inference-2026-09-07-v2.5",
    type = "boundary_safe_directional_share_with_variance",
    contrast = contrast, share_label = label,
    variance_label = variance_label, include_ties = include_ties,
    boundary_rule = paste0(
      "declared_tie_rule_with_zero_share_derivative_at_variance_zero"))
  class(callback) <- c("scmix_v25_rowwise_target",
                       "scmix_rowwise_target", "function")
  callback
}

## Turn a one-column rowwise target into the joint subgroup primitives
## E[B f] and E[B].  Group membership is fixed by respondent identifier.
.scmix_v25_subgroup_primitives <- function(
    base_target, subgroup, label = "subgroup") {
  if (!is.function(base_target)) {
    .scmix_v25_target_stop("base_target must be a callback function.")
  }
  label <- .scmix_v25_target_text(label, "label")
  if (is.null(names(subgroup)) || anyNA(names(subgroup)) ||
      any(!nzchar(names(subgroup))) || anyDuplicated(names(subgroup))) {
    .scmix_v25_target_stop(
      "subgroup must have unique nonempty respondent-id names.")
  }
  subgroup_id <- enc2utf8(names(subgroup))
  subgroup <- as.numeric(subgroup)
  names(subgroup) <- subgroup_id
  if (anyNA(subgroup) || any(!subgroup %in% c(0, 1))) {
    .scmix_v25_target_stop("subgroup values must be binary and nonmissing.")
  }
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold,
                       attr_names) {
    respondent_id <- enc2utf8(as.character(respondent_id))
    index <- match(respondent_id, names(subgroup))
    if (anyNA(index)) {
      .scmix_v25_target_stop(
        "Subgroup membership is absent for one or more callback respondents.")
    }
    B <- unname(subgroup[index])
    base <- base_target(mu = mu, kappa = kappa, Sigma = Sigma, Z = Z,
                        respondent_id = respondent_id, fold = fold,
                        attr_names = attr_names)
    base_value <- as.matrix(base$value)
    N <- nrow(mu)
    p <- ncol(mu)
    if (!identical(dim(base_value), c(N, 1L)) ||
        is.null(base$d_mu) || is.null(base$d_kappa)) {
      .scmix_v25_target_stop(
        "base_target must return one value and analytic derivatives.")
    }
    d_mu <- array(0, c(N, 2L, p))
    d_mu[, 1L, ] <- base$d_mu[, 1L, ] * B
    d_kappa <- matrix(0, N, 2L)
    d_kappa[, 1L] <- as.numeric(base$d_kappa[, 1L]) * B
    answer <- list(
      target_type = "rowwise_expectation",
      value = cbind(numerator = B * base_value[, 1L], denominator = B),
      d_mu = d_mu, d_kappa = d_kappa,
      labels = paste0(label, c(":numerator", ":denominator")))
    if (!is.null(base$d_Sigma)) {
      d_Sigma <- array(0, c(N, 2L, p, p))
      for (i in seq_len(N)) {
        d_Sigma[i, 1L, , ] <- base$d_Sigma[i, 1L, , ] * B[[i]]
      }
      answer$d_Sigma <- d_Sigma
    } else if (isTRUE(base$sigma_invariant)) {
      answer$sigma_invariant <- TRUE
    } else {
      .scmix_v25_target_stop(
        "base_target must declare d_Sigma or sigma_invariant.")
    }
    answer
  }
  attr(callback, "scmix_v25_target") <- list(
    protocol_id = "paperps-inference-2026-09-07-v2.5",
    type = "subgroup_ratio_primitives", label = label,
    primitive_rule = "joint_E_Bf_and_E_B")
  class(callback) <- c("scmix_v25_rowwise_target",
                       "scmix_rowwise_target", "function")
  callback
}

## The rank-one active covariance eigenvalue is trace(Sigma).  Repeating the
## scalar as a respondent-row primitive gives the desired nuisance derivative
## while adding no spurious empirical-P_Z contribution to its influence.
.scmix_v25_trace_sigma_target <- function(
    label = "active_q1_covariance_eigenvalue") {
  label <- .scmix_v25_target_text(label, "label")
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold,
                       attr_names) {
    N <- nrow(mu)
    p <- ncol(mu)
    Sigma <- as.matrix(Sigma)
    if (!identical(dim(Sigma), c(p, p)) || any(!is.finite(Sigma))) {
      .scmix_v25_target_stop("Sigma does not match the coefficient dimension.")
    }
    d_Sigma <- array(0, c(N, 1L, p, p))
    for (i in seq_len(N)) d_Sigma[i, 1L, , ] <- diag(p)
    list(
      target_type = "rowwise_expectation",
      value = matrix(rep(sum(diag(Sigma)), N), ncol = 1L),
      d_mu = array(0, c(N, 1L, p)), d_kappa = matrix(0, N, 1L),
      d_Sigma = d_Sigma, labels = label)
  }
  attr(callback, "scmix_v25_target") <- list(
    protocol_id = "paperps-inference-2026-09-07-v2.5",
    type = "active_rank_one_covariance_eigenvalue", label = label)
  class(callback) <- c("scmix_v25_rowwise_target",
                       "scmix_rowwise_target", "function")
  callback
}

## Algebraically map supporting directional moments into (H_Z, H_R).  This is
## useful for audits and for checking transformations.  It is deliberately not
## the production Riesz route: production must use the direct-H_Z/H_R adapter
## below so the two representers are solved on the minimal primitive vector.
.scmix_v25_heterogeneity_transform <- function(
    estimate, influence, mean_id = "mean", second_id = "second_moment",
    residual_id = "residual_variance") {
  estimate_names <- names(estimate)
  estimate <- as.numeric(estimate)
  names(estimate) <- estimate_names
  influence <- as.matrix(influence)
  if (is.null(names(estimate))) {
    .scmix_v25_target_stop("estimate must be uniquely named.")
  }
  names_estimate <- names(estimate)
  if (anyDuplicated(names_estimate) || any(!nzchar(names_estimate)) ||
      is.null(colnames(influence)) || anyDuplicated(colnames(influence))) {
    .scmix_v25_target_stop(
      "estimate and influence columns must have unique names.")
  }
  required <- c(mean_id, second_id, residual_id)
  ie <- match(required, names_estimate)
  ii <- match(required, colnames(influence))
  if (anyNA(c(ie, ii)) || any(!is.finite(estimate[ie])) ||
      any(!is.finite(influence[, ii, drop = FALSE]))) {
    .scmix_v25_target_stop(
      "The three finite heterogeneity supporting primitives are required.")
  }
  mean_value <- estimate[[ie[[1L]]]]
  HZ <- estimate[[ie[[2L]]]] - mean_value^2
  HR <- estimate[[ie[[3L]]]]
  transformed_influence <- cbind(
    H_Z = influence[, ii[[2L]]] - 2 * mean_value * influence[, ii[[1L]]],
    H_R = influence[, ii[[3L]]])
  rownames(transformed_influence) <- rownames(influence)
  total <- HZ + HR
  list(
    estimate = c(H_Z = HZ, H_R = HR),
    influence = transformed_influence,
    supporting = c(mean = mean_value,
                   second_moment = estimate[[ie[[2L]]]],
                   residual_variance = HR),
    share = if (is.finite(total) && total > 0) HZ / total else NA_real_,
    role = "supporting_moment_audit_not_production_Riesz",
    production_release_eligible = FALSE)
}

## Build the computational adapter that makes scmix_dml solve the two Riesz
## problems directly for H_Z and H_R.  The callback still supplies the mean,
## second moment, and residual variance as supporting audit quantities.  The
## adapter uses the established minimal-primitive hook in scmix_dml, but its
## signed v2.2 fingerprint is deliberately removed and its protocol ID is
## replaced.  It therefore cannot be passed off as a v2.3 release artifact or
## consumed by the v2.3 finalizer.
.scmix_v25_heterogeneity_dml_adapter <- function(
    contrast, callback_id = "heterogeneity_support",
    target_id = "directional_heterogeneity",
    family_id = "directional_heterogeneity",
    primitive_ids = c("H_Z", "H_R")) {
  callback_id <- .scmix_v25_target_text(callback_id, "callback_id")
  target_id <- .scmix_v25_target_text(target_id, "target_id")
  family_id <- .scmix_v25_target_text(family_id, "family_id")
  if (!is.character(primitive_ids) || length(primitive_ids) != 2L ||
      anyNA(primitive_ids) || any(!nzchar(primitive_ids)) ||
      anyDuplicated(primitive_ids)) {
    .scmix_v25_target_stop("primitive_ids must contain unique H_Z and H_R IDs.")
  }
  callback <- scmix_inference_target(
    type = "heterogeneity_primitives", contrast = contrast,
    label = callback_id)
  callback_metadata <- attr(callback, "scmix_v22_metadata_template")
  callback_metadata$protocol_id <-
    "paperps-inference-2026-09-07-v2.5"
  callback_metadata$release_finalizer <- "scmix_v25_only"
  attr(callback, "scmix_v22_metadata_template") <- callback_metadata
  supporting_names <- paste0(
    callback_id, ":", c("mean", "second_moment", "residual_variance"))
  adapter <- scmix_v22_target_spec(
    target_id = target_id, family_id = family_id,
    target_type = "directional_heterogeneity",
    supporting_names = supporting_names,
    primitive_registry_ids = primitive_ids,
    orientation = "positive_registered_direction",
    units = "squared_utility_index",
    parameter_dependencies = c("mean", "loading"),
    family_primitive_registry_ids = primitive_ids,
    family_target_ids = target_id,
    confirmatory_status = "confirmatory",
    uses_numerical_integration = FALSE,
    reference_rule = "respondent_equal_finite_population_variance_N",
    influence_definition =
      "centered_direct_HZ_and_HR_respondent_sequence",
    primitive_estimands = c(
      "finite_population_variance_of_directional_conditional_mean",
      "directional_residual_variance"),
    primitive_orientations = c("nonnegative", "nonnegative"),
    primitive_units = rep("squared_utility_index", 2L),
    primitive_reference_rules = rep(
      "respondent_equal_finite_population_variance_N", 2L),
    primitive_influence_definitions = c(
      "direct_centered_squared_directional_mean_plus_Riesz_correction",
      "directional_residual_variance_plus_Riesz_correction"))
  adapter$protocol_id <- "paperps-inference-2026-09-07-v2.5"
  adapter$fingerprint <- NULL
  adapter$confirmatory_status <- NULL
  adapter$computational_adapter_only <- TRUE
  adapter$release_finalizer <- "scmix_v25_only"
  class(adapter) <- c("scmix_v25_primitive_adapter",
                      "scmix_v22_target_spec", "list")
  extract <- function(inference) {
    if (!inherits(inference, "scmix_dml") ||
        !isTRUE(inference$v22_primitive_mode) ||
        !all(primitive_ids %in% names(inference$estimate)) ||
        !all(primitive_ids %in% colnames(inference$influence))) {
      .scmix_v25_target_stop(
        "inference does not contain the direct v2.5 H_Z and H_R solves.")
    }
    list(
      estimate = inference$estimate[primitive_ids],
      plugin = inference$crossfit_plugin[primitive_ids],
      influence = inference$influence[, primitive_ids, drop = FALSE],
      supporting_audit = lapply(inference$fold_details, function(x)
        x$supporting_audit[[target_id]]),
      primitive_ids = primitive_ids,
      protocol_id = "paperps-inference-2026-09-07-v2.5")
  }
  list(
    plugin_targets = stats::setNames(list(callback), callback_id),
    primitive_registry = list(adapter), primitive_ids = primitive_ids,
    supporting_names = supporting_names, extract = extract,
    protocol_id = "paperps-inference-2026-09-07-v2.5",
    release_finalizer = "scmix_v25_only")
}

.scmix_v25_multiplier_with_rng <- function(seed, expression) {
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != as.integer(seed)) {
    .scmix_v25_target_stop("seed must be a nonnegative R integer.")
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind(kind = "L'Ecuyer-CMRG", normal.kind = "Inversion",
          sample.kind = "Rejection")
  set.seed(as.integer(seed))
  force(expression)
}

## Make the one common normal bank used by every family in one replication.
## Rows are generated in canonical respondent-ID order, making target order and
## job partition irrelevant.
.scmix_v25_multiplier_bank <- function(
    respondent_id, draws = 9999L, seed) {
  respondent_id <- enc2utf8(as.character(respondent_id))
  if (!length(respondent_id) || anyNA(respondent_id) ||
      any(!nzchar(respondent_id)) || anyDuplicated(respondent_id)) {
    .scmix_v25_target_stop(
      "respondent_id must contain unique nonmissing identifiers.")
  }
  if (!is.numeric(draws) || length(draws) != 1L || !is.finite(draws) ||
      draws < 1L || draws != as.integer(draws)) {
    .scmix_v25_target_stop("draws must be a positive integer.")
  }
  canonical_id <- sort(respondent_id, method = "radix")
  zeta <- .scmix_v25_multiplier_with_rng(seed, matrix(
    stats::rnorm(length(canonical_id) * as.integer(draws)),
    nrow = length(canonical_id), ncol = as.integer(draws),
    dimnames = list(canonical_id, NULL)))
  out <- list(
    protocol_id = "paperps-inference-2026-09-07-v2.5",
    respondent_id = canonical_id, draws = as.integer(draws),
    seed = as.integer(seed), distribution = "standard_normal",
    rng_kind = c("L'Ecuyer-CMRG", "Inversion", "Rejection"),
    multipliers = zeta)
  class(out) <- c("scmix_v25_multiplier_bank", "list")
  out
}

.scmix_v25_type1_quantile <- function(x, probability = 0.95) {
  if (!is.numeric(x) || !length(x) || any(!is.finite(x)) ||
      !.scmix_v25_target_probability(probability)) {
    .scmix_v25_target_stop(
      "A finite sample and a probability strictly between zero and one are required.")
  }
  sort(as.numeric(x), method = "radix")[[ceiling(probability * length(x))]]
}

.scmix_v25_influence_se <- function(influence) {
  influence <- as.matrix(influence)
  centered <- sweep(influence, 2L, colMeans(influence), `-`)
  sqrt(colSums(centered^2)) / nrow(centered)
}

## Compute the exact v2.5 family critical value and its simultaneous primitive
## rectangle.  The type-1 empirical quantile uses no interpolation.  No
## pointwise or Bonferroni fallback is produced.
.scmix_v25_family_multiplier <- function(
    estimate, influence, respondent_id, bank, level = 0.95) {
  estimate_names <- names(estimate)
  estimate <- as.numeric(estimate)
  names(estimate) <- estimate_names
  influence <- as.matrix(influence)
  respondent_id <- enc2utf8(as.character(respondent_id))
  if (is.null(names(estimate)) || anyNA(names(estimate)) ||
      anyDuplicated(names(estimate)) || any(!nzchar(names(estimate))) ||
      any(!is.finite(estimate)) || is.null(colnames(influence)) ||
      anyNA(colnames(influence)) || anyDuplicated(colnames(influence)) ||
      !setequal(names(estimate), colnames(influence))) {
    .scmix_v25_target_stop(
      "estimate and influence must contain the same unique finite primitives.")
  }
  if (nrow(influence) != length(respondent_id) ||
      is.null(rownames(influence)) || anyNA(rownames(influence)) ||
      anyDuplicated(rownames(influence)) ||
      !setequal(enc2utf8(rownames(influence)), respondent_id) ||
      any(!is.finite(influence))) {
    .scmix_v25_target_stop(
      "Influence rows must be finite and uniquely keyed by respondent_id.")
  }
  if (!inherits(bank, "scmix_v25_multiplier_bank") ||
      !identical(bank$protocol_id,
                 "paperps-inference-2026-09-07-v2.5") ||
      !setequal(bank$respondent_id, respondent_id) ||
      !identical(nrow(bank$multipliers), length(respondent_id)) ||
      !identical(ncol(bank$multipliers), as.integer(bank$draws)) ||
      !identical(rownames(bank$multipliers), bank$respondent_id) ||
      any(!is.finite(bank$multipliers))) {
    .scmix_v25_target_stop("bank is not a compatible v2.5 multiplier bank.")
  }
  if (!.scmix_v25_target_probability(level)) {
    .scmix_v25_target_stop("level must be strictly between zero and one.")
  }
  primitive_order <- names(estimate)
  IF <- influence[match(bank$respondent_id, enc2utf8(rownames(influence))),
                  primitive_order, drop = FALSE]
  centered <- sweep(IF, 2L, colMeans(IF), `-`)
  denominator <- sqrt(colSums(centered^2))
  usable <- is.finite(denominator) & denominator > 0
  names(usable) <- primitive_order
  base <- list(
    method = "none", critical = NA_real_, level = level,
    primitive_id = primitive_order, estimate = estimate[primitive_order],
    influence = IF, centered_influence = centered,
    se = stats::setNames(denominator / nrow(IF), primitive_order),
    usable = usable, lower = stats::setNames(rep(NA_real_,
      length(primitive_order)), primitive_order),
    upper = stats::setNames(rep(NA_real_, length(primitive_order)),
                            primitive_order),
    release_eligible = FALSE, multiplier_draws = bank$draws,
    multiplier_seed = bank$seed)
  if (!all(usable)) {
    base$reason <- "one_or_more_family_primitives_has_zero_or_invalid_scale"
    class(base) <- c("scmix_v25_family_multiplier", "list")
    return(base)
  }
  statistic <- crossprod(bank$multipliers, centered)
  statistic <- sweep(statistic, 2L, denominator, `/`)
  maximum <- apply(abs(statistic), 1L, max)
  critical <- .scmix_v25_type1_quantile(maximum, level)
  base$method <- "common_normal_max_t_type1"
  base$critical <- critical
  base$lower <- base$estimate - critical * base$se
  base$upper <- base$estimate + critical * base$se
  base$release_eligible <- identical(bank$draws, 9999L)
  base$reason <- if (base$release_eligible) "passed" else
    "diagnostic_draw_count_not_9999"
  base$maximum_statistic <- maximum
  class(base) <- c("scmix_v25_family_multiplier", "list")
  base
}

.scmix_v25_rectangle_result <- function(
    nonempty, lower, upper, boundary_touch, uninformative, reason) {
  list(nonempty = isTRUE(nonempty), lower = as.numeric(lower),
       upper = as.numeric(upper), boundary_touch = boundary_touch,
       uninformative = isTRUE(uninformative), reason = reason)
}

## Map a simultaneous primitive rectangle into a range-respecting reported
## confidence set.  These are confidence-set images, not delta intervals.
.scmix_v25_map_rectangle <- function(
    type = c("unrestricted", "probability", "positive", "subgroup_ratio",
             "heterogeneity_share", "mrs"), lower, upper) {
  type <- match.arg(type)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  if (!length(lower) || length(lower) != length(upper) ||
      any(!is.finite(c(lower, upper))) || any(lower > upper)) {
    return(.scmix_v25_rectangle_result(
      FALSE, NA_real_, NA_real_, NA, FALSE,
      "invalid_primitive_rectangle"))
  }
  if (type == "unrestricted") {
    if (length(lower) != 1L) .scmix_v25_target_stop(
      "unrestricted mapping needs one primitive.")
    return(.scmix_v25_rectangle_result(
      TRUE, lower, upper, FALSE, FALSE, "identity_map"))
  }
  if (type %in% c("probability", "positive")) {
    if (length(lower) != 1L) .scmix_v25_target_stop(
      type, " mapping needs one primitive.")
    support_upper <- if (type == "probability") 1 else Inf
    lo <- max(lower, 0)
    hi <- min(upper, support_upper)
    if (lo > hi) return(.scmix_v25_rectangle_result(
      FALSE, NA_real_, NA_real_, NA, FALSE,
      "rectangle_misses_scalar_support"))
    boundary <- lo == 0 || (is.finite(support_upper) && hi == support_upper)
    return(.scmix_v25_rectangle_result(
      TRUE, lo, hi, boundary, FALSE, "scalar_support_intersection"))
  }
  if (length(lower) != 2L) {
    .scmix_v25_target_stop(type, " mapping needs two primitives.")
  }
  if (type == "mrs") {
    if (lower[[2L]] <= 0 && upper[[2L]] >= 0) {
      return(.scmix_v25_rectangle_result(
        TRUE, NA_real_, NA_real_, TRUE, TRUE,
        "denominator_rectangle_contains_zero"))
    }
    values <- as.vector(outer(
      c(lower[[1L]], upper[[1L]]),
      c(lower[[2L]], upper[[2L]]), function(a, b) -a / b))
    return(.scmix_v25_rectangle_result(
      TRUE, min(values), max(values), FALSE, FALSE,
      "corner_image_of_ratio_rectangle"))
  }
  if (type == "heterogeneity_share") {
    lo <- pmax(lower, 0)
    hi <- upper
    if (any(hi < 0) || any(lo > hi)) return(
      .scmix_v25_rectangle_result(
        FALSE, NA_real_, NA_real_, NA, FALSE,
        "rectangle_misses_nonnegative_orthant"))
    if (!any(hi > 0)) return(.scmix_v25_rectangle_result(
      FALSE, NA_real_, NA_real_, TRUE, FALSE,
      "no_positive_total_heterogeneity"))
    if (all(lo == 0)) return(.scmix_v25_rectangle_result(
      TRUE, 0, 1, TRUE, TRUE, "origin_closure_is_zero_one"))
    share_lower <- lo[[1L]] / (lo[[1L]] + hi[[2L]])
    share_upper <- hi[[1L]] / (hi[[1L]] + lo[[2L]])
    return(.scmix_v25_rectangle_result(
      TRUE, share_lower, share_upper, any(lo == 0), FALSE,
      "image_of_nonnegative_primitive_rectangle"))
  }
  ## Subgroup primitives obey 0 <= E[B f] <= E[B] <= 1.
  lo <- pmax(lower, 0)
  hi <- pmin(upper, 1)
  feasible <- all(lo <= hi) && lo[[1L]] <= hi[[2L]] && hi[[2L]] > 0
  if (!feasible) return(.scmix_v25_rectangle_result(
    FALSE, NA_real_, NA_real_, NA, FALSE,
    "rectangle_misses_subgroup_primitive_support"))
  ratio_lower <- lo[[1L]] / hi[[2L]]
  maximum_numerator <- min(hi[[1L]], hi[[2L]])
  ratio_upper <- if (maximum_numerator <= 0) 0 else
    if (maximum_numerator >= lo[[2L]]) 1 else
      maximum_numerator / lo[[2L]]
  ratio_lower <- max(0, ratio_lower)
  ratio_upper <- min(1, ratio_upper)
  .scmix_v25_rectangle_result(
    TRUE, ratio_lower, ratio_upper,
    lo[[1L]] == 0 || hi[[2L]] == 1 || ratio_upper == 1,
    ratio_lower == 0 && ratio_upper == 1,
    "image_of_constrained_subgroup_rectangle")
}

## Range-respecting one-step point update.  `plugin` is the cross-fit plug-in
## point and `raw` is the unrestricted one-step point.  The chart is defined
## only at an interior plug-in point; absence of an interior chart makes the
## family ineligible rather than silently truncating the estimator.
.scmix_v25_chart_point <- function(
    type = c("unrestricted", "probability", "positive", "subgroup_ratio",
             "heterogeneity_share", "mrs"), plugin, raw) {
  type <- match.arg(type)
  plugin <- as.numeric(plugin)
  raw <- as.numeric(raw)
  if (length(plugin) != length(raw) || any(!is.finite(c(plugin, raw)))) {
    return(list(interior = FALSE, primitive = NA_real_, target = NA_real_,
                reason = "invalid_point_inputs"))
  }
  if (type == "unrestricted") return(list(
    interior = length(plugin) == 1L, primitive = raw, target = raw,
    reason = "identity_chart"))
  if (type == "mrs") {
    target <- if (length(raw) == 2L && raw[[2L]] != 0)
      -raw[[1L]] / raw[[2L]] else NA_real_
    return(list(interior = length(plugin) == 2L && plugin[[2L]] != 0,
                primitive = raw, target = target,
                reason = "identity_primitive_ratio_map"))
  }
  if (type == "probability") {
    interior <- length(plugin) == 1L && plugin > 0 && plugin < 1
    if (!interior) return(list(interior = FALSE, primitive = NA_real_,
                               target = NA_real_, reason = "plugin_on_boundary"))
    value <- stats::plogis(stats::qlogis(plugin) +
                             (raw - plugin) / (plugin * (1 - plugin)))
    return(list(interior = TRUE, primitive = value, target = value,
                reason = "logit_chart"))
  }
  if (type == "positive") {
    interior <- length(plugin) == 1L && plugin > 0
    if (!interior) return(list(interior = FALSE, primitive = NA_real_,
                               target = NA_real_, reason = "plugin_on_boundary"))
    value <- exp(log(plugin) + (raw - plugin) / plugin)
    return(list(interior = TRUE, primitive = value, target = value,
                reason = "log_chart"))
  }
  if (type == "subgroup_ratio") {
    interior <- length(plugin) == 2L &&
      plugin[[1L]] > 0 && plugin[[1L]] < plugin[[2L]] && plugin[[2L]] < 1
    if (!interior) return(list(interior = FALSE, primitive = rep(NA_real_, 2L),
                               target = NA_real_, reason = "plugin_on_boundary"))
    a <- plugin[[1L]]
    b <- plugin[[2L]]
    q <- a / b
    chart <- c(stats::qlogis(q), stats::qlogis(b))
    derivative <- rbind(
      c(1 / (b * q * (1 - q)), -1 / (b * (1 - q))),
      c(0, 1 / (b * (1 - b))))
    updated <- chart + as.numeric(derivative %*% (raw - plugin))
    b_rr <- stats::plogis(updated[[2L]])
    q_rr <- stats::plogis(updated[[1L]])
    primitive <- c(numerator = q_rr * b_rr, denominator = b_rr)
    return(list(interior = TRUE, primitive = primitive, target = q_rr,
                reason = "conditional_logit_chart"))
  }
  ## Heterogeneity uses the joint log chart for H_Z and H_R.
  interior <- length(plugin) == 2L && all(plugin > 0)
  if (!interior) return(list(interior = FALSE, primitive = rep(NA_real_, 2L),
                             target = NA_real_, reason = "plugin_on_boundary"))
  primitive <- exp(log(plugin) + (raw - plugin) / plugin)
  names(primitive) <- c("H_Z", "H_R")
  list(interior = TRUE, primitive = primitive,
       target = primitive[[1L]] / sum(primitive),
       reason = "joint_log_chart")
}

## Complete one family once its exact primitive vector has been assembled.
## `member_map` is a named list whose elements are either one primitive ID or
## two IDs for subgroup/MRS mappings.  Directional heterogeneity has a fixed
## minimal vector and reports H_Z, H_R, and their share.
.scmix_v25_finalize_family <- function(
    family_id, estimate, influence, respondent_id, bank,
    plugin = estimate, member_map = NULL, level = 0.95) {
  family_id <- match.arg(family_id, c(
    "smooth_means", "choice_probabilities", "directional_shares",
    "subgroup_ratios", "directional_heterogeneity", "mrs"))
  multiplier <- .scmix_v25_family_multiplier(
    estimate, influence, respondent_id, bank, level)
  out <- list(family_id = family_id, multiplier = multiplier,
              complete = FALSE, members = list())
  if (!multiplier$release_eligible) {
    out$reason <- multiplier$reason
    class(out) <- c("scmix_v25_family_result", "list")
    return(out)
  }
  raw <- multiplier$estimate
  plugin_names <- names(plugin)
  plugin <- as.numeric(plugin)
  names(plugin) <- plugin_names
  if (!setequal(names(plugin), names(raw)) || any(!is.finite(plugin))) {
    .scmix_v25_target_stop("plugin must match the family primitive vector.")
  }
  plugin <- plugin[names(raw)]
  default_map <- switch(
    family_id,
    smooth_means = stats::setNames(as.list(names(raw)), names(raw)),
    choice_probabilities = stats::setNames(as.list(names(raw)), names(raw)),
    directional_shares = stats::setNames(as.list(names(raw)), names(raw)),
    directional_heterogeneity = list(
      heterogeneity_explained_variance = names(raw)[[1L]],
      heterogeneity_residual_variance = names(raw)[[2L]],
      heterogeneity_explained_share = names(raw)[1:2]),
    mrs = list(mrs = names(raw)[1:2]),
    NULL)
  if (is.null(member_map)) member_map <- default_map
  if (is.null(member_map) || !is.list(member_map) ||
      is.null(names(member_map)) || any(!nzchar(names(member_map)))) {
    .scmix_v25_target_stop(
      "member_map must name each reported family member.")
  }
  for (member in names(member_map)) {
    ids <- as.character(member_map[[member]])
    index <- match(ids, names(raw))
    if (anyNA(index)) .scmix_v25_target_stop(
      "member_map refers to an absent primitive: ", member, ".")
    map_type <- if (family_id == "smooth_means") "unrestricted" else
      if (family_id %in% c("choice_probabilities", "directional_shares"))
        "probability" else
      if (family_id == "subgroup_ratios") "subgroup_ratio" else
      if (family_id == "directional_heterogeneity" && length(index) == 1L)
        "positive" else
      if (family_id == "directional_heterogeneity")
        "heterogeneity_share" else "mrs"
    mapped <- .scmix_v25_map_rectangle(
      map_type, multiplier$lower[index], multiplier$upper[index])
    point <- .scmix_v25_chart_point(
      map_type, plugin[index], raw[index])
    out$members[[member]] <- list(
      primitive_id = ids, estimate = point$target,
      interval = c(lower = mapped$lower, upper = mapped$upper),
      interval_nonempty = mapped$nonempty,
      interval_uninformative = mapped$uninformative,
      boundary_touch = mapped$boundary_touch,
      map_reason = mapped$reason, chart_interior = point$interior,
      chart_reason = point$reason)
  }
  out$complete <- all(vapply(out$members, function(x)
    isTRUE(x$interval_nonempty) && isTRUE(x$chart_interior), logical(1L)))
  out$reason <- if (out$complete) "complete_family" else
    "one_or_more_members_incomplete"
  class(out) <- c("scmix_v25_family_result", "list")
  out
}

## Exact Monte Carlo summaries named in the frozen v2.5 decision registry.
.scmix_v25_wilson90 <- function(success, total) {
  if (!is.numeric(success) || !is.numeric(total) || length(success) != 1L ||
      length(total) != 1L || !is.finite(success) || !is.finite(total) ||
      success < 0 || total < 0 || success > total ||
      success != as.integer(success) || total != as.integer(total)) {
    .scmix_v25_target_stop("success and total must be valid binomial counts.")
  }
  if (total == 0) return(c(lower = NA_real_, upper = NA_real_))
  z <- stats::qnorm(0.95)
  proportion <- success / total
  center <- (proportion + z^2 / (2 * total)) / (1 + z^2 / total)
  half <- z * sqrt(proportion * (1 - proportion) / total +
                     z^2 / (4 * total^2)) / (1 + z^2 / total)
  c(lower = max(0, center - half), upper = min(1, center + half))
}

.scmix_v25_clopper_pearson_upper95 <- function(invalid, completed) {
  if (!is.numeric(invalid) || !is.numeric(completed) ||
      length(invalid) != 1L || length(completed) != 1L ||
      !is.finite(invalid) || !is.finite(completed) || invalid < 0 ||
      completed < 0 || invalid > completed || invalid != as.integer(invalid) ||
      completed != as.integer(completed)) {
    .scmix_v25_target_stop(
      "invalid and completed must be valid binomial counts.")
  }
  if (completed == 0) return(NA_real_)
  if (invalid == completed) return(1)
  stats::qbeta(0.95, invalid + 1, completed - invalid)
}

.scmix_v25_binomial_summary <- function(success, total) {
  interval <- .scmix_v25_wilson90(success, total)
  list(success = as.integer(success), total = as.integer(total),
       estimate = if (total > 0) success / total else NA_real_,
       mcse = if (total > 0) sqrt((success / total) *
         (1 - success / total) / total) else NA_real_,
       wilson90 = interval)
}

## Apply one frozen family-cell rule.  `released` and `covered` are evaluated
## only among completed replications.  Weak cells either demonstrate successful
## withholding (at most two releases), demonstrate usable regular inference
## (at least fifteen releases and at least 90 percent conditional coverage), or
## are mechanically inconclusive.
.scmix_v25_family_approval_rule <- function(
    criterion = c("regular", "exact_boundary", "weak_or_bounded",
                  "descriptive_only"), completed, released, covered) {
  criterion <- match.arg(criterion)
  completed <- as.logical(completed)
  released <- as.logical(released)
  covered <- as.logical(covered)
  if (!length(completed) || length(released) != length(completed) ||
      length(covered) != length(completed) || anyNA(completed) ||
      anyNA(released)) {
    .scmix_v25_target_stop(
      "completed and released must be complete equal-length logical vectors.")
  }
  if (any(released & !completed) || any(!is.na(covered) & covered & !released)) {
    .scmix_v25_target_stop(
      "A release requires completion and coverage requires release.")
  }
  completed_n <- sum(completed)
  release_n <- sum(released & completed)
  coverage_n <- sum(covered %in% TRUE & released & completed)
  coverage <- .scmix_v25_binomial_summary(coverage_n, release_n)
  base <- list(
    criterion = criterion, completed = completed_n,
    releases = release_n, covered = coverage_n,
    release_rate = release_n / length(completed),
    conditional_coverage = coverage$estimate,
    coverage_mcse = coverage$mcse, coverage_wilson90 = coverage$wilson90)
  if (criterion == "descriptive_only") {
    return(c(base, list(state = "descriptive_only", passed = NA,
                        reason = "not_an_approval_row")))
  }
  if (criterion == "regular") {
    passed <- release_n / length(completed) >= 0.75 &&
      is.finite(coverage$estimate) && coverage$estimate >= 0.90
    return(c(base, list(
      state = if (passed) "pass" else "fail", passed = passed,
      reason = "release_at_least_75_and_conditional_coverage_at_least_90")))
  }
  if (criterion == "exact_boundary") {
    invalid_rate <- if (completed_n) release_n / completed_n else NA_real_
    exact_upper <- .scmix_v25_clopper_pearson_upper95(release_n, completed_n)
    passed <- is.finite(invalid_rate) && invalid_rate <= 0.05 &&
      is.finite(exact_upper) && exact_upper <= 0.10
    return(c(base, list(
      invalid_regular_release_rate = invalid_rate,
      clopper_pearson_upper95 = exact_upper,
      state = if (passed) "pass" else "fail", passed = passed,
      reason = "invalid_rate_at_most_05_and_exact_upper_at_most_10")))
  }
  if (release_n <= 2L) return(c(base, list(
    state = "pass", passed = TRUE,
    reason = "successful_withholding_at_most_two_releases")))
  if (release_n < 15L) return(c(base, list(
    state = "inconclusive", passed = FALSE,
    reason = "three_to_fourteen_releases")))
  passed <- is.finite(coverage$estimate) && coverage$estimate >= 0.90
  c(base, list(
    state = if (passed) "pass" else "fail", passed = passed,
    reason = "at_least_fifteen_releases_requires_90_percent_coverage"))
}

## Mechanically assess the full family-by-cell table.  Every scheduled
## replication must appear, including failed executions, so completion and
## false-release denominators cannot be changed after outcomes are observed.
.scmix_v25_assess_validation <- function(
    replications, approval_matrix, expected_reps = 50L) {
  required_columns <- c("cell_id", "family_id", "replication", "completed",
                        "released", "covered")
  if (!is.data.frame(replications) ||
      !all(required_columns %in% names(replications))) {
    .scmix_v25_target_stop(
      "replications lacks the required family-cell result columns.")
  }
  if (!is.data.frame(approval_matrix) ||
      !all(c("cell_id", "family_id", "required", "criterion") %in%
           names(approval_matrix))) {
    .scmix_v25_target_stop("approval_matrix has the wrong schema.")
  }
  if (!is.numeric(expected_reps) || length(expected_reps) != 1L ||
      expected_reps < 1L || expected_reps != as.integer(expected_reps)) {
    .scmix_v25_target_stop("expected_reps must be a positive integer.")
  }
  if (!is.numeric(replications$replication) ||
      any(!is.finite(replications$replication)) ||
      any(replications$replication != as.integer(replications$replication))) {
    .scmix_v25_target_stop("replication must contain scheduled integer IDs.")
  }
  key <- paste(replications$cell_id, replications$family_id,
               replications$replication, sep = "\r")
  if (anyDuplicated(key)) .scmix_v25_target_stop(
    "There must be one row per cell, family, and scheduled replication.")
  matrix_key <- paste(approval_matrix$cell_id, approval_matrix$family_id,
                      sep = "\r")
  observed_key <- unique(paste(replications$cell_id, replications$family_id,
                               sep = "\r"))
  if (anyDuplicated(matrix_key) || !setequal(matrix_key, observed_key)) {
    .scmix_v25_target_stop(
      "Observed family-cell pairs must equal the approval matrix.")
  }
  split_pair <- split(replications, paste(replications$cell_id,
                                          replications$family_id, sep = "\r"))
  expected_sequence <- seq_len(as.integer(expected_reps))
  if (any(vapply(split_pair, function(x)
    !identical(sort(as.integer(x$replication)), expected_sequence),
    logical(1L)))) {
    .scmix_v25_target_stop(
      "Every family-cell pair must contain every scheduled replication.")
  }
  ## Execution completion is a cell-replication property.  All family rows
  ## must carry the same value, preventing family-specific denominator drift.
  cell_rep <- split(replications, paste(replications$cell_id,
                                        replications$replication, sep = "\r"))
  if (any(vapply(cell_rep, function(x)
    length(unique(as.logical(x$completed))) != 1L, logical(1L)))) {
    .scmix_v25_target_stop(
      "completed must agree across families within each cell replication.")
  }
  completion_rows <- lapply(split(cell_rep, vapply(cell_rep, function(x)
    as.character(x$cell_id[[1L]]), character(1L))), function(groups) {
      completed <- vapply(groups, function(x) isTRUE(x$completed[[1L]]),
                           logical(1L))
      data.frame(
        cell_id = groups[[1L]]$cell_id[[1L]],
        completed = sum(completed), scheduled = length(completed),
        completion_rate = mean(completed),
        passed = mean(completed) >= 0.95, stringsAsFactors = FALSE)
    })
  completion <- do.call(rbind, completion_rows)
  rownames(completion) <- NULL
  assessment <- vector("list", nrow(approval_matrix))
  for (i in seq_len(nrow(approval_matrix))) {
    row <- approval_matrix[i, , drop = FALSE]
    dat <- replications[
      replications$cell_id == row$cell_id[[1L]] &
        replications$family_id == row$family_id[[1L]], , drop = FALSE]
    rule <- .scmix_v25_family_approval_rule(
      row$criterion[[1L]], dat$completed, dat$released, dat$covered)
    assessment[[i]] <- data.frame(
      family_id = row$family_id[[1L]], cell_id = row$cell_id[[1L]],
      required = as.logical(row$required[[1L]]),
      criterion = row$criterion[[1L]], state = rule$state,
      passed = if (is.na(rule$passed)) NA else isTRUE(rule$passed),
      completed = rule$completed, releases = rule$releases,
      covered = rule$covered, release_rate = rule$release_rate,
      conditional_coverage = rule$conditional_coverage,
      coverage_mcse = rule$coverage_mcse,
      wilson90_lower = rule$coverage_wilson90[["lower"]],
      wilson90_upper = rule$coverage_wilson90[["upper"]],
      invalid_regular_release_rate = if (is.null(
        rule$invalid_regular_release_rate)) NA_real_ else
          rule$invalid_regular_release_rate,
      clopper_pearson_upper95 = if (is.null(
        rule$clopper_pearson_upper95)) NA_real_ else
          rule$clopper_pearson_upper95,
      reason = rule$reason, stringsAsFactors = FALSE)
  }
  assessment <- do.call(rbind, assessment)
  rownames(assessment) <- NULL
  required_rows <- assessment$required %in% TRUE
  list(
    completion = completion, family_cell = assessment,
    all_cell_completion_pass = all(completion$passed),
    all_required_rows_pass = all(assessment$passed[required_rows] %in% TRUE),
    approved = all(completion$passed) &&
      all(assessment$passed[required_rows] %in% TRUE),
    rule = "all_cells_complete_at_least_95_percent_and_every_required_matrix_row_passes")
}
