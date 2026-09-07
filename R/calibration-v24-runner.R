## Hash-bound end-to-end evaluator for the prospective v2.4 study.
##
## The state machine in calibration-v24-evaluator.R is the only outcome
## unlock.  This file consumes a sealed synthetic outcome, runs the one frozen
## fit, evaluates all registered targets under GH15 and both GH31 branches,
## applies the fixed gates, and writes one complete replication artifact.

.scmix_v24_outcome_check <- function(outcome, plan, template, geometry,
                                     evaluator) {
  if (!.scmix_v24_intact(outcome, "scmix_v24_generated_outcome") ||
      !identical(outcome$protocol_id, plan$protocol_id) ||
      !identical(outcome$plan_id, plan$plan_id) ||
      !identical(outcome$design_id, template$design_id) ||
      !identical(outcome$cell_id, geometry$cell_id) ||
      !identical(outcome$evaluator_approval_sha256, evaluator$sha256) ||
      !identical(outcome$application_outcomes_used, FALSE) ||
      !identical(outcome$synthetic_outcome, TRUE) ||
      !outcome$stream %in% c("pilot", "evidence") ||
      length(outcome$y) != nrow(outcome$deltaX) ||
      length(outcome$respondent_id) != nrow(outcome$deltaX) ||
      ncol(outcome$deltaX) != ncol(template$deltaX) ||
      ncol(outcome$Z) != ncol(template$Z) ||
      any(!outcome$y %in% 0:1) ||
      length(unique(outcome$respondent_id)) != 400L) {
    stop("The v2.4 generated outcome is altered, incomplete, or off contract.",
         call. = FALSE)
  }
  group <- split(seq_along(outcome$respondent_id), outcome$respondent_id)
  if (any(lengths(group) < 1L) ||
      any(vapply(group, function(rows)
        length(unique(outcome$source_respondent_id[rows])) != 1L,
        logical(1L)))) {
    stop("A v2.4 resampled respondent sequence was split or mixed.",
         call. = FALSE)
  }
  invisible(outcome)
}

.scmix_v24_target_callbacks <- function(outcome, geometry, n_nodes) {
  contrast <- geometry$target_dual_raw
  if (is.null(rownames(contrast))) rownames(contrast) <- outcome$deltaX_names
  d <- contrast[, "direction"]
  a <- contrast[, "numerator"]
  b <- contrast[, "denominator"]
  names(d) <- names(a) <- names(b) <- rownames(contrast)
  choice <- .scmix_v24_frozen_choice_target(
    task_x = structure(outcome$deltaX,
                       dimnames = list(NULL, outcome$deltaX_names)),
    task_respondent_id = outcome$respondent_id,
    kappa_sign = 1, n_nodes = n_nodes, label = "choice_probability")
  ids <- unique(as.character(outcome$respondent_id))
  first <- match(ids, outcome$respondent_id)
  z <- geometry$z_task[outcome$source_row_index[first]]
  high <- stats::setNames(as.numeric(z > 0), ids)
  callbacks <- list(
    mean_direction = scmix_inference_target(
      "tau", contrast = d, label = "mean_direction"),
    mean_numerator = scmix_inference_target(
      "tau", contrast = a, label = "mean_numerator"),
    mean_denominator = scmix_inference_target(
      "tau", contrast = b, label = "mean_denominator"),
    choice_probability = choice,
    preference_direction_share = .scmix_v24_directional_share_target(
      d, label = "preference_direction_share"),
    compensating_benefit_share = .scmix_v24_directional_share_target(
      d + 0.5 * b, label = "compensating_benefit_share",
      include_ties = TRUE),
    subgroup_high = .scmix_v24_subgroup_primitives(
      choice, high, label = "subgroup_high"),
    subgroup_low = .scmix_v24_subgroup_primitives(
      choice, 1 - high, label = "subgroup_low"),
    mrs = scmix_inference_target(
      "mrs_primitives", numerator = a, denominator = b,
      label = "mrs"),
    active_q1_covariance_eigenvalue = .scmix_v24_trace_sigma_target())
  heterogeneity <- .scmix_v24_heterogeneity_dml_adapter(
    contrast = d, callback_id = "heterogeneity_support",
    target_id = "directional_heterogeneity",
    primitive_ids = c("H_Z", "H_R"))
  list(callbacks = callbacks, heterogeneity = heterogeneity,
       high = high, contrasts = list(direction = d, numerator = a,
                                     denominator = b))
}

.scmix_v24_match_one <- function(x, pattern) {
  hit <- grep(pattern, x, value = TRUE)
  if (length(hit) != 1L) {
    stop("The v2.4 primitive label map did not resolve `", pattern, "`.",
         call. = FALSE)
  }
  hit
}

.scmix_v24_nonheterogeneity_map <- function(labels) {
  scalar <- c("mean_direction", "mean_numerator", "mean_denominator",
              "choice_probability",
              "active_q1_covariance_eigenvalue")
  old <- vapply(scalar, function(id) .scmix_v24_match_one(
    labels, paste0("^", id, "$")), character(1L))
  subgroup <- c(
    preference_direction_share = .scmix_v24_match_one(
      labels, "^preference_direction_share:preference_direction_share$"),
    preference_direction_variance = .scmix_v24_match_one(
      labels, "^preference_direction_share:.*directional_variance$"),
    compensating_benefit_share = .scmix_v24_match_one(
      labels, "^compensating_benefit_share:compensating_benefit_share$"),
    compensating_benefit_variance = .scmix_v24_match_one(
      labels, "^compensating_benefit_share:.*directional_variance$"),
    subgroup_high_numerator = .scmix_v24_match_one(
      labels, "^subgroup_high:.*numerator$"),
    subgroup_high_denominator = .scmix_v24_match_one(
      labels, "^subgroup_high:.*denominator$"),
    subgroup_low_numerator = .scmix_v24_match_one(
      labels, "^subgroup_low:.*numerator$"),
    subgroup_low_denominator = .scmix_v24_match_one(
      labels, "^subgroup_low:.*denominator$"),
    mrs_numerator = .scmix_v24_match_one(labels, "^mrs:numerator$"),
    mrs_denominator = .scmix_v24_match_one(labels, "^mrs:denominator$"))
  stats::setNames(c(names(old), names(subgroup)), c(unname(old),
                                                    unname(subgroup)))
}

.scmix_v24_dml_call <- function(assembled, plugin_targets, seed,
                                 v22_registry = NULL) {
  scmix_dml(
    fit = assembled, targets = character(), plugin_targets = plugin_targets,
    mu_basis = NULL, nu_grid = NULL, riesz_penalty = "identity",
    riesz_validation_fraction = 0, active_eigenvalue_min = NULL,
    rank_tolerance = max(ncol(assembled$deltaX), 1L) *
      .Machine$double.eps,
    information_eigenvalue_min = NULL,
    derivative_eps = .Machine$double.eps^(1 / 3),
    allow_numeric_derivatives = FALSE,
    derivative_refinement_factor = 2,
    derivative_refinement_tolerance = 1e-4,
    riesz_equation_tolerance = 1e12,
    ridge_sensitivity_tolerance = 1e12,
    riesz_numerical_rank_tolerance = 1e-10,
    v22_registry = v22_registry,
    multiplier_draws = 0L, seed = as.integer(seed),
    riesz_rule = "fixed_n_train_minus_three_quarters",
    riesz_numerical_rank_rule = "dimension_epsilon")
}

.scmix_v24_dml_analysis_specs <- function(target_bundle) {
  callback <- target_bundle$callbacks
  adapter <- target_bundle$heterogeneity
  list(
    active_q1_covariance_eigenvalue = list(
      family_id = "global_fit_regularity",
      target_ids = "active_q1_covariance_eigenvalue",
      plugin_targets = callback["active_q1_covariance_eigenvalue"],
      canonical = "active_q1_covariance_eigenvalue",
      patterns = "^active_q1_covariance_eigenvalue$", global = TRUE),
    mean_direction = list(
      family_id = "smooth_means", target_ids = "mean_direction",
      plugin_targets = callback["mean_direction"],
      canonical = "mean_direction", patterns = "^mean_direction$"),
    mean_numerator = list(
      family_id = "smooth_means", target_ids = "mean_numerator",
      plugin_targets = callback["mean_numerator"],
      canonical = "mean_numerator", patterns = "^mean_numerator$"),
    mean_denominator = list(
      family_id = "smooth_means", target_ids = "mean_denominator",
      plugin_targets = callback["mean_denominator"],
      canonical = "mean_denominator", patterns = "^mean_denominator$"),
    choice_probability = list(
      family_id = "choice_probabilities", target_ids = "choice_probability",
      plugin_targets = callback["choice_probability"],
      canonical = "choice_probability", patterns = "^choice_probability$"),
    preference_direction_share = list(
      family_id = "directional_shares",
      target_ids = "preference_direction_share",
      plugin_targets = callback["preference_direction_share"],
      canonical = c("preference_direction_share",
                    "preference_direction_variance"),
      patterns = c("^preference_direction_share:preference_direction_share$",
                   "^preference_direction_share:.*directional_variance$")),
    compensating_benefit_share = list(
      family_id = "directional_shares",
      target_ids = "compensating_benefit_share",
      plugin_targets = callback["compensating_benefit_share"],
      canonical = c("compensating_benefit_share",
                    "compensating_benefit_variance"),
      patterns = c("^compensating_benefit_share:compensating_benefit_share$",
                   "^compensating_benefit_share:.*directional_variance$")),
    subgroup_high_choice_share = list(
      family_id = "subgroup_ratios",
      target_ids = "subgroup_high_choice_share",
      plugin_targets = callback["subgroup_high"],
      canonical = c("subgroup_high_numerator", "subgroup_high_denominator"),
      patterns = c("^subgroup_high:.*numerator$",
                   "^subgroup_high:.*denominator$")),
    subgroup_low_choice_share = list(
      family_id = "subgroup_ratios",
      target_ids = "subgroup_low_choice_share",
      plugin_targets = callback["subgroup_low"],
      canonical = c("subgroup_low_numerator", "subgroup_low_denominator"),
      patterns = c("^subgroup_low:.*numerator$",
                   "^subgroup_low:.*denominator$")),
    mrs = list(
      family_id = "mrs", target_ids = "mrs",
      plugin_targets = callback["mrs"],
      canonical = c("mrs_numerator", "mrs_denominator"),
      patterns = c("^mrs:numerator$", "^mrs:denominator$")),
    directional_heterogeneity = list(
      family_id = "directional_heterogeneity",
      target_ids = c("heterogeneity_explained_variance",
                     "heterogeneity_residual_variance",
                     "heterogeneity_explained_share"),
      plugin_targets = adapter$plugin_targets,
      v22_registry = adapter$primitive_registry,
      canonical = c("heterogeneity_explained_variance",
                    "heterogeneity_residual_variance"),
      patterns = c("^H_Z$", "^H_R$"), component = "heterogeneity"))
}

.scmix_v24_dml_result_map <- function(dml, spec) {
  required <- c("estimate", "crossfit_plugin", "influence", "respondent_id",
                "fold_details")
  if (!inherits(dml, "scmix_dml") || !all(required %in% names(dml)) ||
      is.null(names(dml$estimate)) || is.null(names(dml$crossfit_plugin)) ||
      is.null(colnames(dml$influence)) || !is.list(dml$fold_details) ||
      !length(dml$fold_details)) {
    stop("The target-specific DML result is incomplete.", call. = FALSE)
  }
  raw <- vapply(spec$patterns, function(pattern) {
    hit <- grep(pattern, names(dml$estimate), value = TRUE)
    if (length(hit) != 1L) stop(
      "The target-specific DML labels do not match their frozen primitive map.",
      call. = FALSE)
    hit
  }, character(1L))
  if (anyDuplicated(raw) || !all(raw %in% names(dml$crossfit_plugin)) ||
      !all(raw %in% colnames(dml$influence)) ||
      any(!is.finite(c(dml$estimate[raw], dml$crossfit_plugin[raw],
                       dml$influence[, raw, drop = FALSE])))) {
    stop("The target-specific DML result contains invalid primitives.",
         call. = FALSE)
  }
  stats::setNames(spec$canonical, raw)
}

.scmix_v24_merge_named_fold_field <- function(results, fold, field) {
  value <- lapply(results, function(x) x$dml$fold_details[[fold]][[field]])
  if (any(vapply(value, is.null, logical(1L)))) {
    stop("A successful target-specific DML result lacks `", field, "`.",
         call. = FALSE)
  }
  out <- do.call(c, value)
  if (is.null(names(out)) || anyDuplicated(names(out))) {
    stop("Target-specific DML fold diagnostics cannot be merged uniquely.",
         call. = FALSE)
  }
  out
}

.scmix_v24_merge_base_dml <- function(results) {
  if (!length(results)) stop("No base DML result is available.", call. = FALSE)
  ids <- lapply(results, function(x) as.character(x$dml$respondent_id))
  if (any(vapply(ids[-1L], function(x) !identical(x, ids[[1L]]),
                 logical(1L)))) {
    stop("Target-specific DML respondent orders differ.", call. = FALSE)
  }
  raw <- unlist(lapply(results, function(x) names(x$map)), use.names = FALSE)
  canonical <- unlist(lapply(results, function(x) unname(x$map)),
                      use.names = FALSE)
  if (anyDuplicated(raw) || anyDuplicated(canonical)) {
    stop("Target-specific DML primitive maps overlap.", call. = FALSE)
  }
  out <- results[[1L]]$dml
  out$estimate <- do.call(c, unname(lapply(results, function(x)
    x$dml$estimate[names(x$map)])))
  out$crossfit_plugin <- do.call(c, unname(lapply(results, function(x)
    x$dml$crossfit_plugin[names(x$map)])))
  out$influence <- do.call(cbind, lapply(results, function(x)
    x$dml$influence[, names(x$map), drop = FALSE]))
  rownames(out$influence) <- ids[[1L]]
  K <- length(out$fold_details)
  if (any(vapply(results, function(x)
    length(x$dml$fold_details) != K, logical(1L)))) {
    stop("Target-specific DML outer-fold counts differ.", call. = FALSE)
  }
  merge_fields <- c(
    "derivative_refinement_error", "derivative_refinement_protocol_exact",
    "honest_information_scaled_riesz_defect",
    "honest_riesz_null_space_defect", "ridge_neighborhood_artifact")
  for (fold in seq_len(K)) for (field in merge_fields) {
    out$fold_details[[fold]][[field]] <-
      .scmix_v24_merge_named_fold_field(results, fold, field)
  }
  out$target_specific_analysis_ids <- names(results)
  out$target_specific_composite <- TRUE
  out$call <- NULL
  class(out) <- unique(c("scmix_v24_composite_dml", class(out)))
  list(dml = out, map = stats::setNames(canonical, raw))
}

.scmix_v24_dml_once <- function(assembled, target_bundle, seed) {
  specs <- .scmix_v24_dml_analysis_specs(target_bundle)
  run <- function(id, fail_global = FALSE) {
    spec <- specs[[id]]
    answer <- tryCatch({
      dml <- .scmix_v24_dml_call(
        assembled, spec$plugin_targets, seed,
        v22_registry = spec$v22_registry %||% NULL)
      map <- .scmix_v24_dml_result_map(dml, spec)
      list(
        analysis_id = id, family_id = spec$family_id,
        target_ids = spec$target_ids, status = "success",
        dml = dml, map = map, error = NULL)
    }, error = function(e) list(
      analysis_id = id, family_id = spec$family_id,
      target_ids = spec$target_ids, status = "failed",
      dml = NULL, map = NULL, error = conditionMessage(e)))
    if (isTRUE(fail_global) && !identical(answer$status, "success")) {
      stop("The global v2.4 fit-regularity DML preflight failed. ",
           answer$error, call. = FALSE)
    }
    answer
  }

  ## This target exercises the common fit, score, fold, and Riesz machinery.
  ## A failure here is global.  Once it passes, all remaining failures are
  ## isolated to their prespecified target and hence to its containing family.
  analyses <- list(active_q1_covariance_eigenvalue =
                     run("active_q1_covariance_eigenvalue", TRUE))
  remaining <- setdiff(names(specs), names(analyses))
  analyses[remaining] <- lapply(remaining, run)
  success <- analyses[vapply(analyses, function(x)
    identical(x$status, "success"), logical(1L))]
  base_success <- success[vapply(success, function(x)
    !identical(specs[[x$analysis_id]]$component, "heterogeneity"),
    logical(1L))]
  merged <- .scmix_v24_merge_base_dml(base_success)
  heterogeneity_result <- success[["directional_heterogeneity"]]

  estimate <- plugin <- numeric()
  influence <- matrix(numeric(length(merged$dml$respondent_id) * 0L),
                      nrow = length(merged$dml$respondent_id), ncol = 0L,
                      dimnames = list(merged$dml$respondent_id, NULL))
  for (result in success) {
    raw <- names(result$map)
    canonical <- unname(result$map)
    one_estimate <- result$dml$estimate[raw]
    one_plugin <- result$dml$crossfit_plugin[raw]
    one_influence <- result$dml$influence[, raw, drop = FALSE]
    names(one_estimate) <- names(one_plugin) <- canonical
    colnames(one_influence) <- canonical
    estimate <- c(estimate, one_estimate)
    plugin <- c(plugin, one_plugin)
    influence <- cbind(influence, one_influence)
  }
  failed <- analyses[vapply(analyses, function(x)
    identical(x$status, "failed"), logical(1L))]
  failure_records <- if (!length(failed)) {
    data.frame(
      analysis_id = character(), family_id = character(),
      target_ids = character(), error = character(),
      stringsAsFactors = FALSE)
  } else do.call(rbind, lapply(failed, function(x) data.frame(
    analysis_id = x$analysis_id, family_id = x$family_id,
    target_ids = paste(x$target_ids, collapse = "|"), error = x$error,
    stringsAsFactors = FALSE)))
  failed_targets <- unique(unlist(lapply(failed, `[[`, "target_ids"),
                                  use.names = FALSE))
  failed_families <- unique(vapply(failed, `[[`, character(1L),
                                   "family_id"))
  failed_families <- setdiff(failed_families, "global_fit_regularity")
  heterogeneity_map <- if (is.null(heterogeneity_result)) {
    c(H_Z = "heterogeneity_explained_variance",
      H_R = "heterogeneity_residual_variance")
  } else heterogeneity_result$map
  rownames(influence) <- merged$dml$respondent_id
  list(
    estimate = estimate, plugin = plugin, influence = influence,
    respondent_id = merged$dml$respondent_id,
    base = merged$dml,
    heterogeneity = if (is.null(heterogeneity_result)) NULL else
      heterogeneity_result$dml,
    base_label_map = merged$map,
    heterogeneity_label_map = heterogeneity_map,
    analyses = analyses, failure_records = failure_records,
    failed_targets = failed_targets, failed_families = failed_families,
    available_targets = setdiff(
      names(.scmix_v24_gate_target_specs()), failed_targets),
    target_specific = TRUE,
    complete_target_registry = !length(failed_targets),
    protocol_id = "paperps-inference-2026-09-06-v2.4")
}

.scmix_v24_reported_targets <- function(estimate) {
  get <- function(id) unname(estimate[[id]])
  ratio <- function(a, b, sign = 1) {
    if (!is.finite(b) || b == 0) NA_real_ else sign * a / b
  }
  HZ <- get("heterogeneity_explained_variance")
  HR <- get("heterogeneity_residual_variance")
  c(
    mean_direction = get("mean_direction"),
    mean_numerator = get("mean_numerator"),
    mean_denominator = get("mean_denominator"),
    choice_probability = get("choice_probability"),
    preference_direction_share = get("preference_direction_share"),
    compensating_benefit_share = get("compensating_benefit_share"),
    subgroup_high_choice_share = ratio(
      get("subgroup_high_numerator"),
      get("subgroup_high_denominator")),
    subgroup_low_choice_share = ratio(
      get("subgroup_low_numerator"), get("subgroup_low_denominator")),
    heterogeneity_explained_variance = HZ,
    heterogeneity_residual_variance = HR,
    heterogeneity_explained_share = ratio(HZ, HZ + HR),
    mrs = ratio(get("mrs_numerator"), get("mrs_denominator"), -1),
    active_q1_covariance_eigenvalue =
      get("active_q1_covariance_eigenvalue"))
}

.scmix_v24_reported_influence <- function(estimate, influence) {
  influence <- as.matrix(influence)
  direct <- c("mean_direction", "mean_numerator", "mean_denominator",
              "choice_probability", "preference_direction_share",
              "compensating_benefit_share",
              "heterogeneity_explained_variance",
              "heterogeneity_residual_variance",
              "active_q1_covariance_eigenvalue")
  out <- influence[, direct, drop = FALSE]
  ratio_if <- function(a, b, sign = 1) {
    av <- estimate[[a]]; bv <- estimate[[b]]
    sign * (influence[, a] / bv - av * influence[, b] / bv^2)
  }
  out <- cbind(
    out,
    subgroup_high_choice_share = ratio_if(
      "subgroup_high_numerator", "subgroup_high_denominator"),
    subgroup_low_choice_share = ratio_if(
      "subgroup_low_numerator", "subgroup_low_denominator"))
  HZ <- estimate[["heterogeneity_explained_variance"]]
  HR <- estimate[["heterogeneity_residual_variance"]]
  total <- HZ + HR
  out <- cbind(
    out,
    heterogeneity_explained_share =
      HR / total^2 * influence[, "heterogeneity_explained_variance"] -
      HZ / total^2 * influence[, "heterogeneity_residual_variance"],
    mrs = ratio_if("mrs_numerator", "mrs_denominator", -1))
  order <- names(.scmix_v24_reported_targets(estimate))
  out[, order, drop = FALSE]
}

.scmix_v24_inference_view <- function(inference) {
  estimate <- inference$estimate
  plugin <- inference$plugin
  influence <- as.matrix(inference$influence)
  respondent_id <- as.character(inference$respondent_id)
  target_ids <- c(
    "mean_direction", "mean_numerator", "mean_denominator",
    "choice_probability", "preference_direction_share",
    "compensating_benefit_share", "subgroup_high_choice_share",
    "subgroup_low_choice_share", "heterogeneity_explained_variance",
    "heterogeneity_residual_variance", "heterogeneity_explained_share",
    "mrs", "active_q1_covariance_eigenvalue")
  transform <- function(value) {
    out <- stats::setNames(rep(NA_real_, length(target_ids)), target_ids)
    direct <- intersect(
      c("mean_direction", "mean_numerator", "mean_denominator",
        "choice_probability", "preference_direction_share",
        "compensating_benefit_share",
        "heterogeneity_explained_variance",
        "heterogeneity_residual_variance",
        "active_q1_covariance_eigenvalue"), names(value))
    out[direct] <- value[direct]
    ratio <- function(a, b, sign = 1) {
      if (all(c(a, b) %in% names(value)) && is.finite(value[[b]]) &&
          value[[b]] != 0) sign * value[[a]] / value[[b]] else NA_real_
    }
    out[["subgroup_high_choice_share"]] <- ratio(
      "subgroup_high_numerator", "subgroup_high_denominator")
    out[["subgroup_low_choice_share"]] <- ratio(
      "subgroup_low_numerator", "subgroup_low_denominator")
    out[["mrs"]] <- ratio("mrs_numerator", "mrs_denominator", -1)
    if (all(c("heterogeneity_explained_variance",
              "heterogeneity_residual_variance") %in% names(value))) {
      total <- value[["heterogeneity_explained_variance"]] +
        value[["heterogeneity_residual_variance"]]
      if (is.finite(total) && total != 0) {
        out[["heterogeneity_explained_share"]] <-
          value[["heterogeneity_explained_variance"]] / total
      }
    }
    out
  }
  reported <- transform(estimate)
  reported_plugin <- transform(plugin)
  reported_if <- matrix(
    NA_real_, nrow(influence), length(target_ids),
    dimnames = list(rownames(influence), target_ids))
  direct <- intersect(
    c("mean_direction", "mean_numerator", "mean_denominator",
      "choice_probability", "preference_direction_share",
      "compensating_benefit_share", "heterogeneity_explained_variance",
      "heterogeneity_residual_variance",
      "active_q1_covariance_eigenvalue"), colnames(influence))
  reported_if[, direct] <- influence[, direct, drop = FALSE]
  ratio_if <- function(target, a, b, sign = 1) {
    if (!all(c(a, b) %in% names(estimate)) ||
        !all(c(a, b) %in% colnames(influence)) ||
        !is.finite(estimate[[b]]) || estimate[[b]] == 0) return()
    reported_if[, target] <<- sign * (
      influence[, a] / estimate[[b]] -
        estimate[[a]] * influence[, b] / estimate[[b]]^2)
  }
  ratio_if("subgroup_high_choice_share", "subgroup_high_numerator",
           "subgroup_high_denominator")
  ratio_if("subgroup_low_choice_share", "subgroup_low_numerator",
           "subgroup_low_denominator")
  ratio_if("mrs", "mrs_numerator", "mrs_denominator", -1)
  hz <- "heterogeneity_explained_variance"
  hr <- "heterogeneity_residual_variance"
  if (all(c(hz, hr) %in% names(estimate)) &&
      all(c(hz, hr) %in% colnames(influence))) {
    total <- estimate[[hz]] + estimate[[hr]]
    if (is.finite(total) && total != 0) {
      reported_if[, "heterogeneity_explained_share"] <-
        estimate[[hr]] / total^2 * influence[, hz] -
        estimate[[hz]] / total^2 * influence[, hr]
    }
  }
  reported_se <- stats::setNames(rep(Inf, length(target_ids)), target_ids)
  valid_reported <- is.finite(reported) & vapply(seq_along(target_ids),
    function(j) all(is.finite(reported_if[, j])), logical(1L))
  if (any(valid_reported)) {
    reported_se[valid_reported] <- .scmix_v24_influence_se(
      reported_if[, valid_reported, drop = FALSE])
  }
  primitive_se <- stats::setNames(rep(Inf, length(estimate)), names(estimate))
  valid_primitive <- vapply(seq_along(estimate), function(j)
    is.finite(estimate[[j]]) && all(is.finite(influence[, j])), logical(1L))
  if (any(valid_primitive)) {
    primitive_se[valid_primitive] <- .scmix_v24_influence_se(
      influence[, valid_primitive, drop = FALSE])
  }
  list(
    primitive_estimate = inference$estimate,
    primitive_plugin = inference$plugin,
    primitive_influence = inference$influence,
    reported_estimate = reported,
    reported_plugin = reported_plugin,
    reported_influence = reported_if,
    reported_se = reported_se,
    primitive_se = primitive_se,
    respondent_id = respondent_id,
    available_reported_targets = names(reported)[valid_reported],
    failed_targets = inference$failed_targets %||% character(),
    failed_families = inference$failed_families %||% character(),
    failure_records = inference$failure_records %||% NULL)
}

.scmix_v24_plugin_primitives <- function(mu, A, kappa, respondent_id,
                                          Z, target_bundle) {
  mu <- as.matrix(mu)
  A <- as.matrix(A)
  respondent_id <- as.character(respondent_id)
  Sigma <- tcrossprod(A)
  cdir <- target_bundle$contrasts$direction
  cnum <- target_bundle$contrasts$numerator
  cden <- target_bundle$contrasts$denominator
  direction <- as.numeric(mu %*% cdir)
  numerator <- as.numeric(mu %*% cnum)
  denominator <- as.numeric(mu %*% cden)
  directional_share <- function(contrast, include_ties = FALSE) {
    m <- as.numeric(mu %*% contrast)
    v <- as.numeric(crossprod(contrast, Sigma %*% contrast))
    if (v > 0) mean(stats::pnorm(m / sqrt(v))) else
      mean(if (include_ties) m >= 0 else m > 0)
  }
  choice_callback <- target_bundle$callbacks$choice_probability
  choice <- choice_callback(
    mu = mu, kappa = kappa, Sigma = Sigma, Z = Z,
    respondent_id = respondent_id, fold = 0L,
    attr_names = colnames(mu))$value[, 1L]
  high <- unname(target_bundle$high[respondent_id])
  if (anyNA(high) || !any(high == 1) || !any(high == 0)) {
    stop("A v2.4 plug-in scope lost one of the frozen subgroups.",
         call. = FALSE)
  }
  HZ <- mean((direction - mean(direction))^2)
  HR <- as.numeric(crossprod(cdir, Sigma %*% cdir))
  c(
    mean_direction = mean(direction),
    mean_numerator = mean(numerator),
    mean_denominator = mean(denominator),
    choice_probability = mean(choice),
    preference_direction_share = directional_share(cdir),
    preference_direction_variance = as.numeric(
      crossprod(cdir, Sigma %*% cdir)),
    compensating_benefit_share = directional_share(
      cdir + 0.5 * cden, include_ties = TRUE),
    compensating_benefit_variance = as.numeric(crossprod(
      cdir + 0.5 * cden, Sigma %*% (cdir + 0.5 * cden))),
    subgroup_high_numerator = mean(high * choice),
    subgroup_high_denominator = mean(high),
    subgroup_low_numerator = mean((1 - high) * choice),
    subgroup_low_denominator = mean(1 - high),
    mrs_numerator = mean(numerator),
    mrs_denominator = mean(denominator),
    active_q1_covariance_eigenvalue = sum(diag(Sigma)),
    heterogeneity_explained_variance = HZ,
    heterogeneity_residual_variance = HR)
}

.scmix_v24_start_plugin <- function(record, outcome, respondent_ids,
                                     target_bundle) {
  all_ids <- unique(as.character(outcome$respondent_id))
  first <- match(all_ids, outcome$respondent_id)
  Z <- outcome$Z[first, , drop = FALSE]
  colnames(Z) <- outcome$Z_names
  rownames(Z) <- all_ids
  keep <- match(respondent_ids, all_ids)
  if (anyNA(keep)) stop("A near-optimum scope has unknown respondents.",
                        call. = FALSE)
  Z_scope <- Z[keep, , drop = FALSE]
  mu <- scmix_predict_network(record$network_state, Z_scope,
                              input = "raw", output = "raw",
                              device = "cpu")
  .scmix_v24_plugin_primitives(
    mu = mu, A = record$A_raw, kappa = record$kappa,
    respondent_id = respondent_ids, Z = Z_scope,
    target_bundle = target_bundle)
}

.scmix_v24_near_optimum_movement <- function(
    fit_bundle, outcome, target_bundle, reported_se,
    objective_tolerance = 1e-6) {
  target_names <- names(reported_se)
  eligible <- is.finite(reported_se) & reported_se > 0
  movement <- stats::setNames(rep(Inf, length(target_names)), target_names)
  movement[eligible] <- 0
  update_movement <- function(surface) {
    if (!is.matrix(surface)) surface <- do.call(rbind, surface)
    if (nrow(surface) < 2L) return(invisible(NULL))
    spread <- apply(surface, 2L, function(x) diff(range(x)))
    use <- intersect(names(spread), names(eligible)[eligible])
    movement[use] <<- pmax(
      movement[use], spread[use] / reported_se[use])
    invisible(NULL)
  }
  all_ids <- unique(as.character(outcome$respondent_id))
  full_records <- fit_bundle$full_tuning$refit$multistart_artifacts
  full_near <- full_records[vapply(full_records, function(x)
    is.finite(x$relative_objective_from_best) &&
      x$relative_objective_from_best <= objective_tolerance, logical(1L))]
  full_surface <- lapply(full_near, function(record) {
    primitive <- .scmix_v24_start_plugin(
      record, outcome, all_ids, target_bundle)
    .scmix_v24_reported_targets(primitive)
  })
  if (length(full_surface)) update_movement(full_surface)

  assembled <- fit_bundle$assembled
  ## assembled$fold_id is task-level on some package versions and
  ## respondent-level on others.  Resolve one fold per unique respondent.
  if (length(assembled$fold_id) == length(outcome$respondent_id)) {
    task_fold <- assembled$fold_id
    first <- match(all_ids, outcome$respondent_id)
    resp_fold <- task_fold[first]
  } else {
    resp_fold <- assembled$fold_id
    names(resp_fold) <- assembled$respondent_id
    resp_fold <- unname(resp_fold[all_ids])
  }
  if (length(resp_fold) != length(all_ids) || anyNA(resp_fold)) {
    stop("Could not recover the fixed v2.4 respondent outer folds.",
         call. = FALSE)
  }
  K <- fit_bundle$assembled$K
  selected_component <- vector("list", K)
  selected_record <- vector("list", K)
  weights <- numeric(K)
  for (k in seq_len(K)) {
    records <- fit_bundle$nested_tuning$tuning[[k]]$refit$
      multistart_artifacts
    selected <- which(vapply(records, `[[`, logical(1L), "selected"))
    if (length(selected) != 1L) {
      stop("Each v2.4 outer fit must retain one selected start.",
           call. = FALSE)
    }
    held <- all_ids[resp_fold == k]
    weights[[k]] <- length(held) / length(all_ids)
    selected_record[[k]] <- records[[selected]]
    selected_component[[k]] <- .scmix_v24_start_plugin(
      records[[selected]], outcome, held, target_bundle)
  }
  baseline <- Reduce(`+`, Map(function(x, w) x * w,
                              selected_component, weights))
  baseline_reported <- .scmix_v24_reported_targets(baseline)
  for (k in seq_len(K)) {
    records <- fit_bundle$nested_tuning$tuning[[k]]$refit$
      multistart_artifacts
    near <- records[vapply(records, function(x)
      is.finite(x$relative_objective_from_best) &&
        x$relative_objective_from_best <= objective_tolerance, logical(1L))]
    held <- all_ids[resp_fold == k]
    surface <- list(baseline_reported)
    for (record in near) {
      component <- .scmix_v24_start_plugin(
        record, outcome, held, target_bundle)
      alternative <- baseline + weights[[k]] *
        (component - selected_component[[k]])
      surface[[length(surface) + 1L]] <-
        .scmix_v24_reported_targets(alternative)
    }
    update_movement(surface)
  }
  movement
}

## A result artifact is intentionally much smaller than the fitted nuisance
## objects.  It retains every decision and interval needed for the prospective
## validation but no live torch module, synthetic response vector, multiplier
## matrix, or respondent influence matrix.
.scmix_v24_compact_family_result <- function(x) {
  if (!inherits(x, "scmix_v24_family_result") && !is.list(x)) {
    stop("A v2.4 family result is malformed.", call. = FALSE)
  }
  out <- x
  if (is.list(out$multiplier)) {
    out$multiplier$influence <- NULL
    out$multiplier$centered_influence <- NULL
    out$multiplier$maximum_statistic <- NULL
  }
  out
}

.scmix_v24_dml_status_record <- function(inference) {
  analyses <- inference$analyses
  if (!is.list(analyses) || !length(analyses)) {
    return(data.frame(
      analysis_id = character(), family_id = character(),
      target_ids = character(), status = character(), error = character(),
      stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(analyses, function(x) data.frame(
    analysis_id = x$analysis_id, family_id = x$family_id,
    target_ids = paste(x$target_ids, collapse = "|"),
    status = x$status, error = x$error %||% "",
    stringsAsFactors = FALSE)))
}

.scmix_v24_compact_gate_result <- function(gates) {
  required <- c("target_gates", "fit_gates", "family_gates",
                "family_results", "branch_family_results",
                "target_metrics", "primitive_metrics",
                "ridge_sensitivities", "gh_movement",
                "near_optimum_movement", "released_families")
  if (!is.list(gates) || !all(required %in% names(gates)) ||
      !isTRUE(gates$complete)) {
    stop("The v2.4 gate engine did not return a complete result.",
         call. = FALSE)
  }
  ridge <- gates$ridge_sensitivities
  if (is.list(ridge$alternatives)) {
    ridge$alternatives <- lapply(ridge$alternatives, function(x) {
      x$primitive_influence <- NULL
      x
    })
  }
  list(
    protocol_id = gates$protocol_id,
    target_gates = gates$target_gates,
    fit_gates = gates$fit_gates,
    family_gates = gates$family_gates,
    family_results = lapply(gates$family_results,
                            .scmix_v24_compact_family_result),
    branch_family_results = lapply(
      gates$branch_family_results,
      function(branch) lapply(branch, .scmix_v24_compact_family_result)),
    target_metrics = gates$target_metrics,
    primitive_metrics = gates$primitive_metrics,
    active_eigenvalue_regularity_z =
      gates$active_eigenvalue_regularity_z,
    ridge_sensitivities = ridge,
    gh_movement = gates$gh_movement,
    near_optimum_movement = gates$near_optimum_movement,
    failed_targets = gates$failed_targets %||% character(),
    failed_families = gates$failed_families %||% character(),
    fit_integrity = gates$fit_integrity,
    complete = gates$complete,
    released_families = gates$released_families)
}

.scmix_v24_family_truth_result <- function(
    family_id, family_result, released, truths) {
  truth <- truths[truths$family_id == family_id, , drop = FALSE]
  members <- family_result$members
  if (!nrow(truth) || !length(members) ||
      !setequal(as.character(truth$target_id), names(members))) {
    stop("The v2.4 family result does not match its frozen truths.",
         call. = FALSE)
  }
  truth <- truth[match(names(members), truth$target_id), , drop = FALSE]
  defined <- identical(as.character(truth$status),
                        rep("defined", nrow(truth)))
  covered <- NA
  if (isTRUE(released) && isTRUE(defined)) {
    covered <- all(vapply(seq_along(members), function(i) {
      interval <- as.numeric(members[[i]]$interval)
      value <- as.numeric(truth$truth[[i]])
      length(interval) == 2L && all(is.finite(interval)) &&
        is.finite(value) && interval[[1L]] <= value &&
        value <= interval[[2L]]
    }, logical(1L)))
  }
  list(
    family_id = family_id,
    released = isTRUE(released),
    covered = if (isTRUE(released) && isTRUE(defined)) isTRUE(covered) else NA,
    truth_status = if (isTRUE(defined)) "defined" else
      "one_or_more_members_undefined_exact_boundary")
}

.scmix_v24_artifact_file <- function(artifact_dir, outcome) {
  stream <- match.arg(outcome$stream, c("pilot", "evidence"))
  cell <- as.character(outcome$cell_id)
  if (!grepl("^[[:alnum:]_]+$", cell)) {
    stop("The v2.4 cell identifier is unsafe for an artifact path.",
         call. = FALSE)
  }
  file.path(artifact_dir, "sealed", stream, cell,
            sprintf("replication-%03d.rds", as.integer(outcome$replication)))
}

.scmix_v24_save_atomic <- function(x, path, overwrite = FALSE) {
  path <- path.expand(path)
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Refusing to overwrite an existing v2.4 artifact: ", path,
         call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE,
             mode = "0700")
  temporary <- tempfile(".v24-artifact-", tmpdir = dirname(path),
                        fileext = ".rds")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(x, temporary, version = 3L, compress = "xz")
  Sys.chmod(temporary, mode = "0600", use_umask = FALSE)
  if (file.exists(path) && isTRUE(overwrite)) unlink(path)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically install the v2.4 artifact.", call. = FALSE)
  }
  Sys.chmod(path, mode = "0600", use_umask = FALSE)
  normalizePath(path, mustWork = TRUE)
}

.scmix_v24_write_csv_atomic <- function(x, path, overwrite = FALSE) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Refusing to overwrite an existing v2.4 CSV record: ", path,
         call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".v24-csv-", tmpdir = dirname(path),
                        fileext = ".csv")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(x, temporary, row.names = FALSE, na = "")
  if (file.exists(path) && isTRUE(overwrite)) unlink(path)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically install the v2.4 CSV record.",
         call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

.scmix_v24_require_run_dir <- function(run_dir, source_root) {
  source_root <- normalizePath(source_root, mustWork = TRUE)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE,
             mode = "0700")
  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  prefix <- paste0(source_root, .Platform$file.sep)
  if (!startsWith(run_dir, prefix)) {
    stop("The prospective v2.4 run directory must be inside the hash-bound source root.",
         call. = FALSE)
  }
  run_dir
}

.scmix_v24_relative_or_absolute <- function(path, root) {
  path <- normalizePath(path, mustWork = TRUE)
  root <- normalizePath(root, mustWork = TRUE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

.scmix_v24_system_record <- function() {
  torch_version <- if (requireNamespace("torch", quietly = TRUE))
    as.character(utils::packageVersion("torch")) else NA_character_
  list(
    R = as.character(getRversion()),
    sconjoint = tryCatch(as.character(utils::packageVersion("sconjoint")),
                         error = function(e) "source_tree"),
    torch = torch_version,
    platform = R.version$platform,
    cpu_threads_visible = parallel::detectCores(logical = TRUE),
    torch_threads = if (requireNamespace("torch", quietly = TRUE))
      as.integer(torch::torch_get_num_threads()) else NA_integer_)
}

## Run the single hash-bound estimator and inference evaluator on one already
## generated, signed synthetic outcome.  The return value is only an artifact
## descriptor.  In particular, a pilot caller cannot accidentally print target
## values or gate decisions from the returned object.
scmix_v24_fit_and_infer <- function(
    outcome, artifact_dir, plan = scmix_v24_validation_plan(),
    state_dir = NULL, source_root = .scmix_v24_source_root(plan),
    overwrite = FALSE, verbose = FALSE, .job_started_elapsed = NULL) {
  started <- if (is.null(.job_started_elapsed)) {
    proc.time()[["elapsed"]]
  } else {
    value <- suppressWarnings(as.numeric(.job_started_elapsed))
    if (length(value) != 1L || !is.finite(value) || value < 0) {
      stop("`.job_started_elapsed` must be one finite process time.",
           call. = FALSE)
    }
    value
  }
  readiness <- .scmix_v24_evaluator_readiness(
    plan, state_dir = state_dir, source_root = source_root)
  if (!isTRUE(readiness$evaluator_approval_valid)) {
    stop("The v2.4 evaluator has not been hash-approved.", call. = FALSE)
  }
  permitted <- if (identical(outcome$stream, "pilot"))
    readiness$pilot_outcome_generation_permitted else
      readiness$evidence_outcome_generation_permitted
  if (!isTRUE(permitted)) {
    stop("The v2.4 ", outcome$stream,
         " evaluation stream is not currently open.", call. = FALSE)
  }
  template <- scmix_v24_load_design(outcome$design_id, plan)
  geometry <- scmix_v24_dgp_geometry(outcome$cell_id, plan)
  evaluator <- readiness$evaluator
  .scmix_v24_outcome_check(outcome, plan, template, geometry, evaluator)
  if (!identical(outcome$evaluator_settings_fingerprint,
                 evaluator$settings$fingerprint)) {
    stop("The outcome is not bound to the approved evaluator settings.",
         call. = FALSE)
  }

  fit <- .scmix_v24_fit_q1(template, outcome, verbose = verbose)
  baseline_targets <- .scmix_v24_target_callbacks(outcome, geometry, 15L)
  refined_targets <- .scmix_v24_target_callbacks(outcome, geometry, 31L)
  dml_seed <- as.integer(outcome$seeds$fit)
  baseline_inference <- .scmix_v24_dml_once(
    fit$assembled, baseline_targets, dml_seed)
  baseline <- list(
    inference = baseline_inference,
    view = .scmix_v24_inference_view(baseline_inference))

  fixed_assembled <- .scmix_v24_fixed_state_gh(fit$assembled, 31L)
  fixed_inference <- .scmix_v24_dml_once(
    fixed_assembled, refined_targets, dml_seed)
  fixed31 <- list(
    inference = fixed_inference,
    view = .scmix_v24_inference_view(fixed_inference))

  repolished_fit <- .scmix_v24_repolish_bundle(fit, verbose = verbose)
  fit_metrics <- .scmix_v24_add_repolish_metrics(
    fit$metrics, repolished_fit)
  repolished_inference <- .scmix_v24_dml_once(
    repolished_fit$assembled, refined_targets, dml_seed)
  repolished31 <- list(
    inference = repolished_inference,
    view = .scmix_v24_inference_view(repolished_inference))

  bank <- .scmix_v24_multiplier_bank(
    baseline$view$respondent_id, draws = plan$counts$multipliers,
    seed = as.integer(outcome$seeds$multiplier))
  near_optimum <- .scmix_v24_near_optimum_movement(
    fit, outcome, baseline_targets, baseline$view$reported_se,
    objective_tolerance = 1e-6)
  gates <- .scmix_v24_gate_engine(
    baseline = baseline, fixed31 = fixed31,
    repolished31 = repolished31, fit_metrics = fit_metrics,
    near_optimum_movement = near_optimum, multiplier_bank = bank,
    decision_registry = plan$decisions)
  compact_gates <- .scmix_v24_compact_gate_result(gates)

  truths <- plan$truths[plan$truths$cell_id == outcome$cell_id,
                        , drop = FALSE]
  family_ids <- as.character(plan$targets$family_id)
  family_cell <- lapply(family_ids, function(family) {
    released <- family %in% gates$released_families
    .scmix_v24_family_truth_result(
      family, gates$family_results[[family]], released, truths)
  })
  family_cell <- do.call(rbind, lapply(family_cell, function(x) data.frame(
    family_id = x$family_id, released = x$released,
    covered = x$covered, truth_status = x$truth_status,
    stringsAsFactors = FALSE)))

  elapsed <- max(proc.time()[["elapsed"]] - started, .Machine$double.eps)
  payload <- list(
    protocol_id = plan$protocol_id, plan_id = plan$plan_id,
    evaluator_interface_id = .scmix_v24_evaluator_interface_id,
    release_algorithm_id = .scmix_v24_release_algorithm_id,
    evaluator_approval_sha256 = evaluator$sha256,
    evaluator_settings_fingerprint = evaluator$settings$fingerprint,
    implementation_manifest_sha256 =
      evaluator$raw[["Implementation-Manifest-SHA256"]],
    stream = outcome$stream, cell_id = outcome$cell_id,
    design_id = outcome$design_id,
    replication = as.integer(outcome$replication),
    outcome_fingerprint = outcome$fingerprint,
    dgp_geometry_fingerprint = geometry$fingerprint,
    truth_table_sha256 = outcome$truth_table_sha256,
    fit_contract = list(
      rank = 1L, reference_nodes = 15L, refined_nodes = 31L,
      refined_branches = c(
        "fixed_GH15_state_evaluated_at_GH31",
        "GH31_repolish_from_exact_GH15_state"),
      metrics = fit_metrics),
    inference_contract = list(
      ridge_primary = "Ntrain^(-3/4)",
      ridge_sensitivities = c("Ntrain^(-1)", "Ntrain^(-1/2)"),
      ridge_selection = "none",
      numerical_rank_rule = "dimension_epsilon",
      multiplier_draws = as.integer(bank$draws),
      multiplier_seed = as.integer(bank$seed)),
    target_specific_dml = list(
      baseline = .scmix_v24_dml_status_record(baseline_inference),
      fixed31 = .scmix_v24_dml_status_record(fixed_inference),
      repolished31 = .scmix_v24_dml_status_record(
        repolished_inference)),
    estimates = baseline$view$reported_estimate,
    standard_errors = baseline$view$reported_se,
    gates = compact_gates,
    family_cell = family_cell,
    system = .scmix_v24_system_record(),
    evaluation_elapsed_seconds = elapsed,
    completed = TRUE, synthetic_outcome = TRUE,
    application_outcomes_used = FALSE)
  artifact <- .scmix_v24_sign(payload, "scmix_v24_replication_artifact")
  artifact_file <- .scmix_v24_artifact_file(artifact_dir, outcome)
  artifact_file <- .scmix_v24_save_atomic(
    artifact, artifact_file, overwrite = overwrite)
  descriptor <- list(
    protocol_id = plan$protocol_id, stream = outcome$stream,
    cell_id = outcome$cell_id, replication = as.integer(outcome$replication),
    completed = TRUE, elapsed_seconds = elapsed,
    artifact_file = artifact_file,
    artifact_sha256 = unname(as.character(tools::sha256sum(artifact_file))))
  .scmix_v24_sign(descriptor, "scmix_v24_artifact_descriptor")
}

.scmix_v24_read_artifact <- function(path, expected = NULL) {
  if (!file.exists(path)) stop("A scheduled v2.4 artifact is absent.",
                               call. = FALSE)
  artifact <- readRDS(path)
  if (!.scmix_v24_intact(artifact, "scmix_v24_replication_artifact") ||
      !identical(artifact$synthetic_outcome, TRUE) ||
      !identical(artifact$application_outcomes_used, FALSE) ||
      !identical(artifact$completed, TRUE)) {
    stop("A v2.4 replication artifact failed its integrity check.",
         call. = FALSE)
  }
  if (!is.null(expected) &&
      (!identical(as.character(artifact$cell_id),
                  as.character(expected$cell_id)) ||
       !identical(as.integer(artifact$replication),
                  as.integer(expected$replication)))) {
    stop("A v2.4 artifact was assigned to the wrong scheduled job.",
         call. = FALSE)
  }
  artifact
}

.scmix_v24_validate_reusable_artifact <- function(
    path, expected, plan, evaluator, expected_sha256 = NULL) {
  if (!is.list(expected) && !is.data.frame(expected)) {
    stop("A reusable v2.4 artifact needs its scheduled identity.",
         call. = FALSE)
  }
  cell_id <- as.character(expected$cell_id[[1L]])
  replication <- as.integer(expected$replication[[1L]])
  stream <- as.character(expected$stream[[1L]])
  cell_index <- match(cell_id, as.character(plan$cells$cell_id))
  if (is.na(cell_index) || !stream %in% c("pilot", "evidence") ||
      is.na(replication)) {
    stop("A reusable v2.4 artifact has no frozen scheduled identity.",
         call. = FALSE)
  }
  path <- normalizePath(path, mustWork = TRUE)
  observed_sha256 <- unname(as.character(tools::sha256sum(path)))
  if (!is.null(expected_sha256) &&
      !identical(observed_sha256, as.character(expected_sha256))) {
    stop("A reusable v2.4 artifact does not match its recorded file hash.",
         call. = FALSE)
  }
  artifact <- .scmix_v24_read_artifact(
    path, list(cell_id = cell_id, replication = replication))
  implementation_sha256 <-
    evaluator$raw[["Implementation-Manifest-SHA256"]]
  settings_fingerprint <- evaluator$settings$fingerprint
  elapsed <- suppressWarnings(as.numeric(
    artifact$evaluation_elapsed_seconds))
  expected_design <- as.character(plan$cells$design_id[[cell_index]])
  if (!identical(artifact$protocol_id, plan$protocol_id) ||
      !identical(artifact$plan_id, plan$plan_id) ||
      !identical(artifact$stream, stream) ||
      !identical(artifact$design_id, expected_design) ||
      !identical(artifact$evaluator_approval_sha256, evaluator$sha256) ||
      !identical(artifact$evaluator_settings_fingerprint,
                 settings_fingerprint) ||
      !identical(artifact$implementation_manifest_sha256,
                 implementation_sha256) ||
      length(elapsed) != 1L || !is.finite(elapsed) || elapsed <= 0) {
    stop("An existing v2.4 artifact is not reusable under the exact current schedule and evaluator binding.",
         call. = FALSE)
  }
  list(
    artifact = artifact, artifact_file = path,
    artifact_sha256 = observed_sha256, elapsed_seconds = elapsed)
}

.scmix_v24_pilot_private_manifest_file <- function(run_dir) {
  file.path(run_dir, "sealed", "pilot", "pilot-artifact-manifest.rds")
}

.scmix_v24_pilot_manifest_schedule <- function(jobs) {
  schedule <- jobs[, c("cell_id", "replication"), drop = FALSE]
  schedule$cell_id <- as.character(schedule$cell_id)
  schedule$replication <- as.integer(schedule$replication)
  rownames(schedule) <- NULL
  schedule
}

.scmix_v24_open_pilot_private_manifest <- function(
    run_dir, jobs, plan, evaluator) {
  path <- .scmix_v24_pilot_private_manifest_file(run_dir)
  schedule <- .scmix_v24_pilot_manifest_schedule(jobs)
  implementation_sha256 <-
    evaluator$raw[["Implementation-Manifest-SHA256"]]
  settings_fingerprint <- evaluator$settings$fingerprint
  required_binding <- c(
    plan$protocol_id, plan$plan_id, evaluator$sha256,
    settings_fingerprint, implementation_sha256)
  if (length(required_binding) != 5L || anyNA(required_binding) ||
      any(!nzchar(required_binding))) {
    stop("The pilot private manifest lacks an exact evaluator binding.",
         call. = FALSE)
  }
  if (file.exists(path)) {
    manifest <- readRDS(path)
    records <- manifest$records
    records_valid <- is.null(records)
    required_records <- c(
      "cell_id", "replication", "completed", "worker_id",
      "elapsed_seconds", "resumed", "artifact_file",
      "artifact_sha256", "error")
    if (is.data.frame(records) &&
        identical(names(records), required_records)) {
      record_schedule <- records[, c("cell_id", "replication"), drop = FALSE]
      record_schedule$cell_id <- as.character(record_schedule$cell_id)
      record_schedule$replication <- as.integer(record_schedule$replication)
      rownames(record_schedule) <- NULL
      expected_worker <- match(record_schedule$cell_id,
                               as.character(plan$cells$cell_id))
      records_valid <- identical(record_schedule, schedule) &&
        identical(as.integer(records$worker_id),
                  as.integer(expected_worker)) &&
        is.logical(records$completed) && !anyNA(records$completed) &&
        is.logical(records$resumed) && !anyNA(records$resumed) &&
        is.numeric(records$elapsed_seconds) &&
        all(is.finite(records$elapsed_seconds)) &&
        all(records$elapsed_seconds > 0) &&
        all(!records$completed | grepl(
          "^[[:xdigit:]]{64}$", records$artifact_sha256))
    }
    attempt_raw <- suppressWarnings(as.numeric(manifest$attempt_count))
    attempt_count <- suppressWarnings(as.integer(attempt_raw))
    attempt_valid <- length(attempt_raw) == 1L && is.finite(attempt_raw) &&
      attempt_raw == attempt_count && attempt_count >= 0L
    start_valid <- is.numeric(manifest$wave_started_unix) &&
      length(manifest$wave_started_unix) == 1L &&
      is.finite(manifest$wave_started_unix)
    complete_valid <- is.logical(manifest$complete) &&
      length(manifest$complete) == 1L && !is.na(manifest$complete) &&
      identical(manifest$complete,
                is.data.frame(records) && all(records$completed))
    timing_valid <- start_valid && if (isTRUE(manifest$complete)) {
      is.numeric(manifest$wave_finished_unix) &&
        length(manifest$wave_finished_unix) == 1L &&
        is.finite(manifest$wave_finished_unix) &&
        manifest$wave_finished_unix > manifest$wave_started_unix
    } else {
      length(manifest$wave_finished_unix) == 1L &&
        is.na(manifest$wave_finished_unix)
    }
    public_valid <- TRUE
    if (is.data.frame(records)) {
      public_file <- manifest$public_manifest_file
      public_hash <- manifest$public_manifest_sha256
      expected_public <- normalizePath(
        file.path(run_dir, "pilot-run-manifest.csv"), mustWork = FALSE)
      public_valid <- is.character(public_file) && length(public_file) == 1L &&
        identical(normalizePath(public_file, mustWork = FALSE),
                  expected_public) &&
        is.character(public_hash) && length(public_hash) == 1L &&
        grepl("^[[:xdigit:]]{64}$", public_hash) &&
        (!file.exists(public_file) || identical(
          unname(as.character(tools::sha256sum(public_file))), public_hash))
    }
    if (!.scmix_v24_intact(
          manifest, "scmix_v24_pilot_artifact_manifest") ||
        !identical(manifest$protocol_id, plan$protocol_id) ||
        !identical(manifest$plan_id, plan$plan_id) ||
        !identical(manifest$evaluator_approval_sha256, evaluator$sha256) ||
        !identical(manifest$evaluator_settings_fingerprint,
                   settings_fingerprint) ||
        !identical(manifest$implementation_manifest_sha256,
                   implementation_sha256) ||
        !identical(manifest$workers, 6L) ||
        !identical(manifest$schedule, schedule) ||
        !attempt_valid || !records_valid || !complete_valid ||
        !timing_valid || !public_valid ||
        !identical(manifest$target_or_gate_results_inspected, FALSE) ||
        !identical(manifest$application_outcomes_used, FALSE)) {
      stop("The existing private pilot manifest is altered or off contract.",
           call. = FALSE)
    }
    return(list(path = normalizePath(path, mustWork = TRUE),
                manifest = manifest, resumed = TRUE))
  }
  expected_artifacts <- vapply(seq_len(nrow(schedule)), function(i) {
    .scmix_v24_artifact_file(
      run_dir, list(stream = "pilot", cell_id = schedule$cell_id[[i]],
                    replication = schedule$replication[[i]]))
  }, character(1L))
  public_path <- file.path(run_dir, "pilot-run-manifest.csv")
  if (any(file.exists(expected_artifacts)) || file.exists(public_path)) {
    stop("Pilot artifacts or public metadata exist without their private hash-bound manifest. Use a fresh run directory.",
         call. = FALSE)
  }
  payload <- list(
    protocol_id = plan$protocol_id, plan_id = plan$plan_id,
    evaluator_approval_sha256 = evaluator$sha256,
    evaluator_settings_fingerprint = settings_fingerprint,
    implementation_manifest_sha256 = implementation_sha256,
    workers = 6L, schedule = schedule,
    wave_started_unix = as.numeric(Sys.time()),
    wave_finished_unix = NA_real_, attempt_count = 0L,
    complete = FALSE, records = NULL,
    public_manifest_file = NULL, public_manifest_sha256 = NULL,
    target_or_gate_results_inspected = FALSE,
    application_outcomes_used = FALSE)
  manifest <- .scmix_v24_sign(
    payload, "scmix_v24_pilot_artifact_manifest")
  path <- .scmix_v24_save_atomic(manifest, path)
  list(path = path, manifest = manifest, resumed = FALSE)
}

.scmix_v24_update_pilot_private_manifest <- function(
    opened, records, finished, public_manifest) {
  manifest <- unclass(opened$manifest)
  manifest$fingerprint <- NULL
  manifest$records <- records
  manifest$attempt_count <- as.integer(manifest$attempt_count) + 1L
  manifest$complete <- all(records$completed)
  manifest$wave_finished_unix <- if (isTRUE(manifest$complete)) {
    if (isTRUE(opened$manifest$complete) &&
        is.finite(opened$manifest$wave_finished_unix)) {
      opened$manifest$wave_finished_unix
    } else as.numeric(finished)
  } else NA_real_
  manifest$last_attempt_finished_unix <- as.numeric(finished)
  manifest$public_manifest_file <- normalizePath(
    public_manifest, mustWork = TRUE)
  manifest$public_manifest_sha256 <- unname(as.character(
    tools::sha256sum(public_manifest)))
  manifest <- .scmix_v24_sign(
    manifest, "scmix_v24_pilot_artifact_manifest")
  path <- .scmix_v24_save_atomic(
    manifest, opened$path, overwrite = TRUE)
  list(path = path, manifest = manifest,
       sha256 = unname(as.character(tools::sha256sum(path))))
}

.scmix_v24_verify_pilot_results <- function(
    result, jobs, plan, evaluator, run_dir, previous_records = NULL) {
  schedule <- .scmix_v24_pilot_manifest_schedule(jobs)
  expected_worker <- match(schedule$cell_id,
                           as.character(plan$cells$cell_id))
  if (!is.list(result) || length(result) != nrow(schedule)) {
    stop("The pilot workers did not return the complete frozen schedule.",
         call. = FALSE)
  }
  records <- do.call(rbind, lapply(result, function(x) data.frame(
    cell_id = as.character(x$cell_id),
    replication = as.integer(x$replication),
    completed = isTRUE(x$completed), worker_id = as.integer(x$worker_id),
    elapsed_seconds = as.numeric(x$elapsed_seconds),
    resumed = isTRUE(x$resumed),
    artifact_file = as.character(x$artifact_file %||% ""),
    artifact_sha256 = as.character(x$artifact_sha256 %||% ""),
    error = as.character(x$error %||% ""), stringsAsFactors = FALSE)))
  key <- paste(records$cell_id, records$replication, sep = "\r")
  expected_key <- paste(schedule$cell_id, schedule$replication, sep = "\r")
  if (anyDuplicated(key) || !setequal(key, expected_key)) {
    stop("The pilot worker results do not match the frozen schedule.",
         call. = FALSE)
  }
  records <- records[match(expected_key, key), , drop = FALSE]
  rownames(records) <- NULL
  if (!identical(records$worker_id, as.integer(expected_worker)) ||
      anyNA(records$elapsed_seconds) ||
      any(!is.finite(records$elapsed_seconds)) ||
      any(records$elapsed_seconds <= 0)) {
    stop("The pilot worker mapping or elapsed-time record changed.",
         call. = FALSE)
  }
  for (i in seq_len(nrow(records))) {
    if (!isTRUE(records$completed[[i]])) next
    expected_file <- .scmix_v24_artifact_file(
      run_dir, list(stream = "pilot", cell_id = records$cell_id[[i]],
                    replication = records$replication[[i]]))
    prior_sha256 <- NULL
    if (is.data.frame(previous_records) && nrow(previous_records)) {
      prior <- previous_records[
        as.character(previous_records$cell_id) == records$cell_id[[i]] &
          as.integer(previous_records$replication) ==
            records$replication[[i]], , drop = FALSE]
      if (nrow(prior) == 1L && isTRUE(prior$completed[[1L]])) {
        prior_sha256 <- as.character(prior$artifact_sha256[[1L]])
      }
    }
    recorded_sha256 <- if (is.null(prior_sha256)) {
      records$artifact_sha256[[i]]
    } else if (identical(prior_sha256,
                         records$artifact_sha256[[i]])) {
      prior_sha256
    } else "prior-private-manifest-hash-mismatch"
    verified <- tryCatch(.scmix_v24_validate_reusable_artifact(
      records$artifact_file[[i]],
      list(stream = "pilot", cell_id = records$cell_id[[i]],
           replication = records$replication[[i]]),
      plan, evaluator,
      expected_sha256 = recorded_sha256), error = identity)
    same_path <- !inherits(verified, "error") && identical(
      normalizePath(expected_file, mustWork = TRUE),
      verified$artifact_file)
    same_elapsed <- !inherits(verified, "error") && isTRUE(all.equal(
      records$elapsed_seconds[[i]], verified$elapsed_seconds,
      tolerance = 64 * .Machine$double.eps,
      check.attributes = FALSE))
    if (inherits(verified, "error") || !same_path || !same_elapsed) {
      records$completed[[i]] <- FALSE
      detail <- if (inherits(verified, "error")) {
        conditionMessage(verified)
      } else if (!same_path) {
        "artifact path differs from the frozen scheduled path"
      } else "artifact elapsed time differs from its signed payload"
      records$error[[i]] <- paste0("pilot artifact read-back failed: ", detail)
    } else {
      records$artifact_file[[i]] <- verified$artifact_file
      records$artifact_sha256[[i]] <- verified$artifact_sha256
      records$elapsed_seconds[[i]] <- verified$elapsed_seconds
    }
  }
  records
}

.scmix_v24_run_job <- function(
    cell_id, replication, stream, worker_id, artifact_dir,
    plan, state_dir, source_root, verbose = FALSE,
    expected_artifact_sha256 = NULL, reuse_existing = TRUE) {
  started <- proc.time()[["elapsed"]]
  answer <- tryCatch({
    readiness <- .scmix_v24_evaluator_readiness(
      plan, state_dir = state_dir, source_root = source_root)
    expected_file <- .scmix_v24_artifact_file(
      artifact_dir, list(stream = stream, cell_id = cell_id,
                         replication = replication))
    if (file.exists(expected_file)) {
      if (!isTRUE(reuse_existing)) {
        stop("The private pilot manifest does not permit reuse of this existing artifact.",
             call. = FALSE)
      }
      existing <- .scmix_v24_validate_reusable_artifact(
        expected_file,
        list(cell_id = cell_id, replication = replication,
             stream = stream), plan, readiness$evaluator,
        expected_sha256 = expected_artifact_sha256)
      list(
        completed = TRUE, artifact_file = expected_file,
        artifact_sha256 = existing$artifact_sha256,
        elapsed_seconds = existing$elapsed_seconds,
        error = "", resumed = TRUE)
    } else {
      outcome <- scmix_v24_generate_study_outcome(
        cell_id = cell_id, replication = replication, stream = stream,
        plan = plan, state_dir = state_dir, source_root = source_root)
      descriptor <- scmix_v24_fit_and_infer(
        outcome, artifact_dir = artifact_dir, plan = plan,
        state_dir = state_dir, source_root = source_root,
        overwrite = FALSE, verbose = verbose,
        .job_started_elapsed = started)
      verified <- .scmix_v24_validate_reusable_artifact(
        descriptor$artifact_file,
        list(cell_id = cell_id, replication = replication,
             stream = stream), plan, readiness$evaluator,
        expected_sha256 = descriptor$artifact_sha256)
      list(completed = TRUE, artifact_file = verified$artifact_file,
           artifact_sha256 = verified$artifact_sha256,
           elapsed_seconds = verified$elapsed_seconds,
           error = "", resumed = FALSE)
    }
  }, error = function(e) list(
    completed = FALSE, artifact_file = "", artifact_sha256 = "",
    error = conditionMessage(e), resumed = FALSE))
  answer$cell_id <- as.character(cell_id)
  answer$replication <- as.integer(replication)
  answer$worker_id <- as.integer(worker_id)
  if (!is.numeric(answer$elapsed_seconds) ||
      length(answer$elapsed_seconds) != 1L ||
      !is.finite(answer$elapsed_seconds) || answer$elapsed_seconds <= 0) {
    answer$elapsed_seconds <- max(
      proc.time()[["elapsed"]] - started, .Machine$double.eps)
  }
  answer
}

.scmix_v24_run_worker_schedule <- function(
    jobs, workers, job_fun, parallel = TRUE) {
  if (!is.data.frame(jobs) ||
      !all(c("cell_id", "replication") %in% names(jobs)) ||
      nrow(jobs) %% workers != 0L || workers != 6L) {
    stop("A v2.4 wave must partition evenly over exactly six workers.",
         call. = FALSE)
  }
  assignments <- split(seq_len(nrow(jobs)),
                       rep(seq_len(workers), length.out = nrow(jobs)))
  work <- function(worker_id) {
    lapply(assignments[[worker_id]], function(i)
      job_fun(jobs$cell_id[[i]], jobs$replication[[i]], worker_id))
  }
  if (isTRUE(parallel)) {
    if (.Platform$OS.type == "windows") {
      stop("The exact v2.4 fork schedule is unavailable on Windows.",
           call. = FALSE)
    }
    result <- parallel::mclapply(
      seq_len(workers), work, mc.cores = workers,
      mc.preschedule = TRUE, mc.set.seed = FALSE)
  } else {
    result <- lapply(seq_len(workers), work)
  }
  if (any(vapply(result, inherits, logical(1L), what = "try-error"))) {
    stop("One or more v2.4 worker processes failed outside a job guard.",
         call. = FALSE)
  }
  unlist(result, recursive = FALSE)
}

## Run the twelve-job integrity pilot.  Only the five frozen runtime and
## integrity fields are placed in the public CSV and returned to the caller.
## Target, gate, coverage, interval, and artifact fields remain sealed.
scmix_v24_run_pilot_wave <- function(
    run_dir, plan = scmix_v24_validation_plan(), state_dir = NULL,
    source_root = .scmix_v24_source_root(plan), verbose = FALSE,
    .parallel = TRUE, .job_fun = NULL) {
  readiness <- .scmix_v24_evaluator_readiness(
    plan, state_dir = state_dir, source_root = source_root)
  if (!isTRUE(readiness$pilot_outcome_generation_permitted)) {
    stop("The prospective v2.4 integrity pilot is not open.", call. = FALSE)
  }
  run_dir <- .scmix_v24_require_run_dir(run_dir, source_root)
  jobs <- .scmix_v24_expected_pilot_jobs(plan$cells$cell_id)
  private <- .scmix_v24_open_pilot_private_manifest(
    run_dir, jobs, plan, readiness$evaluator)
  started <- private$manifest$wave_started_unix
  if (is.null(.job_fun)) .job_fun <- function(cell, replication, worker) {
    previous <- private$manifest$records
    prior <- if (is.data.frame(previous) && nrow(previous)) {
      previous[
        as.character(previous$cell_id) == cell &
          as.integer(previous$replication) == as.integer(replication),
        , drop = FALSE]
    } else NULL
    prior_sha256 <- if (is.data.frame(prior) && nrow(prior) == 1L &&
                        isTRUE(prior$completed[[1L]])) {
      as.character(prior$artifact_sha256[[1L]])
    } else NULL
    reuse_existing <- !is.data.frame(prior) || !nrow(prior) ||
      isTRUE(prior$completed[[1L]])
    .scmix_v24_run_job(
      cell, replication, "pilot", worker, run_dir,
      plan, state_dir, source_root, verbose,
      expected_artifact_sha256 = prior_sha256,
      reuse_existing = reuse_existing)
  }
  result <- .scmix_v24_run_worker_schedule(
    jobs, 6L, .job_fun, parallel = .parallel)
  attempted_finished <- as.numeric(Sys.time())
  records <- .scmix_v24_verify_pilot_results(
    result, jobs, plan, readiness$evaluator, run_dir,
    previous_records = private$manifest$records)
  metadata <- records[, c(
    "cell_id", "replication", "completed", "worker_id",
    "elapsed_seconds"), drop = FALSE]
  finished <- if (isTRUE(private$manifest$complete) &&
                  is.finite(private$manifest$wave_finished_unix)) {
    private$manifest$wave_finished_unix
  } else attempted_finished
  manifest <- .scmix_v24_write_csv_atomic(
    metadata, file.path(run_dir, "pilot-run-manifest.csv"),
    overwrite = file.exists(file.path(run_dir, "pilot-run-manifest.csv")))
  private <- .scmix_v24_update_pilot_private_manifest(
    private, records, attempted_finished, manifest)
  projection <- if (all(metadata$completed)) {
    scmix_v24_runtime_projection(
      metadata, started, finished, workers = 6L,
      cell_ids = plan$cells$cell_id,
      maximum_hours = plan$counts$maximum_projected_hours)
  } else list(
    projected_evidence_wall_seconds = Inf,
    projected_evidence_wall_hours = Inf, pass = FALSE)
  list(
    protocol_id = plan$protocol_id,
    pilot_run_manifest = manifest,
    pilot_run_manifest_relative_to_source =
      .scmix_v24_relative_or_absolute(manifest, source_root),
    pilot_run_manifest_sha256 =
      unname(as.character(tools::sha256sum(manifest))),
    private_artifact_manifest = private$path,
    private_artifact_manifest_relative_to_source =
      .scmix_v24_relative_or_absolute(private$path, source_root),
    private_artifact_manifest_sha256 = private$sha256,
    wave_started_unix = started, wave_finished_unix = finished,
    completed_jobs = sum(metadata$completed), workers = 6L,
    integrity_status = if (all(metadata$completed)) "pass" else "fail",
    projected_evidence_wall_seconds =
      projection$projected_evidence_wall_seconds,
    projected_evidence_wall_hours = projection$projected_evidence_wall_hours,
    projection_pass = projection$pass,
    target_or_gate_results_inspected = FALSE,
    application_outcomes_used = FALSE)
}

.scmix_v24_evidence_jobs <- function(plan, batch_index) {
  batch_index <- as.integer(batch_index)
  number_batches <- plan$counts$evidence_reps %/% plan$counts$batch_size
  if (length(batch_index) != 1L || is.na(batch_index) || batch_index < 1L ||
      batch_index > number_batches) {
    stop("The v2.4 evidence batch index is outside its frozen schedule.",
         call. = FALSE)
  }
  first <- (batch_index - 1L) * plan$counts$batch_size + 1L
  replication <- first:(first + plan$counts$batch_size - 1L)
  expand.grid(
    cell_id = plan$cells$cell_id, replication = replication,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

## Run one evidence batch.  Each of six workers owns one cell and executes its
## ten scheduled replications sequentially.  The function never stops early
## because of target results or a successful count.
scmix_v24_run_evidence_batch <- function(
    batch_index, run_dir, plan = scmix_v24_validation_plan(),
    state_dir = NULL, source_root = .scmix_v24_source_root(plan),
    verbose = FALSE, .parallel = TRUE, .job_fun = NULL) {
  readiness <- .scmix_v24_evaluator_readiness(
    plan, state_dir = state_dir, source_root = source_root)
  if (!isTRUE(readiness$evidence_outcome_generation_permitted)) {
    stop("The prospective v2.4 evidence stream is not open.", call. = FALSE)
  }
  run_dir <- .scmix_v24_require_run_dir(run_dir, source_root)
  jobs <- .scmix_v24_evidence_jobs(plan, batch_index)
  if (is.null(.job_fun)) .job_fun <- function(cell, replication, worker) {
    .scmix_v24_run_job(
      cell, replication, "evidence", worker, run_dir,
      plan, state_dir, source_root, verbose)
  }
  started <- as.numeric(Sys.time())
  result <- .scmix_v24_run_worker_schedule(
    jobs, 6L, .job_fun, parallel = .parallel)
  finished <- as.numeric(Sys.time())
  manifest <- do.call(rbind, lapply(result, function(x) data.frame(
    cell_id = x$cell_id, replication = as.integer(x$replication),
    completed = isTRUE(x$completed),
    artifact_file = as.character(x$artifact_file),
    artifact_sha256 = as.character(x$artifact_sha256),
    worker_id = as.integer(x$worker_id),
    elapsed_seconds = as.numeric(x$elapsed_seconds),
    resumed = isTRUE(x$resumed),
    error = as.character(x$error), stringsAsFactors = FALSE)))
  manifest <- manifest[order(match(manifest$cell_id, plan$cells$cell_id),
                             manifest$replication), , drop = FALSE]
  rownames(manifest) <- NULL
  path <- .scmix_v24_write_csv_atomic(
    manifest, file.path(run_dir, "evidence-batches",
                        sprintf("batch-%02d.csv", as.integer(batch_index))))
  list(
    protocol_id = plan$protocol_id, batch_index = as.integer(batch_index),
    manifest_file = path,
    manifest_sha256 = unname(as.character(tools::sha256sum(path))),
    attempted_jobs = nrow(manifest), completed_jobs = sum(manifest$completed),
    workers = 6L, started_unix = started, finished_unix = finished,
    application_outcomes_used = FALSE)
}

## Bind all ten evidence-batch manifests without opening any result artifact.
## This is the only public evidence record used by the completion-state DCF.
scmix_v24_collect_evidence_manifest <- function(
    run_dir, plan = scmix_v24_validation_plan(),
    source_root = .scmix_v24_source_root(plan)) {
  run_dir <- .scmix_v24_require_run_dir(run_dir, source_root)
  number_batches <- plan$counts$evidence_reps %/% plan$counts$batch_size
  paths <- file.path(run_dir, "evidence-batches",
                     sprintf("batch-%02d.csv", seq_len(number_batches)))
  if (!all(file.exists(paths))) {
    stop("All ten v2.4 evidence batch manifests are required.",
         call. = FALSE)
  }
  batches <- lapply(paths, utils::read.csv, stringsAsFactors = FALSE,
                    check.names = FALSE)
  manifest <- do.call(rbind, batches)
  expected <- expand.grid(
    cell_id = plan$cells$cell_id,
    replication = seq_len(plan$counts$evidence_reps),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  key <- paste(manifest$cell_id, manifest$replication, sep = "\r")
  expected_key <- paste(expected$cell_id, expected$replication, sep = "\r")
  if (nrow(manifest) != nrow(expected) || anyDuplicated(key) ||
      !setequal(key, expected_key)) {
    stop("The v2.4 evidence manifests do not contain all 600 attempts.",
         call. = FALSE)
  }
  manifest <- manifest[match(expected_key, key), , drop = FALSE]
  required <- c("cell_id", "replication", "completed", "artifact_file",
                "artifact_sha256")
  if (!all(required %in% names(manifest)) || anyNA(manifest$completed) ||
      any(manifest$completed & !grepl(
        "^[[:xdigit:]]{64}$", manifest$artifact_sha256)) ||
      any(!manifest$completed & nzchar(manifest$artifact_sha256))) {
    stop("One or more evidence manifest rows are malformed.", call. = FALSE)
  }
  keep <- manifest[, required, drop = FALSE]
  path <- .scmix_v24_write_csv_atomic(
    keep, file.path(run_dir, "evidence-run-manifest.csv"))
  list(
    protocol_id = plan$protocol_id, manifest_file = path,
    manifest_relative_to_source =
      .scmix_v24_relative_or_absolute(path, source_root),
    manifest_sha256 = unname(as.character(tools::sha256sum(path))),
    expected_attempts = nrow(expected), attempted_replications = nrow(keep),
    completed_replications = sum(keep$completed),
    application_outcomes_used = FALSE)
}

## After the evidence-completion record validates, inspect all sealed artifacts
## and apply the already-frozen family-cell rules.  This writes the mechanical
## result manifest but deliberately does not create a family approval DCF.
scmix_v24_assess_evidence <- function(
    run_dir, plan = scmix_v24_validation_plan(), state_dir = NULL,
    source_root = .scmix_v24_source_root(plan)) {
  readiness <- .scmix_v24_evaluator_readiness(
    plan, state_dir = state_dir, source_root = source_root)
  if (!isTRUE(readiness$evidence_study_completed)) {
    stop("A valid v2.4 evidence-completion record is required before result inspection.",
         call. = FALSE)
  }
  run_dir <- .scmix_v24_require_run_dir(run_dir, source_root)
  manifest_path <- file.path(run_dir, "evidence-run-manifest.csv")
  if (!file.exists(manifest_path)) {
    stop("The complete v2.4 evidence run manifest is absent.",
         call. = FALSE)
  }
  manifest_hash <- unname(as.character(tools::sha256sum(manifest_path)))
  if (!identical(
      manifest_hash,
      readiness$evidence$raw[["Evidence-Run-Manifest-SHA256"]])) {
    stop("The result assessment is not using the hash-bound evidence manifest.",
         call. = FALSE)
  }
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                              check.names = FALSE)
  family_ids <- as.character(plan$targets$family_id)
  rows <- vector("list", nrow(manifest) * length(family_ids))
  position <- 0L
  for (i in seq_len(nrow(manifest))) {
    artifact <- NULL
    if (isTRUE(manifest$completed[[i]])) {
      path <- manifest$artifact_file[[i]]
      if (!file.exists(path)) path <- file.path(source_root, path)
      observed_hash <- if (file.exists(path))
        unname(as.character(tools::sha256sum(path))) else ""
      if (!identical(observed_hash, manifest$artifact_sha256[[i]])) {
        stop("A completed evidence artifact no longer matches its manifest.",
             call. = FALSE)
      }
      artifact <- .scmix_v24_read_artifact(path, manifest[i, ])
      if (!identical(artifact$evaluator_approval_sha256,
                     readiness$evaluator$sha256) ||
          !identical(artifact$protocol_id, plan$protocol_id) ||
          !identical(artifact$plan_id, plan$plan_id) ||
          !identical(artifact$stream, "evidence") ||
          !identical(
            artifact$implementation_manifest_sha256,
            readiness$evaluator$raw[["Implementation-Manifest-SHA256"]])) {
        stop("An evidence artifact is bound to another evaluator approval.",
             call. = FALSE)
      }
    }
    for (family in family_ids) {
      position <- position + 1L
      value <- if (is.null(artifact)) NULL else
        artifact$family_cell[artifact$family_cell$family_id == family,
                             , drop = FALSE]
      if (!is.null(artifact) && nrow(value) != 1L) {
        stop("An evidence artifact has an incomplete family result.",
             call. = FALSE)
      }
      rows[[position]] <- data.frame(
        cell_id = manifest$cell_id[[i]], family_id = family,
        replication = as.integer(manifest$replication[[i]]),
        completed = isTRUE(manifest$completed[[i]]),
        released = if (is.null(artifact)) FALSE else
          isTRUE(value$released[[1L]]),
        covered = if (is.null(artifact)) NA else value$covered[[1L]],
        stringsAsFactors = FALSE)
    }
  }
  replication <- do.call(rbind, rows)
  assessment <- .scmix_v24_assess_validation(
    replication, plan$approval,
    expected_reps = plan$counts$evidence_reps)
  result <- assessment$family_cell
  result$pass <- ifelse(result$required, result$passed, TRUE)
  result$passed <- NULL
  path <- .scmix_v24_write_csv_atomic(
    result, file.path(run_dir, "family-result-manifest.csv"))
  list(
    protocol_id = plan$protocol_id,
    result_manifest_file = path,
    result_manifest_relative_to_source =
      .scmix_v24_relative_or_absolute(path, source_root),
    result_manifest_sha256 =
      unname(as.character(tools::sha256sum(path))),
    completion = assessment$completion,
    all_cell_completion_pass = assessment$all_cell_completion_pass,
    all_required_rows_pass = assessment$all_required_rows_pass,
    mechanically_approved = assessment$approved,
    approved_families = family_ids[vapply(family_ids, function(family) {
      index <- result$family_id == family & result$required
      all(result$pass[index])
    }, logical(1L))],
    application_outcomes_used = FALSE)
}
