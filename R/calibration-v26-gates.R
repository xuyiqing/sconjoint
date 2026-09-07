## Fixed per-replication gates for prospective inference protocol v2.6.
##
## The functions in this file consume, but never create, synthetic outcomes.
## They translate the diagnostics retained by the two scmix_dml() calls into
## the reported-target and complete-family decisions frozen in the v2.6
## registry.  In particular, a ridge-neighborhood record changes one outer
## fold at a time.  The code below reconstructs the full cross-fit estimator
## under each fixed ridge rate before measuring sensitivity.

.scmix_v26_gate_target_specs <- function() {
  list(
    mean_direction = list(
      family = "smooth_means", primitive = "mean_direction",
      map = "unrestricted", constraint = "unrestricted"),
    mean_numerator = list(
      family = "smooth_means", primitive = "mean_numerator",
      map = "unrestricted", constraint = "unrestricted"),
    mean_denominator = list(
      family = "smooth_means", primitive = "mean_denominator",
      map = "unrestricted", constraint = "unrestricted"),
    choice_probability = list(
      family = "choice_probabilities", primitive = "choice_probability",
      map = "probability", constraint = "probability"),
    preference_direction_share = list(
      family = "directional_shares",
      primitive = "preference_direction_share",
      regularity_primitive = "preference_direction_variance",
      map = "probability", constraint = "probability"),
    compensating_benefit_share = list(
      family = "directional_shares",
      primitive = "compensating_benefit_share",
      regularity_primitive = "compensating_benefit_variance",
      map = "probability", constraint = "probability"),
    subgroup_high_choice_share = list(
      family = "subgroup_ratios",
      primitive = c("subgroup_high_numerator",
                    "subgroup_high_denominator"),
      map = "subgroup_ratio", constraint = "subgroup"),
    subgroup_low_choice_share = list(
      family = "subgroup_ratios",
      primitive = c("subgroup_low_numerator",
                    "subgroup_low_denominator"),
      map = "subgroup_ratio", constraint = "subgroup"),
    heterogeneity_explained_variance = list(
      family = "directional_heterogeneity",
      primitive = "heterogeneity_explained_variance",
      map = "positive", constraint = "positive"),
    heterogeneity_residual_variance = list(
      family = "directional_heterogeneity",
      primitive = "heterogeneity_residual_variance",
      map = "positive", constraint = "positive"),
    heterogeneity_explained_share = list(
      family = "directional_heterogeneity",
      primitive = c("heterogeneity_explained_variance",
                    "heterogeneity_residual_variance"),
      map = "heterogeneity_share", constraint = "positive_vector"),
    mrs = list(
      family = "mrs", primitive = c("mrs_numerator", "mrs_denominator"),
      map = "mrs", constraint = "nonzero_denominator"))
}

.scmix_v26_gate_family_specs <- function() {
  targets <- .scmix_v26_gate_target_specs()
  family_ids <- c("smooth_means", "choice_probabilities",
                  "directional_shares", "subgroup_ratios",
                  "directional_heterogeneity", "mrs")
  stats::setNames(lapply(family_ids, function(id) {
    members <- names(targets)[vapply(targets, function(x)
      identical(x$family, id), logical(1L))]
    primitives <- unique(unlist(lapply(targets[members], `[[`,
                                       "primitive"), use.names = FALSE))
    list(members = members, primitives = primitives)
  }), family_ids)
}

.scmix_v26_gate_branch <- function(x, label) {
  if (!is.list(x)) {
    stop("The v2.6 ", label, " branch must be a list.", call. = FALSE)
  }
  inference <- if (is.list(x$inference)) x$inference else x
  view <- if (is.list(x$view)) x$view else
    .scmix_v26_inference_view(inference)
  required_inference <- c("estimate", "plugin", "influence",
                          "respondent_id", "base")
  required_view <- c("primitive_estimate", "primitive_plugin",
                     "primitive_influence", "reported_estimate",
                     "reported_influence", "reported_se", "respondent_id")
  if (!all(required_inference %in% names(inference)) ||
      !all(required_view %in% names(view))) {
    stop("The v2.6 ", label,
         " branch is not a complete DML/inference-view bundle.",
         call. = FALSE)
  }
  primitive_names <- names(view$primitive_estimate)
  specs <- .scmix_v26_gate_target_specs()
  target_names <- names(specs)
  declared_failed <- unique(as.character(
    inference$failed_targets %||% character()))
  if (any(!declared_failed %in% target_names)) {
    stop("The v2.6 ", label, " branch declares an unknown failed target.",
         call. = FALSE)
  }
  missing_target <- target_names[vapply(specs, function(spec) {
    needed <- unique(c(spec$primitive,
                       spec$regularity_primitive %||% character()))
    !all(needed %in% primitive_names)
  }, logical(1L))]
  if (length(setdiff(missing_target, declared_failed))) {
    stop("The v2.6 ", label,
         " branch lost a primitive without declaring its target failure.",
         call. = FALSE)
  }
  if (is.null(primitive_names) || anyDuplicated(primitive_names) ||
      !all(target_names %in% names(view$reported_estimate)) ||
      !all(target_names %in% colnames(view$reported_influence)) ||
      !identical(as.character(inference$respondent_id),
                 as.character(view$respondent_id))) {
    stop("The v2.6 ", label,
         " branch has an incomplete or inconsistent target registry.",
         call. = FALSE)
  }
  IF <- as.matrix(view$reported_influence)
  primitive_IF <- as.matrix(view$primitive_influence)
  if (nrow(IF) != length(view$respondent_id) ||
      is.null(rownames(IF)) || anyDuplicated(rownames(IF)) ||
      !setequal(as.character(rownames(IF)),
                as.character(view$respondent_id)) ||
      nrow(primitive_IF) != length(view$respondent_id) ||
      is.null(rownames(primitive_IF)) ||
      !setequal(as.character(rownames(primitive_IF)),
                as.character(view$respondent_id)) ||
      any(!is.finite(c(view$primitive_estimate,
                       view$primitive_plugin, primitive_IF)))) {
    stop("The v2.6 ", label,
         " branch has invalid primitive estimates or influence rows.",
         call. = FALSE)
  }
  available <- setdiff(target_names, missing_target)
  regular_report <- available[vapply(available, function(id) {
    is.finite(view$reported_estimate[[id]]) &&
      is.finite(view$reported_se[[id]]) && view$reported_se[[id]] > 0 &&
      all(is.finite(IF[, id]))
  }, logical(1L))]
  list(
    inference = inference, view = view,
    failed_targets = declared_failed,
    failed_families = unique(as.character(
      inference$failed_families %||% character())),
    missing_targets = missing_target,
    available_targets = available,
    regular_reported_targets = regular_report,
    unavailable_targets = setdiff(target_names, regular_report))
}

.scmix_v26_gate_raw_map <- function(inference, component) {
  dml <- inference[[component]]
  if (is.null(dml) && identical(component, "heterogeneity")) {
    return(stats::setNames(character(), character()))
  }
  if (!inherits(dml, "scmix_dml") && !is.list(dml)) {
    stop("The v2.6 ", component, " DML object is absent.", call. = FALSE)
  }
  raw <- names(dml$estimate)
  if (is.null(raw) || anyDuplicated(raw)) {
    stop("The v2.6 ", component,
         " DML primitive labels are malformed.", call. = FALSE)
  }
  all_canonical <- names(inference$estimate)
  if (identical(component, "heterogeneity")) {
    canonical <- intersect(
      c("heterogeneity_explained_variance",
        "heterogeneity_residual_variance"), all_canonical)
  } else {
    canonical <- setdiff(all_canonical,
                         c("heterogeneity_explained_variance",
                           "heterogeneity_residual_variance"))
  }
  answer <- stats::setNames(rep(NA_character_, length(canonical)), canonical)
  answer[canonical %in% raw] <- canonical[canonical %in% raw]

  ## base_label_map has existed in both orientations during development.
  ## Accept either representation, but require a unique exact resolution.
  declared <- if (identical(component, "base")) {
    inference$base_label_map
  } else {
    inference$heterogeneity_label_map
  }
  if (length(declared)) {
    for (j in seq_along(declared)) {
      left <- names(declared)[[j]]
      right <- as.character(declared[[j]])
      if (left %in% canonical && right %in% raw) answer[[left]] <- right
      if (right %in% canonical && left %in% raw) answer[[right]] <- left
    }
  }
  patterns <- c(
    subgroup_high_numerator = "^subgroup_high:.*numerator$",
    subgroup_high_denominator = "^subgroup_high:.*denominator$",
    subgroup_low_numerator = "^subgroup_low:.*numerator$",
    subgroup_low_denominator = "^subgroup_low:.*denominator$",
    mrs_numerator = "^mrs:numerator$",
    mrs_denominator = "^mrs:denominator$")
  for (id in intersect(names(patterns), canonical[is.na(answer)])) {
    hit <- grep(patterns[[id]], raw, value = TRUE)
    if (length(hit) == 1L) answer[[id]] <- hit
  }
  if (anyNA(answer) || anyDuplicated(unname(answer))) {
    stop("The v2.6 ", component,
         " DML labels cannot be mapped uniquely to frozen primitives.",
         call. = FALSE)
  }
  answer
}

.scmix_v26_gate_fold_metric <- function(inference, field,
                                         require_exact_derivative = FALSE) {
  result <- stats::setNames(rep(NA_real_, length(inference$estimate)),
                            names(inference$estimate))
  for (component in c("base", "heterogeneity")) {
    dml <- inference[[component]]
    mapping <- .scmix_v26_gate_raw_map(inference, component)
    if (!length(mapping)) next
    values <- stats::setNames(rep(-Inf, length(mapping)), names(mapping))
    seen <- stats::setNames(rep(FALSE, length(mapping)), names(mapping))
    if (!is.list(dml$fold_details) || !length(dml$fold_details)) {
      stop("The v2.6 DML object has no outer-fold diagnostics.",
           call. = FALSE)
    }
    for (fd in dml$fold_details) {
      metric <- fd[[field]]
      if (is.null(metric) || is.null(names(metric))) {
        stop("A v2.6 DML fold lacks named `", field, "` diagnostics.",
             call. = FALSE)
      }
      for (id in names(mapping)) {
        raw <- mapping[[id]]
        value <- suppressWarnings(as.numeric(metric[[raw]]))
        if (length(value) != 1L || !is.finite(value)) value <- Inf
        if (isTRUE(require_exact_derivative)) {
          exact <- fd$derivative_refinement_protocol_exact
          exact <- !is.null(exact) && !is.null(names(exact)) &&
            identical(as.logical(exact[[raw]]), TRUE)
          if (!exact) value <- Inf
        }
        values[[id]] <- max(values[[id]], value)
        seen[[id]] <- TRUE
      }
    }
    values[!seen] <- Inf
    result[names(values)] <- values
  }
  result
}

.scmix_v26_gate_svd_rule <- function(inference) {
  components <- c("base", if (!is.null(inference[["heterogeneity"]]))
    "heterogeneity" else character())
  details <- unlist(lapply(components, function(component) {
    lapply(inference[[component]]$fold_details, function(fd) {
      I <- as.matrix(fd$honest_evaluation_information)
      if (!identical(fd$riesz_numerical_rank_rule, "dimension_epsilon") ||
          !nrow(I) || nrow(I) != ncol(I) || any(!is.finite(I))) {
        return(FALSE)
      }
      value <- eigen((I + t(I)) / 2, symmetric = TRUE,
                     only.values = TRUE)$values
      cutoff <- nrow(I) * .Machine$double.eps * max(abs(value))
      recorded <- as.numeric(fd$riesz_numerical_rank_cutoff)
      rank <- as.integer(fd$honest_evaluation_numerical_rank)
      is.finite(recorded) &&
        isTRUE(all.equal(recorded, cutoff, tolerance = 64 *
                           .Machine$double.eps, check.attributes = FALSE)) &&
        length(rank) == 1L && !is.na(rank) &&
        identical(rank, as.integer(sum(value > cutoff)))
    })
  }), recursive = FALSE)
  length(details) > 0L && all(vapply(details, isTRUE, logical(1L)))
}

.scmix_v26_gate_pick_nu <- function(artifact, wanted, label) {
  nu <- suppressWarnings(as.numeric(artifact$nu))
  if (!length(nu) || any(!is.finite(nu)) || !is.finite(wanted)) {
    stop("The v2.6 ridge artifact for `", label, "` is malformed.",
         call. = FALSE)
  }
  error <- abs(nu - wanted)
  index <- which(error == min(error))
  tolerance <- 256 * .Machine$double.eps * max(1, abs(wanted))
  if (length(index) != 1L || error[[index]] > tolerance) {
    stop("The v2.6 ridge artifact for `", label,
         "` lacks its fixed fold-specific sensitivity rate.",
         call. = FALSE)
  }
  index
}

.scmix_v26_gate_ridge_component <- function(dml, mapping, exponent) {
  if (!is.list(dml) || is.null(dml$estimate) || is.null(dml$influence) ||
      !length(mapping)) {
    stop("A v2.6 ridge component is absent or empty.", call. = FALSE)
  }
  raw_ids <- unname(mapping)
  primary_estimate <- dml$estimate[raw_ids]
  primary_IF <- as.matrix(dml$influence[, raw_ids, drop = FALSE])
  respondent_id <- as.character(dml$respondent_id)
  if (is.null(rownames(primary_IF))) rownames(primary_IF) <- respondent_id
  if (!identical(rownames(primary_IF), respondent_id)) {
    primary_IF <- primary_IF[match(respondent_id, rownames(primary_IF)), ,
                             drop = FALSE]
  }
  alternative_estimate <- primary_estimate
  alternative_IF <- primary_IF
  for (fd in dml$fold_details) {
    wanted <- as.numeric(fd$training_n)^exponent
    artifacts <- fd$ridge_neighborhood_artifact
    if (!is.list(artifacts)) {
      stop("A v2.6 DML fold lacks ridge-neighborhood artifacts.",
           call. = FALSE)
    }
    for (j in seq_along(raw_ids)) {
      raw <- raw_ids[[j]]
      artifact <- artifacts[[raw]]
      if (!is.list(artifact)) {
        stop("A v2.6 ridge artifact is absent for `", raw, "`.",
             call. = FALSE)
      }
      index <- .scmix_v26_gate_pick_nu(artifact, wanted, raw)
      one_estimate <- as.numeric(artifact$raw_one_step[[index]])
      one_IF <- as.matrix(artifact$influence[, index, drop = FALSE])[, 1L]
      artifact_ids <- rownames(artifact$influence)
      if (is.null(artifact_ids) || anyDuplicated(artifact_ids) ||
          !setequal(artifact_ids, respondent_id)) {
        stop("A v2.6 ridge artifact has unkeyed influence rows.",
             call. = FALSE)
      }
      one_IF <- one_IF[match(respondent_id, artifact_ids)]
      if (!is.finite(one_estimate) || any(!is.finite(one_IF))) {
        stop("A v2.6 ridge sensitivity contains nonfinite values.",
             call. = FALSE)
      }
      ## Each artifact replaces only this fold.  Add its departure from the
      ## common primary estimator to obtain the all-fold fixed-rate result.
      alternative_estimate[[j]] <- alternative_estimate[[j]] +
        one_estimate - primary_estimate[[j]]
      alternative_IF[, j] <- alternative_IF[, j] +
        one_IF - primary_IF[, j]
    }
  }
  names(alternative_estimate) <- names(mapping)
  colnames(alternative_IF) <- names(mapping)
  rownames(alternative_IF) <- respondent_id
  list(estimate = alternative_estimate, influence = alternative_IF)
}

.scmix_v26_reconstruct_ridge <- function(inference,
                                          exponent = c(-1, -1 / 2)) {
  exponent <- as.numeric(exponent)
  if (length(exponent) != 1L || !is.finite(exponent) ||
      !exponent %in% c(-1, -1 / 2)) {
    stop("The v2.6 ridge sensitivity exponent must be -1 or -1/2.",
         call. = FALSE)
  }
  components <- c("base", if (!is.null(inference[["heterogeneity"]]))
    "heterogeneity" else character())
  pieces <- lapply(components, function(component) {
    map <- .scmix_v26_gate_raw_map(inference, component)
    .scmix_v26_gate_ridge_component(inference[[component]], map, exponent)
  })
  estimate <- do.call(c, unname(lapply(pieces, `[[`, "estimate")))
  influence <- do.call(cbind, unname(lapply(pieces, `[[`, "influence")))
  order <- names(inference$estimate)
  if (!setequal(names(estimate), order)) {
    stop("The reconstructed v2.6 ridge estimator lost a primitive.",
         call. = FALSE)
  }
  list(
    exponent = exponent,
    rule = if (exponent == -1) "Ntrain^(-1)" else "Ntrain^(-1/2)",
    estimate = estimate[order], plugin = inference$plugin[order],
    influence = influence[, order, drop = FALSE],
    respondent_id = inference$respondent_id,
    failed_targets = inference$failed_targets %||% character(),
    failed_families = inference$failed_families %||% character(),
    failure_records = inference$failure_records %||% NULL,
    target_specific = isTRUE(inference$target_specific))
}

.scmix_v26_gate_chart_points <- function(view) {
  specs <- .scmix_v26_gate_target_specs()
  out <- stats::setNames(rep(NA_real_, length(specs)), names(specs))
  interior <- stats::setNames(rep(FALSE, length(specs)), names(specs))
  for (id in names(specs)) {
    primitive <- specs[[id]]$primitive
    point <- .scmix_v26_chart_point(
      specs[[id]]$map, view$primitive_plugin[primitive],
      view$primitive_estimate[primitive])
    out[[id]] <- point$target
    interior[[id]] <- isTRUE(point$interior) && is.finite(point$target)
  }
  list(estimate = out, interior = interior)
}

.scmix_v26_gate_primitive_to_target <- function(value) {
  specs <- .scmix_v26_gate_target_specs()
  stats::setNames(vapply(specs, function(spec) {
    primitive <- unique(c(
      spec$primitive, spec$regularity_primitive %||% character()))
    x <- value[primitive]
    if (length(x) != length(primitive) || any(!is.finite(x))) Inf else
      max(x)
  }, numeric(1L)), names(specs))
}

.scmix_v26_gate_constraint_z <- function(view) {
  specs <- .scmix_v26_gate_target_specs()
  estimate <- view$primitive_estimate
  IF <- as.matrix(view$primitive_influence)
  se_of <- function(x) unname(.scmix_v26_influence_se(as.matrix(x))[[1L]])
  ratio <- function(margin, influence) {
    scale <- se_of(influence)
    if (!is.finite(margin) || !is.finite(scale) || scale <= 0) -Inf else
      margin / scale
  }
  active_id <- "active_q1_covariance_eigenvalue"
  if (!active_id %in% names(estimate) || !active_id %in% colnames(IF)) {
    stop("The global v2.6 active-q1 regularity primitive is absent.",
         call. = FALSE)
  }
  active <- ratio(estimate[[active_id]], IF[, active_id])
  target <- stats::setNames(rep(active, length(specs)), names(specs))
  for (id in names(specs)) {
    spec <- specs[[id]]
    p <- spec$primitive
    needed <- unique(c(p, spec$regularity_primitive %||% character()))
    if (!all(needed %in% names(estimate)) ||
        !all(needed %in% colnames(IF))) {
      target[[id]] <- -Inf
      next
    }
    local <- switch(
      spec$constraint,
      unrestricted = Inf,
      probability = min(
        ratio(estimate[[p]], IF[, p]),
        ratio(1 - estimate[[p]], -IF[, p])),
      positive = ratio(estimate[[p]], IF[, p]),
      positive_vector = min(vapply(p, function(j)
        ratio(estimate[[j]], IF[, j]), numeric(1L))),
      subgroup = {
        a <- p[[1L]]; b <- p[[2L]]
        min(ratio(estimate[[a]], IF[, a]),
            ratio(estimate[[b]] - estimate[[a]], IF[, b] - IF[, a]),
            ratio(1 - estimate[[b]], -IF[, b]))
      },
      nonzero_denominator = {
        b <- p[[2L]]
        ratio(abs(estimate[[b]]),
              sign(estimate[[b]]) * IF[, b])
      },
      stop("Unknown v2.6 primitive constraint.", call. = FALSE))
    regularity_id <- spec$regularity_primitive %||% character()
    if (length(regularity_id)) {
      if (length(regularity_id) != 1L ||
          !regularity_id %in% names(estimate) ||
          !regularity_id %in% colnames(IF)) {
        target[[id]] <- -Inf
        next
      }
      local <- min(local, ratio(
        estimate[[regularity_id]], IF[, regularity_id]))
    }
    target[[id]] <- min(active, local)
  }
  list(target = target, active = active)
}

.scmix_v26_gate_if_movement <- function(reference, alternative,
                                         respondent_id) {
  reference <- as.matrix(reference)
  alternative <- as.matrix(alternative)
  ids <- as.character(respondent_id)
  if (is.null(rownames(reference)) || is.null(rownames(alternative)) ||
      !setequal(rownames(reference), ids) ||
      !setequal(rownames(alternative), ids) ||
      !setequal(colnames(reference), colnames(alternative))) {
    stop("The v2.6 GH influence arrays are not conformable by respondent.",
         call. = FALSE)
  }
  reference <- reference[match(ids, rownames(reference)), , drop = FALSE]
  alternative <- alternative[match(ids, rownames(alternative)),
                             colnames(reference), drop = FALSE]
  reference <- sweep(reference, 2L, colMeans(reference), `-`)
  alternative <- sweep(alternative, 2L, colMeans(alternative), `-`)
  denominator <- sqrt(colMeans(reference^2))
  numerator <- sqrt(colMeans((alternative - reference)^2))
  out <- numerator / denominator
  out[!is.finite(out)] <- Inf
  out
}

.scmix_v26_gate_registry <- function(decision_registry = NULL) {
  registry <- decision_registry
  if (is.null(registry)) {
    registry <- utils::read.csv(.scmix_v26_file("decisions"),
                                stringsAsFactors = FALSE,
                                check.names = FALSE)
  } else if (is.character(registry) && length(registry) == 1L) {
    registry <- utils::read.csv(registry, stringsAsFactors = FALSE,
                                check.names = FALSE)
  }
  required <- c("rule_id", "scope", "metric", "comparator", "value")
  rules <- c(
    "regularity_z", "chart_nonlinear_discrepancy", "riesz_defect",
    "ridge_movement", "block_kkt", "nested_objective",
    "near_optimum_objective", "near_optimum_target_spread",
    "integration_target", "integration_if", "loading_invariance",
    "derivative", "null_space", "svd_rank")
  if (!is.data.frame(registry) || !all(required %in% names(registry)) ||
      anyDuplicated(registry$rule_id) || !all(rules %in% registry$rule_id)) {
    stop("The fixed v2.6 decision registry is absent or malformed.",
         call. = FALSE)
  }
  registry[match(rules, registry$rule_id), , drop = FALSE]
}

.scmix_v26_gate_compare <- function(observed, row) {
  comparator <- as.character(row$comparator[[1L]])
  if (identical(comparator, "formula")) {
    return(isTRUE(observed) ||
             (is.numeric(observed) && length(observed) == 1L &&
                is.finite(observed) && observed == 1))
  }
  threshold <- suppressWarnings(as.numeric(row$value[[1L]]))
  if (!is.numeric(observed) || length(observed) != 1L ||
      !is.finite(observed) || !is.finite(threshold)) return(FALSE)
  if (identical(comparator, "less_than_or_equal")) observed <= threshold else
    if (identical(comparator, "greater_than_or_equal"))
      observed >= threshold else
        stop("Unsupported v2.6 gate comparator `", comparator, "`.",
             call. = FALSE)
}

.scmix_v26_gate_incomplete_family <- function(
    family, reason, failed_targets = character()) {
  members <- .scmix_v26_gate_family_specs()[[family]]$members
  member <- stats::setNames(lapply(members, function(id) list(
    primitive_id = character(), estimate = NA_real_,
    interval = c(lower = NA_real_, upper = NA_real_),
    interval_nonempty = FALSE, interval_uninformative = TRUE,
    boundary_touch = NA, map_reason = reason, chart_interior = FALSE,
    chart_reason = reason)), members)
  structure(list(
    family_id = family, multiplier = NULL, complete = FALSE,
    members = member, reason = reason,
    failed_targets = unique(as.character(failed_targets))),
    class = c("scmix_v26_family_result", "list"))
}

.scmix_v26_gate_finalize_all <- function(view, bank) {
  family_specs <- .scmix_v26_gate_family_specs()
  result <- lapply(names(family_specs), function(family) {
    spec <- family_specs[[family]]
    family_targets <- spec$members
    missing <- setdiff(spec$primitives, names(view$primitive_estimate))
    invalid <- intersect(spec$primitives, names(view$primitive_estimate))
    invalid <- invalid[!vapply(invalid, function(id)
      is.finite(view$primitive_estimate[[id]]) &&
        id %in% names(view$primitive_plugin) &&
        is.finite(view$primitive_plugin[[id]]) &&
        id %in% colnames(view$primitive_influence) &&
        all(is.finite(view$primitive_influence[, id])), logical(1L))]
    declared <- intersect(
      view$failed_targets %||% character(), family_targets)
    if (length(missing) || length(invalid) || length(declared)) {
      return(.scmix_v26_gate_incomplete_family(
        family, "target_specific_dml_failure",
        unique(c(declared, family_targets[vapply(family_targets, function(id) {
          needed <- .scmix_v26_gate_target_specs()[[id]]$primitive
          any(needed %in% c(missing, invalid))
        }, logical(1L))]))))
    }
    member_map <- if (identical(family, "subgroup_ratios")) list(
      subgroup_high_choice_share = c("subgroup_high_numerator",
                                     "subgroup_high_denominator"),
      subgroup_low_choice_share = c("subgroup_low_numerator",
                                    "subgroup_low_denominator")) else NULL
    tryCatch(
      .scmix_v26_finalize_family(
        family, estimate = view$primitive_estimate[spec$primitives],
        influence = view$primitive_influence[, spec$primitives, drop = FALSE],
        respondent_id = view$respondent_id, bank = bank,
        plugin = view$primitive_plugin[spec$primitives],
        member_map = member_map),
      error = function(e) .scmix_v26_gate_incomplete_family(
        family, paste0("family_finalizer_failure:", conditionMessage(e)),
        family_targets))
  })
  stats::setNames(result, names(family_specs))
}

.scmix_v26_gate_finalizer_ready <- function(x) {
  is.list(x) && isTRUE(x$complete) && length(x$members) > 0L &&
    all(vapply(x$members, function(member) {
      isTRUE(member$interval_nonempty) &&
        !isTRUE(member$interval_uninformative) &&
        !isTRUE(member$boundary_touch) &&
        isTRUE(member$chart_interior) &&
        all(is.finite(c(member$estimate, member$interval)))
    }, logical(1L)))
}

.scmix_v26_gate_finalizer_points <- function(x) {
  stats::setNames(vapply(x$members, `[[`, numeric(1L), "estimate"),
                  names(x$members))
}

.scmix_v26_gate_ridge_movement <- function(
    baseline_view, alternatives, baseline_final, alternative_final) {
  target_names <- names(.scmix_v26_gate_target_specs())
  se <- baseline_view$reported_se[target_names]
  base_point <- do.call(c, unname(lapply(
    baseline_final, .scmix_v26_gate_finalizer_points)))
  base_point <- base_point[target_names]
  movement <- stats::setNames(numeric(length(target_names)), target_names)
  detail <- list()
  for (i in seq_along(alternatives)) {
    alt_point <- do.call(c, unname(lapply(
      alternative_final[[i]], .scmix_v26_gate_finalizer_points)))
    alt_point <- alt_point[target_names]
    point <- abs(alt_point - base_point) / se
    endpoint <- stats::setNames(numeric(length(target_names)), target_names)
    for (family in names(baseline_final)) {
      for (target in names(baseline_final[[family]]$members)) {
        base_interval <- baseline_final[[family]]$members[[target]]$interval
        alt_member <- alternative_final[[i]][[family]]$members[[target]]
        alt_interval <- if (is.list(alt_member)) alt_member$interval else
          numeric()
        endpoint[[target]] <- if (length(base_interval) != 2L ||
          length(alt_interval) != 2L ||
          any(!is.finite(c(base_interval, alt_interval)))) Inf else
            max(abs(alt_interval - base_interval)) / se[[target]]
      }
    }
    value <- pmax(point, endpoint)
    value[!is.finite(value)] <- Inf
    movement <- pmax(movement, value)
    detail[[alternatives[[i]]$rule]] <- list(
      point = point, endpoint = endpoint, maximum = value,
      primitive_estimate = alternatives[[i]]$estimate,
      primitive_influence = alternatives[[i]]$influence)
  }
  list(maximum = movement, alternatives = detail)
}

## Apply every within-replication v2.6 gate.
##
## `near_optimum_movement` is deliberately an explicit, complete vector.  The
## gate engine does not rediscover or redefine the near-optimum state set.
## That set and its target movement are constructed by the hash-bound runner.
.scmix_v26_gate_engine <- function(
    baseline, fixed31, repolished31, fit_metrics,
    near_optimum_movement, multiplier_bank,
    decision_registry = NULL) {
  branch <- list(
    baseline = .scmix_v26_gate_branch(baseline, "baseline"),
    fixed31 = .scmix_v26_gate_branch(fixed31, "fixed-GH31"),
    repolished31 = .scmix_v26_gate_branch(repolished31,
                                          "repolished-GH31"))
  target_specs <- .scmix_v26_gate_target_specs()
  target_names <- names(target_specs)
  family_specs <- .scmix_v26_gate_family_specs()
  failed_targets <- unique(unlist(lapply(branch, `[[`, "failed_targets"),
                                  use.names = FALSE))
  failed_families <- unique(unlist(lapply(branch, `[[`, "failed_families"),
                                   use.names = FALSE))
  if (!is.numeric(near_optimum_movement) ||
      is.null(names(near_optimum_movement)) ||
      !all(target_names %in% names(near_optimum_movement)) ||
      anyNA(near_optimum_movement[target_names]) ||
      any(near_optimum_movement[target_names] < 0) ||
      any(!is.finite(near_optimum_movement[setdiff(
        target_names, failed_targets)]))) {
    stop("near_optimum_movement must be a complete named nonnegative ",
         "vector, with infinity allowed only for a declared failed target.",
         call. = FALSE)
  }
  near_optimum_movement <- near_optimum_movement[target_names]
  registry <- .scmix_v26_gate_registry(decision_registry)
  base <- branch$baseline
  constraint <- .scmix_v26_gate_constraint_z(base$view)
  base_chart <- .scmix_v26_gate_chart_points(base$view)
  chart_discrepancy <- abs(
    base_chart$estimate - base$view$reported_estimate[target_names]) /
    base$view$reported_se[target_names]
  chart_discrepancy[!base_chart$interior |
                      !is.finite(chart_discrepancy)] <- Inf

  ## v2.6 prospectively gates the recorded analytic-versus-h/h2 relative
  ## derivative error itself.  The separate `protocol_exact` flag belongs to
  ## the superseded v2.2 geometry and is deliberately not a v2.6 criterion.
  primitive_derivative <- .scmix_v26_gate_fold_metric(
    base$inference, "derivative_refinement_error", FALSE)
  primitive_riesz <- .scmix_v26_gate_fold_metric(
    base$inference, "honest_information_scaled_riesz_defect")
  primitive_null <- .scmix_v26_gate_fold_metric(
    base$inference, "honest_riesz_null_space_defect")
  derivative <- .scmix_v26_gate_primitive_to_target(primitive_derivative)
  riesz <- .scmix_v26_gate_primitive_to_target(primitive_riesz)
  null <- .scmix_v26_gate_primitive_to_target(primitive_null)

  gh_target <- gh_if <- stats::setNames(numeric(length(target_names)),
                                         target_names)
  for (id in c("fixed31", "repolished31")) {
    chart <- .scmix_v26_gate_chart_points(branch[[id]]$view)
    movement <- abs(chart$estimate - base_chart$estimate) /
      base$view$reported_se[target_names]
    movement[!chart$interior | !is.finite(movement)] <- Inf
    gh_target <- pmax(gh_target, movement)
    influence_movement <- .scmix_v26_gate_if_movement(
      base$view$reported_influence[, target_names, drop = FALSE],
      branch[[id]]$view$reported_influence[, target_names, drop = FALSE],
      base$view$respondent_id)
    gh_if <- pmax(gh_if, influence_movement[target_names])
  }

  final <- lapply(branch, function(x)
    .scmix_v26_gate_finalize_all(x$view, multiplier_bank))
  ridge_alt <- lapply(c(-1, -1 / 2), function(exponent) {
    reconstructed <- .scmix_v26_reconstruct_ridge(
      base$inference, exponent)
    reconstructed$view <- .scmix_v26_inference_view(reconstructed)
    reconstructed
  })
  ridge_final <- lapply(ridge_alt, function(x)
    .scmix_v26_gate_finalize_all(x$view, multiplier_bank))
  ridge <- .scmix_v26_gate_ridge_movement(
    base$view, ridge_alt, final$baseline, ridge_final)

  target_metric <- list(
    regularity_z = constraint$target,
    chart_nonlinear_discrepancy = chart_discrepancy,
    riesz_defect = riesz, ridge_movement = ridge$maximum,
    near_optimum_target_spread = near_optimum_movement,
    integration_target = gh_target, integration_if = gh_if,
    derivative = derivative, null_space = null)
  for (rule in names(target_metric)) {
    target_metric[[rule]][failed_targets] <- Inf
  }
  target_rules <- names(target_metric)
  target_gates <- do.call(rbind, lapply(target_names, function(target) {
    do.call(rbind, lapply(target_rules, function(rule) {
      row <- registry[registry$rule_id == rule, , drop = FALSE]
      observed <- unname(target_metric[[rule]][[target]])
      data.frame(
        target_id = target, family_id = target_specs[[target]]$family,
        rule_id = rule, observed = observed,
        comparator = as.character(row$comparator[[1L]]),
        threshold = suppressWarnings(as.numeric(row$value[[1L]])),
        pass = !target %in% failed_targets &&
          .scmix_v26_gate_compare(observed, row),
        stringsAsFactors = FALSE)
    }))
  }))
  rownames(target_gates) <- NULL

  if (!is.list(fit_metrics)) {
    stop("fit_metrics must contain the frozen v2.6 fit diagnostics.",
         call. = FALSE)
  }
  near_objective <- fit_metrics$maximum_near_optimum_objective_gap
  if (is.null(near_objective)) {
    surface <- fit_metrics$start_surface
    gaps <- if (is.data.frame(surface) &&
                "objective_relative_gap" %in% names(surface))
      surface$objective_relative_gap else numeric()
    near_cutoff <- suppressWarnings(as.numeric(registry$value[
      registry$rule_id == "near_optimum_objective"]))
    gaps <- gaps[is.finite(gaps) & gaps <= near_cutoff]
    near_objective <- if (length(gaps)) max(gaps) else Inf
  }
  fit_metric <- c(
    block_kkt = fit_metrics$maximum_block_kkt,
    nested_objective = fit_metrics$maximum_nested_objective_gap,
    near_optimum_objective = near_objective,
    loading_invariance = fit_metrics$maximum_loading_probability_error,
    svd_rank = .scmix_v26_gate_svd_rule(base$inference))
  fit_gates <- do.call(rbind, lapply(names(fit_metric), function(rule) {
    row <- registry[registry$rule_id == rule, , drop = FALSE]
    observed <- fit_metric[[rule]]
    data.frame(
      rule_id = rule,
      observed = if (is.logical(observed)) as.numeric(observed) else
        as.numeric(observed),
      comparator = as.character(row$comparator[[1L]]),
      threshold = if (identical(row$comparator[[1L]], "formula")) NA_real_
        else suppressWarnings(as.numeric(row$value[[1L]])),
      pass = .scmix_v26_gate_compare(observed, row),
      stringsAsFactors = FALSE)
  }))
  rownames(fit_gates) <- NULL
  fit_integrity <- isTRUE(fit_metrics$every_polish_completed) &&
    isTRUE(fit_metrics$every_polish_accepted) &&
    isTRUE(fit_metrics$every_repolish_exact_state) &&
    isTRUE(fit_metrics$every_state_float64) &&
    isTRUE(fit_metrics$all_finite)
  common_fit_pass <- fit_integrity && all(fit_gates$pass)

  family_gates <- do.call(rbind, lapply(names(family_specs), function(family) {
    members <- family_specs[[family]]$members
    tg <- target_gates[target_gates$target_id %in% members, , drop = FALSE]
    finalizer_ready <- .scmix_v26_gate_finalizer_ready(
      final$baseline[[family]])
    target_failure <- intersect(members, failed_targets)
    declared_family_failure <- family %in% failed_families
    branch_reasons <- unique(vapply(final, function(x)
      as.character(x[[family]]$reason %||% ""), character(1L)))
    branch_dml_failure <- any(
      branch_reasons == "target_specific_dml_failure")
    branch_finalizer_reasons <- branch_reasons[startsWith(
      branch_reasons, "family_finalizer_failure:")]
    dml_failure <- length(target_failure) > 0L ||
      declared_family_failure || branch_dml_failure
    isolated_failure <- dml_failure || length(branch_finalizer_reasons) > 0L
    dml_failure_label <- if (length(target_failure)) {
      paste(target_failure, collapse = ",")
    } else family
    failed <- unique(c(
      if (!common_fit_pass) paste0("fit:",
        fit_gates$rule_id[!fit_gates$pass]) else character(),
      paste0(tg$target_id[!tg$pass], ":", tg$rule_id[!tg$pass]),
      if (!finalizer_ready) "family_confidence_set_not_regular" else
        character(),
      if (!fit_integrity) "fit:integrity" else character(),
      if (dml_failure) paste0(
        "target_specific_dml_failure:",
        dml_failure_label) else character(),
      branch_finalizer_reasons))
    data.frame(
      family_id = family, common_fit_pass = common_fit_pass,
      all_target_gates_pass = nrow(tg) > 0L && all(tg$pass),
      family_confidence_set_ready = finalizer_ready,
      target_specific_failure = isolated_failure,
      released = common_fit_pass && nrow(tg) > 0L && all(tg$pass) &&
        finalizer_ready && !isolated_failure,
      failed_gates = paste(failed, collapse = "|"),
      stringsAsFactors = FALSE)
  }))
  rownames(family_gates) <- NULL
  list(
    protocol_id = "paperps-inference-2026-09-07-v2.6",
    target_gates = target_gates, fit_gates = fit_gates,
    family_gates = family_gates, family_results = final$baseline,
    branch_family_results = final,
    target_metrics = target_metric,
    primitive_metrics = list(
      derivative = primitive_derivative, riesz_defect = primitive_riesz,
      null_space = primitive_null),
    active_eigenvalue_regularity_z = constraint$active,
    ridge_sensitivities = ridge,
    gh_movement = list(target = gh_target, influence = gh_if),
    near_optimum_movement = near_optimum_movement,
    failed_targets = failed_targets,
    failed_families = unique(c(failed_families,
      family_gates$family_id[family_gates$target_specific_failure])),
    fit_integrity = fit_integrity,
    complete = TRUE,
    released_families = family_gates$family_id[family_gates$released])
}
