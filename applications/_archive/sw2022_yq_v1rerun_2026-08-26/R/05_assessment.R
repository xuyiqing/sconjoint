#!/usr/bin/env Rscript

## Saha--Weeks (2022): application-specific specification assessment.
##
## This script consumes only local prepared, fit, and inference artifacts.  It
## never reads or writes the ConjointStructural source tree.  A diagnostic is
## recorded as executed only when its required artifact and provenance are
## present; a maintained assumption or unavailable fielding probability is
## never converted into a passing assessment.

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(arg)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}

.parse_args <- function(x) {
  out <- list()
  for (arg in x) {
    if (!startsWith(arg, "--") || !grepl("=", arg, fixed = TRUE)) {
      stop("Arguments must have the form --name=value: ", arg,
           call. = FALSE)
    }
    pair <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    out[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <-
      paste(pair[-1L], collapse = "=")
  }
  out
}

.read_optional <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  readRDS(path)
}

.write_csv <- function(x, path) {
  if (is.null(x)) return(invisible(FALSE))
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(TRUE)
}

.first_existing <- function(directory, stems) {
  paths <- file.path(directory, stems)
  hit <- paths[file.exists(paths)]
  if (length(hit)) hit[[1L]] else NA_character_
}

.finite_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

.fit_core <- function(x) {
  if (is.null(x)) return(NULL)
  x$refit %||% x$full_fit %||% x
}

.fit_summary_row <- function(x, label, expected_q = NA_integer_) {
  core <- .fit_core(x)
  if (is.null(core)) {
    return(data.frame(
      specification = label, q = expected_q, artifact_available = FALSE,
      objective = NA_real_, kappa = NA_real_, smallest_active_eigenvalue = NA_real_,
      integration = NA_character_, integration_points = NA_integer_,
      note = "refit artifact not available", stringsAsFactors = FALSE
    ))
  }
  q <- core$specification$q %||% core$q %||% expected_q
  Sigma <- core$Sigma
  eig <- if (is.matrix(Sigma) && nrow(Sigma) == ncol(Sigma)) {
    sort(eigen((Sigma + t(Sigma)) / 2, symmetric = TRUE,
               only.values = TRUE)$values, decreasing = TRUE)
  } else numeric()
  q_int <- suppressWarnings(as.integer(q))
  active <- if (length(eig) && is.finite(q_int) && q_int > 0L &&
                q_int <= length(eig)) eig[[q_int]] else NA_real_
  grid <- core$integration_grid %||% core$gh
  metadata <- grid$metadata %||% list()
  method <- metadata$method %||% metadata$integration %||%
    core$specification$integration %||% NA_character_
  points <- if (is.list(grid) && !is.null(grid$U)) nrow(as.matrix(grid$U)) else
    NA_integer_
  objective <- core$optimization$objective %||% core$objective %||% NA_real_
  data.frame(
    specification = label, q = q_int, artifact_available = TRUE,
    objective = if (.finite_scalar(objective)) objective else NA_real_,
    kappa = if (.finite_scalar(core$kappa)) core$kappa else NA_real_,
    smallest_active_eigenvalue = active,
    integration = as.character(method)[1L], integration_points = points,
    note = paste(
      "Full-sample attained solution; objective values across q need not be",
      "comparable if integration/tuning differs. This is sensitivity, not rank selection."
    ), stringsAsFactors = FALSE
  )
}

.cluster_lm <- function(y, X, cluster) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  cluster <- as.character(cluster)
  if (nrow(X) != length(y) || length(cluster) != length(y) ||
      any(!is.finite(X)) || any(!is.finite(y)) || anyNA(cluster)) {
    stop("Malformed inputs to respondent-clustered linear model.",
         call. = FALSE)
  }
  fit <- stats::lm.fit(X, y)
  if (fit$rank != ncol(X)) {
    stop("The AMCE benchmark design matrix is rank deficient.", call. = FALSE)
  }
  bread <- solve(crossprod(X))
  cluster_score <- rowsum(X * as.numeric(fit$residuals), cluster,
                          reorder = FALSE)
  G <- nrow(cluster_score)
  n <- nrow(X)
  k <- ncol(X)
  correction <- if (G > 1L && n > k) (G / (G - 1)) * ((n - 1) / (n - k)) else
    NA_real_
  vcov <- correction * bread %*% crossprod(cluster_score) %*% bread
  list(coef = as.numeric(fit$coefficients), vcov = vcov,
       residual = as.numeric(fit$residuals), rank = fit$rank,
       clusters = G, n = n)
}

.amce_contrasts <- function(p, coordinate_names) {
  if (p != 13L) {
    C <- diag(p)
    rownames(C) <- paste(coordinate_names, "vs preparation reference")
    return(C)
  }
  C <- matrix(0, 13L, p)
  rownames(C) <- c(
    "Female vs Male", "Previously ran: Yes vs No",
    "Collaborative vs Empathetic", "Determined vs Empathetic",
    "Assertive vs Empathetic", "Good Communicator vs Empathetic",
    "Hard-Working vs Empathetic", "Tough Negotiator vs Empathetic",
    "Moderate Changes vs Very Few", "Complete Overhaul vs Very Few",
    "1 child vs No children", "2 children vs No children",
    "3 children vs No children"
  )
  C[1L, 1L] <- -1
  C[2L, 2L] <- 1
  C[3L, c(3L, 5L)] <- c(1, -1)
  C[4L, c(4L, 5L)] <- c(1, -1)
  C[5L, 5L] <- -1
  C[6L, c(6L, 5L)] <- c(1, -1)
  C[7L, c(7L, 5L)] <- c(1, -1)
  C[8L, c(8L, 5L)] <- c(1, -1)
  C[9L, 9L] <- 1
  C[10L, 10L] <- 1
  C[11L, 11L] <- 1
  C[12L, 12L] <- 1
  C[13L, 13L] <- 1
  colnames(C) <- coordinate_names
  C
}

.run_amce <- function(prepared) {
  dx <- as.matrix(prepared$deltaX)
  p <- ncol(dx)
  coordinate_names <- colnames(dx) %||%
    prepared$coordinate_dictionary$name %||% paste0("b", seq_len(p))
  X <- cbind(`(Candidate A intercept)` = 1, dx)
  fit <- .cluster_lm(prepared$y, X, prepared$respondent_id)
  C <- .amce_contrasts(p, coordinate_names)
  b <- fit$coef[-1L]
  V <- fit$vcov[-1L, -1L, drop = FALSE]
  estimate <- as.numeric(C %*% b)
  variance <- diag(C %*% V %*% t(C))
  se <- sqrt(pmax(variance, 0))
  data.frame(
    contrast = rownames(C), estimate = estimate, se = se,
    conf_low = estimate - stats::qnorm(0.975) * se,
    conf_high = estimate + stats::qnorm(0.975) * se,
    n_tasks = fit$n, n_respondents = fit$clusters,
    estimand = paste(
      "Respondent-clustered, difference-coded linear-probability AMCE-style",
      "marginal choice effect; this is not a mixed-logit preference coefficient."
    ),
    design_condition = paste(
      "Conditional on the advertised independent randomized profile assignment;",
      "the fielded protocol probabilities were not machine-verified."
    ), stringsAsFactors = FALSE
  )
}

.party_by_task <- function(prepared) {
  meta <- prepared$respondent_meta
  if (!is.data.frame(meta) ||
      !all(c("respondent_id", "party") %in% names(meta))) return(NULL)
  as.character(meta$party[match(as.character(prepared$respondent_id),
                                as.character(meta$respondent_id))])
}

.respondent_field_by_task <- function(prepared, field) {
  meta <- prepared$respondent_meta
  if (!is.data.frame(meta) ||
      !all(c("respondent_id", field) %in% names(meta))) return(NULL)
  as.character(meta[[field]][match(as.character(prepared$respondent_id),
                                   as.character(meta$respondent_id))])
}

.serial_summary <- function(task_predictions) {
  d <- task_predictions
  d$residual <- d$observed - d$predicted
  d <- d[order(d$respondent_id, d$task_order, method = "radix"), , drop = FALSE]
  parts <- split(seq_len(nrow(d)), d$respondent_id)
  pairs <- do.call(rbind, lapply(parts, function(ii) {
    if (length(ii) < 2L) return(NULL)
    data.frame(
      respondent_id = d$respondent_id[ii[-1L]],
      task_order = d$task_order[ii[-1L]],
      lag_observed = d$observed[ii[-length(ii)]],
      observed = d$observed[ii[-1L]],
      lag_residual = d$residual[ii[-length(ii)]],
      residual = d$residual[ii[-1L]], stringsAsFactors = FALSE
    )
  }))
  if (is.null(pairs) || nrow(pairs) < 2L) return(NULL)
  serial_fit <- .cluster_lm(
    pairs$residual, cbind(intercept = 1, lag_residual = pairs$lag_residual),
    pairs$respondent_id
  )
  order_fit <- .cluster_lm(
    d$residual, cbind(intercept = 1, task_order = d$task_order),
    d$respondent_id
  )
  data.frame(
    diagnostic = c("adjacent residual correlation",
                   "adjacent residual slope", "task-order residual slope"),
    estimate = c(stats::cor(pairs$lag_residual, pairs$residual),
                 serial_fit$coef[[2L]], order_fit$coef[[2L]]),
    se = c(NA_real_, sqrt(serial_fit$vcov[2L, 2L]),
           sqrt(order_fit$vcov[2L, 2L])),
    n_rows = c(nrow(pairs), nrow(pairs), nrow(d)),
    interpretation = paste(
      "Descriptive held-out residual diagnostic; it can reveal dependence or",
      "order-related lack of fit but cannot establish shock independence."
    ), stringsAsFactors = FALSE
  )
}

.completion_object <- function(prepared, completion_fun, design_audit = NULL) {
  meta <- design_audit$completion$status %||% prepared$respondent_meta
  if (!is.data.frame(meta) || !"respondent_id" %in% names(meta)) return(NULL)
  completed_name <- intersect(
    c("completed_tasks", "T_i", "n_tasks", "tasks"), names(meta)
  )
  if (!length(completed_name)) return(NULL)
  completed <- as.numeric(meta[[completed_name[[1L]]]])
  if (any(!is.finite(completed)) || length(unique(completed)) < 2L) return(NULL)
  predictor_names <- intersect(
    c("party", "respondent_gender", "age", "finished", "progress",
      "all_primary_demographics_missing", "primary_demographics_valid",
      "final_analysis_sample"), names(meta)
  )
  pattern <- if (all(c("finished", "progress") %in% names(meta))) {
    paste0(ifelse(meta$finished, "finished", "unfinished"),
           "_progress", meta$progress, "_T", completed)
  } else paste0("T", completed)
  completion_fun(
    completed_tasks = completed,
    predictors = meta[, predictor_names, drop = FALSE],
    completion_pattern = pattern, respondent_id = meta$respondent_id
  )
}

.run_amce_by_group <- function(prepared, group, group_name) {
  if (is.null(group) || length(group) != length(prepared$y) || anyNA(group)) {
    return(NULL)
  }
  out <- lapply(unique(as.character(group)), function(level) {
    keep <- as.character(group) == level
    sub <- prepared
    sub$deltaX <- prepared$deltaX[keep, , drop = FALSE]
    sub$y <- prepared$y[keep]
    sub$respondent_id <- prepared$respondent_id[keep]
    tab <- .run_amce(sub)
    tab$group_variable <- group_name
    tab$group <- level
    tab
  })
  do.call(rbind, out)
}

.flatten_information <- function(inference) {
  details <- inference$fold_details
  if (!is.list(details) || !length(details)) return(NULL)
  do.call(rbind, lapply(seq_along(details), function(k) {
    eig <- details[[k]]$information_eigenvalues
    if (!is.numeric(eig) || !length(eig)) return(NULL)
    data.frame(
      fold = k, eigen_index = seq_along(eig), eigenvalue = eig,
      structural_min = details[[k]]$information_structural_min %||% min(eig),
      structural_norm = details[[k]]$structural_norm %||% NA_character_,
      source = paste(
        "Complete respondent-sequence scores in the finite-sieve structural",
        "norm used by the diagnostic inference routine."
      ), stringsAsFactors = FALSE
    )
  }))
}

.profile_status <- function(profiles) {
  required <- c(
    "kappa", "gender average preference",
    "Democrat-Republican gender difference",
    "smallest active covariance eigenvalue", "complete-overhaul contest",
    "moderate-changes contest", "very-few-changes contest"
  )
  objects <- if (inherits(profiles, "scmix_profile_sequence_likelihood")) {
    list(`unnamed supplied profile` = profiles)
  } else if (is.list(profiles)) profiles else list()
  data.frame(
    direction = required,
    status = vapply(required, function(nm) {
      hit <- objects[[nm]]
      if (inherits(hit, "scmix_profile_sequence_likelihood") &&
          isTRUE(hit$verified_profile)) "run_descriptive_profile" else "not_run"
    }, character(1L)),
    note = paste(
      "A profile requires nuisance reoptimization at every grid point with",
      "sieve tuning fixed; a likelihood slice is not relabeled as a profile."
    ), stringsAsFactors = FALSE
  )
}

.collect_quantities <- function(x, path = "qoi", depth = 0L) {
  if (inherits(x, "scmix_paper_quantity")) {
    return(stats::setNames(list(x), path))
  }
  if (!is.list(x) || depth >= 6L || !length(x)) return(list())
  nm <- names(x)
  if (is.null(nm)) nm <- paste0("item", seq_along(x))
  out <- list()
  for (j in seq_along(x)) {
    if (is.environment(x[[j]]) || is.function(x[[j]])) next
    child <- .collect_quantities(x[[j]], paste(path, nm[[j]], sep = "/"),
                                 depth + 1L)
    out <- c(out, child)
  }
  out
}

.quantity_tables <- function(qoi, inference) {
  objects <- .collect_quantities(qoi)
  if (!length(objects)) return(list(estimates = data.frame(), gates = data.frame()))
  estimates <- do.call(rbind, lapply(names(objects), function(nm) {
    value <- unlist(objects[[nm]]$estimate, recursive = TRUE, use.names = TRUE)
    value <- value[is.numeric(value)]
    if (!length(value)) return(NULL)
    if (is.null(names(value)) || any(!nzchar(names(value)))) {
      names(value) <- paste0("value_", seq_along(value))
    }
    data.frame(
      quantity_id = nm, quantity = objects[[nm]]$quantity,
      component = names(value), estimate = as.numeric(value),
      source = paste0("full-sample structural plug-in; ",
                      "posterior respondent summaries used: no"),
      regular_interval = "not linked by this generic exporter",
      stringsAsFactors = FALSE
    )
  }))
  inference_status <- inference$status %||% "not supplied"
  gates <- do.call(rbind, lapply(names(objects), function(nm) {
    object <- objects[[nm]]
    gate <- object$gate
    gate_state <- if (is.null(gate)) "not_applicable" else
      if (isTRUE(gate$pass)) "plugin_margin_met" else
        if (identical(gate$pass, FALSE)) "plugin_margin_not_met" else
          "margin_not_prespecified"
    support <- object$details$support %||% "support not audited"
    data.frame(
      quantity_id = nm, quantity = object$quantity,
      plugin_gate_status = gate_state,
      gate_value = gate$value %||% NA_real_, gate_margin = gate$margin %||% NA_real_,
      gate_reason = gate$reason %||% "no quantity-specific plug-in gate",
      support = support, inference_status = inference_status,
      regular_reporting = if (inference_status %in%
        c("available", "conditional_available"))
        "requires exact target linkage and all global gates" else "withheld",
      majority_claim = "withheld unless a linked regular interval excludes 1/2",
      stringsAsFactors = FALSE
    )
  }))
  list(estimates = estimates, gates = gates)
}

.status_table <- function(q_summary, heldout_state, optimization, numerical,
                          completion, information, inference, interaction_file,
                          z19_file, sensitivity_file) {
  q_available <- sum(q_summary$artifact_available) >= 2L
  opt_available <- inherits(optimization, "scmix_optimization_audit")
  num_available <- inherits(numerical, "scmix_integration_refinement") ||
    inherits(numerical, "scmix_numerical_gate")
  rows <- list(
    c("rank q=0,1,2 stability", if (q_available) "run_descriptive" else "not_run",
      "Alternative ranks are sensitivities; primary q=1 is not reselected."),
    c("integration resolution and replication", if (num_available)
      "run_descriptive" else "not_run",
      "A fixed finite grid is a maintained approximation until fresh-refit refinement is run."),
    c("multiple starts and attained optimization", if (opt_available)
      "run_diagnostic" else "not_run",
      "Attained gradients/objective dispersion do not bound the global optimum gap."),
    c("skewed residual distribution", "maintained_assumption; alternative_refit_not_run",
      "Primary fit retains a centered unit-covariance normal factor."),
    c("bimodal residual distribution", "maintained_assumption; alternative_refit_not_run",
      "Primary fit retains a centered unit-covariance normal factor."),
    c("heavy-tailed residual distribution", "maintained_assumption; alternative_refit_not_run",
      "Primary fit retains a finite-covariance normal factor."),
    c("covariance varying by party/Z", "maintained_assumption; alternative_refit_not_run",
      "Primary fit uses one common residual covariance."),
    c("task order, fatigue, and learning", heldout_state,
      "Held-out task-order residual checks are diagnostic, not a causal fatigue model."),
    c("serial shock dependence", heldout_state,
      "Adjacent joint calibration and residual association cannot verify independence."),
    c("position/profile swap", if (heldout_state == "not_run") "not_run" else
      "primary_kappa_run; swap_refit_not_run",
      "The primary utility includes kappa; a full profile-swap refit remains separate."),
    c("random response scale", "maintained_fixed_logit_scale; refit_not_run",
      "Scale heterogeneity is not identified by merely rescaling fitted coefficients."),
    c("completion/attrition", if (inherits(completion,
      "scmix_completion_assessment")) "run_descriptive" else
      "not_run_for_raw_completion_universe",
      "The primary estimand is the 1,191 three-task complete-case respondents."),
    c("post-conjoint 19-Z moderators", if (file.exists(z19_file))
      "artifact_supplied; review_before_use" else "not_run",
      "Four post-conjoint fields are excluded from the primary moderator set."),
    c("Male x prior-run interaction", if (file.exists(interaction_file))
      "artifact_supplied; review_before_use" else "not_run",
      "Targeted sensitivity needed to recover the original gender-by-ambition focus."),
    c("application structural-sensitivity battery", if (file.exists(sensitivity_file))
      "artifact_supplied; review_component_status" else "not_run",
      "Alternative models remain sensitivities and inherit no normal-model identification result."),
    c("finite-sieve local information", if (is.data.frame(information) &&
      nrow(information)) "run_diagnostic" else "not_run",
      "Small eigenvalues diagnose weak local information; no global identification claim."),
    c("regular respondent-level inference", inference$status %||% "not_run",
      inference$reason %||%
        "Any available interval remains conditional on documented high-level assumptions.")
  )
  data.frame(
    component = vapply(rows, `[[`, character(1L), 1L),
    status = vapply(rows, `[[`, character(1L), 2L),
    note = vapply(rows, `[[`, character(1L), 3L),
    maintained_assumption_verified = FALSE,
    stringsAsFactors = FALSE
  )
}

.main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- .parse_args(args)
  script <- .script_file()
  project <- normalizePath(file.path(dirname(script), "../../.."),
                           mustWork = TRUE)
  app <- file.path(project, "applications", "sw2022")
  profile <- opt$profile %||% Sys.getenv("SCONJOINT_SW_PROFILE", "production")
  prep_path <- opt$prep %||% file.path(app, "results", "prep_analysis_data.rds")
  fit_dir <- opt$fit_dir %||%
    file.path(app, "results", "mixed_logit", profile)
  out_dir <- opt$out_dir %||%
    file.path(app, "results", "assessment", profile)
  table_dir <- file.path(out_dir, "tables")
  object_dir <- file.path(out_dir, "objects")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(object_dir, recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(prep_path)) {
    stop("Prepared input not found: ", prep_path,
         ". Run 01_prepare_data.R first.", call. = FALSE)
  }
  if (!requireNamespace("sconjoint", quietly = TRUE)) {
    stop("Install this checkout into the project-local R library before running assessment.",
         call. = FALSE)
  }
  fun <- function(name) getExportedValue("sconjoint", name)
  prepared <- readRDS(prep_path)
  design_audit_path <- file.path(app, "results", "design_completion_audit.rds")
  design_task_path <- file.path(app, "results", "design_task_metadata.rds")
  design_audit <- .read_optional(design_audit_path)
  design_task <- .read_optional(design_task_path)

  paths <- c(
    primary_full = .first_existing(fit_dir, "fit_primary_full.rds"),
    primary_assembled = .first_existing(fit_dir, "fit_primary_assembled.rds"),
    q0_full = .first_existing(fit_dir, "fit_q0_full.rds"),
    q2_full = .first_existing(fit_dir, "fit_q2_full.rds"),
    qoi = .first_existing(fit_dir, "qoi_plugin.rds"),
    inference = .first_existing(fit_dir, "inference_diagnostic.rds"),
    inference_transforms = .first_existing(fit_dir, "inference_transforms.rds"),
    optimization = .first_existing(
      fit_dir, c("optimization_primary_nested.rds",
                 "optimization_primary_full.rds")),
    numerical = .first_existing(
      fit_dir, c("integration_refinement.rds", "numerical_refinement.rds")),
    profiles = .first_existing(
      fit_dir, c("profile_likelihoods.rds", "profiles.rds")),
    sensitivity = .first_existing(
      fit_dir, c("structural_sensitivity.rds", "sensitivity.rds")),
    interaction = .first_existing(
      fit_dir, c("fit_male_run_interaction.rds", "sensitivity_male_run.rds")),
    z19 = .first_existing(
      fit_dir, c("fit_z19_sensitivity.rds", "sensitivity_z19.rds"))
  )
  path_or_missing <- function(nm) if (is.na(paths[[nm]])) "" else paths[[nm]]
  primary_full <- .read_optional(path_or_missing("primary_full"))
  assembled <- .read_optional(path_or_missing("primary_assembled"))
  q0 <- .read_optional(path_or_missing("q0_full"))
  q2 <- .read_optional(path_or_missing("q2_full"))
  qoi <- .read_optional(path_or_missing("qoi"))
  inference <- .read_optional(path_or_missing("inference")) %||% list(
    status = "not_run", reason = "inference artifact not available",
    inference_available = FALSE
  )
  optimization <- .read_optional(path_or_missing("optimization"))
  numerical <- .read_optional(path_or_missing("numerical"))
  profiles <- .read_optional(path_or_missing("profiles"))

  q_summary <- rbind(
    .fit_summary_row(q0, "q=0 sensitivity", 0L),
    .fit_summary_row(primary_full, "q=1 primary", 1L),
    .fit_summary_row(q2, "q=2 sensitivity", 2L)
  )
  .write_csv(q_summary, file.path(table_dir, "rank_q_stability.csv"))

  optimization_state <- "not_run"
  if (inherits(optimization, "scmix_optimization_audit")) {
    .write_csv(optimization$summary,
               file.path(table_dir, "optimization_summary.csv"))
    .write_csv(optimization$starts,
               file.path(table_dir, "optimization_starts.csv"))
    optimization_state <- "run_diagnostic"
  }

  integration_table <- .fit_summary_row(primary_full, "primary finite grid", 1L)
  integration_table$refinement_artifact <- !is.null(numerical)
  integration_table$interpretation <- paste(
    "A finite number of nodes/draws is not treated as exact likelihood evaluation;",
    "fresh-refit refinement is required for an empirical numerical-stability claim."
  )
  .write_csv(integration_table,
             file.path(table_dir, "integration_primary_grid.csv"))
  if (inherits(numerical, "scmix_integration_refinement")) {
    .write_csv(numerical$checks,
               file.path(table_dir, "integration_refinement_checks.csv"))
    if (inherits(numerical$gate, "scmix_numerical_gate")) {
      .write_csv(numerical$gate$comparison,
                 file.path(table_dir, "integration_refinement_gate.csv"))
    }
  } else if (inherits(numerical, "scmix_numerical_gate")) {
    .write_csv(numerical$comparison,
               file.path(table_dir, "integration_refinement_gate.csv"))
  }

  prediction_objects <- NULL
  prediction_error <- NULL
  if (!is.null(assembled)) {
    prediction_objects <- tryCatch({
      predictions <- fun("scmix_heldout_predictions")(
        assembled, task_order = prepared$task,
        include_counts = TRUE, include_adjacent = TRUE,
        include_repeated = TRUE
      )
      party <- .party_by_task(prepared)
      design_cell <- if (is.data.frame(design_task) &&
                         "design_cell" %in% names(design_task) &&
                         nrow(design_task) == nrow(prepared$deltaX)) {
        design_task$design_cell
      } else {
        paste0("nonzero_coordinates_",
               rowSums(abs(prepared$deltaX) > 1e-12))
      }
      fun("scmix_prediction_assessment")(
        predictions, design_cell = design_cell,
        respondent_group = party
      )
    }, error = function(e) {
      prediction_error <<- conditionMessage(e)
      NULL
    })
  }
  heldout_state <- "not_run"
  serial <- NULL
  if (!is.null(prediction_objects)) {
    heldout_state <- if (isTRUE(prediction_objects$score$verified_heldout) &&
      isTRUE(prediction_objects$calibration$verified_heldout))
      "run_verified_heldout" else "run_diagnostic_unverified"
    .write_csv(data.frame(
      model = names(prediction_objects$score$estimate),
      mean_complete_sequence_log_score =
        as.numeric(prediction_objects$score$estimate),
      respondent_se = as.numeric(prediction_objects$score$se),
      verified_heldout = prediction_objects$score$verified_heldout,
      provenance = prediction_objects$score$provenance,
      stringsAsFactors = FALSE
    ), file.path(table_dir, "heldout_sequence_score.csv"))
    .write_csv(prediction_objects$calibration$marginal,
               file.path(table_dir, "calibration_marginal.csv"))
    .write_csv(prediction_objects$calibration$joint,
               file.path(table_dir, "calibration_joint.csv"))
    .write_csv(prediction_objects$calibration$response_count,
               file.path(table_dir, "calibration_response_count.csv"))
    .write_csv(prediction_objects$predictions$task,
               file.path(table_dir, "heldout_task_predictions.csv"))
    serial <- .serial_summary(prediction_objects$predictions$task)
    .write_csv(serial, file.path(table_dir, "order_serial_diagnostics.csv"))
    position <- data.frame(
      diagnostic = c("candidate-A observed choice rate",
                     "candidate-A held-out predicted rate",
                     "candidate-A calibration gap", "full-fit kappa"),
      estimate = c(
        mean(prediction_objects$predictions$task$observed),
        mean(prediction_objects$predictions$task$predicted),
        mean(prediction_objects$predictions$task$observed -
               prediction_objects$predictions$task$predicted),
        .fit_core(primary_full)$kappa %||% NA_real_
      ),
      interpretation = c(
        rep("Descriptive position/alternative-side assessment.", 3L),
        "Estimated position/alternative intercept; not a profile-swap sensitivity refit."
      ), stringsAsFactors = FALSE
    )
    .write_csv(position, file.path(table_dir, "position_diagnostics.csv"))
  }

  amce <- .run_amce(prepared)
  .write_csv(amce, file.path(table_dir, "design_amce_lpm.csv"))
  amce_party <- .run_amce_by_group(
    prepared, .respondent_field_by_task(prepared, "party"), "party"
  )
  amce_gender <- .run_amce_by_group(
    prepared, .respondent_field_by_task(prepared, "respondent_gender"),
    "respondent_gender"
  )
  .write_csv(rbind(amce_party, amce_gender),
             file.path(table_dir, "design_amce_lpm_by_group.csv"))
  design_status <- data.frame(
    component = c(
      "realized support/rank audit", "exact ordered-contrast HT benchmark",
      "marginal AMCE-style benchmark", "structural-design discrepancy"
    ),
    status = c(
      if (file.exists(file.path(app, "tables", "design_rank_summary.csv")))
        "run_realized_design_algebra" else "not_run",
      "protocol_unavailable", "run_conditional_on_advertised_randomization",
      "not_run"
    ),
    reason = c(
      paste(
        "Realized/design-algebra ranks do not substitute for the unavailable",
        "machine-readable fielding protocol."
      ),
      paste(
        "The respondent exposure probabilities bar(pi)_i(d), assignment",
        "restrictions, and completion strata were not recovered from a fielded instrument."
      ),
      paste(
        "Respondent-clustered marginal probability benchmark; a different",
        "estimand and scale from structural mixed-logit preference coefficients."
      ),
      "No exact structural-versus-HT discrepancy is formed without protocol probabilities."
    ), maintained_assumption_verified = FALSE,
    stringsAsFactors = FALSE
  )
  .write_csv(design_status, file.path(table_dir, "design_assessment_status.csv"))

  completion <- tryCatch(
    .completion_object(prepared, fun("scmix_completion_diagnostics"),
                       design_audit),
    error = function(e) NULL
  )
  completion_note <- if (inherits(completion, "scmix_completion_assessment")) {
    .write_csv(as.data.frame(as.list(completion$summary)),
               file.path(table_dir, "completion_summary.csv"))
    .write_csv(completion$associations,
               file.path(table_dir, "completion_associations.csv"))
    .write_csv(completion$completion_patterns,
               file.path(table_dir, "completion_patterns.csv"))
    "Raw completion universe contained variation and was assessed descriptively."
  } else {
    paste(
      "The prepared structural sample contains only the 1,191 three-task",
      "complete cases; raw-universe completion diagnostics were not run here."
    )
  }
  .write_csv(data.frame(
    estimand = prepared$estimand,
    completion_assessment = if (inherits(completion,
      "scmix_completion_assessment")) "run_descriptive" else
      "not_run_for_raw_completion_universe",
    note = completion_note,
    noninformative_completion_verified = FALSE,
    stringsAsFactors = FALSE
  ), file.path(table_dir, "completion_status.csv"))

  information <- .flatten_information(inference)
  .write_csv(information, file.path(table_dir, "information_eigenvalues.csv"))
  profile_status <- .profile_status(profiles)
  .write_csv(profile_status,
             file.path(table_dir, "profile_likelihood_status.csv"))

  quantity_tables <- .quantity_tables(qoi, inference)
  .write_csv(quantity_tables$estimates,
             file.path(table_dir, "structural_plugin_quantities.csv"))
  .write_csv(quantity_tables$gates,
             file.path(table_dir, "quantity_reporting_gates.csv"))

  interaction_path <- path_or_missing("interaction")
  z19_path <- path_or_missing("z19")
  sensitivity_path <- path_or_missing("sensitivity")
  status <- .status_table(
    q_summary = q_summary, heldout_state = heldout_state,
    optimization = optimization, numerical = numerical,
    completion = completion, information = information,
    inference = inference, interaction_file = interaction_path,
    z19_file = z19_path, sensitivity_file = sensitivity_path
  )
  .write_csv(status, file.path(table_dir, "assessment_component_status.csv"))

  claims <- data.frame(
    claim = c(
      "primary sample", "primary factor rank", "held-out predictive fit",
      "normal residual shape", "common residual covariance",
      "independent logit shocks", "noninformative completion",
      "exact design-based ordered-contrast benchmark", "regular intervals",
      "majority/sign-share statements", "off-support contests",
      "Male x prior-run interaction"
    ),
    manuscript_state = c(
      "report with sample-flow caveat", "q=1 fixed primary; q=0/q=2 sensitivity",
      heldout_state, "maintained; not verified", "maintained; not verified",
      "maintained; not verified", "maintained; not verified",
      "withheld: protocol probabilities unavailable",
      if (isTRUE(inference$inference_available))
        "conditional only; review target-specific gates" else "withheld",
      "withheld unless linked regular CI excludes 1/2",
      "label explicitly as structural extrapolation",
      if (file.exists(interaction_path)) "sensitivity artifact supplied" else
        "not run"
    ),
    evidence_does_not_establish_assumption = TRUE,
    stringsAsFactors = FALSE
  )
  .write_csv(claims, file.path(table_dir, "manuscript_claims_ledger.csv"))

  artifact_paths <- c(prep = prep_path,
                      design_completion_audit = design_audit_path,
                      design_task_metadata = design_task_path, paths)
  artifact_exists <- !is.na(artifact_paths) & file.exists(artifact_paths)
  input_manifest <- data.frame(
    artifact = names(artifact_paths), path = unname(artifact_paths),
    exists = artifact_exists,
    bytes = ifelse(artifact_exists, file.info(artifact_paths)$size, NA_real_),
    md5 = NA_character_, stringsAsFactors = FALSE
  )
  input_manifest$md5[artifact_exists] <-
    unname(tools::md5sum(artifact_paths[artifact_exists]))
  .write_csv(input_manifest, file.path(out_dir, "input_manifest.csv"))

  assessment <- list(
    schema_version = "sw2022-assessment-v1",
    application = prepared$application,
    profile = profile,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    estimand = prepared$estimand,
    sample = prepared$sample,
    coordinate_dictionary = prepared$coordinate_dictionary,
    design = list(status = design_status, amce = amce,
                  amce_by_group = rbind(amce_party, amce_gender),
                  audit = design_audit,
                  exact_protocol_benchmark = NULL),
    prediction = prediction_objects,
    prediction_error = prediction_error,
    serial_order = serial,
    completion = completion,
    q_stability = q_summary,
    optimization = optimization,
    numerical = numerical,
    local_information = information,
    profiles = profiles,
    profile_status = profile_status,
    qoi = qoi,
    inference = inference,
    inference_transforms = .read_optional(path_or_missing("inference_transforms")),
    quantity_tables = quantity_tables,
    component_status = status,
    claims_ledger = claims,
    input_manifest = input_manifest,
    maintained_assumptions_verified = FALSE,
    disclaimer = paste(
      "These assessments can reveal poor fit or sensitivity. They cannot",
      "establish normality, common covariance, noninformative completion,",
      "or independent logit shocks. Missing artifacts never pass by default."
    )
  )
  class(assessment) <- c("sw2022_application_assessment", "list")
  saveRDS(assessment, file.path(object_dir, "assessment_bundle.rds"), version = 3)
  capture.output(utils::sessionInfo(),
                 file = file.path(out_dir, "sessionInfo.txt"))

  cat(sprintf(
    paste0("Saha--Weeks assessment (%s): %s; AMCE rows=%d; ",
           "formal inference=%s.\n"),
    profile, heldout_state, nrow(amce), inference$status %||% "not_run"
  ))
  invisible(assessment)
}

if (sys.nframe() == 0L) .main()
