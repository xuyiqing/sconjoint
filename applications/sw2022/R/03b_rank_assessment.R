#!/usr/bin/env Rscript

## Common-outer-fold rank assessment and q=2 numerical/orientation checks.
##
## This companion runner does not alter the primary or rank-sensitivity fits
## produced by 03_fit_models.R.  It adds the assessment that those full-sample
## objects cannot supply: q=0,1,2 are evaluated on exactly the same held-out
## respondent folds.  Its q=2 refinement holds the base-selected learner fixed
## in each training sample while changing only the product-GH resolution.
##
## Usage:
##   applications/bin/Rscript45 applications/sw2022/R/03b_rank_assessment.R \
##     --profile=smoke --stage=all --force=true
##
## Stages are `common`, `q2_refinement`, and `all`.

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

parse_cli <- function(args) {
  out <- list(profile = "smoke", stage = "all", force = FALSE)
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- bits[[1L]]
    if (!key %in% names(out)) stop("Unknown argument --", key)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  out
}

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path)
  invisible(path)
}

run_or_load <- function(path, overwrite, code, validator = NULL) {
  if (file.exists(path) && !overwrite) {
    value <- readRDS(path)
    if (!is.null(validator) && !isTRUE(validator(value))) {
      stop("Stale rank-assessment checkpoint: ", path,
           ". Rerun with --force=true.", call. = FALSE)
    }
    message("checkpoint: loading ", path)
    return(value)
  }
  value <- force(code)
  atomic_save_rds(value, path)
  message("checkpoint: wrote ", path)
  value
}

write_csv <- function(x, path) {
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
}

root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
app_root <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app_root)
source(file.path(app_root, "config", "analysis_config.R"), local = FALSE)
source(file.path(app_root, "config", "rank_assessment_config.R"), local = FALSE)
cli <- parse_cli(commandArgs(trailingOnly = TRUE))
if (!cli$profile %in% names(sw_analysis_config$profiles)) {
  stop("Unknown profile: ", cli$profile, call. = FALSE)
}
if (!cli$stage %in% c("common", "q2_refinement", "all")) {
  stop("--stage must be common, q2_refinement, or all.", call. = FALSE)
}
profile <- sw_analysis_config$profiles[[cli$profile]]
rank_cfg <- sw_rank_assessment_config

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The project-local library must include pkgload.", call. = FALSE)
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
ns_fun <- function(name) get(name, envir = asNamespace("sconjoint"),
                             inherits = FALSE)

prepared <- readRDS(sw_analysis_config$input$prepared)
deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z <- as.matrix(prepared[[sw_analysis_config$input$primary_Z]])
respondent_id <- as.character(prepared$respondent_id)
if (!identical(colnames(deltaX), sw_analysis_config$coefficients$order) ||
    length(unique(respondent_id)) != 1191L || nrow(deltaX) != 3573L) {
  stop("Prepared Saha--Weeks input does not match the frozen analysis basis.",
       call. = FALSE)
}

fit_dir <- file.path(sw_analysis_config$output_root, cli$profile)
output_dir <- file.path(app_root, "results", "rank_assessment", cli$profile)
table_dir <- file.path(output_dir, "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

required_primary <- file.path(fit_dir, c(
  "fit_primary_full.rds", "fit_primary_nested.rds",
  "fit_primary_assembled.rds", "fit_q0_full.rds", "fit_q2_full.rds"
))
if (!all(file.exists(required_primary))) {
  stop(
    "Run 03_fit_models.R --profile=", cli$profile,
    " --stage=all before common-fold rank assessment. Missing: ",
    paste(basename(required_primary[!file.exists(required_primary)]),
          collapse = ", "), call. = FALSE
  )
}
full_by_q <- list(
  `0` = readRDS(file.path(fit_dir, "fit_q0_full.rds")),
  `1` = readRDS(file.path(fit_dir, "fit_primary_full.rds")),
  `2` = readRDS(file.path(fit_dir, "fit_q2_full.rds"))
)
base_nested <- readRDS(file.path(fit_dir, "fit_primary_nested.rds"))
base_assembled <- readRDS(file.path(fit_dir, "fit_primary_assembled.rds"))

valid_main_stamp <- function(x, role, q) {
  z <- x$sw_application_specification
  is.list(z) && identical(z$config_version, sw_analysis_config$version) &&
    identical(z$profile, cli$profile) && identical(z$role, role) &&
    identical(z$q, as.integer(q)) &&
    identical(z$profile_specification, profile)
}
if (!valid_main_stamp(full_by_q[["0"]], "q0_full", 0L) ||
    !valid_main_stamp(full_by_q[["1"]], "primary_full", 1L) ||
    !valid_main_stamp(full_by_q[["2"]], "q2_full", 2L) ||
    !valid_main_stamp(base_nested, "primary_nested", 1L) ||
    !valid_main_stamp(base_assembled, "primary_assembled", 1L)) {
  stop("Primary fit artifacts do not match the active analysis configuration.",
       call. = FALSE)
}

rank_provenance <- function(role, dependency_paths = character()) {
  dependency_paths <- as.character(dependency_paths)
  if (length(dependency_paths) &&
      (any(!nzchar(dependency_paths)) || any(!file.exists(dependency_paths)))) {
    stop("Cannot stamp missing rank-assessment dependencies.", call. = FALSE)
  }
  config_paths <- c(
    rank_runner = script_path(),
    main_config = file.path(app_root, "config", "analysis_config.R"),
    rank_config = file.path(app_root, "config", "rank_assessment_config.R")
  )
  artifact_paths <- c(
    config_paths,
    stats::setNames(required_primary, basename(required_primary)),
    if (length(dependency_paths)) stats::setNames(
      dependency_paths, paste0("rank_dependency_", basename(dependency_paths))
    ) else character()
  )
  list(
    schema_version = "sw2022-rank-assessment-provenance-v1",
    role = role,
    profile = cli$profile,
    rank_config_version = rank_cfg$version,
    main_config_version = sw_analysis_config$version,
    prepared_path = sw_analysis_config$input$prepared,
    prepared_md5 = unname(tools::md5sum(sw_analysis_config$input$prepared)),
    artifact_paths = artifact_paths,
    artifact_md5 = stats::setNames(
      unname(tools::md5sum(artifact_paths)), names(artifact_paths)
    ),
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

common_seed <- sw_analysis_config$optimizer$seed +
  1000L * sw_analysis_config$primary$q
outer_fold_id <- base_nested$outer_fold_id
outer_fold_index <- base_nested$outer_fold_index
if (length(outer_fold_id) != nrow(deltaX) ||
    any(vapply(split(outer_fold_id, respondent_id),
               function(x) length(unique(x)) != 1L, logical(1L)))) {
  stop("The primary outer-fold artifact is malformed.", call. = FALSE)
}

rank_stamp <- function(x, role, q, nodes = NA_integer_) {
  x$sw_rank_assessment_specification <- list(
    version = rank_cfg$version, main_config_version = sw_analysis_config$version,
    profile = cli$profile, role = role, q = as.integer(q),
    nodes = as.integer(nodes), common_outer_seed = common_seed,
    outer_fold_id = outer_fold_id, profile_specification = profile
  )
  x
}
valid_rank_stamp <- function(x, role, q, nodes = NA_integer_) {
  z <- x$sw_rank_assessment_specification
  is.list(z) && identical(z$version, rank_cfg$version) &&
    identical(z$main_config_version, sw_analysis_config$version) &&
    identical(z$profile, cli$profile) && identical(z$role, role) &&
    identical(z$q, as.integer(q)) && identical(z$nodes, as.integer(nodes)) &&
    identical(z$common_outer_seed, common_seed) &&
    identical(z$outer_fold_id, outer_fold_id) &&
    identical(z$profile_specification, profile)
}

nodes_for_q <- function(q) {
  if (q == 2L) {
    unname(sw_analysis_config$rank_sensitivity$q2_nodes[[cli$profile]])
  } else profile$n_nodes
}

grid_for <- function(q, nodes, specifications = profile$grid) {
  lapply(specifications, function(spec) {
    spec$q <- NULL
    spec$integration <- "gh"
    spec$n_nodes <- as.integer(nodes)
    spec
  })
}

tune_controls <- function() list(
  allow_q_tuning = FALSE, allow_integration_tuning = FALSE,
  n_epochs = profile$n_epochs, learning_rate = profile$learning_rate,
  n_starts = profile$n_starts,
  mu_bound = sw_analysis_config$optimizer$mu_bound,
  kappa_bound = sw_analysis_config$optimizer$kappa_bound,
  a_bound = sw_analysis_config$optimizer$a_bound,
  weight_bound = sw_analysis_config$optimizer$weight_bound,
  opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
  device = sw_analysis_config$optimizer$device,
  keep_cv_fits = FALSE, verbose = FALSE
)

fit_common_nested <- function(q) {
  nodes <- nodes_for_q(q)
  args <- c(list(
    deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
    grid = grid_for(q, nodes), q = as.integer(q),
    outer_K = profile$outer_K, inner_K = profile$inner_K,
    outer_fold_id = outer_fold_id, seed = common_seed
  ), tune_controls())
  do.call(scmix_tune_outer_matrix, args)
}

assemble_nested <- function(x) {
  scmix_assemble_nested(
    x, attr_names = colnames(deltaX), z_names = colnames(Z),
    require_optimization_gate = !isTRUE(profile$diagnostic_only),
    diagnostic_only = isTRUE(profile$diagnostic_only)
  )
}

common_nested <- list(`1` = base_nested)
common_assembled <- list(`1` = base_assembled)
ensure_common_rank <- function(q, overwrite = cli$force) {
  key <- as.character(q)
  if (!is.null(common_assembled[[key]])) return(common_assembled[[key]])
  nodes <- nodes_for_q(q)
  nested_path <- file.path(output_dir,
                           paste0("fit_q", q, "_nested_common_outer.rds"))
  nested <- run_or_load(
    nested_path, overwrite,
    rank_stamp(fit_common_nested(q), "common_outer_nested", q, nodes),
    validator = function(x) valid_rank_stamp(
      x, "common_outer_nested", q, nodes)
  )
  assembled_path <- file.path(
    output_dir, paste0("fit_q", q, "_assembled_common_outer.rds"))
  assembled <- run_or_load(
    assembled_path, overwrite,
    rank_stamp(assemble_nested(nested), "common_outer_assembled", q, nodes),
    validator = function(x) valid_rank_stamp(
      x, "common_outer_assembled", q, nodes)
  )
  common_nested[[key]] <<- nested
  common_assembled[[key]] <<- assembled
  assembled
}

first <- !duplicated(respondent_id)
ids <- respondent_id[first]
meta <- prepared$respondent_meta
meta <- meta[match(ids, as.character(meta$respondent_id)), , drop = FALSE]
party <- ifelse(grepl("Republican", meta$party), "Republican",
                ifelse(grepl("Independent", meta$party), "Independent",
                       "Democrat"))
respondent_gender <- tolower(as.character(meta$respondent_gender))
party_task <- party[match(respondent_id, ids)]
respondent_gender_task <- respondent_gender[match(respondent_id, ids)]

unit_contrast <- function(...) {
  entries <- list(...)
  out <- stats::setNames(numeric(ncol(deltaX)), colnames(deltaX))
  for (entry in entries) out[[entry[[1L]]]] <-
    out[[entry[[1L]]]] + entry[[2L]]
  unname(out)
}
e <- function(name, value = 1) list(name, value)
tau_contrasts <- list(
  female_vs_male = unit_contrast(e("cand_genderMale", -1)),
  run_yes_vs_no = unit_contrast(e("cand_runYes", 1)),
  talent_assertive_vs_empathetic = unit_contrast(
    e("cand_talentEmpathetic", -1)),
  talent_collaborative_vs_empathetic = unit_contrast(
    e("cand_talentCollaborative", 1), e("cand_talentEmpathetic", -1)),
  talent_determined_vs_empathetic = unit_contrast(
    e("cand_talentDetermined.to.Succeed", 1),
    e("cand_talentEmpathetic", -1)),
  talent_good_communicator_vs_empathetic = unit_contrast(
    e("cand_talentGood.Communicator", 1), e("cand_talentEmpathetic", -1)),
  talent_hard_working_vs_empathetic = unit_contrast(
    e("cand_talentHard.Working", 1), e("cand_talentEmpathetic", -1)),
  talent_tough_negotiator_vs_empathetic = unit_contrast(
    e("cand_talentTough.Negotiator", 1), e("cand_talentEmpathetic", -1)),
  agenda_moderate_vs_very_few = unit_contrast(
    e("cand_agendaModerate.Changes", 1)),
  agenda_complete_vs_very_few = unit_contrast(
    e("cand_agendaComplete.Overhaul", 1)),
  one_child_vs_none = unit_contrast(e("cand_child1.child", 1)),
  two_children_vs_none = unit_contrast(e("cand_child2.children", 1)),
  three_children_vs_none = unit_contrast(e("cand_child3.children", 1))
)
contest_contrasts <- lapply(sw_analysis_config$qoi$contests, as.numeric)
heterogeneity_names <- c(
  "female_vs_male", "agenda_moderate_vs_very_few",
  "agenda_complete_vs_very_few"
)
sign_names <- c(
  "female_vs_male", "talent_hard_working_vs_empathetic",
  "agenda_moderate_vs_very_few", "agenda_complete_vs_very_few"
)

plugin_view <- function(tuning) list(
  respondent_id = respondent_id, Z = Z, attr_names = colnames(deltaX),
  full_fit = list(mu = tuning$refit$mu, Sigma = tuning$refit$Sigma,
                  A = tuning$refit$A, kappa = tuning$refit$kappa),
  analysis_signature = tuning$analysis_signature
)

qoi_vector <- function(tuning) {
  view <- plugin_view(tuning)
  theta <- scmix_paper_theta(view)$estimate
  out <- c(kappa = tuning$refit$kappa)
  out <- c(out, stats::setNames(vapply(tau_contrasts, function(d) {
    sum(d * theta)
  }, numeric(1L)), paste0("tau.", names(tau_contrasts))))
  for (g in c("Democrat", "Independent", "Republican")) {
    theta_g <- colMeans(tuning$refit$mu[first &
      (party_task == g), , drop = FALSE])
    out[paste0("tau.female_vs_male.party_", tolower(g))] <- -theta_g[[1L]]
  }
  for (g in c("female", "male")) {
    theta_g <- colMeans(tuning$refit$mu[first &
      (respondent_gender_task == g), , drop = FALSE])
    out[paste0("tau.female_vs_male.respondent_gender_", g)] <- -theta_g[[1L]]
  }
  for (nm in names(contest_contrasts)) {
    out[paste0("choice.contest_", nm)] <- scmix_paper_choice(
      view, contrast = contest_contrasts[[nm]], position_neutral = TRUE,
      n_nodes = sw_analysis_config$inference$choice_nodes, on_support = NA
    )$estimate
  }
  for (nm in sign_names) {
    out[paste0("sign.", nm)] <- scmix_paper_signshare(
      view, contrast = tau_contrasts[[nm]], ties = "exclude",
      variance_margin = sw_analysis_config$inference$variance_floor,
      ci = NULL
    )$estimate
  }
  for (nm in heterogeneity_names) {
    h <- scmix_paper_heterogeneity(
      view, direction = tau_contrasts[[nm]],
      total_margin = sw_analysis_config$inference$total_heterogeneity_margin
    )$estimate
    out <- c(out, stats::setNames(h,
      paste0("heterogeneity.", nm, ".", names(h))))
  }
  eig <- sort(eigen(tuning$refit$Sigma, symmetric = TRUE,
                    only.values = TRUE)$values, decreasing = TRUE)
  c(out, stats::setNames(eig, paste0("Sigma.eigen_", seq_along(eig))))
}

prediction_for <- function(fit) scmix_heldout_predictions(
  fit, task_order = prepared$task, include_counts = TRUE,
  include_adjacent = TRUE, include_repeated = TRUE
)

inner_fold_audit <- function(nested_by_q) {
  K <- length(base_nested$outer_folds)
  do.call(rbind, lapply(seq_len(K), function(k) {
    reference <- nested_by_q[["1"]]$tuning[[k]]$fold_id
    data.frame(
      outer_fold = k,
      q = as.integer(names(nested_by_q)),
      same_outer_holdout = vapply(nested_by_q, function(x) {
        identical(x$outer_fold_index, outer_fold_index)
      }, logical(1L)),
      same_inner_fold_assignment_as_q1 = vapply(nested_by_q, function(x) {
        identical(x$tuning[[k]]$fold_id, reference)
      }, logical(1L)),
      selected_candidate = vapply(nested_by_q, function(x) {
        x$tuning[[k]]$selected$name
      }, character(1L)),
      stringsAsFactors = FALSE
    )
  }))
}

common_results <- NULL
run_common_assessment <- function() {
  assembled <- lapply(rank_cfg$ranks, ensure_common_rank)
  names(assembled) <- as.character(rank_cfg$ranks)
  nested <- common_nested[as.character(rank_cfg$ranks)]
  if (!all(vapply(assembled, function(x) {
    identical(as.integer(x$fold_id), as.integer(outer_fold_index))
  }, logical(1L)))) {
    stop("Rank fits do not use the primary common outer folds.", call. = FALSE)
  }
  predictions <- lapply(assembled, prediction_for)
  score_matrix <- do.call(cbind, lapply(predictions, function(x) {
    unname(x$sequence_loglik[ids])
  }))
  colnames(score_matrix) <- paste0("q", rank_cfg$ranks)
  rownames(score_matrix) <- ids
  fold_verified <- all(vapply(predictions, function(x) {
    isTRUE(x$out_of_fold) && isTRUE(x$training_only_tuning)
  }, logical(1L)))
  score <- scmix_heldout_sequence_score(
    score_matrix, respondent_id = ids, out_of_fold = TRUE,
    training_only_tuning = fold_verified,
    provenance = paste(
      "Common primary outer folds with all rank-specific fitting and learner",
      "selection rerun inside each outer training sample. The candidate grid",
      "was adapted after a same-sample pilot, so end-to-end outcome-blind",
      "assessment is not claimed."
    ),
    analysis_signature = NULL
  )
  score_summary <- data.frame(
    q = rank_cfg$ranks, model = names(score$estimate),
    mean_complete_sequence_log_score = as.numeric(score$estimate),
    respondent_se = as.numeric(score$se),
    fold_construction_verified = fold_verified,
    candidate_grid_outcome_blind = FALSE,
    interpretation = "cross-fitted diagnostic; rank is not selected",
    stringsAsFactors = FALSE
  )
  paired <- score$paired_differences
  qoi <- lapply(rank_cfg$ranks, function(q) qoi_vector(full_by_q[[as.character(q)]]))
  names(qoi) <- as.character(rank_cfg$ranks)
  qoi_names <- names(qoi[[1L]])
  if (!all(vapply(qoi, function(x) identical(names(x), qoi_names), logical(1L)))) {
    stop("Rank QOI vectors are not conformable.", call. = FALSE)
  }
  qoi_table <- do.call(rbind, lapply(seq_along(qoi), function(j) {
    data.frame(q = rank_cfg$ranks[j], quantity = qoi_names,
               estimate = as.numeric(qoi[[j]]),
               comparison_role = paste(
                 "descriptive fixed-rank sensitivity; q remains unselected;",
                 "this runner supplies no rank-specific regular interval"
               ),
               reporting_status = ifelse(
                 rank_cfg$ranks[j] == 0L & startsWith(qoi_names, "sign."),
                 "threshold boundary: ordinary comparison withheld",
                 "descriptive plugin available"
               ),
               stringsAsFactors = FALSE)
  }))
  fold_audit <- inner_fold_audit(nested)
  write_csv(score_summary,
            file.path(table_dir, "common_outer_sequence_scores.csv"))
  write_csv(paired,
            file.path(table_dir, "common_outer_paired_score_differences.csv"))
  write_csv(data.frame(respondent_id = ids, score_matrix,
                       stringsAsFactors = FALSE),
            file.path(table_dir, "common_outer_respondent_scores.csv"))
  write_csv(qoi_table, file.path(table_dir, "rank_qoi_stability.csv"))
  write_csv(fold_audit, file.path(table_dir, "common_fold_audit.csv"))
  checkpoint_paths <- list(
    q0_nested = file.path(output_dir, "fit_q0_nested_common_outer.rds"),
    q0_assembled = file.path(output_dir,
                             "fit_q0_assembled_common_outer.rds"),
    q1_nested = file.path(fit_dir, "fit_primary_nested.rds"),
    q1_assembled = file.path(fit_dir, "fit_primary_assembled.rds"),
    q2_nested = file.path(output_dir, "fit_q2_nested_common_outer.rds"),
    q2_assembled = file.path(output_dir,
                             "fit_q2_assembled_common_outer.rds")
  )
  out <- list(
    provenance = rank_provenance(
      "common_outer_rank_assessment",
      unlist(checkpoint_paths[c("q0_nested", "q0_assembled",
                                "q2_nested", "q2_assembled")], use.names = FALSE)
    ),
    score = score, score_summary = score_summary, predictions = predictions,
    qoi = qoi, qoi_table = qoi_table, fold_audit = fold_audit,
    checkpoint_paths = checkpoint_paths,
    outer_fold_id = outer_fold_id,
    primary_q = sw_analysis_config$primary$q,
    rank_selected = FALSE,
    end_to_end_outcome_blind = FALSE,
    interpretation = rank_cfg$interpretation
  )
  class(out) <- c("sw_common_outer_rank_assessment", "list")
  atomic_save_rds(out, file.path(output_dir, "common_outer_rank_assessment.rds"))
  common_results <<- out
  out
}

rotation_matrix <- function(angle) {
  matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2L, 2L)
}

rotation_diagnostic <- function(fit, nodes) {
  angles <- rank_cfg$q2_rotation_angles
  score <- vector("list", length(angles))
  covariance_error <- numeric(length(angles))
  for (j in seq_along(angles)) {
    rotated <- fit
    R <- rotation_matrix(angles[j])
    rotated$A_folds <- lapply(fit$A_folds, `%*%`, R)
    rotated$A_computational_folds <- rotated$A_folds
    score[[j]] <- unname(prediction_for(rotated)$sequence_loglik[ids])
    covariance_error[j] <- max(vapply(seq_along(fit$A_folds), function(k) {
      max(abs(tcrossprod(rotated$A_folds[[k]]) -
                tcrossprod(fit$A_folds[[k]])))
    }, numeric(1L)))
  }
  baseline <- score[[1L]]
  data.frame(
    nodes = as.integer(nodes), angle_radians = angles,
    mean_sequence_log_score = vapply(score, mean, numeric(1L)),
    mean_difference_from_zero_angle = vapply(score, function(x) {
      mean(x - baseline)
    }, numeric(1L)),
    respondent_difference_l2 = vapply(score, function(x) {
      sqrt(mean((x - baseline)^2))
    }, numeric(1L)),
    respondent_difference_max_abs = vapply(score, function(x) {
      max(abs(x - baseline))
    }, numeric(1L)),
    covariance_max_abs_error = covariance_error,
    interpretation = paste(
      "Same fitted structural covariance, with only the q=2 loading",
      "orientation changed before finite product-GH evaluation."
    ),
    stringsAsFactors = FALSE
  )
}

fixed_spec_nested_q2 <- function(nodes, base) {
  folds <- base$outer_folds
  common_grid <- ns_fun(".sc_mixed_grid")(
    q = 2L, integration = "gh", n_nodes = as.integer(nodes),
    n_draws = 4096L, seed = common_seed,
    antithetic = TRUE, scramble = TRUE
  )
  fits <- lapply(seq_along(folds), function(k) {
    train <- base$outer_fold_index != k
    selected <- base$tuning[[k]]$selected
    args <- c(list(
      deltaX = deltaX[train, , drop = FALSE], y = y[train],
      Z = Z[train, , drop = FALSE], respondent_id = respondent_id[train],
      grid = grid_for(2L, nodes, list(selected)), q = 2L,
      K = profile$inner_K,
      seed = ns_fun(".sc_comp_seed")(common_seed, "outer", folds[[k]]),
      refit = TRUE, refit_integration_grid = common_grid
    ), tune_controls())
    do.call(scmix_tune_matrix, args)
  })
  names(fits) <- names(base$tuning)
  specifications <- lapply(fits, function(x) x$selected)
  signature <- ns_fun(".sc_analysis_signature")(
    deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
    fold_id = base$outer_fold_index,
    specification = list(
      workflow = "q2-integration-refinement-fixed-base-selected-learners",
      base_analysis_signature = base$analysis_signature,
      selected_specifications = lapply(specifications,
                                       ns_fun(".sc_comp_signature_spec"))
    )
  )
  out <- list(
    outer_fold_id = base$outer_fold_id,
    outer_fold_index = base$outer_fold_index,
    outer_folds = base$outer_folds,
    tuning = fits,
    candidate_selection_gate_by_outer_fold = vapply(fits, function(x) {
      isTRUE(x$candidate_selection_gate$pass) &&
        isTRUE(x$candidate_selection_gate$selection_eligible[x$selected_index])
    }, logical(1L)),
    specifications = specifications,
    integration_policy = list(
      varies = FALSE, diagnostic_only = FALSE,
      primary_inference_eligible = TRUE,
      interpretation = "one fixed GH resolution within this refinement"
    ),
    common_refit_integration_grid = common_grid,
    raw_data = list(deltaX = deltaX, y = y, Z = Z,
                    respondent_id = respondent_id),
    analysis_signature = signature,
    n_respondents = length(ids),
    nesting = paste(
      "base-selected learner fixed separately in every outer training set;",
      "single-candidate inner fits and selected refit exclude held-out respondents"
    ),
    assembly = "compatible with scmix_assemble_nested"
  )
  class(out) <- c("scmix_nested_tuning", "list")
  out
}

fixed_spec_full_q2 <- function(nodes, base) {
  selected <- base$selected
  q2_full_seed <- sw_analysis_config$optimizer$seed + 2000L
  args <- c(list(
    deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
    grid = grid_for(2L, nodes, list(selected)), q = 2L,
    K = profile$inner_K, seed = q2_full_seed, refit = TRUE
  ), tune_controls())
  do.call(scmix_tune_matrix, args)
}

refinement_fit <- function(nodes, base_nodes, base_nested_q2,
                           base_assembled_q2) {
  key <- as.character(nodes)
  if (nodes == base_nodes) {
    return(list(full = full_by_q[["2"]], nested = base_nested_q2,
                assembled = base_assembled_q2, reused_base = TRUE))
  }
  nested_path <- file.path(
    output_dir, paste0("q2_refinement_nodes", nodes, "_nested.rds"))
  nested <- run_or_load(
    nested_path, cli$force,
    rank_stamp(
      fixed_spec_nested_q2(nodes, base_nested_q2),
      "q2_refinement_nested", 2L, nodes
    ),
    validator = function(x) valid_rank_stamp(
      x, "q2_refinement_nested", 2L, nodes)
  )
  assembled_path <- file.path(
    output_dir, paste0("q2_refinement_nodes", nodes, "_assembled.rds"))
  assembled <- run_or_load(
    assembled_path, cli$force,
    rank_stamp(assemble_nested(nested),
               "q2_refinement_assembled", 2L, nodes),
    validator = function(x) valid_rank_stamp(
      x, "q2_refinement_assembled", 2L, nodes)
  )
  full_path <- file.path(
    output_dir, paste0("q2_refinement_nodes", nodes, "_full.rds"))
  full <- run_or_load(
    full_path, cli$force,
    rank_stamp(fixed_spec_full_q2(nodes, full_by_q[["2"]]),
               "q2_refinement_full", 2L, nodes),
    validator = function(x) valid_rank_stamp(
      x, "q2_refinement_full", 2L, nodes)
  )
  list(full = full, nested = nested, assembled = assembled,
       reused_base = FALSE)
}

run_q2_refinement <- function() {
  ## `--force` at this stage refreshes the node-ladder fits, not the common-q2
  ## prerequisite. Rebuilding that prerequisite is the responsibility of the
  ## `common` stage, which prevents an expensive duplicate fit in staged runs.
  base_assembled_q2 <- ensure_common_rank(2L, overwrite = FALSE)
  base_nested_q2 <- common_nested[["2"]]
  base_nodes <- nodes_for_q(2L)
  resolutions <- as.integer(rank_cfg$q2_refinement_nodes[[cli$profile]])
  if (!base_nodes %in% resolutions) {
    stop("q2 node ladder must include the base q=2 resolution.", call. = FALSE)
  }
  fits <- lapply(resolutions, refinement_fit,
                 base_nodes = base_nodes,
                 base_nested_q2 = base_nested_q2,
                 base_assembled_q2 = base_assembled_q2)
  names(fits) <- as.character(resolutions)
  score <- lapply(fits, function(x) {
    unname(prediction_for(x$assembled)$sequence_loglik[ids])
  })
  qoi <- lapply(fits, function(x) qoi_vector(x$full))
  reference <- which.max(resolutions)
  score_ref <- score[[reference]]
  qoi_ref <- qoi[[reference]]
  summary <- do.call(rbind, lapply(seq_along(resolutions), function(j) {
    ds <- score[[j]] - score_ref
    dq <- qoi[[j]] - qoi_ref
    data.frame(
      nodes = resolutions[j], reference_nodes = resolutions[reference],
      reused_base_fit = fits[[j]]$reused_base,
      mean_sequence_log_score = mean(score[[j]]),
      mean_sequence_log_score_difference = mean(ds),
      respondent_score_l2 = sqrt(mean(ds^2)),
      respondent_score_max_abs = max(abs(ds)),
      qoi_max_absolute_difference = max(abs(dq)),
      mean_score_gate_pass = abs(mean(ds)) <=
        rank_cfg$tolerances$mean_sequence_log_score,
      respondent_score_l2_gate_pass = sqrt(mean(ds^2)) <=
        rank_cfg$tolerances$respondent_score_l2,
      qoi_gate_pass = max(abs(dq)) <=
        rank_cfg$tolerances$qoi_max_absolute,
      stringsAsFactors = FALSE
    )
  }))
  qoi_table <- do.call(rbind, lapply(seq_along(resolutions), function(j) {
    data.frame(nodes = resolutions[j], quantity = names(qoi[[j]]),
               estimate = as.numeric(qoi[[j]]),
               difference_from_highest_resolution =
                 as.numeric(qoi[[j]] - qoi_ref),
               comparison_role = paste(
                 "numerical sensitivity with base-selected learner fixed;",
                 "no interval implied"
               ), stringsAsFactors = FALSE)
  }))
  rotation <- do.call(rbind, lapply(seq_along(resolutions), function(j) {
    rotation_diagnostic(fits[[j]]$assembled, resolutions[j])
  }))
  rotation$mean_score_gate_pass <-
    abs(rotation$mean_difference_from_zero_angle) <=
      rank_cfg$tolerances$rotation_mean_sequence_log_score
  rotation$respondent_l2_gate_pass <-
    rotation$respondent_difference_l2 <=
      rank_cfg$tolerances$rotation_respondent_score_l2
  rotation$covariance_gate_pass <-
    rotation$covariance_max_abs_error <=
      rank_cfg$tolerances$rotation_covariance_max_absolute_error
  gate <- list(
    pass = all(summary$mean_score_gate_pass) &&
      all(summary$respondent_score_l2_gate_pass) &&
      all(summary$qoi_gate_pass) &&
      all(rotation$mean_score_gate_pass) &&
      all(rotation$respondent_l2_gate_pass) &&
      all(rotation$covariance_gate_pass),
    tolerances = rank_cfg$tolerances,
    reference_nodes = resolutions[reference],
    note = paste(
      "Empirical fresh-refit/fixed-learner and rotation gates do not establish",
      "the asymptotic numerical-error rates assumed by paperps."
    )
  )
  write_csv(summary, file.path(table_dir, "q2_integration_summary.csv"))
  write_csv(qoi_table, file.path(table_dir, "q2_integration_qoi.csv"))
  write_csv(rotation, file.path(table_dir, "q2_rotation_stability.csv"))
  refinement_dependency_paths <- c(
    file.path(output_dir, "fit_q2_nested_common_outer.rds"),
    file.path(output_dir, "fit_q2_assembled_common_outer.rds"),
    unlist(lapply(setdiff(resolutions, base_nodes), function(nodes) {
      file.path(output_dir, paste0(
        "q2_refinement_nodes", nodes, c("_nested.rds", "_assembled.rds",
                                        "_full.rds")
      ))
    }), use.names = FALSE)
  )
  out <- list(
    provenance = rank_provenance(
      "q2_numerical_orientation_assessment", refinement_dependency_paths
    ),
    resolutions = resolutions, base_nodes = base_nodes,
    summary = summary, qoi = qoi, qoi_table = qoi_table,
    respondent_scores = score, rotation = rotation, gate = gate,
    fits = NULL,
    learner_policy = paste(
      "The full-sample and each outer-training sample retain their own",
      "base-q2 selected architecture and penalty at every node resolution."
    ),
    interpretation = rank_cfg$interpretation
  )
  class(out) <- c("sw_q2_numerical_orientation_assessment", "list")
  atomic_save_rds(out, file.path(output_dir,
                                 "q2_numerical_orientation_assessment.rds"))
  out
}

if (cli$stage %in% c("common", "all")) invisible(run_common_assessment())
q2_result <- NULL
if (cli$stage %in% c("q2_refinement", "all")) {
  q2_result <- run_q2_refinement()
}
if (is.null(common_results)) {
  common_path <- file.path(output_dir, "common_outer_rank_assessment.rds")
  if (file.exists(common_path)) common_results <- readRDS(common_path)
}

status <- data.frame(
  component = c(
    "common outer folds q=0,1,2", "common inner folds q=0,1,2",
    "paired held-out sequence scores", "q=2 fresh-refit node ladder",
    "q=2 finite-grid rotation check", "rank selected"
  ),
  status = c(
    if (file.exists(file.path(output_dir,
                              "common_outer_rank_assessment.rds")))
      "run_crossfitted_diagnostic" else "not_run",
    if (!is.null(common_results) &&
        all(common_results$fold_audit$same_inner_fold_assignment_as_q1))
      "verified_common" else "not_run_or_not_common",
    if (file.exists(file.path(table_dir,
                              "common_outer_paired_score_differences.csv")))
      "run_crossfitted_diagnostic" else "not_run",
    if (!is.null(q2_result)) if (all(q2_result$summary$mean_score_gate_pass) &&
        all(q2_result$summary$respondent_score_l2_gate_pass) &&
        all(q2_result$summary$qoi_gate_pass)) "run_gate_pass" else
          "run_gate_fail" else "not_run",
    if (!is.null(q2_result)) if (all(q2_result$rotation$mean_score_gate_pass) &&
        all(q2_result$rotation$respondent_l2_gate_pass) &&
        all(q2_result$rotation$covariance_gate_pass)) "run_gate_pass" else
          "run_gate_fail" else "not_run",
    "no"
  ),
  note = c(
    rank_cfg$interpretation,
    "Identical respondent assignments inside each common outer training set.",
    "One complete-sequence score per held-out respondent; comparisons are paired.",
    "Base-selected learners fixed; only product-GH resolution changes.",
    "Same covariance is evaluated after nontrivial orthogonal loading rotations.",
    "Primary q=1 remains the inherited, non-outcome-blind specification."
  ),
  stringsAsFactors = FALSE
)
write_csv(status, file.path(table_dir, "rank_assessment_status.csv"))

capture.output(sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
artifact_paths <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
artifact_paths <- artifact_paths[file.info(artifact_paths)$isdir %in% FALSE]
manifest_path <- file.path(output_dir, "artifact_manifest.csv")
artifact_paths <- setdiff(artifact_paths, manifest_path)
write_csv(data.frame(
  path = substring(artifact_paths, nchar(root) + 2L),
  bytes = file.info(artifact_paths)$size,
  md5 = unname(tools::md5sum(artifact_paths)),
  generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
), manifest_path)
message("Saha--Weeks common-fold rank assessment complete: ", output_dir)
