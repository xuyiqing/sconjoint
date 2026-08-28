#!/usr/bin/env Rscript

## Fit the low-rank normal mixed-logit models for Saha--Weeks.
##
## Usage from the package root:
##   applications/bin/Rscript45 applications/sw2022/R/03_fit_models.R \
##     --profile=pilot --stage=all
##   applications/bin/Rscript45 applications/sw2022/R/03_fit_models.R \
##     --profile=production --stage=primary
##
## Stages are `primary`, `sensitivity`, or `all`. Existing checkpoints are
## loaded unless `--force=true` is supplied. Source application data are never
## modified; all files are written beneath applications/sw2022/results/.

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

parse_cli <- function(args) {
  out <- list(profile = "pilot", stage = "all", force = FALSE)
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- bits[[1L]]
    value <- paste(bits[-1L], collapse = "=")
    if (!key %in% names(out)) stop("Unknown argument --", key)
    out[[key]] <- value
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  out
}

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  ## Live Torch modules contain session-local external pointers. New fits carry
  ## portable architecture/state bundles, so disk checkpoints retain those and
  ## all numeric outputs while omitting misleading dead module objects.
  saveRDS(scmix_portable_copy(object), tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path)
  invisible(path)
}

run_or_load <- function(path, overwrite, code, validator = NULL) {
  if (file.exists(path) && !overwrite) {
    message("checkpoint: loading ", path)
    value <- readRDS(path)
    if (!is.null(validator) && !isTRUE(validator(value))) {
      stop("Checkpoint was created under a different analysis configuration: ",
           path, ". Rerun with --force=true; stale fits are never reused.")
    }
    return(value)
  }
  value <- force(code)
  atomic_save_rds(value, path)
  message("checkpoint: wrote ", path)
  value
}

root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
app_root <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app_root)
source(file.path(app_root, "config", "analysis_config.R"), local = FALSE)

cli <- parse_cli(commandArgs(trailingOnly = TRUE))
if (!cli$profile %in% names(sw_analysis_config$profiles)) {
  stop("--profile must be one of: ",
       paste(names(sw_analysis_config$profiles), collapse = ", "))
}
if (!cli$stage %in% c("primary", "sensitivity", "all")) {
  stop("--stage must be primary, sensitivity, or all.")
}
profile <- sw_analysis_config$profiles[[cli$profile]]

stamp <- function(x, role, q = sw_analysis_config$primary$q) {
  x$sw_application_specification <- list(
    config_version = sw_analysis_config$version,
    profile = cli$profile, role = role, q = as.integer(q),
    profile_specification = profile,
    learner_grid_provenance = sw_analysis_config$network$grid_provenance
  )
  x
}

valid_stamp <- function(x, role, q = sw_analysis_config$primary$q) {
  z <- x$sw_application_specification
  is.list(z) && identical(z$config_version, sw_analysis_config$version) &&
    identical(z$profile, cli$profile) && identical(z$role, role) &&
    identical(z$q, as.integer(q)) &&
    identical(z$profile_specification, profile)
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The local analysis library must include pkgload.")
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

prepared <- readRDS(sw_analysis_config$input$prepared)
required <- c("deltaX", "y", sw_analysis_config$input$primary_Z,
              "respondent_id", "respondent_meta")
missing_fields <- setdiff(required, names(prepared))
if (length(missing_fields)) {
  stop("Prepared analysis object is missing: ",
       paste(missing_fields, collapse = ", "))
}

deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z <- as.matrix(prepared[[sw_analysis_config$input$primary_Z]])
respondent_id <- as.character(prepared$respondent_id)
expected_names <- sw_analysis_config$coefficients$order
if (!identical(colnames(deltaX), expected_names)) {
  stop("The 13 DeltaX coordinates are not in the frozen likelihood basis.\n",
       "Expected: ", paste(expected_names, collapse = ", "), "\nObserved: ",
       paste(colnames(deltaX), collapse = ", "))
}
if (nrow(deltaX) != length(y) || nrow(deltaX) != nrow(Z) ||
    nrow(deltaX) != length(respondent_id) || any(!is.finite(deltaX)) ||
    any(!is.finite(Z)) || anyNA(respondent_id) || !all(y %in% c(0, 1))) {
  stop("Prepared matrix dimensions, values, or binary outcomes are invalid.")
}
if (length(unique(respondent_id)) != 1191L || nrow(deltaX) != 3573L) {
  stop("Primary complete-case sample must contain 1,191 respondents and 3,573 tasks.")
}
if (any(vapply(split(seq_len(nrow(Z)), respondent_id), function(ii) {
  max(abs(sweep(Z[ii, , drop = FALSE], 2L, Z[ii[1L], ], `-`))) > 1e-12
}, logical(1L)))) {
  stop("Primary moderators must be constant within respondent.")
}

output_dir <- file.path(sw_analysis_config$output_root, cli$profile)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

profile_grid <- function(q) {
  grid <- profile$grid
  nodes <- profile$n_nodes
  if (q == 2L) {
    nodes <- unname(sw_analysis_config$rank_sensitivity$q2_nodes[[cli$profile]])
  }
  lapply(grid, function(spec) {
    spec$q <- NULL
    spec$integration <- "gh"
    spec$n_nodes <- as.integer(nodes)
    spec
  })
}

common_fit_args <- function(q) list(
  deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
  grid = profile_grid(q), q = as.integer(q),
  allow_q_tuning = FALSE, allow_integration_tuning = FALSE,
  n_epochs = profile$n_epochs,
  learning_rate = profile$learning_rate,
  n_starts = profile$n_starts,
  mu_bound = sw_analysis_config$optimizer$mu_bound,
  kappa_bound = sw_analysis_config$optimizer$kappa_bound,
  a_bound = sw_analysis_config$optimizer$a_bound,
  weight_bound = sw_analysis_config$optimizer$weight_bound,
  opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
  seed = sw_analysis_config$optimizer$seed + 1000L * as.integer(q),
  device = sw_analysis_config$optimizer$device,
  keep_cv_fits = FALSE, verbose = FALSE
)

fit_full_q <- function(q) {
  args <- common_fit_args(q)
  args$K <- profile$inner_K
  args$refit <- TRUE
  do.call(scmix_tune_matrix, args)
}

make_plugin_view <- function(tuning) {
  if (!inherits(tuning, "scmix_tuning") ||
      !inherits(tuning$refit, "scmix_tuned_matrix_fit")) {
    stop("A full-sample selected tuning refit is required.")
  }
  list(
    respondent_id = respondent_id,
    Z = Z,
    attr_names = colnames(deltaX),
    full_fit = list(mu = tuning$refit$mu, Sigma = tuning$refit$Sigma,
                    A = tuning$refit$A, kappa = tuning$refit$kappa),
    analysis_signature = tuning$analysis_signature
  )
}

selected_cv_score <- function(tuning) {
  tuning$candidates$cv_sequence_log_score[[tuning$selected_index]]
}

primary_full <- primary_nested <- primary_assembled <- NULL
if (cli$stage %in% c("primary", "all")) {
  primary_full_path <- file.path(output_dir, "fit_primary_full.rds")
  primary_full <- run_or_load(
    primary_full_path, cli$force,
    stamp(fit_full_q(sw_analysis_config$primary$q), "primary_full"),
    validator = function(x) valid_stamp(x, "primary_full")
  )

  nested_path <- file.path(output_dir, "fit_primary_nested.rds")
  primary_nested <- run_or_load(nested_path, cli$force, stamp({
    args <- common_fit_args(sw_analysis_config$primary$q)
    args$outer_K <- profile$outer_K
    args$inner_K <- profile$inner_K
    do.call(scmix_tune_outer_matrix, args)
  }, "primary_nested"),
  validator = function(x) valid_stamp(x, "primary_nested"))

  assembled_path <- file.path(output_dir, "fit_primary_assembled.rds")
  primary_assembled <- run_or_load(assembled_path, cli$force, stamp({
    scmix_assemble_nested(
      primary_nested, attr_names = colnames(deltaX), z_names = colnames(Z),
      require_optimization_gate = !isTRUE(profile$diagnostic_only),
      diagnostic_only = isTRUE(profile$diagnostic_only)
    )
  }, "primary_assembled"),
  validator = function(x) valid_stamp(x, "primary_assembled"))

  full_audit <- scmix_optimization_audit(primary_full$refit)
  nested_audit <- scmix_optimization_audit(primary_assembled)
  atomic_save_rds(full_audit,
                  file.path(output_dir, "optimization_primary_full.rds"))
  atomic_save_rds(nested_audit,
                  file.path(output_dir, "optimization_primary_nested.rds"))
}

if (cli$stage %in% c("sensitivity", "all")) {
  if (is.null(primary_full)) {
    primary_path <- file.path(output_dir, "fit_primary_full.rds")
    if (!file.exists(primary_path)) {
      stop("Run --stage=primary before rank sensitivity.")
    }
    primary_full <- readRDS(primary_path)
    if (!valid_stamp(primary_full, "primary_full")) {
      stop("Primary checkpoint was created under a different analysis ",
           "configuration. Rerun --stage=primary --force=true first.")
    }
  }
  fit_by_q <- new.env(parent = emptyenv())
  assign(as.character(sw_analysis_config$primary$q), primary_full,
         envir = fit_by_q)
  q_refitter <- function(q) {
    key <- as.character(q)
    path <- file.path(output_dir, paste0("fit_q", q, "_full.rds"))
    if (exists(key, envir = fit_by_q, inherits = FALSE)) {
      fit <- get(key, envir = fit_by_q, inherits = FALSE)
      if (!file.exists(path)) atomic_save_rds(fit, path)
      return(fit)
    }
    fit <- run_or_load(
      path, cli$force, stamp(fit_full_q(q), paste0("q", q, "_full"), q),
      validator = function(x) valid_stamp(x, paste0("q", q, "_full"), q)
    )
    assign(key, fit, envir = fit_by_q)
    fit
  }
  q_extractors <- list(
    qoi = function(fit) {
      view <- make_plugin_view(fit)
      theta <- scmix_paper_theta(view)$estimate
      contests <- vapply(sw_analysis_config$qoi$contests, function(d) {
        scmix_paper_choice(
          view, contrast = d, position_neutral = TRUE,
          n_nodes = sw_analysis_config$inference$choice_nodes,
          on_support = NA
        )$estimate
      }, numeric(1L))
      c(female_vs_male = -unname(theta[[1L]]), contests)
    },
    score = function(fit) c(heldout_sequence = selected_cv_score(fit))
  )
  q_sensitivity <- scmix_q_sensitivity(
    primary_q = sw_analysis_config$primary$q,
    alternatives = sw_analysis_config$primary$alternative_q,
    refitter = q_refitter, extractors = q_extractors, keep_fits = FALSE
  )
  q_sensitivity$primary_rank_provenance <-
    sw_analysis_config$primary$provenance
  q_sensitivity$rank_sensitivity_numerical_note <-
    sw_analysis_config$rank_sensitivity$note
  atomic_save_rds(q_sensitivity,
                  file.path(output_dir, "q_sensitivity.rds"))
}

artifact_paths <- list.files(output_dir, full.names = TRUE,
                             pattern = "\\.rds$")
manifest <- list(
  analysis_config_version = sw_analysis_config$version,
  profile = cli$profile, profile_specification = profile,
  stage = cli$stage, completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  input = sw_analysis_config$input$prepared,
  input_md5 = unname(tools::md5sum(sw_analysis_config$input$prepared)),
  n_respondents = length(unique(respondent_id)), n_tasks = nrow(deltaX),
  p = ncol(deltaX), p_z = ncol(Z), coefficient_order = colnames(deltaX),
  primary_q = sw_analysis_config$primary$q,
  alternative_q = sw_analysis_config$primary$alternative_q,
  primary_rank_provenance = sw_analysis_config$primary$provenance,
  learner_grid_provenance = sw_analysis_config$network$grid_provenance,
  rank_selection_performed = FALSE,
  respondent_level_folds = TRUE,
  training_only_preprocessing = TRUE,
  integration_resolution_tuned_with_learner = FALSE,
  posterior_summaries_used = FALSE,
  artifacts = stats::setNames(unname(tools::md5sum(artifact_paths)),
                              basename(artifact_paths)),
  session_info = utils::capture.output(sessionInfo())
)
atomic_save_rds(manifest, file.path(output_dir, "fit_manifest.rds"))
message("Saha--Weeks fit stage complete: ", output_dir)
