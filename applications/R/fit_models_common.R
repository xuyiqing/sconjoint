## Shared fit driver for the gs2020 and br2017 applications (step 03).
##
## Adapted from Avidit's sw2022 03_fit_models.R. The caller (a thin per-app
## wrapper) must define, before sourcing this file:
##   app_name  - "gs2020" or "br2017"
##   cfg       - the app's analysis config list
##   app_root  - normalized path to applications/<app>
##   root      - normalized package root
##   cli       - list(profile=, stage=, force=) already parsed
## Fits are written beneath <app_root>/results/mixed_logit/<profile>/, and the
## fitted-sieve inference basis is extracted HERE because torch modules do not
## survive saveRDS (a reloaded checkpoint has dead network pointers).

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path)
  invisible(path)
}

run_or_load <- function(path, overwrite, code) {
  if (file.exists(path) && !overwrite) {
    message("checkpoint: loading ", path)
    return(readRDS(path))
  }
  value <- force(code)
  atomic_save_rds(value, path)
  message("checkpoint: wrote ", path)
  value
}

if (!cli$profile %in% names(cfg$profiles)) {
  stop("--profile must be one of: ", paste(names(cfg$profiles), collapse = ", "))
}
if (!cli$stage %in% c("primary", "sensitivity", "all")) {
  stop("--stage must be primary, sensitivity, or all.")
}
profile <- cfg$profiles[[cli$profile]]

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The local analysis library must include pkgload.")
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

prepared <- readRDS(cfg$input$prepared)
required <- c("deltaX", "y", cfg$input$primary_Z, "respondent_id")
missing_fields <- setdiff(required, names(prepared))
if (length(missing_fields)) {
  stop("Prepared analysis object is missing: ",
       paste(missing_fields, collapse = ", "))
}

deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z <- as.matrix(prepared[[cfg$input$primary_Z]])
respondent_id <- as.character(prepared$respondent_id)
if (!identical(colnames(deltaX), cfg$coefficients$order)) {
  stop("DeltaX coordinates are not in the frozen likelihood basis.\nExpected: ",
       paste(cfg$coefficients$order, collapse = ", "), "\nObserved: ",
       paste(colnames(deltaX), collapse = ", "))
}
if (nrow(deltaX) != length(y) || nrow(deltaX) != nrow(Z) ||
    nrow(deltaX) != length(respondent_id) || any(!is.finite(deltaX)) ||
    any(!is.finite(Z)) || anyNA(respondent_id) || !all(y %in% c(0, 1))) {
  stop("Prepared matrix dimensions, values, or binary outcomes are invalid.")
}
es <- cfg$expected_sample
if (length(unique(respondent_id)) != es$n_respondents ||
    nrow(deltaX) != es$n_tasks) {
  stop("Sample must contain ", es$n_respondents, " respondents and ",
       es$n_tasks, " tasks; got ", length(unique(respondent_id)), " / ",
       nrow(deltaX), ".")
}
if (isTRUE(es$constant_task_count) &&
    length(unique(table(respondent_id))) != 1L) {
  stop("Expected a constant task count per respondent.")
}
if (any(vapply(split(seq_len(nrow(Z)), respondent_id), function(ii) {
  max(abs(sweep(Z[ii, , drop = FALSE], 2L, Z[ii[1L], ]))) > 1e-12
}, logical(1L)))) {
  stop("Moderators must be constant within respondent.")
}

output_dir <- file.path(cfg$output_root, cli$profile)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

profile_grid <- function(q) {
  grid <- profile$grid
  nodes <- profile$n_nodes
  if (q == 2L) {
    nodes <- unname(cfg$rank_sensitivity$q2_nodes[[cli$profile]])
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
  mu_bound = cfg$optimizer$mu_bound,
  kappa_bound = cfg$optimizer$kappa_bound,
  a_bound = cfg$optimizer$a_bound,
  weight_bound = cfg$optimizer$weight_bound,
  opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
  ## Profiles may override the nested-pooled-objective tolerance. The
  ## package default (1e-6) sits AT the float32 noise floor for large
  ## objectives: on gs2020 (~8 nats), a converged constant continuation
  ## can land ~1e-6 above the shared prefit by optimizer jitter alone,
  ## and because that fit is shared it disqualifies every candidate.
  nested_objective_tol = if (is.null(profile$nested_objective_tol)) 1e-6
                         else profile$nested_objective_tol,
  seed = cfg$optimizer$seed + 1000L * as.integer(q),
  device = cfg$optimizer$device,
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
  primary_full <- run_or_load(primary_full_path, cli$force,
                              fit_full_q(cfg$primary$q))

  nested_path <- file.path(output_dir, "fit_primary_nested.rds")
  primary_nested <- run_or_load(nested_path, cli$force, {
    args <- common_fit_args(cfg$primary$q)
    args$outer_K <- profile$outer_K
    args$inner_K <- profile$inner_K
    do.call(scmix_tune_outer_matrix, args)
  })

  assembled_path <- file.path(output_dir, "fit_primary_assembled.rds")
  primary_assembled <- run_or_load(assembled_path, cli$force, {
    scmix_assemble_nested(
      primary_nested, attr_names = colnames(deltaX), z_names = colnames(Z),
      require_optimization_gate = !isTRUE(profile$diagnostic_only),
      diagnostic_only = isTRUE(profile$diagnostic_only)
    )
  })

  full_audit <- scmix_optimization_audit(primary_full$refit)
  nested_audit <- scmix_optimization_audit(primary_assembled)
  atomic_save_rds(full_audit,
                  file.path(output_dir, "optimization_primary_full.rds"))
  atomic_save_rds(nested_audit,
                  file.path(output_dir, "optimization_primary_nested.rds"))

  basis_path <- file.path(output_dir, "inference_basis.rds")
  if (!file.exists(basis_path) || cli$force) {
    basis <- tryCatch({
      resp <- unique(respondent_id)
      first_row <- match(resp, respondent_id)
      Z_resp_raw <- as.matrix(primary_assembled$Z)[first_row, , drop = FALSE]
      hidden_features <- function(net, Z_transformed) {
        torch::with_no_grad({
          h <- torch::torch_tensor(as.matrix(Z_transformed),
                                   dtype = torch::torch_float())
          for (i in seq_along(net$hidden)) {
            h <- torch::nnf_relu(net$hidden[[i]](h))
          }
          as.matrix(h$to(dtype = torch::torch_float64()))
        })
      }
      mu_basis <- vector("list", primary_assembled$K)
      for (k in seq_len(primary_assembled$K)) {
        Zk <- sconjoint:::.sc_apply_z_transform(
          Z_resp_raw, primary_assembled$z_transform_folds[[k]])
        H <- hidden_features(primary_assembled$nets[[k]], Zk)
        mu_basis[[k]] <- cbind(intercept = 1, H)
      }
      list(mu_basis = mu_basis, respondents = resp,
           analysis_signature = primary_assembled$analysis_signature)
    }, error = function(e) {
      message("inference basis NOT written (need a fresh fit session): ",
              conditionMessage(e))
      NULL
    })
    if (!is.null(basis)) {
      atomic_save_rds(basis, basis_path)
      message("checkpoint: wrote ", basis_path)
    }
  }
}

if (cli$stage %in% c("sensitivity", "all")) {
  if (is.null(primary_full)) {
    primary_path <- file.path(output_dir, "fit_primary_full.rds")
    if (!file.exists(primary_path)) {
      stop("Run --stage=primary before rank sensitivity.")
    }
    primary_full <- readRDS(primary_path)
  }
  fit_by_q <- new.env(parent = emptyenv())
  assign(as.character(cfg$primary$q), primary_full, envir = fit_by_q)
  q_refitter <- function(q) {
    key <- as.character(q)
    path <- file.path(output_dir, paste0("fit_q", q, "_full.rds"))
    if (exists(key, envir = fit_by_q, inherits = FALSE)) {
      fit <- get(key, envir = fit_by_q, inherits = FALSE)
      if (!file.exists(path)) atomic_save_rds(fit, path)
      return(fit)
    }
    fit <- run_or_load(path, cli$force, fit_full_q(q))
    assign(key, fit, envir = fit_by_q)
    fit
  }
  headline <- cfg$qoi$headline_coordinate
  q_extractors <- list(
    qoi = function(fit) {
      view <- make_plugin_view(fit)
      theta <- scmix_paper_theta(view)$estimate
      out <- theta[[headline]]
      names(out) <- headline
      out
    },
    score = function(fit) c(heldout_sequence = selected_cv_score(fit))
  )
  q_sensitivity <- scmix_q_sensitivity(
    primary_q = cfg$primary$q,
    alternatives = cfg$primary$alternative_q,
    refitter = q_refitter, extractors = q_extractors, keep_fits = FALSE
  )
  q_sensitivity$primary_rank_provenance <- cfg$primary$provenance
  atomic_save_rds(q_sensitivity, file.path(output_dir, "q_sensitivity.rds"))
}

artifact_paths <- list.files(output_dir, full.names = TRUE, pattern = "\\.rds$")
manifest <- list(
  analysis_config_version = cfg$version,
  application = app_name,
  profile = cli$profile, profile_specification = profile,
  stage = cli$stage, completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  input = cfg$input$prepared,
  input_md5 = unname(tools::md5sum(cfg$input$prepared)),
  n_respondents = length(unique(respondent_id)), n_tasks = nrow(deltaX),
  p = ncol(deltaX), p_z = ncol(Z), coefficient_order = colnames(deltaX),
  primary_q = cfg$primary$q,
  alternative_q = cfg$primary$alternative_q,
  primary_rank_provenance = cfg$primary$provenance,
  rank_selection_performed = FALSE,
  respondent_level_folds = TRUE,
  session_info = utils::capture.output(sessionInfo())
)
atomic_save_rds(manifest, file.path(output_dir, "fit_manifest.rds"))
message(app_name, " fit stage complete: ", output_dir)
