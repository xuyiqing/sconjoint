#!/usr/bin/env Rscript

## Structural plug-ins, typed paper-QOI inference, and GH refinement for the
## rebuilt Saha--Weeks application.
##
## Usage from the package root:
##   applications/bin/Rscript45 applications/sw2022/R/04_inference_qoi.R \
##     --profile=production --stage=qoi
##   applications/bin/Rscript45 applications/sw2022/R/04_inference_qoi.R \
##     --profile=production --stage=refinement
##
## `stage=all` runs both. Formal standard errors and intervals remain NA unless
## the classed fit-linked verification record required by scmix_dml() can be
## constructed. The shipped configuration deliberately leaves that switch off
## pending a paper-specific sieve approximation/product-rate argument.

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
  saveRDS(object, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path)
  invisible(path)
}

root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
app_root <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app_root)
source(file.path(app_root, "config", "analysis_config.R"), local = FALSE)
cli <- parse_cli(commandArgs(trailingOnly = TRUE))
if (!cli$profile %in% names(sw_analysis_config$profiles)) {
  stop("Unknown profile: ", cli$profile)
}
if (!cli$stage %in% c("qoi", "refinement", "all")) {
  stop("--stage must be qoi, refinement, or all.")
}
profile <- sw_analysis_config$profiles[[cli$profile]]

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The local analysis library must include pkgload.")
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

prepared <- readRDS(sw_analysis_config$input$prepared)
deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z <- as.matrix(prepared[[sw_analysis_config$input$primary_Z]])
respondent_id <- as.character(prepared$respondent_id)
if (!identical(colnames(deltaX), sw_analysis_config$coefficients$order)) {
  stop("Prepared DeltaX does not use the frozen coefficient basis.")
}
output_dir <- file.path(sw_analysis_config$output_root, cli$profile)
full_path <- file.path(output_dir, "fit_primary_full.rds")
nested_path <- file.path(output_dir, "fit_primary_nested.rds")
assembled_path <- file.path(output_dir, "fit_primary_assembled.rds")
if (!all(file.exists(c(full_path, nested_path, assembled_path)))) {
  stop("Run 03_fit_models.R --stage=primary for this profile first.")
}
full_tuning <- readRDS(full_path)
base_nested <- readRDS(nested_path)
assembled <- readRDS(assembled_path)
valid_fit_stamp <- function(x, role) {
  z <- x$sw_application_specification
  is.list(z) && identical(z$config_version, sw_analysis_config$version) &&
    identical(z$profile, cli$profile) && identical(z$role, role) &&
    identical(z$profile_specification, profile)
}
if (!valid_fit_stamp(full_tuning, "primary_full") ||
    !valid_fit_stamp(base_nested, "primary_nested") ||
    !valid_fit_stamp(assembled, "primary_assembled")) {
  stop("Primary fit checkpoints do not match the active configuration. ",
       "Rerun 03_fit_models.R --stage=primary --force=true.")
}

rid <- unique(respondent_id)
first <- match(rid, respondent_id)
meta <- as.data.frame(prepared$respondent_meta)
if (!all(c("respondent_id", "party", "respondent_gender") %in% names(meta)) ||
    anyDuplicated(meta$respondent_id) || !setequal(meta$respondent_id, rid)) {
  stop("respondent_meta must uniquely cover fitted respondents and include party/gender.")
}
meta <- meta[match(rid, meta$respondent_id), , drop = FALSE]
party <- ifelse(grepl("Republican", meta$party), "Republican",
                ifelse(grepl("Independent", meta$party), "Independent",
                       ifelse(grepl("Democrat", meta$party), "Democrat", NA)))
if (anyNA(party)) stop("Could not map every respondent to a party subgroup.")
respondent_gender <- tolower(trimws(meta$respondent_gender))
if (any(!respondent_gender %in% c("female", "male"))) {
  stop("Unexpected respondent-gender value in respondent_meta.")
}

plugin_view <- list(
  respondent_id = respondent_id, Z = Z, attr_names = colnames(deltaX),
  full_fit = list(mu = full_tuning$refit$mu,
                  Sigma = full_tuning$refit$Sigma,
                  A = full_tuning$refit$A,
                  kappa = full_tuning$refit$kappa),
  analysis_signature = full_tuning$analysis_signature
)

unit_contrast <- function(...) {
  entries <- list(...)
  out <- numeric(ncol(deltaX))
  names(out) <- colnames(deltaX)
  for (entry in entries) out[[entry[[1L]]]] <- out[[entry[[1L]]]] + entry[[2L]]
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
    e("cand_talentDetermined.to.Succeed", 1), e("cand_talentEmpathetic", -1)),
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
heterogeneity_contrasts <- tau_contrasts[c(
  "female_vs_male", "agenda_moderate_vs_very_few",
  "agenda_complete_vs_very_few"
)]
sign_contrasts <- tau_contrasts[c(
  "female_vs_male", "talent_hard_working_vs_empathetic",
  "agenda_moderate_vs_very_few", "agenda_complete_vs_very_few"
)]

make_fold_basis <- function(fit) {
  fold_task <- as.integer(fit$fold_id)
  fold_resp <- fold_task[first]
  Z_resp <- Z[first, , drop = FALSE]
  K <- length(unique(fold_resp))
  lapply(seq_len(K), function(k) {
    train <- fold_resp != k
    center <- colMeans(Z_resp[train, , drop = FALSE])
    scale <- apply(Z_resp[train, , drop = FALSE], 2L, stats::sd)
    scale[!is.finite(scale) | scale < 1e-12] <- 1
    B <- cbind(`(Intercept)` = 1,
               sweep(sweep(Z_resp, 2L, center, `-`), 2L, scale, `/`))
    qr_train <- qr(B[train, , drop = FALSE], tol = 1e-10, LAPACK = FALSE)
    keep <- sort(qr_train$pivot[seq_len(qr_train$rank)])
    B[, keep, drop = FALSE]
  })
}

kappa_target <- function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
  N <- nrow(mu); p <- ncol(mu)
  list(
    target_type = "rowwise_expectation",
    value = matrix(rep(kappa, N), ncol = 1L),
    d_mu = array(0, c(N, 1L, p)), d_kappa = matrix(1, N, 1L),
    sigma_invariant = TRUE, labels = "kappa"
  )
}

directional_variance_by_fold <- function(fit, contrast) {
  vapply(fit$A_folds, function(A) {
    sum(as.numeric(crossprod(A, contrast))^2)
  }, numeric(1L))
}

build_typed_targets <- function(fit) {
  targets <- list(kappa = kappa_target)
  for (nm in names(tau_contrasts)) {
    targets[[paste0("tau_", nm)]] <- scmix_inference_target(
      type = "tau", contrast = tau_contrasts[[nm]], label = nm)
  }
  g_party <- list(
    democrat = party == "Democrat", independent = party == "Independent",
    republican = party == "Republican"
  )
  for (nm in names(g_party)) {
    g <- stats::setNames(as.numeric(g_party[[nm]]), rid)
    targets[[paste0("female_party_", nm)]] <- scmix_inference_target(
      type = "subgroup_tau_primitives",
      contrast = tau_contrasts$female_vs_male, subgroup = g)
  }
  g_gender <- list(female = respondent_gender == "female",
                   male = respondent_gender == "male")
  for (nm in names(g_gender)) {
    g <- stats::setNames(as.numeric(g_gender[[nm]]), rid)
    targets[[paste0("female_respgender_", nm)]] <- scmix_inference_target(
      type = "subgroup_tau_primitives",
      contrast = tau_contrasts$female_vs_male, subgroup = g)
  }
  for (nm in names(contest_contrasts)) {
    targets[[paste0("choice_contest_", nm)]] <- scmix_inference_target(
      type = "choice", contrast = contest_contrasts[[nm]],
      position_neutral = TRUE,
      n_nodes = sw_analysis_config$inference$choice_nodes,
      label = paste0("contest_", nm))
  }
  for (nm in names(heterogeneity_contrasts)) {
    targets[[paste0("heterogeneity_", nm)]] <- scmix_inference_target(
      type = "heterogeneity_primitives",
      contrast = heterogeneity_contrasts[[nm]])
  }
  sign_gate <- lapply(sign_contrasts, function(d) {
    variance <- directional_variance_by_fold(fit, d)
    list(variance_by_fold = variance,
         margin = sw_analysis_config$inference$variance_floor,
         pass = all(variance >= sw_analysis_config$inference$variance_floor))
  })
  for (nm in names(sign_gate)) {
    if (isTRUE(sign_gate[[nm]]$pass)) {
      targets[[paste0("sign_", nm)]] <- scmix_inference_target(
        type = "sign", contrast = sign_contrasts[[nm]],
        variance_floor = sw_analysis_config$inference$variance_floor,
        label = paste0("sign_", nm))
    }
  }
  list(targets = targets, sign_gate = sign_gate)
}

run_dml <- function(fit, basis, target_bundle, verification = NULL,
                    multiplier_draws = 0L) {
  scmix_dml(
    fit, targets = "theta", plugin_targets = target_bundle$targets,
    mu_basis = basis, nu_grid = NULL, riesz_penalty = "identity",
    riesz_validation_fraction =
      sw_analysis_config$inference$riesz_validation_fraction,
    active_eigenvalue_min =
      sw_analysis_config$inference$active_eigenvalue_min,
    rank_tolerance = sw_analysis_config$inference$rank_tolerance,
    information_eigenvalue_min =
      sw_analysis_config$inference$information_eigenvalue_min,
    riesz_equation_tolerance =
      sw_analysis_config$inference$riesz_equation_tolerance,
    ridge_sensitivity_tolerance =
      sw_analysis_config$inference$ridge_sensitivity_tolerance,
    allow_numeric_derivatives = FALSE,
    verification = verification,
    multiplier_draws = as.integer(multiplier_draws),
    multiplier = sw_analysis_config$inference$multiplier,
    level = sw_analysis_config$inference$level,
    seed = sw_analysis_config$optimizer$seed + 90000L
  )
}

make_transforms <- function(inference) {
  if (!inherits(inference, "scmix_dml") ||
      is.null(inference$diagnostic_covariance) ||
      !is.matrix(inference$diagnostic_covariance)) {
    return(list(status = "unavailable", reason = inference$reason))
  }
  out <- list()
  for (nm in c("democrat", "independent", "republican")) {
    stem <- paste0("female_party_", nm)
    out[[stem]] <- scmix_delta_transform(
      inference, type = "subgroup_ratio",
      primitives = c(paste0(stem, ":weighted_tau"),
                     paste0(stem, ":subgroup_probability")),
      denominator_margin =
        sw_analysis_config$inference$subgroup_probability_margin,
      level = sw_analysis_config$inference$level
    )
  }
  for (nm in c("female", "male")) {
    stem <- paste0("female_respgender_", nm)
    out[[stem]] <- scmix_delta_transform(
      inference, type = "subgroup_ratio",
      primitives = c(paste0(stem, ":weighted_tau"),
                     paste0(stem, ":subgroup_probability")),
      denominator_margin =
        sw_analysis_config$inference$subgroup_probability_margin,
      level = sw_analysis_config$inference$level
    )
  }
  for (nm in names(heterogeneity_contrasts)) {
    stem <- paste0("heterogeneity_", nm)
    out[[stem]] <- scmix_delta_transform(
      inference, type = "directional_heterogeneity",
      primitives = paste0(stem, c(":mean", ":second_moment",
                                  ":residual_variance")),
      total_margin =
        sw_analysis_config$inference$total_heterogeneity_margin,
      level = sw_analysis_config$inference$level
    )
  }
  attr(out, "formal_inference_available") <-
    isTRUE(inference$inference_available)
  out
}

make_plugin_quantities <- function() {
  as_paper_quantity <- function(quantity, estimate, details = list()) {
    structure(
      list(quantity = quantity, estimate = estimate, details = details,
           gate = NULL,
           sources = list(full_sample_selected_refit = TRUE),
           respondent_weighting = "equal weight per respondent",
           posterior_summaries_used = FALSE),
      class = c("scmix_paper_quantity", "list")
    )
  }
  theta <- scmix_paper_theta(plugin_view, subgroup = party)
  raw_theta <- theta$estimate
  transformed_tau <- vapply(tau_contrasts, function(d) sum(d * raw_theta),
                            numeric(1L))
  subgroup_party <- t(vapply(c("Democrat", "Independent", "Republican"),
    function(g) {
      colMeans(full_tuning$refit$mu[first[party == g], , drop = FALSE])
    }, numeric(ncol(deltaX))))
  subgroup_gender <- t(vapply(c("female", "male"), function(g) {
    colMeans(full_tuning$refit$mu[first[respondent_gender == g], , drop = FALSE])
  }, numeric(ncol(deltaX))))
  colnames(subgroup_party) <- colnames(subgroup_gender) <- colnames(deltaX)
  transform_rows <- function(x) t(apply(x, 1L, function(theta_g) {
    vapply(tau_contrasts, function(d) sum(d * theta_g), numeric(1L))
  }))
  flatten_named_matrix <- function(x) {
    labels <- unlist(lapply(rownames(x), function(rn) {
      paste(rn, colnames(x), sep = ":")
    }), use.names = FALSE)
    stats::setNames(as.numeric(t(x)), labels)
  }
  reporting_party <- transform_rows(subgroup_party)
  reporting_gender <- transform_rows(subgroup_gender)
  choices <- lapply(contest_contrasts, function(d) {
    x <- scmix_paper_choice(
      plugin_view, contrast = d, position_neutral = TRUE,
      n_nodes = sw_analysis_config$inference$choice_nodes,
      on_support = NA
    )
    x$details$conditional_protocol_support <-
      sw_analysis_config$qoi$contests_support
    x
  })
  signs <- lapply(sign_contrasts, function(d) {
    scmix_paper_signshare(
      plugin_view, contrast = d, ties = "exclude",
      variance_margin = sw_analysis_config$inference$variance_floor,
      ci = NULL
    )
  })
  heterogeneity <- lapply(heterogeneity_contrasts, function(d) {
    scmix_paper_heterogeneity(
      plugin_view, direction = d,
      total_margin = sw_analysis_config$inference$total_heterogeneity_margin
    )
  })
  list(
    estimator = "full-sample integrated respondent-sequence mixed likelihood",
    posterior_summaries_used = FALSE,
    respondent_weighting = "equal respondent weight",
    kappa = as_paper_quantity(
      "candidate-A position/alternative intercept",
      c(kappa = full_tuning$refit$kappa)
    ),
    theta_raw_likelihood_basis = theta,
    theta_reporting_basis = as_paper_quantity(
      "average preferences in the Saha--Weeks reporting basis",
      transformed_tau,
      details = list(reporting_reference =
                       sw_analysis_config$qoi$reporting_reference)
    ),
    subgroup_raw_theta_party = subgroup_party,
    subgroup_raw_theta_respondent_gender = subgroup_gender,
    subgroup_reporting_theta_party = as_paper_quantity(
      "party-specific average preferences in the reporting basis",
      flatten_named_matrix(reporting_party),
      details = list(subgroup_n = table(party))
    ),
    subgroup_reporting_theta_respondent_gender = as_paper_quantity(
      "respondent-gender-specific average preferences in the reporting basis",
      flatten_named_matrix(reporting_gender),
      details = list(subgroup_n = table(respondent_gender))
    ),
    choice_probabilities_position_neutral = choices,
    sign_shares_integrated_normal = signs,
    heterogeneity = heterogeneity,
    covariance_decomposition = scmix_paper_heterogeneity(plugin_view),
    mrs = list(status = "not_applicable", reason = sw_analysis_config$qoi$mrs),
    support_note = sw_analysis_config$qoi$contests_support,
    analysis_signature = full_tuning$analysis_signature
  )
}

basis <- make_fold_basis(assembled)
target_bundle <- build_typed_targets(assembled)
diagnostic_inference <- NULL

if (cli$stage %in% c("qoi", "all")) {
  plugin_qoi <- make_plugin_quantities()
  atomic_save_rds(plugin_qoi, file.path(output_dir, "qoi_plugin.rds"))

  diagnostic_inference <- run_dml(
    assembled, basis, target_bundle, verification = NULL,
    multiplier_draws = sw_analysis_config$inference$multiplier_draws
  )
  atomic_save_rds(diagnostic_inference,
                  file.path(output_dir, "inference_diagnostic.rds"))
  transforms <- make_transforms(diagnostic_inference)
  atomic_save_rds(transforms,
                  file.path(output_dir, "inference_transforms.rds"))

  inference_manifest <- list(
    config_version = sw_analysis_config$version, profile = cli$profile,
    analysis_signature = assembled$analysis_signature,
    basis_dimensions = vapply(basis, ncol, integer(1L)),
    basis_scope = paste(
      "intercept plus primary moderator terms, standardized and rank-reduced",
      "inside each outer training sample"
    ),
    status = diagnostic_inference$status,
    inference_available = isTRUE(diagnostic_inference$inference_available),
    inference_claim = diagnostic_inference$inference_claim,
    reason = diagnostic_inference$reason,
    formal_inference_enabled_in_config =
      sw_analysis_config$inference$enable_conditional_formal_inference,
    sign_share_fold_gates = target_bundle$sign_gate,
    majority_rule = paste(
      "No majority claim unless a formally available interval excludes 1/2;",
      "diagnostic or plug-in shares alone never trigger a claim."
    ),
    typed_targets = names(target_bundle$targets),
    completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  atomic_save_rds(inference_manifest,
                  file.path(output_dir, "inference_manifest.rds"))
}

if (cli$stage %in% c("refinement", "all")) {
  if (is.null(diagnostic_inference)) {
    inf_path <- file.path(output_dir, "inference_diagnostic.rds")
    diagnostic_inference <- if (file.exists(inf_path) && !cli$force) {
      readRDS(inf_path)
    } else {
      run_dml(assembled, basis, target_bundle, verification = NULL,
              multiplier_draws = 0L)
    }
  }
  if (is.null(diagnostic_inference$diagnostic_covariance) ||
      !is.matrix(diagnostic_inference$diagnostic_covariance)) {
    stop("Primary diagnostic inference failed before numerical refinement: ",
         diagnostic_inference$reason)
  }

  headline <- c(
    "kappa", "tau_female_vs_male",
    "tau_talent_hard_working_vs_empathetic",
    "tau_agenda_moderate_vs_very_few",
    "tau_agenda_complete_vs_very_few",
    paste0("choice_contest_", names(contest_contrasts))
  )
  headline <- intersect(headline, names(diagnostic_inference$estimate))

  refinement_metrics <- function(fit) {
    inf <- fit$refinement_inference
    if (!inherits(inf, "scmix_dml") ||
        is.null(inf$diagnostic_covariance) ||
        !is.matrix(inf$diagnostic_covariance)) {
      stop("A refinement fit did not yield finite diagnostic inference.")
    }
    list(
      qoi = inf$estimate[headline],
      likelihood = c(heldout_sequence = mean(vapply(
        inf$fold_details, `[[`, numeric(1L), "mean_heldout_loglik"))),
      score = c(information_min = min(vapply(
        inf$fold_details, `[[`, numeric(1L), "information_structural_min"))),
      riesz = c(max_equation_residual =
                  inf$riesz_equation_max_relative_residual),
      if_l2 = sqrt(colMeans(inf$influence[, headline, drop = FALSE]^2)),
      se = inf$diagnostic_se[headline]
    )
  }

  grid_at_nodes <- function(nodes) lapply(profile$grid, function(spec) {
    spec$q <- NULL; spec$integration <- "gh"; spec$n_nodes <- as.integer(nodes)
    spec
  })
  fresh_refitter <- function(setting) {
    nested <- scmix_tune_outer_matrix(
      deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
      grid = grid_at_nodes(setting$resolution),
      q = sw_analysis_config$primary$q,
      outer_K = profile$outer_K, inner_K = profile$inner_K,
      outer_fold_id = base_nested$outer_fold_id,
      seed = sw_analysis_config$optimizer$seed +
        1000L * sw_analysis_config$primary$q,
      allow_q_tuning = FALSE, allow_integration_tuning = FALSE,
      n_epochs = profile$n_epochs,
      learning_rate = profile$learning_rate,
      n_starts = profile$n_starts,
      mu_bound = sw_analysis_config$optimizer$mu_bound,
      kappa_bound = sw_analysis_config$optimizer$kappa_bound,
      a_bound = sw_analysis_config$optimizer$a_bound,
      weight_bound = sw_analysis_config$optimizer$weight_bound,
      opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
      device = sw_analysis_config$optimizer$device,
      keep_cv_fits = FALSE, verbose = FALSE
    )
    fit <- scmix_assemble_nested(
      nested, attr_names = colnames(deltaX), z_names = colnames(Z),
      require_optimization_gate = !isTRUE(profile$diagnostic_only),
      diagnostic_only = isTRUE(profile$diagnostic_only)
    )
    target_r <- build_typed_targets(fit)
    fit$refinement_inference <- run_dml(
      fit, make_fold_basis(fit), target_r, verification = NULL,
      multiplier_draws = 0L
    )
    fit
  }
  extractors <- list(
    qoi = function(fit) refinement_metrics(fit)$qoi,
    likelihood = function(fit) refinement_metrics(fit)$likelihood,
    score = function(fit) refinement_metrics(fit)$score,
    riesz = function(fit) refinement_metrics(fit)$riesz,
    if_l2 = function(fit) refinement_metrics(fit)$if_l2,
    se = function(fit) refinement_metrics(fit)$se
  )
  base_for_names <- assembled
  base_for_names$refinement_inference <- diagnostic_inference
  base_metrics <- refinement_metrics(base_for_names)
  category_tol <- sw_analysis_config$numerical_refinement$tolerances
  tolerances <- unlist(lapply(names(base_metrics), function(category) {
    values <- base_metrics[[category]]
    stats::setNames(rep(category_tol[[category]], length(values)),
                    paste(category, names(values), sep = "."))
  }), use.names = TRUE)
  resolutions <- if (identical(cli$profile, "production")) {
    sw_analysis_config$numerical_refinement$production_nodes
  } else sw_analysis_config$numerical_refinement$pilot_nodes

  refinement_path <- file.path(output_dir, "integration_refinement.rds")
  refinement <- if (file.exists(refinement_path) && !cli$force) {
    readRDS(refinement_path)
  } else {
    result <- tryCatch(
      scmix_integration_refinement(
        resolutions = resolutions, scrambles = NULL,
        refitter = fresh_refitter, extractors = extractors,
        tolerances = tolerances, keep_fits = FALSE
      ),
      error = function(e) structure(
        list(status = "failed", reason = conditionMessage(e),
             resolutions = resolutions,
             analysis_signature = assembled$analysis_signature),
        class = c("sw_integration_refinement_failure", "list"))
    )
    atomic_save_rds(result, refinement_path)
    result
  }

  verification_status <- list(
    status = "not_constructed",
    inference_available = FALSE,
    reason = paste(
      "Conditional formal inference is disabled pending an approved",
      "paper-specific fitted-sieve approximation and product-rate argument."
    ),
    numerical_refinement_class = class(refinement),
    numerical_gate_pass = inherits(refinement, "scmix_integration_refinement") &&
      isTRUE(refinement$gate$pass),
    signature_match = inherits(refinement, "scmix_integration_refinement") &&
      isTRUE(refinement$signature_match)
  )

  if (isTRUE(sw_analysis_config$inference$enable_conditional_formal_inference)) {
    tangent <- sw_analysis_config$inference$tangent
    needed_text <- c(tangent$approximation_argument,
                     tangent$product_rate_argument, tangent$provenance)
    if (any(!nzchar(trimws(needed_text)))) {
      stop("Formal inference was enabled without complete tangent arguments.")
    }
    if (!inherits(refinement, "scmix_integration_refinement") ||
        !isTRUE(refinement$gate$pass) || !isTRUE(refinement$signature_match)) {
      stop("Formal inference was enabled but GH refinement did not pass/link.")
    }
    opt_audit <- readRDS(file.path(output_dir,
                                   "optimization_primary_nested.rds"))
    verification <- scmix_inference_verification(
      fit = assembled, mu_basis = basis, tangent = tangent,
      numerical = list(
        artifact = refinement,
        rate_argument = paste(
          "The prespecified empirical GH differences are required to be",
          "negligible relative to the root-N target scale; this remains a",
          "documented high-level numerical-rate condition."
        ),
        provenance = "fresh nested GH refits from 04_inference_qoi.R"
      ),
      optimization = list(
        artifact = opt_audit,
        gap_argument = paste(
          "All returned-state and bound gates pass and the remaining",
          "approximate-maximization error is assumed root-N negligible;",
          "the diagnostics do not certify the nonconvex global gap."
        ),
        provenance = "multiple-start nested optimization audit"
      )
    )
    verified <- run_dml(
      assembled, basis, target_bundle, verification = verification,
      multiplier_draws = sw_analysis_config$inference$multiplier_draws
    )
    verified_transforms <- make_transforms(verified)
    atomic_save_rds(verification,
                    file.path(output_dir, "inference_verification.rds"))
    atomic_save_rds(verified,
                    file.path(output_dir, "inference_verified.rds"))
    atomic_save_rds(verified_transforms,
                    file.path(output_dir, "inference_verified_transforms.rds"))
    verification_status <- list(
      status = verified$status,
      inference_available = isTRUE(verified$inference_available),
      reason = verified$reason,
      inference_claim = verified$inference_claim,
      analysis_signature = verified$analysis_signature
    )
  }
  atomic_save_rds(verification_status,
                  file.path(output_dir, "inference_verification_status.rds"))
}

message("Saha--Weeks QOI/inference stage complete: ", output_dir)
