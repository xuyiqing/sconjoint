test_that("respondent-weighted contrast scaling does not task-weight respondents", {
  dx <- matrix(c(2, 4, 4, 4), ncol = 1L,
               dimnames = list(NULL, "x"))
  rid <- c("one", "two", "two", "two")
  out <- sconjoint:::.sc_comp_fit_dx_scale(dx, rid)
  expect_equal(unname(out$scale), sqrt((2^2 + 4^2) / 2))
  expect_false(isTRUE(all.equal(unname(out$scale), sqrt(mean(dx^2)))))
  expect_identical(out$n_respondents, 2L)
  expect_identical(out$centering, "none")
})

test_that("tuning grids keep q fixed unless selection is explicit", {
  fixed <- sconjoint:::.sc_comp_normalize_grid(
    list(
      list(name = "small", hidden = c(4L, 2L), weight_decay = 0),
      list(name = "wide", hidden = 8L, weight_decay = 1e-3,
           integration = "gh", n_nodes = 9L)
    ),
    q = 1L, p = 3L
  )
  expect_equal(vapply(fixed, `[[`, integer(1L), "q"), c(1L, 1L))
  expect_equal(fixed[[2L]]$n_nodes, 9L)

  varying <- list(
    list(hidden = 4L, weight_decay = 0, q = 0L),
    list(hidden = 4L, weight_decay = 0, q = 1L)
  )
  expect_error(
    sconjoint:::.sc_comp_normalize_grid(varying, p = 3L),
    "varies `q`"
  )
  allowed <- sconjoint:::.sc_comp_normalize_grid(
    varying, p = 3L, allow_q_tuning = TRUE)
  expect_equal(vapply(allowed, `[[`, integer(1L), "q"), 0:1)

  expect_error(
    sconjoint:::.sc_comp_normalize_grid(
      list(list(hidden = 4L, q = 4L, integration = "qmc",
                n_draws = 17L)), p = 6L),
    "must be even"
  )
  even_qmc <- sconjoint:::.sc_comp_normalize_grid(
    list(list(hidden = 4L, q = 4L, integration = "qmc",
              n_draws = 18L)), p = 6L)
  expect_identical(even_qmc[[1L]]$n_draws, 18L)
})

test_that("integration resolution is refinement, not ordinary learner CV", {
  specs <- sconjoint:::.sc_comp_normalize_grid(
    list(
      list(name = "coarse", hidden = 4L, weight_decay = 0,
           integration = "gh", n_nodes = 7L),
      list(name = "fine", hidden = 4L, weight_decay = 0,
           integration = "gh", n_nodes = 15L)
    ),
    q = 1L, p = 3L)
  expect_error(
    sconjoint:::.sc_comp_integration_policy(specs),
    "scmix_integration_refinement"
  )
  diagnostic <- sconjoint:::.sc_comp_integration_policy(
    specs, allow_integration_tuning = TRUE)
  expect_true(diagnostic$diagnostic_only)
  expect_false(diagnostic$primary_inference_eligible)

  fixed <- specs
  fixed[[2L]]$n_nodes <- fixed[[1L]]$n_nodes
  ordinary <- sconjoint:::.sc_comp_integration_policy(fixed)
  expect_false(ordinary$varies)
  expect_true(ordinary$primary_inference_eligible)
})

test_that("candidate selection excludes failed inner optimizations", {
  score <- matrix(c(10, 10, 1, 1), nrow = 2L, byrow = TRUE)
  count <- matrix(5L, nrow = 2L, ncol = 2L)
  gate <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2L, byrow = TRUE)
  out <- sconjoint:::.sc_comp_select_candidate(score, count, gate)
  expect_identical(out$selected, 2L)
  expect_equal(out$cv_log_score, c(10, 1))
  expect_equal(out$selection_score, c(-Inf, 1))
  expect_false(out$computationally_eligible[1L])
  expect_true(out$selection_eligible[2L])
  expect_match(out$ineligible_reason[1L], "computational_gate_failed")

  score[2L, 2L] <- NA_real_
  expect_error(
    sconjoint:::.sc_comp_select_candidate(score, count, gate),
    "No learner candidate passed every inner-fold computational gate"
  )

  active <- sconjoint:::.sc_comp_inner_fit_gate(list(
    optimization_gate_pass = TRUE,
    optimization_failure_reasons = character(),
    bounds = list(mu_active = FALSE, kappa_active = FALSE,
                  a_active = TRUE, weight_active = FALSE)))
  expect_false(active$pass)
  expect_true("parameter_bound_active" %in% active$failure_reasons)

  incomplete <- sconjoint:::.sc_comp_inner_fit_gate(list(
    optimization_gate_pass = TRUE,
    optimization_failure_reasons = character(),
    bounds = list(mu_active = FALSE, kappa_active = FALSE)))
  expect_false(incomplete$pass)
  expect_true("compact_bound_diagnostics_incomplete" %in%
                incomplete$failure_reasons)

  dominance_failure <- sconjoint:::.sc_comp_inner_fit_gate(list(
    optimization_gate_pass = TRUE,
    optimization_failure_reasons = character(),
    bounds = list(mu_active = FALSE, alpha_active = FALSE,
                  kappa_active = FALSE, a_active = FALSE,
                  weight_active = FALSE),
    nested_objective_gate = list(pass = FALSE)))
  expect_false(dominance_failure$pass)
  expect_true("nested_pooled_objective_not_attained" %in%
                dominance_failure$failure_reasons)
  dominance_matrix <- matrix(
    c(dominance_failure$pass, TRUE, TRUE, TRUE), nrow = 2L, byrow = TRUE)
  dominance_selection <- sconjoint:::.sc_comp_select_candidate(
    matrix(c(10, 10, 1, 1), nrow = 2L, byrow = TRUE),
    matrix(5L, nrow = 2L, ncol = 2L), dominance_matrix)
  expect_identical(dominance_selection$selected, 2L)
  expect_false(dominance_selection$selection_eligible[1L])
})

test_that("optimization audit reports attained diagnostics, not a global gap", {
  starts <- data.frame(
    start = 1:2, objective = c(-12, -10), penalized_nll = c(12, 10),
    unpenalized_nll = c(11.5, 9.5), gradient_norm = c(0.1, 0.001),
    objective_finite = TRUE, optimization_gate_pass = FALSE,
    converged = c(FALSE, TRUE), epochs = c(100L, 80L),
    stop_reason = c("maximum_epochs", "criterion_and_gradient_tolerance"),
    a_bound_active = FALSE, weight_bound_active = FALSE,
    bound_diagnostics_complete = TRUE, bound_activity = FALSE
  )
  block <- list(
    best_start = 2L, starts = starts, objective = -10,
    gradient_norm = 0.001, converged = TRUE,
    structural_gradient_norm = 0.0005, sieve_gradient_norm = 0.001,
    criterion_diagnostic_source =
      "returned_state_vs_immediately_preceding_attained_state",
    state_restored = FALSE, objective_finite = TRUE,
    optimization_gate_pass = FALSE,
    stop_reason = "criterion_and_gradient_tolerance",
    bounds = list(mu_active = FALSE, kappa_active = FALSE,
                  a_active = TRUE, weight_active = FALSE)
  )
  fake <- list(
    optimization = list(full = block, folds = list(block)),
    analysis_signature = "scmix-v1-0123456789abcdef"
  )
  out <- sconjoint:::scmix_optimization_audit(fake)
  expect_equal(nrow(out$starts), 4L)
  expect_true(out$any_bound_activity)
  expect_true(all(out$summary$selected_a_bound_active))
  expect_true(out$all_selected_tolerances_met)
  expect_true(out$all_objectives_finite)
  expect_false(out$all_computational_gates_pass)
  expect_true(out$all_bound_diagnostics_complete)
  expect_identical(out$analysis_signature, fake$analysis_signature)
  expect_true(out$signature_match)
  expect_false(out$global_optimality_gap_known)
  expect_match(out$disclaimer, "do not bound")
  expect_true(all(out$summary$attained_objective_range == 2))
})

test_that("integration refinement refits and recomputes every requested metric", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  refitter <- function(setting) {
    calls$n <- calls$n + 1L
    list(resolution = setting$resolution,
         scramble = if (is.na(setting$scramble)) 0 else setting$scramble,
         analysis_signature = "scmix-v1-fedcba9876543210")
  }
  extractors <- list(
    qoi = function(fit) c(estimate = 1 + 1 / fit$resolution),
    se = function(fit) c(main = 0.2 + 1 / (100 * fit$resolution)),
    score = function(fit) c(heldout = -2 + fit$scramble / 1e5),
    eigen = function(fit) c(minimum = 0.4 - 1 / (100 * fit$resolution))
  )
  out <- sconjoint:::scmix_integration_refinement(
    resolutions = c(100L, 200L), scrambles = c(11L, 22L),
    refitter = refitter, extractors = extractors,
    tolerances = c(qoi.estimate = 0.02, se.main = 0.01,
                   score.heldout = 0.01, eigen.minimum = 0.01),
    keep_fits = TRUE
  )
  expect_identical(calls$n, 4L)
  expect_identical(out$refit_count, 4L)
  expect_equal(length(out$fits), 4L)
  expect_true(out$gate$pass)
  expect_true(out$signature_match)
  expect_identical(out$analysis_signature, "scmix-v1-fedcba9876543210")
  expect_true(all(c("qoi.estimate", "se.main", "score.heldout",
                    "eigen.minimum") %in% names(out$checks)))
  expect_match(out$disclaimer, "fresh refit")

  mismatched <- 0L
  bad <- sconjoint:::scmix_integration_refinement(
    resolutions = c(100L, 200L),
    refitter = function(setting) {
      mismatched <<- mismatched + 1L
      list(resolution = setting$resolution, scramble = 0,
           analysis_signature = paste0("analysis-", mismatched))
    },
    extractors = list(qoi = function(fit) c(estimate = 1)),
    tolerances = c(qoi.estimate = 0)
  )
  expect_false(bad$signature_match)
  expect_true(is.na(bad$analysis_signature))
})

test_that("q sensitivity preserves the prespecified primary specification", {
  calls <- integer()
  refitter <- function(q) {
    calls <<- c(calls, q)
    list(q = q)
  }
  out <- sconjoint:::scmix_q_sensitivity(
    primary_q = 1L, alternatives = c(0L, 2L), refitter = refitter,
    extractors = list(
      qoi = function(fit) c(estimate = 0.5 + fit$q / 10),
      se = function(fit) c(main = 0.1 + fit$q / 100)
    ),
    keep_fits = TRUE
  )
  expect_equal(calls, c(1L, 0L, 2L))
  expect_identical(out$primary_q, 1L)
  expect_false(out$selection_performed)
  expect_identical(out$maintained_model, "low-rank normal mixed logit")
  expect_true(out$table$primary[out$table$q == 1L])
  expect_equal(out$table$qoi.estimate.difference_from_primary[out$table$q == 1L], 0)
  expect_match(out$coverage_warning, "selection-adjusted coverage")
})

test_that("matrix tuning uses respondent folds and training-only preprocessing", {
  skip_if_not_installed("torch")
  N <- 12L
  T <- 5L
  rid <- rep(seq_len(N), each = T)
  dx <- matrix(0, N * T, 1L, dimnames = list(NULL, "x"))
  z_resp <- seq(-2, 2, length.out = N)
  Z <- matrix(rep(z_resp, each = T), ncol = 1L,
              dimnames = list(NULL, "z"))
  y <- rep(c(1, 1, 1, 1, 0), N)
  grid <- list(
    list(name = "small", hidden = 2L, weight_decay = 0),
    list(name = "wide", hidden = 4L, weight_decay = 1e-3)
  )
  out <- sconjoint:::scmix_tune_matrix(
    dx, y, Z, rid, grid = grid, q = 0L, K = 2L,
    n_epochs = 40L, learning_rate = 0.05, n_starts = 1L,
    opt_tol = 1e6, grad_tol = 1e6, seed = 20260823L
  )
  expect_s3_class(out, "scmix_tuning")
  expect_true(all(vapply(split(out$fold_id, rid),
                         function(x) length(unique(x)) == 1L, logical(1L))))
  expect_true(all(is.finite(out$candidates$cv_sequence_log_score)))
  expect_true(all(out$candidates$selection_eligible))
  expect_true(all(out$fold_computational_gate))
  expect_true(out$candidate_selection_gate$pass)
  expect_identical(out$scoring,
                   "unpenalized complete respondent-sequence log likelihood")
  for (k in seq_len(2L)) {
    train <- out$fold_id != k
    first <- !duplicated(rid[train])
    expect_equal(unname(out$preprocessing_folds[[k]]$z$center),
                 mean(Z[train, 1L][first]))
  }
  expect_match(out$dml_scope, "rerun inside each outer")
  expect_s3_class(out$refit, "scmix_tuned_matrix_fit")
  expect_s3_class(out$refit$network_state, "scmix_network_state")
  expect_equal(out$refit$network_state$integration_grid$U,
               out$refit$integration_grid$U)
  state_path <- tempfile(fileext = ".rds")
  saveRDS(out$refit$network_state, state_path, version = 3)
  state_reloaded <- readRDS(state_path)
  expect_equal(
    unname(scmix_predict_network(state_reloaded, Z)),
    unname(out$refit$mu), tolerance = 1e-6)
  expect_identical(out$analysis_signature, out$refit$analysis_signature)
  expect_true(nzchar(out$analysis_signature))

  fixed <- scmix_refit_selected_matrix(
    dx, y, Z, rid, specification = out$selected,
    integration_grid = out$refit$integration_grid,
    preprocessing = out$refit$preprocessing,
    n_epochs = 2L, learning_rate = 0.05, n_starts = 1L,
    opt_tol = 1e6, grad_tol = 1e6, seed = 20260823L,
    source_analysis_signature = out$analysis_signature)
  expect_s3_class(fixed, "scmix_tuned_matrix_fit")
  expect_false(fixed$retuning_performed)
  expect_identical(fixed$analysis_signature, out$analysis_signature)
  expect_false(identical(fixed$refit_analysis_signature,
                         out$analysis_signature))
  expect_equal(
    unname(scmix_predict_network(fixed$network_state, Z)),
    unname(fixed$mu), tolerance = 1e-6)
})

test_that("nested tuning assembles the fold nuisance contract for DML", {
  skip_if_not_installed("torch")
  N <- 8L
  T <- 3L
  rid <- rep(seq_len(N), each = T)
  dx <- matrix(rep(c(-1, 0, 1), N), ncol = 1L,
               dimnames = list(NULL, "x"))
  Z <- matrix(rep(seq(-1, 1, length.out = N), each = T), ncol = 1L,
              dimnames = list(NULL, "z"))
  y <- rep(c(0, 1, 1), N)
  outer <- rep(rep(1:2, length.out = N), each = T)
  nested <- sconjoint:::scmix_tune_outer_matrix(
    dx, y, Z, rid,
    grid = list(list(name = "fixed", hidden = 2L, weight_decay = 0,
                     integration = "auto", n_nodes = 7L)),
    q = 0L, inner_K = 2L, outer_fold_id = outer,
    n_epochs = 2L, n_starts = 1L, opt_tol = 1e6, grad_tol = 1e6,
    seed = 20260823L)
  assembled <- sconjoint:::scmix_assemble_nested(
    nested, require_optimization_gate = FALSE, diagnostic_only = TRUE)
  expect_s3_class(assembled, "scmix_nested_assembled")
  expect_length(assembled$mu_all_folds, 2L)
  expect_length(assembled$A_computational_folds, 2L)
  expect_length(assembled$network_states, 2L)
  expect_true(all(vapply(assembled$network_states, inherits, logical(1L),
                         what = "scmix_network_state")))
  expect_equal(dim(assembled$mu_hat), dim(dx))
  expect_true(all(vapply(
    assembled$integration_grids_folds[-1L],
    sconjoint:::.sc_comp_same_integration_grid, logical(1L),
    y = assembled$integration_grids_folds[[1L]], tolerance = 0)))
  layout <- sconjoint:::.scmix_dml_layout(assembled)
  nuisance <- sconjoint:::.scmix_dml_resolve_A_gh(
    assembled, p = layout$p, K = layout$K)
  expect_identical(nuisance$q, 0L)
  expect_false(assembled$eligible_for_ordinary_inference)
  expect_identical(assembled$analysis_signature, nested$analysis_signature)
  expect_true(all(nested$candidate_selection_gate_by_outer_fold))
  expect_true(all(assembled$optimization$candidate_selection_gate_by_fold))

  ## Disk-reloaded nested artifacts use portable states, never dead module
  ## pointers, when they are assembled again.
  nested_path <- tempfile(fileext = ".rds")
  saveRDS(nested, nested_path, version = 3)
  reloaded <- readRDS(nested_path)
  reassembled <- sconjoint:::scmix_assemble_nested(
    reloaded, require_optimization_gate = FALSE, diagnostic_only = TRUE)
  expect_equal(reassembled$mu_all_folds, assembled$mu_all_folds,
               tolerance = 1e-7)

  ## A continued-constant dominance failure is consumed by the same
  ## fail-closed stage gate during assembly; it cannot remain inference
  ## eligible even when diagnostic assembly is explicitly requested.
  bad_nested <- nested
  bad_continued <- bad_nested$tuning[[1L]]$refit$optimization
  bad_continued$nested_objective_gate <- list(pass = FALSE)
  bad_nested$tuning[[1L]]$refit$continued_constant_optimization <-
    bad_continued
  bad_assembled <- sconjoint:::scmix_assemble_nested(
    bad_nested, require_optimization_gate = FALSE, diagnostic_only = TRUE)
  expect_false(bad_assembled$optimization$continued_constant_gate_by_fold[1L])
  expect_false(bad_assembled$optimization$gate_by_fold[1L])
  expect_false(bad_assembled$eligible_for_ordinary_inference)
})
