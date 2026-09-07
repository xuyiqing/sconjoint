#!/usr/bin/env Rscript

## Outcome-free focused tests for the Ballard--Rosa v2.7 target stage.

options(stringsAsFactors = FALSE, warn = 1)

.test_script <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this test with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

root <- normalizePath(file.path(dirname(.test_script()), "..", "..", ".."),
                      mustWork = TRUE)
if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required")
pkgload::load_all(root, quiet = TRUE, export_all = FALSE)
sys.source(file.path(root, "applications", "br2017", "R",
                     "br_v27_contract.R"), envir = environment())
sys.source(file.path(root, "applications", "R",
                     "targeted_q1_inference.R"), envir = environment())
sys.source(file.path(root, "applications", "br2017", "R",
                     "br_v27_targets_contract.R"), envir = environment())

checks <- 0L
check <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}

protocol <- .br_v27t_validate_protocol(root, require_clean = FALSE)
registries <- .br_v27t_registries(root, protocol)
check(nrow(registries$targets) == 72L && nrow(registries$families) == 10L,
      "The frozen target/family registry counts changed")
mapping <- registries$target_types$status[
  match(registries$targets$target_type, registries$target_types$target_type)]
check(sum(mapping == "supported") == 66L && sum(mapping == "unavailable") == 6L,
      "Target coverage is not 66 represented plus six explicit AMEs")
unavailable <- registries$targets$target_id[mapping == "unavailable"]
check(length(unavailable) == 6L && all(grepl("^ame_rate_", unavailable)) &&
        identical(unique(registries$targets$multiplicity_family[
          mapping == "unavailable"]), "structural_ame"),
      "Unavailable targets are not exactly the six frozen structural AMEs")
check(all(table(registries$targets$multiplicity_family) > 0L) &&
        setequal(registries$targets$multiplicity_family,
                 registries$families$family_id),
      "A frozen multiplicity family is uncovered")

adapter_body <- paste(deparse(body(.br_v27t_run_dml_family)), collapse = "\n")
check(grepl('riesz_rule = "fixed_n_train_minus_three_quarters"',
            adapter_body, fixed = TRUE) &&
        grepl('riesz_numerical_rank_rule = "dimension_epsilon"',
              adapter_body, fixed = TRUE) &&
        grepl("nu_grid = NULL", adapter_body, fixed = TRUE) &&
        grepl("riesz_equation_tolerance = 0.2", adapter_body,
              fixed = TRUE) &&
        grepl("ridge_sensitivity_tolerance = 0.2", adapter_body,
              fixed = TRUE) &&
        grepl("allow_numeric_derivatives = FALSE", adapter_body,
              fixed = TRUE) &&
        grepl("multiplier_draws = 9999L", adapter_body, fixed = TRUE),
      "The inherited v2.7 target adapter changed its fixed DML contract")

mock_parent <- list(
  protocol_id = .br_v27t_parent_protocol_id,
  protocol_hash = paste(rep("b", 64L), collapse = ""),
  git = list(commit = paste(rep("c", 40L), collapse = "")),
  inputs = data.frame(input_id = "prepared", sha256 =
    paste(rep("d", 64L), collapse = ""), stringsAsFactors = FALSE))
old_run_commit <- paste(rep("a", 40L), collapse = "")
mock_upstream <- list(binding = list(
  protocol_id = mock_parent$protocol_id,
  protocol_hash = mock_parent$protocol_hash,
  git_commit = old_run_commit,
  application = "br2017", kind = "assembled", q = 1L, scope = "all",
  input_hashes = stats::setNames(mock_parent$inputs$sha256,
                                 mock_parent$inputs$input_id)))
check(identical(.br_v27t_assert_upstream_binding(
  mock_upstream, mock_parent, "assembled", 1L, "all"), old_run_commit),
  "An intact upstream checkpoint from the pre-target commit was rejected")
bad_upstream <- mock_upstream
bad_upstream$binding$protocol_hash <- paste(rep("e", 64L), collapse = "")
check(inherits(try(.br_v27t_assert_upstream_binding(
  bad_upstream, mock_parent, "assembled", 1L, "all"), silent = TRUE),
  "try-error"), "A checkpoint with the wrong parent protocol hash was accepted")

respondents <- sprintf("r%02d", 1:12)
pid <- rep(c(1, 2, 3, 4, 5, 6), 2)
synthetic_preflight <- list(prepared = list(respondent_meta = data.frame(
  respondent_id = respondents, pid7 = pid, stringsAsFactors = FALSE)))
synthetic_fit <- list(
  attr_names = c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
                 "rate_175_375", "rate_375P", "revenue_score"),
  respondent_id = respondents)
families <- lapply(registries$families$family_id, function(id) {
  .br_v27t_build_family(id, synthetic_fit, synthetic_preflight, registries)
})
names(families) <- registries$families$family_id
check(sum(vapply(families, function(x) length(x$targets), integer(1L))) == 66L,
      "Analytic family builders do not cover all 66 represented targets")
check(identical(families$structural_ame$status,
                .br_v27t_unavailable_status) &&
        !length(families$structural_ame$targets),
      "The mismatched AME estimand was not failed closed")
check(all(vapply(families[names(families) != "structural_ame"], function(x) {
  identical(names(x$targets), x$rows$target_id) &&
    identical(names(x$recipes), x$rows$target_id)
}, logical(1L))), "A supported family is not complete and registry ordered")
heterogeneity_families <- families[grepl(
  "^directional_heterogeneity_", names(families))]
check(all(vapply(heterogeneity_families, function(x) {
  length(x$primitive_registry) == nrow(x$rows) &&
    all(vapply(x$primitive_registry, inherits, logical(1L),
               what = "scmix_v22_target_spec")) &&
    all(vapply(x$recipes, function(recipe) {
      identical(sub(".*:", "", recipe$primitive_names), c("H_Z", "H_R"))
    }, logical(1L)))
}, logical(1L))),
"Directional heterogeneity does not use direct minimal H_Z/H_R primitives")

N <- length(respondents); p <- length(synthetic_fit$attr_names)
mu <- matrix(seq_len(N * p) / 100, N, p)
Sigma <- diag(seq(0.2, 0.5, length.out = p))
representatives <- list(
  position = families$average_preferences$targets$position_effect,
  mean = families$average_preferences$targets$mean_rate_L10,
  subgroup = families$progressivity_means$targets$
    mean_progressivity_top_bottom_democrat,
  choice = families$choice_probabilities$targets$choice_progressive_vs_flat,
  share = families$directional_shares$targets$share_progressivity_top_bottom,
  compensating = families$compensating_benefit_shares$targets$
    compensating_progressive_vs_flat,
  heterogeneity = families$directional_heterogeneity_progressivity$targets$
    heterogeneity_progressivity_top_bottom,
  mrs = families$mean_tradeoff_mrs$targets$mrs_bottom_rate_per_top_rate)
evaluated <- lapply(representatives, function(fun) fun(
  mu = mu, kappa = 0.1, Sigma = Sigma, Z = matrix(0, N, 1L),
  respondent_id = respondents, fold = 1L,
  attr_names = synthetic_fit$attr_names))
check(all(vapply(evaluated, function(x) {
  is.matrix(x$value) && nrow(x$value) == N &&
    is.array(x$d_mu) && dim(x$d_mu)[1L] == N &&
    is.matrix(x$d_kappa) && nrow(x$d_kappa) == N
}, logical(1L))), "A representative target lacks analytic rowwise derivatives")
check(isTRUE(evaluated$position$sigma_invariant) &&
        all(evaluated$position$d_kappa == 1) &&
        all(evaluated$position$d_mu == 0),
      "Position-effect callback is not the exact kappa functional")

finite_difference <- function(theta, kind, j, eps = 1e-6) {
  up <- down <- theta; up[j] <- up[j] + eps; down[j] <- down[j] - eps
  (.br_v27t_geometry(up, kind)$value -
     .br_v27t_geometry(down, kind)$value) / (2 * eps)
}
geometry_cases <- list(
  subgroup_ratio = c(0.2, 0.4),
  heterogeneity_share = c(0.3, 0.2),
  mrs = c(0.4, -0.3))
check(all(vapply(names(geometry_cases), function(kind) {
  theta <- geometry_cases[[kind]]
  analytic <- .br_v27t_geometry(theta, kind)$gradient
  numeric <- vapply(seq_along(theta), function(j)
    finite_difference(theta, kind, j), numeric(1L))
  max(abs(analytic - numeric)) < 1e-6
}, logical(1L))), "A smooth target geometry has an incorrect derivative")

mock_if <- matrix(seq(-1, 1, length.out = 20), ncol = 1L,
                  dimnames = list(respondents[1:20 %% N + 1L], "mock"))
mock_if <- sweep(mock_if, 2L, colMeans(mock_if), `-`)
mock_dml <- structure(list(
  estimate = c(mock = 0.3), plugin_estimate = c(mock = 0.29),
  influence = mock_if, respondent_id = paste0("m", seq_len(nrow(mock_if))),
  fold_details = replicate(5L, list(
    honest_information_scaled_riesz_defect = c(mock = 0.1),
    riesz_equation_relative_residual = c(mock = 0.1),
    ridge_relative_sensitivity = c(mock = 0.1)), simplify = FALSE),
  derivative_refinement_error = matrix(1e-8, 5L, 1L,
    dimnames = list(NULL, "mock")),
  honest_riesz_null_space_defect_max = c(mock = 1e-8)),
  class = c("scmix_dml", "list"))
mock_recipe <- list(mock = .br_v27t_recipe(
  "mock", "mean", "mock", "identity"))
mock_row <- data.frame(target_index = 1L, target_id = "mock",
                       target_type = "mean", stringsAsFactors = FALSE)
mock_transformed <- .br_v27t_transform_dml(
  mock_dml, mock_recipe, mock_row, fit_gate_pass = TRUE)
check(isTRUE(mock_transformed$summary$primitive_diagnostic_gate_pass) &&
        !mock_transformed$summary$rapid_available_gate_pass &&
        !mock_transformed$summary$target_gate_pass &&
        !mock_transformed$summary$formal_release &&
        identical(mock_transformed$summary$status,
                  "diagnostic_computed_formal_withheld"),
      "Primitive diagnostics were confused with an available target-gate pass")

IF <- cbind(a = seq(-1, 1, length.out = 21),
            b = cos(seq(0, 2 * pi, length.out = 21)))
IF <- sweep(IF, 2L, colMeans(IF), `-`)
m1 <- .br_v27t_family_multiplier(IF, c(a = 0.2, b = 0.4),
                                  seed = 9876L, draws = 99L,
                                  block_size = 17L)
m2 <- .br_v27t_family_multiplier(IF, c(a = 0.2, b = 0.4),
                                  seed = 9876L, draws = 99L,
                                  block_size = 17L)
check(identical(m1$centered_draw_hash, m2$centered_draw_hash) &&
        isTRUE(m1$common_across_complete_frozen_family) &&
        identical(m1$included_targets, c("a", "b")) &&
        is.finite(m1$simultaneous_critical_value),
      "Common-family multiplier construction is not deterministic")

td <- tempfile("br-v27-target-checkpoint-")
dir.create(td)
path <- file.path(td, "family.rds")
binding <- list(protocol_id = .br_v27t_protocol_id, family_id = "mock")
.br_v27t_save_atomic(list(message = "test"), path, binding)
got <- .br_v27t_read_checkpoint(path, binding)
check(identical(got$payload$message, "test") &&
        file.exists(paste0(path, ".sha256")),
      "Target checkpoint round trip failed")
check(inherits(try(.br_v27t_save_atomic(list(), path, binding), silent = TRUE),
               "try-error"), "Immutable target checkpoint was overwritten")
check(inherits(try(.br_v27t_read_checkpoint(
  path, c(binding, list(rank = 1L))), silent = TRUE), "try-error"),
  "Target checkpoint accepted a mismatched binding")
wrong <- readRDS(path)
wrong$binding$family_id <- "foreign"
saveRDS(wrong, path, version = 3L)
writeLines(paste(.br_v27t_sha256(path), basename(path)),
           paste0(path, ".sha256"), useBytes = TRUE)
check(inherits(try(.br_v27t_read_checkpoint(
  path, binding, payload = FALSE), silent = TRUE), "try-error"),
  "Hash-valid metadata-only read accepted a mismatched binding")
unlink(td, recursive = TRUE, force = TRUE)

runner_text <- paste(readLines(file.path(
  root, "applications", "br2017", "R", "08_run_targets_v27.R"),
  warn = FALSE), collapse = "\n")
check(grepl(".br_v27t_verify_upstream", runner_text, fixed = TRUE) &&
        !grepl('.br_v27_checkpoint_path(run_dir, "assembled", 2',
               runner_text, fixed = TRUE) &&
        grepl("q2_used_for_target_selection = FALSE", runner_text,
              fixed = TRUE),
      "Target runner does not preserve the q1-only information boundary")
check(any(grepl("all 72", readLines(file.path(
  root, "applications", "br2017", "protocol", "v27-targets", "README.md"),
  warn = FALSE), fixed = TRUE)),
  "Protocol README does not require complete 72-target export coverage")

cat(sprintf("Ballard--Rosa v2.7 target-stage checks passed: %d\n", checks))
