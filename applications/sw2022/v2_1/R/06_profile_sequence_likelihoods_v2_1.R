#!/usr/bin/env Rscript

## Saha--Weeks v2.1 nuisance-reoptimized descriptive penalized-criterion
## profile sequences with unpenalized likelihood overlays. This is downstream,
## outcome-informed assessment only. It is not a literal profile likelihood
## and supplies no likelihood-ratio critical values or formal inference.

options(stringsAsFactors = FALSE, warn = 1)
`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  out <- list(direction = "all", force = FALSE, reviewed_launch = FALSE)
  for (arg in x) {
    if (grepl("^--direction=", arg)) {
      out$direction <- sub("^--direction=", "", arg)
    } else if (grepl("^--force=", arg)) {
      out$force <- tolower(sub("^--force=", "", arg)) %in%
        c("1", "true", "yes")
    } else if (grepl("^--reviewed-launch=", arg)) {
      out$reviewed_launch <- tolower(sub("^--reviewed-launch=", "", arg)) %in%
        c("1", "true", "yes")
    } else stop("Unknown argument: ", arg, call. = FALSE)
  }
  out
}

cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
runner_path <- .script_file()
root <- normalizePath(file.path(dirname(runner_path), "..", "..", "..",
                                ".."), mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app)
config_path <- file.path(
  app, "v2_1", "config", "profile_sequence_config_v2_1.R")
helper_path <- file.path(
  app, "v2_1", "R", "profile_sequence_helpers_v2_1.R")
authorization_creator_path <- file.path(
  app, "v2_1", "R", "00_create_profile_sequence_authorization_v2_1.R")
primary_contract_path <- file.path(
  app, "v2_1", "sensitivity", "R", "reported_primary_contract_v2_1.R")
parent_config_path <- file.path(
  app, "v2_1", "config", "analysis_config_v2_1.R")
postfit_config_path <- file.path(
  app, "v2_1", "config", "postfit_evidence_config_v2_1.R")
source(config_path, local = FALSE)
source(helper_path, local = FALSE)
source(primary_contract_path, local = FALSE)
source(parent_config_path, local = FALSE)
source(postfit_config_path, local = FALSE)
source(file.path(root, "R", "provenance.R"), local = FALSE)

directions <- names(sw_v21_profile_config$grids)
if (!identical(cli$direction, "all") && !cli$direction %in% directions) {
  stop("--direction must be all or one of: ",
       paste(directions, collapse = ", "), call. = FALSE)
}
if (!isTRUE(cli$reviewed_launch)) {
  stop("No fit was started. After review and authorization, rerun with --reviewed-launch=true.",
       call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
context <- .sw_v21_validate_reported_primary(
  sw_v21_profile_config$input$reported_primary_pointer,
  sw_v21_config, load_fits = TRUE)
if (!identical(context$pointer$reported_primary,
               sw_v21_profile_config$fixed_fit$require_reported_primary)) {
  stop("The reported primary is not the required selected q=1 learner. No fit was started.",
       call. = FALSE)
}
generation_paths <- .sw_v21_profile_generation_paths(
  root, app, config_path, helper_path, runner_path,
  authorization_creator_path, primary_contract_path, parent_config_path,
  postfit_config_path, context)
generation_md5 <- .sc_md5_paths(generation_paths)
runtime_signature <- .sc_runtime_signature(file.path(root, "DESCRIPTION"))
if (!identical(runtime_signature, context$pointer$runtime_signature)) {
  stop("The current runtime differs from the reported-primary runtime. No fit was started.",
       call. = FALSE)
}
authorization <- if (file.exists(sw_v21_profile_config$authorization_file)) {
  tryCatch(readRDS(sw_v21_profile_config$authorization_file),
           error = function(e) NULL)
} else NULL
if (!.sw_v21_profile_authorization_valid(
    authorization, sw_v21_profile_config, config_path, generation_md5,
    runtime_signature, context)) {
  stop("The profile runner is fail-closed: a separately reviewed authorization must bind the live pointer, manifest, all hashes, runtime, fixed learner/tuning/sieve, and descriptive-only status. No fit was started.",
       call. = FALSE)
}
authorization_md5 <- unname(tools::md5sum(
  sw_v21_profile_config$authorization_file))

prepared <- readRDS(sw_v21_profile_config$input$prepared)
dx_raw <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z_raw <- as.matrix(prepared[[sw_v21_profile_config$input$primary_Z]])
rid <- as.character(prepared$respondent_id)
ids <- unique(rid)
first <- !duplicated(rid)
if (!identical(nrow(dx_raw), 3573L) || !identical(length(ids), 1191L) ||
    nrow(Z_raw) != nrow(dx_raw) || length(y) != nrow(dx_raw) ||
    length(rid) != nrow(dx_raw) || any(!is.finite(dx_raw)) ||
    any(!is.finite(Z_raw)) || any(!y %in% c(0, 1))) {
  stop("The frozen Saha--Weeks profile sample is malformed.", call. = FALSE)
}
full <- context$full
refit <- full$refit
selected_spec <- full$selected
expected <- sw_v21_profile_config$fixed_fit
if (!identical(as.integer(selected_spec$q), expected$q) ||
    !identical(selected_spec$mean_family, expected$mean_family) ||
    !identical(as.integer(selected_spec$hidden), expected$hidden) ||
    !identical(as.numeric(selected_spec$weight_decay), expected$weight_decay) ||
    !identical(selected_spec$integration, expected$integration) ||
    !identical(as.integer(selected_spec$n_nodes), expected$n_nodes) ||
    !inherits(refit$network_state, "scmix_network_state") ||
    !identical(refit$network_state$architecture$q, 1L)) {
  stop("The full-sample reported learner/tuning/sieve does not match the frozen profile contract.",
       call. = FALSE)
}

ns_fun <- function(name) get(name, envir = asNamespace("sconjoint"),
                             inherits = FALSE)
apply_z <- ns_fun(".sc_apply_z_transform")
project_bounds <- ns_fun(".sc_mixed_project_parameters")
nll_fun <- ns_fun(".sc_mixed_nll")
penalty_fun <- ns_fun(".sc_mixed_penalty")
sequence_fun <- ns_fun(".sc_comp_sequence_loglik")
predict_internal <- ns_fun(".sc_predict_beta")
bound_fun <- ns_fun(".sc_mixed_bound_diagnostics")

state <- refit$network_state
scale <- as.numeric(state$architecture$coefficient_scale)
names(scale) <- state$coefficient_names
Z_fit <- apply_z(Z_raw, state$preprocessing$Z)
dx_fit <- sweep(dx_raw, 2L, scale, `/`)
grid <- refit$integration_grid
if (!identical(as.integer(ncol(grid$U)), 1L) ||
    !identical(as.integer(nrow(grid$U)), expected$n_nodes)) {
  stop("The reported-primary integration grid is not frozen q=1 GH31.",
       call. = FALSE)
}

opt <- sw_v21_profile_config$optimizer
dev <- torch::torch_device(opt$device)
rf <- factor(rid, levels = ids)
dx_t <- torch::torch_tensor(dx_fit, dtype = torch::torch_float(), device = dev)
z_t <- torch::torch_tensor(Z_fit, dtype = torch::torch_float(), device = dev)
y_t <- torch::torch_tensor(y, dtype = torch::torch_float(), device = dev)
idx_t <- torch::torch_tensor(as.integer(rf), dtype = torch::torch_long(),
                             device = dev)
z_resp_t <- torch::torch_tensor(Z_fit[first, , drop = FALSE],
                                dtype = torch::torch_float(), device = dev)
U_t <- torch::torch_tensor(grid$U, dtype = torch::torch_float(), device = dev)
w_t <- torch::torch_tensor(grid$w, dtype = torch::torch_float(), device = dev)
logw_t <- torch::torch_tensor(log(grid$w), dtype = torch::torch_float(),
                              device = dev)
scale_t <- torch::torch_tensor(scale, dtype = torch::torch_float(), device = dev)
base_A_raw <- as.matrix(refit$A)

.contrast <- function(direction) {
  d <- sw_v21_profile_config$grids[[direction]]$contrast
  if (is.null(d)) return(NULL)
  torch::torch_tensor(as.numeric(d), dtype = torch::torch_float(),
                      device = dev)
}

.metric <- function(net, direction) {
  .sw_v21_profile_metric_tensor(
    net, direction, z_resp_t, scale_t, U_t, w_t,
    .contrast(direction),
    isTRUE(sw_v21_profile_config$grids[[direction]]$position_neutral))
}

.project <- function(net, direction, target) {
  project_bounds(
    net, coefficient_scale = scale,
    alpha_bound = state$architecture$alpha_bound,
    a_bound = state$architecture$a_bound,
    weight_bound = state$architecture$weight_bound)
  .sw_v21_profile_project_constraint(
    net, direction, target, z_resp_t, scale_t, U_t, w_t,
    .contrast(direction),
    isTRUE(sw_v21_profile_config$grids[[direction]]$position_neutral),
    alpha_bound = state$architecture$alpha_bound,
    base_A_raw = base_A_raw, tolerance = opt$target_tol)
}

.jitter <- function(net, start, seed) {
  if (start == 1L) return(invisible(net))
  torch::with_torch_manual_seed({
    torch::with_no_grad({
      for (p in net$parameters) {
        magnitude <- as.numeric(torch::torch_mean(
          torch::torch_abs(p$detach()))$cpu()$item())
        p$add_(torch::torch_randn_like(p) * opt$jitter_fraction *
                 max(0.05, magnitude))
      }
    })
  }, seed = as.integer(seed))
  invisible(net)
}

.fit_start <- function(direction, target, start, seed) {
  started <- Sys.time()
  net <- sconjoint::scmix_restore_network(state, device = opt$device)
  .jitter(net, start, seed)
  initial_projection <- .project(net, direction, target)
  if (!initial_projection$pass) {
    stop("Initial profile projection missed its target.", call. = FALSE)
  }
  optimizer <- torch::optim_adam(net$parameters, lr = opt$learning_rate,
                                 weight_decay = 0)
  trace <- vector("list", ceiling(opt$n_epochs / opt$check_every) + 1L)
  z <- 0L
  for (epoch in seq_len(opt$n_epochs)) {
    net$train(); optimizer$zero_grad()
    nll <- nll_fun(net, dx_t, z_t, y_t, U_t, logw_t, idx_t, length(ids))
    loss <- nll + penalty_fun(net, selected_spec$weight_decay)
    loss$backward(); optimizer$step()
    projection <- .project(net, direction, target)
    if (!projection$pass) {
      stop("Constraint projection failed at epoch ", epoch, call. = FALSE)
    }
    if (epoch %% opt$check_every == 0L || epoch == opt$n_epochs) {
      net$eval()
      attained <- as.numeric(.metric(net, direction)$detach()$cpu()$item())
      value <- as.numeric(torch::with_no_grad(
        nll_fun(net, dx_t, z_t, y_t, U_t, logw_t, idx_t,
                length(ids)) + penalty_fun(net, selected_spec$weight_decay)
      )$item())
      z <- z + 1L
      trace[[z]] <- data.frame(
        epoch = epoch, penalized_nll = value,
        target_attained = attained, target_error = attained - target,
        stringsAsFactors = FALSE)
    }
  }
  trace <- do.call(rbind, trace[seq_len(z)])
  net$eval(); final_projection <- .project(net, direction, target)
  loss_fun <- function() nll_fun(
    net, dx_t, z_t, y_t, U_t, logw_t, idx_t, length(ids)) +
    penalty_fun(net, selected_spec$weight_decay)
  gradient <- .sw_v21_profile_projected_gradient(
    net, loss_fun, function() .metric(net, direction))
  final_nll <- as.numeric(torch::with_no_grad(nll_fun(
    net, dx_t, z_t, y_t, U_t, logw_t, idx_t, length(ids)))$item())
  penalty <- as.numeric(torch::with_no_grad(
    penalty_fun(net, selected_spec$weight_decay))$item())
  internal_mu <- predict_internal(net, Z_fit)
  kappa <- as.numeric(net$get_kappa()$detach()$cpu()$item())
  bounds <- bound_fun(
    net, internal_mu, kappa, scale,
    mu_bound = state$architecture$mu_bound,
    kappa_bound = state$architecture$kappa_bound,
    alpha_bound = state$architecture$alpha_bound,
    a_bound = state$architecture$a_bound,
    weight_bound = state$architecture$weight_bound)
  bound_active <- any(unlist(bounds[c(
    "mu_active", "alpha_active", "kappa_active", "a_active",
    "weight_active")], use.names = FALSE))
  relative_change <- if (nrow(trace) >= 2L) {
    abs(diff(tail(trace$penalized_nll, 2L))) /
      max(1, abs(tail(trace$penalized_nll, 1L)))
  } else Inf
  gate <- isTRUE(final_projection$pass) && is.finite(final_nll + penalty) &&
    !bound_active && isTRUE(gradient$pass) &&
    gradient$projected_gradient_norm <= opt$projected_gradient_tol &&
    relative_change <= opt$opt_tol
  sequence_loglik <- sequence_fun(
    net, dx_fit, y, Z_fit, rid, grid, device = opt$device)
  captured <- get(".scmix_capture_network_state",
                  envir = asNamespace("sconjoint"))(
    net, p = ncol(dx_raw), p_Z = ncol(Z_raw), q = 1L,
    hidden = selected_spec$hidden, mean_family = selected_spec$mean_family,
    mu_bound = state$architecture$mu_bound,
    kappa_bound = state$architecture$kappa_bound,
    alpha_bound = state$architecture$alpha_bound,
    a_bound = state$architecture$a_bound,
    weight_bound = state$architecture$weight_bound,
    coefficient_scale = scale,
    z_transform = state$preprocessing$Z,
    dx_transform = state$preprocessing$deltaX,
    coefficient_names = state$coefficient_names,
    moderator_names = state$moderator_names,
    integration_grid = grid,
    analysis_signature = refit$analysis_signature,
    scope = paste("full-sample nuisance-reoptimized descriptive",
                  "penalized-criterion profile sequence with unpenalized",
                  "likelihood overlay; fixed reported learner/tuning/sieve"))
  list(
    start = start, seed = seed, gate_pass = gate,
    target = target, target_attained = final_projection$attained,
    target_error = final_projection$target_error,
    mean_sequence_loglik = -final_nll,
    penalized_objective = -(final_nll + penalty),
    penalty = penalty, projected_gradient = gradient,
    last_relative_change = relative_change, bounds = bounds,
    bound_active = bound_active, trace = trace,
    sequence_loglik = sequence_loglik, network_state = captured,
    elapsed_seconds = as.numeric(difftime(
      Sys.time(), started, units = "secs")))
}

.fit_point <- function(direction, target, point_index) {
  started <- Sys.time()
  starts <- vector("list", opt$n_starts)
  for (s in seq_len(opt$n_starts)) {
    message(direction, " target ", format(target, digits = 8),
            ": start ", s, "/", opt$n_starts)
    seed <- as.integer(opt$seed + 100000L * match(direction, directions) +
                         1000L * point_index + s)
    starts[[s]] <- .fit_start(direction, target, s, seed)
  }
  selection <- .sw_v21_profile_select_eligible_start(starts)
  chosen <- selection$index
  best <- selection$best
  list(
    schema_version = "sw2022-v2.1-profile-point-v2",
    direction = direction, target = target, selected_start = chosen,
    nuisance_reoptimization_gate_pass = TRUE,
    all_start_diagnostics = lapply(starts, function(x) x[setdiff(
      names(x), c("network_state", "sequence_loglik"))]),
    sequence_loglik = best$sequence_loglik,
    mean_sequence_loglik = best$mean_sequence_loglik,
    penalized_objective = best$penalized_objective,
    penalty = best$penalty,
    target_attained = best$target_attained,
    target_error = best$target_error,
    projected_gradient = best$projected_gradient,
    last_relative_change = best$last_relative_change,
    bounds = best$bounds,
    bound_active = best$bound_active,
    elapsed_seconds = as.numeric(difftime(
      Sys.time(), started, units = "secs")),
    network_state = best$network_state,
    profile_specification = list(
      config_version = sw_v21_profile_config$version,
      direction = direction, target = target,
      generation_input_md5 = generation_md5,
      runtime_signature = runtime_signature,
      authorization_md5 = authorization_md5,
      reported_primary_lock_md5 = context$lock_md5,
      reported_primary = context$pointer$reported_primary,
      selected_specification = selected_spec,
      learner_tuning_sieve_fixed = TRUE,
      retuning_performed = FALSE,
      penalized_nuisance_reoptimization = TRUE,
      artifact_kind = "descriptive penalized-criterion profile sequence",
      literal_likelihood_profile = FALSE,
      unpenalized_sequence_likelihood_overlay = TRUE,
      descriptive_only = TRUE, formal_inference_available = FALSE,
      formal_test = FALSE, likelihood_ratio_critical_values = FALSE,
      outcome_blind = FALSE))
}

base_net <- sconjoint::scmix_restore_network(state, device = opt$device)
base_mu <- sconjoint::scmix_predict_network(
  state, Z_raw[first, , drop = FALSE], input = "raw", output = "raw",
  device = opt$device)
base_centers <- c(
  kappa = refit$kappa,
  female_vs_male_mean = .sw_v21_profile_metric_from_arrays(
    "female_vs_male_mean", base_mu, refit$kappa, refit$A,
    grid$U, grid$w,
    sw_v21_profile_config$grids$female_vs_male_mean$contrast),
  active_covariance_eigenvalue = sum(refit$A^2),
  headline_contest_probability = .sw_v21_profile_metric_from_arrays(
    "headline_contest_probability", base_mu, refit$kappa, refit$A,
    grid$U, grid$w,
    sw_v21_profile_config$grids$headline_contest_probability$contrast,
    position_neutral = TRUE))
rm(base_net)

run_directions <- if (identical(cli$direction, "all")) directions else
  cli$direction
dir.create(sw_v21_profile_config$output_root, recursive = TRUE,
           showWarnings = FALSE)
for (direction in run_directions) {
  direction_started <- Sys.time()
  target_grid <- .sw_v21_profile_grid(
    base_centers[[direction]], direction, sw_v21_profile_config)
  direction_dir <- file.path(sw_v21_profile_config$output_root, direction)
  point_dir <- file.path(direction_dir, "checkpoints")
  dir.create(point_dir, recursive = TRUE, showWarnings = FALSE)
  points <- vector("list", length(target_grid))
  for (j in seq_along(target_grid)) {
    path <- file.path(point_dir, sprintf("point_%02d.rds", j))
    if (file.exists(path) && !isTRUE(cli$force)) {
      point <- readRDS(path)
      if (!.sw_v21_profile_checkpoint_valid(
          point, direction, target_grid[[j]], generation_md5,
          runtime_signature, authorization_md5, context, selected_spec,
          ids, sw_v21_profile_config)) {
        stop("Stale profile checkpoint: ", path,
             ". Use --force=true only after reviewing the mismatch.",
             call. = FALSE)
      }
      message("checkpoint: loading ", path)
    } else {
      point <- .fit_point(direction, target_grid[[j]], j)
      if (!.sw_v21_profile_checkpoint_valid(
          point, direction, target_grid[[j]], generation_md5,
          runtime_signature, authorization_md5, context, selected_spec,
          ids, sw_v21_profile_config)) {
        stop("A new profile checkpoint failed its own stamp.", call. = FALSE)
      }
      .sw_v21_profile_atomic_save(point, path, portable = TRUE)
      message("checkpoint: wrote ", path)
    }
    points[[j]] <- point
    if (!.sw_v21_same_md5(.sw_v21_md5(context$lock_paths),
                          context$lock_md5)) {
      stop("A reported-primary lock file changed during profile fitting.",
           call. = FALSE)
    }
  }
  gates <- vapply(points, `[[`, logical(1L),
                  "nuisance_reoptimization_gate_pass")
  direction_verified <- all(gates)
  if (!direction_verified) {
    stop(
      "At least one profile point failed nuisance reoptimization. No direction result or manifest was emitted.",
      call. = FALSE)
  }
  ll <- do.call(cbind, lapply(points, `[[`, "sequence_loglik"))
  rownames(ll) <- ids
  mean_ll <- colMeans(ll)
  profile_table <- data.frame(
    grid = target_grid,
    mean_sequence_loglik = mean_ll,
    total_loglik_difference = length(ids) * (mean_ll - max(mean_ll)),
    stringsAsFactors = FALSE)
  profile <- structure(list(
    table = profile_table,
    direction = sw_v21_profile_config$grids[[direction]]$label,
    kind = paste(
      "descriptive penalized-criterion profile sequence with an",
      "unpenalized complete-sequence likelihood overlay"),
    nuisance_reoptimized = TRUE,
    sieve_tuning_fixed = TRUE,
    penalized_criterion_profile_sequence = TRUE,
    unpenalized_likelihood_overlay = TRUE,
    literal_likelihood_profile = FALSE,
    verified_penalized_criterion_profile_sequence = direction_verified,
    rank_boundary = isTRUE(
      sw_v21_profile_config$grids[[direction]]$rank_boundary),
    likelihood_ratio_critical_values = FALSE,
    provenance = paste(
      "Reported-primary pointer/manifest/runtime/hash locked; selected",
      "full-sample q=1 ReLU learner, penalty, GH31 integration, preprocessing,",
      "and sieve held fixed; remaining nuisance components reoptimized under",
      "the fixed penalized criterion at each finite grid point; the",
      "unpenalized likelihood values are overlays, not a profile likelihood."),
    disclaimer = paste(
      "This is not a likelihood profile and receives no likelihood-ratio",
      "critical value or formal-inference interpretation.")),
    class = c("sw_v21_penalized_criterion_profile_sequence", "list"))
  table <- profile$table
  table$direction <- direction
  table$center <- base_centers[[direction]]
  table$target_attained <- vapply(points, `[[`, numeric(1L),
                                  "target_attained")
  table$target_error <- vapply(points, `[[`, numeric(1L), "target_error")
  table$penalized_objective <- vapply(points, `[[`, numeric(1L),
                                      "penalized_objective")
  table$penalized_objective_difference <-
    table$penalized_objective - max(table$penalized_objective)
  table$penalty <- vapply(points, `[[`, numeric(1L), "penalty")
  table$elapsed_seconds <- vapply(points, `[[`, numeric(1L),
                                  "elapsed_seconds")
  table$projected_gradient_norm <- vapply(points, function(x) {
    x$projected_gradient$projected_gradient_norm
  }, numeric(1L))
  table$nuisance_reoptimization_gate_pass <- gates
  table$verified_penalized_criterion_profile_sequence <- direction_verified
  table$literal_likelihood_profile <- FALSE
  table$unpenalized_likelihood_overlay <- TRUE
  table$descriptive_only <- TRUE
  table$formal_inference_available <- FALSE
  table$likelihood_ratio_critical_values <- FALSE
  table$penalized_nuisance_reoptimization <- TRUE
  result <- list(
    schema_version = "sw2022-v2.1-profile-direction-v2",
    direction = direction, center = base_centers[[direction]],
    grid = target_grid, profile = profile, table = table,
    checkpoint_paths = stats::setNames(
      file.path(point_dir, sprintf("point_%02d.rds", seq_along(points))),
      sprintf("point_%02d", seq_along(points))),
    all_nuisance_reoptimization_gates_pass = direction_verified,
    verified_penalized_criterion_profile_sequence = direction_verified,
    literal_likelihood_profile = FALSE,
    unpenalized_likelihood_overlay = TRUE,
    learner_tuning_sieve_fixed = TRUE, retuning_performed = FALSE,
    penalized_nuisance_reoptimization = TRUE,
    formal_inference_available = FALSE, formal_test = FALSE,
    likelihood_ratio_critical_values = FALSE, outcome_blind = FALSE,
    generation_input_md5 = generation_md5,
    runtime_signature = runtime_signature,
    authorization_md5 = authorization_md5,
    reported_primary_lock_md5 = context$lock_md5,
    elapsed_seconds = as.numeric(difftime(
      Sys.time(), direction_started, units = "secs")),
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  result_path <- file.path(direction_dir, "profile_sequence.rds")
  table_path <- file.path(direction_dir, "profile_sequence.csv")
  .sw_v21_profile_atomic_save(result, result_path, portable = TRUE)
  .sw_v21_profile_write_csv(table, table_path)
  artifacts <- c(
    profile_sequence.rds = result_path,
    profile_sequence.csv = table_path,
    stats::setNames(result$checkpoint_paths,
                    file.path("checkpoints", basename(result$checkpoint_paths))))
  artifact_md5 <- unname(tools::md5sum(artifacts))
  names(artifact_md5) <- names(artifacts)
  manifest <- list(
    schema_version = "sw2022-v2.1-profile-direction-manifest-v2",
    config_version = sw_v21_profile_config$version,
    direction = direction,
    verified_penalized_criterion_profile_sequence = direction_verified,
    literal_likelihood_profile = FALSE,
    unpenalized_likelihood_overlay = TRUE,
    all_nuisance_reoptimization_gates_pass = direction_verified,
    learner_tuning_sieve_fixed = TRUE, retuning_performed = FALSE,
    penalized_nuisance_reoptimization = TRUE,
    unpenalized_complete_sequence_likelihood_reported = TRUE,
    descriptive_only = TRUE, formal_inference_available = FALSE,
    formal_test = FALSE, likelihood_ratio_critical_values = FALSE,
    outcome_blind = FALSE, input_paths = generation_paths,
    generation_input_md5 = generation_md5,
    runtime_signature = runtime_signature,
    authorization_md5 = authorization_md5,
    reported_primary_lock_md5 = context$lock_md5,
    elapsed_seconds = result$elapsed_seconds,
    artifacts = artifact_md5,
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  .sw_v21_profile_atomic_save(
    manifest, file.path(direction_dir, "manifest.rds"), portable = FALSE)
  message("completed descriptive penalized-criterion profile direction: ",
          direction)
}
cat(paste(
  "Completed requested descriptive penalized-criterion profile sequences",
  "with unpenalized likelihood overlays. These are not likelihood profiles;",
  "formal inference remains unavailable.\n"))
