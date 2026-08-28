#!/usr/bin/env Rscript

## Design-specific structural misspecification experiments for Saha--Weeks.
##
## The observed-data estimator remains the normal, common-covariance mixed
## logit.  This runner generates choices on the fielded contrast sequences
## from calibrated alternatives, refits that primary normal estimator, and
## records plug-in bias and stability.  It does not fit or identify a second
## structural model.  Every output is isolated under sensitivity_analysis.

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(args) {
  out <- list(profile = "smoke", scenarios = "all", replications = "0",
              force = "false")
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg,
                                       call. = FALSE)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", bits[[1L]], fixed = TRUE)
    if (!key %in% names(out)) stop("Unknown argument --", bits[[1L]],
                                   call. = FALSE)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(out$force) %in% c("1", "true", "yes")
  out$replications <- as.integer(out$replications)
  if (is.na(out$replications) || out$replications < 0L) {
    stop("--replications must be zero (profile default) or a positive integer.",
         call. = FALSE)
  }
  out
}

.atomic_save <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(path)
}

.run_or_load <- function(path, force, fun, validator = NULL) {
  if (file.exists(path) && !isTRUE(force)) {
    message("checkpoint: loading ", path)
    value <- readRDS(path)
    if (!is.null(validator) && !isTRUE(validator(value))) {
      stop("Stale misspecification checkpoint: ", path,
           ". Rerun it with --force=true.", call. = FALSE)
    }
    return(value)
  }
  value <- fun()
  .atomic_save(value, path)
  message("checkpoint: wrote ", path)
  value
}

.unit_contrast <- function(coefficient_names, ...) {
  out <- stats::setNames(numeric(length(coefficient_names)), coefficient_names)
  entries <- list(...)
  for (entry in entries) out[[entry[[1L]]]] <-
    out[[entry[[1L]]]] + entry[[2L]]
  unname(out)
}

.qoi_definitions <- function(coefficient_names, analysis_config) {
  e <- function(name, value = 1) list(name, value)
  u <- function(...) .unit_contrast(coefficient_names, ...)
  tau <- list(
    female_vs_male = u(e("cand_genderMale", -1)),
    run_yes_vs_no = u(e("cand_runYes", 1)),
    talent_assertive_vs_empathetic = u(e("cand_talentEmpathetic", -1)),
    talent_collaborative_vs_empathetic = u(
      e("cand_talentCollaborative", 1), e("cand_talentEmpathetic", -1)),
    talent_determined_vs_empathetic = u(
      e("cand_talentDetermined.to.Succeed", 1),
      e("cand_talentEmpathetic", -1)),
    talent_good_communicator_vs_empathetic = u(
      e("cand_talentGood.Communicator", 1),
      e("cand_talentEmpathetic", -1)),
    talent_hard_working_vs_empathetic = u(
      e("cand_talentHard.Working", 1), e("cand_talentEmpathetic", -1)),
    talent_tough_negotiator_vs_empathetic = u(
      e("cand_talentTough.Negotiator", 1),
      e("cand_talentEmpathetic", -1)),
    agenda_moderate_vs_very_few = u(e("cand_agendaModerate.Changes", 1)),
    agenda_complete_vs_very_few = u(e("cand_agendaComplete.Overhaul", 1)),
    one_child_vs_none = u(e("cand_child1.child", 1)),
    two_children_vs_none = u(e("cand_child2.children", 1)),
    three_children_vs_none = u(e("cand_child3.children", 1))
  )
  list(
    tau = tau,
    choice = lapply(analysis_config$qoi$contests, as.numeric),
    sign = tau[c("female_vs_male", "talent_hard_working_vs_empathetic",
                 "agenda_moderate_vs_very_few",
                 "agenda_complete_vs_very_few")],
    heterogeneity = tau[c("female_vs_male",
                          "agenda_moderate_vs_very_few",
                          "agenda_complete_vs_very_few")]
  )
}

.map_meta <- function(prepared, ids) {
  meta <- as.data.frame(prepared$respondent_meta)
  if (anyDuplicated(meta$respondent_id) ||
      !setequal(as.character(meta$respondent_id), ids)) {
    stop("respondent_meta does not uniquely cover the primary respondents.",
         call. = FALSE)
  }
  meta <- meta[match(ids, as.character(meta$respondent_id)), , drop = FALSE]
  party <- ifelse(grepl("Republican", meta$party), "Republican",
    ifelse(grepl("Independent", meta$party), "Independent",
      ifelse(grepl("Democrat", meta$party), "Democrat", NA_character_)))
  gender <- tolower(trimws(meta$respondent_gender))
  if (anyNA(party) || any(!gender %in% c("female", "male"))) {
    stop("Party or respondent gender could not be mapped.", call. = FALSE)
  }
  list(party = party, gender = gender, meta = meta)
}

.make_view <- function(mu_task, A, kappa, respondent_id, Z, signature = NULL) {
  list(
    respondent_id = respondent_id, Z = Z, attr_names = colnames(mu_task),
    full_fit = list(mu = mu_task, A = A, Sigma = tcrossprod(A),
                    kappa = kappa),
    analysis_signature = signature
  )
}

.extract_qoi <- function(view, definitions, party, gender, choice_nodes) {
  rid <- unique(as.character(view$respondent_id))
  first <- match(rid, as.character(view$respondent_id))
  mu <- as.matrix(view$full_fit$mu)[first, , drop = FALSE]
  ans <- c(kappa = as.numeric(view$full_fit$kappa))
  raw <- colMeans(mu)
  names(raw) <- paste0("theta_raw:", colnames(mu))
  ans <- c(ans, raw)
  tau <- vapply(definitions$tau, function(d) sum(d * colMeans(mu)), numeric(1L))
  names(tau) <- paste0("tau:", names(tau))
  ans <- c(ans, tau)
  for (g in c("Democrat", "Independent", "Republican")) {
    tg <- vapply(definitions$tau, function(d)
      sum(d * colMeans(mu[party == g, , drop = FALSE])), numeric(1L))
    names(tg) <- paste("tau_party", tolower(g), names(tg), sep = ":")
    ans <- c(ans, tg)
  }
  for (g in c("female", "male")) {
    tg <- vapply(definitions$tau, function(d)
      sum(d * colMeans(mu[gender == g, , drop = FALSE])), numeric(1L))
    names(tg) <- paste("tau_respondent_gender", g, names(tg), sep = ":")
    ans <- c(ans, tg)
  }
  for (nm in names(definitions$choice)) {
    q <- scmix_paper_choice(
      view, contrast = definitions$choice[[nm]], position_neutral = TRUE,
      n_nodes = choice_nodes, on_support = NA
    )
    ans[[paste0("choice:", nm)]] <- as.numeric(q$estimate)
  }
  for (nm in names(definitions$sign)) {
    q <- scmix_paper_signshare(view, contrast = definitions$sign[[nm]],
                               ties = "exclude", variance_margin = NULL)
    ans[[paste0("sign:", nm)]] <- as.numeric(q$estimate)
  }
  for (nm in names(definitions$heterogeneity)) {
    q <- scmix_paper_heterogeneity(
      view, direction = definitions$heterogeneity[[nm]], total_margin = NULL
    )
    z <- as.numeric(q$estimate); names(z) <- names(q$estimate)
    names(z) <- paste("heterogeneity", nm, names(z), sep = ":")
    ans <- c(ans, z)
  }
  ans
}

.orient_loading <- function(A, coefficient_names) {
  A <- as.matrix(A)
  if (ncol(A) != 1L || nrow(A) != length(coefficient_names)) {
    stop("This experiment requires the prespecified q=1 primary loading.",
         call. = FALSE)
  }
  pivot <- which.max(abs(A[, 1L]))
  sign <- if (A[pivot, 1L] < 0) -1 else 1
  out <- A * sign
  rownames(out) <- coefficient_names
  list(A = out, pivot = coefficient_names[[pivot]], sign_applied = sign,
       rule = "largest-absolute loading coordinate positive")
}

.party_multiplier <- function(party, config) {
  raw <- unname(config$dgp$covariance_by_party$raw_sd_multiplier[party])
  if (any(!is.finite(raw)) || any(raw <= 0)) {
    stop("Invalid party-specific covariance multiplier.", call. = FALSE)
  }
  normalized <- raw / sqrt(mean(raw^2))
  if (abs(mean(normalized^2) - 1) > 1e-12) {
    stop("Party covariance multipliers did not normalize.", call. = FALSE)
  }
  normalized
}

.deterministic_draws <- function(scenario, M, config) {
  M <- as.integer(M)
  if (M < 100L) stop("At least 100 deterministic draws are required.",
                     call. = FALSE)
  if (scenario == "shape_bimodal" && M %% 2L) M <- M - 1L
  v <- (seq_len(M) - 0.5) / M
  normal <- stats::qnorm(v)
  u <- switch(scenario,
    shape_skewed_positive = {
      z <- config$dgp$skewed
      (stats::qchisq(v, z$df) - z$df) / sqrt(2 * z$df)
    },
    shape_skewed_negative = {
      z <- config$dgp$skewed
      -(stats::qchisq(v, z$df) - z$df) / sqrt(2 * z$df)
    },
    shape_bimodal = {
      z <- config$dgp$bimodal; h <- M %/% 2L
      plus <- z$location + z$component_sd *
        stats::qnorm((seq_len(h) - 0.5) / h)
      c(plus, -plus)
    },
    shape_heavy_tail = {
      z <- config$dgp$heavy_tail
      stats::qt(v, z$df) * z$multiplier
    },
    normal
  )
  ## Irrational rotation supplies a deterministic second dimension without
  ## making factor and scale/shock quantiles comonotone.
  v2 <- (seq_len(M) * 0.7548776662466927) %% 1
  eps <- sqrt(.Machine$double.eps)
  v2 <- pmin(pmax(v2, eps), 1 - eps)
  scale <- rep(1, M); serial <- rep(0, M)
  if (scenario == "random_scale") {
    sdlog <- config$dgp$random_scale$log_sd
    scale <- exp(sdlog * stats::qnorm(v2) - 0.5 * sdlog^2)
    scale <- scale / mean(scale)
  }
  if (scenario == "serial_shock") {
    serial <- config$dgp$serial_shock$stationary_sd * stats::qnorm(v2)
  }
  list(u = u, scale = scale, serial = serial, M = M)
}

.moment_row <- function(scenario, draws, config, party_multiplier) {
  u <- draws$u
  centered <- u - mean(u)
  variance <- mean(centered^2)
  data.frame(
    scenario = scenario, numerical_draws = draws$M,
    factor_mean = mean(u), factor_variance = variance,
    factor_skewness = if (variance > 0)
      mean(centered^3) / variance^(3 / 2) else NA_real_,
    factor_excess_kurtosis = if (variance > 0)
      mean(centered^4) / variance^2 - 3 else NA_real_,
    factor_mean_zero_by_definition = TRUE,
    factor_unit_variance_by_definition = TRUE,
    finite_covariance_by_definition = TRUE,
    factor_orientation_prespecified = TRUE,
    party_multiplier_min = if (scenario == "covariance_by_party")
      min(party_multiplier) else NA_real_,
    party_multiplier_max = if (scenario == "covariance_by_party")
      max(party_multiplier) else NA_real_,
    party_multiplier_mean_square = if (scenario == "covariance_by_party")
      mean(party_multiplier^2) else NA_real_,
    random_scale_mean = if (scenario == "random_scale")
      mean(draws$scale) else NA_real_,
    random_scale_cv = if (scenario == "random_scale")
      stats::sd(draws$scale) / mean(draws$scale) else NA_real_,
    serial_rho = if (scenario == "serial_shock")
      config$dgp$serial_shock$rho else NA_real_,
    serial_stationary_sd = if (scenario == "serial_shock")
      config$dgp$serial_shock$stationary_sd else NA_real_,
    stringsAsFactors = FALSE
  )
}

.nonlinear_truth <- function(scenario, M, mu, A, kappa, definitions,
                             party_multiplier, config, chunk = 500L) {
  draws <- .deterministic_draws(scenario, M, config)
  u <- draws$u; N <- nrow(mu); loading <- as.numeric(A[, 1L])
  choice <- stats::setNames(numeric(length(definitions$choice)),
                            names(definitions$choice))
  sign <- stats::setNames(numeric(length(definitions$sign)),
                          names(definitions$sign))
  evaluate_direction <- function(d, kind) {
    eta <- as.numeric(mu %*% d)
    ell <- sum(d * loading)
    total <- 0; denom <- 0
    for (lo in seq.int(1L, draws$M, by = chunk)) {
      hi <- min(draws$M, lo + chunk - 1L); jj <- lo:hi
      if (scenario == "covariance_by_party") {
        latent <- outer(ell * party_multiplier, u[jj])
      } else {
        latent <- outer(rep(ell, N), u[jj])
      }
      index <- latent + eta
      if (kind == "sign") {
        total <- total + sum(index > 0)
      } else {
        plus <- index + kappa
        minus <- index - kappa
        if (scenario == "random_scale") {
          plus <- sweep(plus, 2L, draws$scale[jj], `*`)
          minus <- sweep(minus, 2L, draws$scale[jj], `*`)
        }
        if (scenario == "serial_shock") {
          plus <- sweep(plus, 2L, draws$serial[jj], `+`)
          minus <- sweep(minus, 2L, draws$serial[jj], `+`)
        }
        total <- total + sum((stats::plogis(plus) +
                              stats::plogis(minus)) / 2)
      }
      denom <- denom + N * length(jj)
    }
    total / denom
  }
  for (nm in names(choice)) {
    choice[[nm]] <- evaluate_direction(definitions$choice[[nm]], "choice")
  }
  for (nm in names(sign)) {
    sign[[nm]] <- evaluate_direction(definitions$sign[[nm]], "sign")
  }
  list(choice = choice, sign = sign, draws = draws)
}

.scenario_truth <- function(scenario, M, base_qoi, mu, A, kappa,
                            definitions, party_multiplier, config) {
  fine <- .nonlinear_truth(scenario, M, mu, A, kappa, definitions,
                           party_multiplier, config)
  coarse <- .nonlinear_truth(scenario, max(100L, M %/% 2L), mu, A, kappa,
                             definitions, party_multiplier, config)
  value <- base_qoi
  value[paste0("choice:", names(fine$choice))] <- fine$choice
  value[paste0("sign:", names(fine$sign))] <- fine$sign
  coarse_value <- c(stats::setNames(coarse$choice,
                                    paste0("choice:", names(coarse$choice))),
                    stats::setNames(coarse$sign,
                                    paste0("sign:", names(coarse$sign))))
  fine_value <- value[names(coarse_value)]
  comparable <- stats::setNames(rep(TRUE, length(value)), names(value))
  comparability_note <- stats::setNames(rep(
    "same normalized structural quantity as the primary model", length(value)),
    names(value))
  if (scenario == "random_scale") {
    keep <- grepl("^(choice|sign):", names(value))
    comparable[!keep] <- FALSE
    comparability_note[!keep] <- paste(
      "raw coefficient/heterogeneity magnitude is not compared under",
      "random response scale"
    )
    comparability_note[keep & grepl("^sign:", names(value))] <-
      "invariant to a strictly positive respondent scale"
    comparability_note[keep & grepl("^choice:", names(value))] <-
      "choice probability is directly comparable across scale specifications"
  }
  list(
    scenario = scenario, value = value, comparable = comparable,
    comparability_note = comparability_note,
    truth_draws = fine$draws$M,
    max_truth_refinement_difference = max(abs(fine_value - coarse_value)),
    truth_refinement = data.frame(
      quantity = names(fine_value), fine = as.numeric(fine_value),
      coarse = as.numeric(coarse_value),
      absolute_difference = abs(as.numeric(fine_value - coarse_value)),
      stringsAsFactors = FALSE
    ),
    calibration = .moment_row(scenario, fine$draws, config,
                              party_multiplier)
  )
}

.common_random_numbers <- function(N, n, seed) {
  set.seed(seed)
  list(
    factor_rank = stats::runif(N), mixture_component = stats::runif(N),
    mixture_noise = stats::rnorm(N), scale_noise = stats::rnorm(N),
    serial_noise = matrix(stats::rnorm(3L * N), nrow = N, ncol = 3L),
    choice_uniform = stats::runif(n)
  )
}

.simulate_choices <- function(scenario, random, mu, A, kappa, deltaX,
                              respondent_index, task, party_multiplier,
                              config) {
  N <- nrow(mu); u0 <- pmin(pmax(random$factor_rank,
                                 sqrt(.Machine$double.eps)),
                            1 - sqrt(.Machine$double.eps))
  u <- switch(scenario,
    shape_skewed_positive = {
      z <- config$dgp$skewed
      (stats::qchisq(u0, z$df) - z$df) / sqrt(2 * z$df)
    },
    shape_skewed_negative = {
      z <- config$dgp$skewed
      -(stats::qchisq(u0, z$df) - z$df) / sqrt(2 * z$df)
    },
    shape_bimodal = {
      z <- config$dgp$bimodal
      ifelse(random$mixture_component < 0.5, -z$location, z$location) +
        z$component_sd * random$mixture_noise
    },
    shape_heavy_tail = {
      z <- config$dgp$heavy_tail
      stats::qt(u0, z$df) * z$multiplier
    },
    stats::qnorm(u0)
  )
  if (scenario == "covariance_by_party") u <- u * party_multiplier
  beta <- mu + u * rep(as.numeric(A[, 1L]), each = N)
  beta_task <- beta[respondent_index, , drop = FALSE]
  index <- kappa + rowSums(deltaX * beta_task)
  scale <- rep(1, N)
  if (scenario == "random_scale") {
    sdlog <- config$dgp$random_scale$log_sd
    scale <- exp(sdlog * random$scale_noise - 0.5 * sdlog^2)
    scale <- scale / mean(scale)
    index <- scale[respondent_index] * index
  }
  serial <- matrix(0, nrow = N, ncol = 3L)
  if (scenario == "serial_shock") {
    z <- config$dgp$serial_shock
    serial[, 1L] <- z$stationary_sd * random$serial_noise[, 1L]
    for (tt in 2:3) {
      serial[, tt] <- z$rho * serial[, tt - 1L] +
        z$innovation_sd * random$serial_noise[, tt]
    }
    if (any(!task %in% 1:3)) stop("Serial DGP requires tasks 1, 2, and 3.",
                                   call. = FALSE)
    index <- index + serial[cbind(respondent_index, task)]
  }
  probability <- stats::plogis(index)
  list(
    y = as.numeric(random$choice_uniform < probability),
    probability = probability,
    latent = list(
      factor_mean = mean(u), factor_variance = mean((u - mean(u))^2),
      scale_mean = mean(scale), scale_cv = stats::sd(scale) / mean(scale),
      serial_empirical_lag1 = if (scenario == "serial_shock")
        stats::cor(as.numeric(serial[, 1:2]),
                   as.numeric(serial[, 2:3])) else NA_real_
    )
  )
}

.fit_replication <- function(scenario, replication, prepared, primary,
                             helper, controls, definitions, party, gender,
                             party_multiplier, orientation, analysis_config,
                             misspec_config, seed) {
  dx <- as.matrix(prepared$deltaX); Z <- as.matrix(prepared$Z_primary)
  rid_task <- as.character(prepared$respondent_id)
  ids <- unique(rid_task); first <- match(ids, rid_task)
  respondent_index <- match(rid_task, ids)
  task <- as.integer(prepared$task)
  mu <- as.matrix(primary$refit$mu)[first, , drop = FALSE]
  random <- .common_random_numbers(length(ids), nrow(dx), seed)
  simulated <- .simulate_choices(
    scenario, random, mu, orientation$A, primary$refit$kappa, dx,
    respondent_index, task, party_multiplier, misspec_config
  )
  fit <- helper$.fit_one_fixed(
    dx = dx, y = simulated$y, Z_raw = Z, rid = rid_task,
    train = rep(TRUE, nrow(dx)), spec = primary$selected,
    grid = primary$refit$integration_grid, controls = controls,
    seed = seed + 500000L, role = paste0("misspec_", scenario, "_r",
                                         sprintf("%03d", replication))
  )
  view <- .make_view(fit$mu_all, fit$A, fit$kappa, rid_task, Z)
  estimate <- .extract_qoi(
    view, definitions, party, gender,
    choice_nodes = analysis_config$inference$choice_nodes
  )
  list(
    schema_version = "sw2022-misspecification-replication-v1",
    scenario = scenario, replication = as.integer(replication), seed = seed,
    status = if (isTRUE(fit$optimization$optimization_gate_pass))
      "completed_optimizer_gate_pass" else "completed_optimizer_gate_fail",
    optimization_gate_pass = isTRUE(fit$optimization$optimization_gate_pass),
    optimization = list(
      objective = fit$optimization$objective,
      gradient_norm = fit$optimization$gradient_norm,
      structural_gradient_norm = fit$optimization$structural_gradient_norm,
      sieve_gradient_norm = fit$optimization$sieve_gradient_norm,
      stop_reason = fit$optimization$stop_reason,
      bound_activity = any(unlist(fit$optimization$bounds[
        c("mu_active", "kappa_active", "a_active", "weight_active")]))
    ),
    simulated_choice_rate = mean(simulated$y),
    mean_simulated_probability = mean(simulated$probability),
    latent_realization = simulated$latent,
    estimate = estimate,
    posterior_summaries_used = FALSE,
    tuning_repeated = FALSE, formal_inference_computed = FALSE,
    scope = paste(
      "Design-specific simulated-data refit of the selected normal primary",
      "specification; sensitivity only, not an alternative identification claim."
    )
  )
}

.safe_replication <- function(...) {
  tryCatch(.fit_replication(...), error = function(e) list(
    schema_version = "sw2022-misspecification-replication-v1",
    status = "failed_captured", error = conditionMessage(e),
    optimization_gate_pass = FALSE, formal_inference_computed = FALSE
  ))
}

.replication_tables <- function(objects, truths) {
  qoi_rows <- list(); opt_rows <- list(); kk <- 0L
  for (obj in objects) {
    kk <- kk + 1L
    scenario <- obj$scenario %||% attr(obj, "scenario") %||% NA_character_
    replication <- obj$replication %||% attr(obj, "replication") %||% NA_integer_
    opt_rows[[kk]] <- data.frame(
      scenario = scenario, replication = replication,
      status = obj$status %||% "malformed",
      optimizer_gate_pass = isTRUE(obj$optimization_gate_pass),
      objective = obj$optimization$objective %||% NA_real_,
      gradient_norm = obj$optimization$gradient_norm %||% NA_real_,
      bound_activity = obj$optimization$bound_activity %||% NA,
      simulated_choice_rate = obj$simulated_choice_rate %||% NA_real_,
      error = obj$error %||% "", stringsAsFactors = FALSE
    )
    if (is.numeric(obj$estimate) && scenario %in% names(truths)) {
      tr <- truths[[scenario]]
      common <- intersect(names(obj$estimate), names(tr$value))
      qoi_rows[[length(qoi_rows) + 1L]] <- data.frame(
        scenario = scenario, replication = replication, quantity = common,
        estimate = as.numeric(obj$estimate[common]),
        truth = as.numeric(tr$value[common]),
        error = as.numeric(obj$estimate[common] - tr$value[common]),
        comparable = as.logical(tr$comparable[common]),
        comparability_note = as.character(tr$comparability_note[common]),
        optimizer_gate_pass = isTRUE(obj$optimization_gate_pass),
        stringsAsFactors = FALSE
      )
    }
  }
  list(
    qoi = if (length(qoi_rows)) do.call(rbind, qoi_rows) else data.frame(),
    optimization = if (length(opt_rows)) do.call(rbind, opt_rows) else
      data.frame()
  )
}

.summarize_qoi <- function(qoi) {
  if (!nrow(qoi)) return(data.frame())
  keys <- unique(qoi[, c("scenario", "quantity")])
  rows <- lapply(seq_len(nrow(keys)), function(j) {
    z <- qoi[qoi$scenario == keys$scenario[j] &
             qoi$quantity == keys$quantity[j], , drop = FALSE]
    eligible <- z$comparable & z$optimizer_gate_pass &
      is.finite(z$estimate) & is.finite(z$truth)
    x <- z$estimate[eligible]; err <- z$error[eligible]
    data.frame(
      scenario = keys$scenario[j], quantity = keys$quantity[j],
      comparable = all(z$comparable),
      comparability_note = z$comparability_note[[1L]],
      replications_attempted = nrow(z),
      replications_optimizer_gate_pass = sum(z$optimizer_gate_pass),
      replications_used = length(x),
      truth = if (any(is.finite(z$truth))) z$truth[which(is.finite(z$truth))[1L]]
        else NA_real_,
      mean_estimate = if (length(x)) mean(x) else NA_real_,
      bias = if (length(err)) mean(err) else NA_real_,
      empirical_sd = if (length(x) > 1L) stats::sd(x) else NA_real_,
      bias_monte_carlo_se = if (length(x) > 1L)
        stats::sd(err) / sqrt(length(err)) else NA_real_,
      rmse = if (length(err)) sqrt(mean(err^2)) else NA_real_,
      median_absolute_error = if (length(err)) stats::median(abs(err)) else
        NA_real_,
      estimate_q05 = if (length(x) > 1L)
        unname(stats::quantile(x, 0.05, type = 8)) else NA_real_,
      estimate_q95 = if (length(x) > 1L)
        unname(stats::quantile(x, 0.95, type = 8)) else NA_real_,
      max_absolute_error = if (length(err)) max(abs(err)) else NA_real_,
      formal_coverage_evaluated = FALSE,
      formal_coverage = NA_real_,
      coverage_status = paste(
        "withheld: the application has no approved formal interval procedure"
      ),
      materiality_tolerance = NA_real_, materiality_pass = NA,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  baseline <- out[out$scenario == "normal_benchmark",
                  c("quantity", "bias", "rmse")]
  names(baseline)[-1L] <- c("normal_benchmark_bias", "normal_benchmark_rmse")
  out <- merge(out, baseline, by = "quantity", all.x = TRUE, sort = FALSE)
  out$excess_absolute_bias_over_normal <-
    abs(out$bias) - abs(out$normal_benchmark_bias)
  out$excess_rmse_over_normal <- out$rmse - out$normal_benchmark_rmse
  out[order(match(out$scenario, unique(qoi$scenario)), out$quantity), ]
}

.component_result <- function(scenarios, objects, qoi_summary, calibration,
                              orientation, coverage_note) {
  use <- vapply(objects, function(x) (x$scenario %||% "") %in% scenarios,
                logical(1L))
  attempted <- sum(use)
  passed <- sum(vapply(objects[use], function(x)
    isTRUE(x$optimization_gate_pass), logical(1L)))
  qs <- qoi_summary[qoi_summary$scenario %in% scenarios, , drop = FALSE]
  list(
    scenarios = scenarios, attempted_replications = attempted,
    optimizer_gate_passing_replications = passed,
    all_optimizer_gates_pass = attempted > 0L && passed == attempted,
    qoi_summary = qs,
    calibration = calibration[calibration$scenario %in% scenarios, , drop = FALSE],
    mean_zero = TRUE, unit_covariance = TRUE, finite_covariance = TRUE,
    factor_orientation_prespecified = TRUE,
    factor_orientation = orientation,
    design_specific_simulation = TRUE, empirical_alternative_refit = FALSE,
    identification_established = FALSE,
    identification_note = paste(
      "A simulation experiment is not identification of the alternative",
      "observed-data likelihood."
    ),
    tolerance_applied = FALSE, materiality_tolerance = NA_real_,
    materiality_value = if (nrow(qs) && any(is.finite(qs$bias)))
      max(abs(qs$bias), na.rm = TRUE) else NA_real_,
    passed = NA, coverage_evaluated = FALSE, coverage_note = coverage_note
  )
}

.main <- function() {
  cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
  script <- .script_file()
  root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  options(sconjoint.sw_application_root = app)
  source(file.path(app, "config", "analysis_config.R"), local = FALSE)
  source(file.path(dirname(script), "misspecification_config.R"), local = FALSE)
  if (!cli$profile %in% names(sw_analysis_config$profiles)) {
    stop("Unknown profile: ", cli$profile, call. = FALSE)
  }
  if (!cli$profile %in% names(sw_misspecification_config$profiles)) {
    stop("No misspecification profile for: ", cli$profile, call. = FALSE)
  }
  profile <- sw_analysis_config$profiles[[cli$profile]]
  experiment_profile <- sw_misspecification_config$profiles[[cli$profile]]
  B <- if (cli$replications > 0L) cli$replications else
    experiment_profile$replications
  scenarios <- if (identical(cli$scenarios, "all"))
    sw_misspecification_config$scenarios else
      trimws(strsplit(cli$scenarios, ",", fixed = TRUE)[[1L]])
  unknown <- setdiff(scenarios, sw_misspecification_config$scenarios)
  if (length(unknown)) stop("Unknown scenario(s): ", paste(unknown, collapse = ", "),
                            call. = FALSE)

  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The project-local library must contain pkgload.", call. = FALSE)
  }
  suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
  helper <- new.env(parent = .GlobalEnv)
  sys.source(file.path(dirname(script), "07_run_sensitivities.R"), envir = helper)

  fit_dir <- file.path(sw_analysis_config$output_root, cli$profile)
  paths <- list(
    prepared = sw_analysis_config$input$prepared,
    primary = file.path(fit_dir, "fit_primary_full.rds")
  )
  missing <- names(paths)[!file.exists(unlist(paths))]
  if (length(missing)) stop("Required artifact(s) missing: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  prepared <- readRDS(paths$prepared); primary <- readRDS(paths$primary)
  config_path <- file.path(dirname(script), "misspecification_config.R")
  config_md5 <- unname(tools::md5sum(config_path))
  primary_md5 <- unname(tools::md5sum(paths$primary))
  prepared_md5 <- unname(tools::md5sum(paths$prepared))
  stamp <- primary$sw_application_specification
  if (!is.list(stamp) || !identical(stamp$profile, cli$profile) ||
      !identical(stamp$role, "primary_full") ||
      !identical(as.integer(stamp$q), 1L)) {
    stop("Primary fit stamp does not match this q=1 profile.", call. = FALSE)
  }
  dx <- as.matrix(prepared$deltaX); Z <- as.matrix(prepared$Z_primary)
  rid_task <- as.character(prepared$respondent_id); ids <- unique(rid_task)
  first <- match(ids, rid_task)
  if (nrow(dx) != 3573L || length(ids) != 1191L ||
      any(tabulate(match(rid_task, ids), length(ids)) != 3L)) {
    stop("The frozen 1,191-by-three primary design is required.", call. = FALSE)
  }
  meta <- .map_meta(prepared, ids)
  orientation <- .orient_loading(primary$refit$A, colnames(dx))
  party_multiplier <- .party_multiplier(meta$party, sw_misspecification_config)
  definitions <- .qoi_definitions(colnames(dx), sw_analysis_config)
  base_view <- .make_view(
    primary$refit$mu, orientation$A, primary$refit$kappa, rid_task, Z,
    primary$analysis_signature
  )
  base_qoi <- .extract_qoi(
    base_view, definitions, meta$party, meta$gender,
    sw_analysis_config$inference$choice_nodes
  )
  mu <- as.matrix(primary$refit$mu)[first, , drop = FALSE]

  out_dir <- file.path(fit_dir, "sensitivity_analysis", "misspecification")
  truth_dir <- file.path(out_dir, "truth")
  refit_dir <- file.path(out_dir, "refits")
  table_dir <- file.path(out_dir, "tables")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  truths <- list()
  for (scenario in scenarios) {
    path <- file.path(truth_dir, paste0(scenario, ".rds"))
    truths[[scenario]] <- .run_or_load(path, cli$force, function() {
      z <- .scenario_truth(
        scenario, experiment_profile$truth_draws, base_qoi, mu,
        orientation$A, primary$refit$kappa, definitions, party_multiplier,
        sw_misspecification_config
      )
      z$config_md5 <- config_md5; z$primary_md5 <- primary_md5
      z$prepared_md5 <- prepared_md5; z$profile <- cli$profile
      z
    }, validator = function(x) {
      identical(x$config_md5, config_md5) &&
        identical(x$primary_md5, primary_md5) &&
        identical(x$prepared_md5, prepared_md5) &&
        identical(x$profile, cli$profile) &&
        identical(x$scenario, scenario) &&
        identical(as.integer(x$truth_draws),
                  as.integer(experiment_profile$truth_draws))
    })
  }

  controls <- list(
    n_epochs = profile$n_epochs, learning_rate = profile$learning_rate,
    n_starts = profile$n_starts, opt_tol = profile$opt_tol,
    grad_tol = profile$grad_tol,
    mu_bound = sw_analysis_config$optimizer$mu_bound,
    kappa_bound = sw_analysis_config$optimizer$kappa_bound,
    a_bound = sw_analysis_config$optimizer$a_bound,
    weight_bound = sw_analysis_config$optimizer$weight_bound,
    device = sw_analysis_config$optimizer$device
  )
  objects <- list()
  for (scenario in scenarios) {
    scenario_index <- match(scenario, sw_misspecification_config$scenarios)
    for (b in seq_len(B)) {
      seed <- sw_misspecification_config$seed + 100000L * scenario_index + b
      path <- file.path(refit_dir, scenario,
                        paste0("rep_", sprintf("%03d", b), ".rds"))
      obj <- .run_or_load(path, cli$force, function() {
        z <- .safe_replication(
          scenario = scenario, replication = b, prepared = prepared,
          primary = primary, helper = helper, controls = controls,
          definitions = definitions, party = meta$party, gender = meta$gender,
          party_multiplier = party_multiplier, orientation = orientation,
          analysis_config = sw_analysis_config,
          misspec_config = sw_misspecification_config, seed = seed
        )
        z$scenario <- scenario; z$replication <- b; z$seed <- seed
        z$config_md5 <- config_md5; z$primary_md5 <- primary_md5
        z$prepared_md5 <- prepared_md5; z$profile <- cli$profile
        z
      }, validator = function(x) {
        identical(x$config_md5, config_md5) &&
          identical(x$primary_md5, primary_md5) &&
          identical(x$prepared_md5, prepared_md5) &&
          identical(x$profile, cli$profile) &&
          identical(x$scenario, scenario) &&
          identical(as.integer(x$replication), as.integer(b)) &&
          identical(as.integer(x$seed), as.integer(seed))
      })
      objects[[length(objects) + 1L]] <- obj
    }
  }
  tables <- .replication_tables(objects, truths)
  summary <- .summarize_qoi(tables$qoi)
  calibration <- do.call(rbind, lapply(truths, `[[`, "calibration"))
  refinement <- do.call(rbind, lapply(truths, function(x) {
    cbind(scenario = x$scenario, x$truth_refinement,
          stringsAsFactors = FALSE)
  }))
  truth_summary <- data.frame(
    scenario = names(truths),
    truth_draws = vapply(truths, `[[`, integer(1L), "truth_draws"),
    max_truth_refinement_difference = vapply(
      truths, `[[`, numeric(1L), "max_truth_refinement_difference"),
    stringsAsFactors = FALSE
  )

  .write_csv(tables$qoi, file.path(table_dir, "replication_qoi.csv"))
  .write_csv(tables$optimization,
             file.path(table_dir, "replication_optimization.csv"))
  .write_csv(summary, file.path(table_dir, "qoi_bias_stability.csv"))
  .write_csv(calibration, file.path(table_dir, "dgp_calibration.csv"))
  .write_csv(refinement, file.path(table_dir, "truth_refinement.csv"))
  .write_csv(truth_summary, file.path(table_dir, "truth_resolution.csv"))
  coverage_status <- data.frame(
    nominal_coverage = NA_real_, coverage_evaluated = FALSE,
    status = sw_misspecification_config$coverage$reason,
    oracle_interval_substituted = FALSE, formal_inference_available = FALSE,
    stringsAsFactors = FALSE
  )
  .write_csv(coverage_status, file.path(table_dir, "coverage_status.csv"))

  scenario_ok <- function(s) {
    z <- objects[vapply(objects, function(x) identical(x$scenario, s), logical(1L))]
    length(z) == B && all(vapply(z, function(x)
      isTRUE(x$optimization_gate_pass), logical(1L)))
  }
  component <- function(ss) .component_result(
    ss, objects, summary, calibration, orientation,
    sw_misspecification_config$coverage$reason
  )
  results <- list()
  add_result <- function(name, ss) {
    available <- all(ss %in% names(truths))
    results[[name]] <<- if (!available) {
      list(status = "not_run", provenance = out_dir)
    } else {
      cr <- component(ss)
      list(
        status = if (all(vapply(ss, scenario_ok, logical(1L))))
          "run_pass" else "run_fail",
        provenance = out_dir, result = cr,
        identification_established = FALSE,
        identification_note = cr$identification_note
      )
    }
  }
  add_result("shape_skewed_simulation",
             c("shape_skewed_positive", "shape_skewed_negative"))
  add_result("shape_bimodal_simulation", "shape_bimodal")
  add_result("shape_heavy_tail_simulation", "shape_heavy_tail")
  add_result("covariance_by_Z", "covariance_by_party")
  add_result("serial_shocks", "serial_shock")
  add_result("scale", "random_scale")
  structural <- scmix_structural_sensitivity(
    results = results,
    q_values = c(1L, 0L, 2L), materiality_tolerances = NULL,
    prespecified = FALSE
  )
  structural$simulation_plan_fixed_before_results <-
    sw_misspecification_config$prespecified_before_simulation_results
  structural$run_pass_interpretation <- paste(
    "run_pass means the configured simulation/refit executed and all its",
    "optimizer gates passed; it is not a materiality or maintained-assumption pass"
  )
  structural$empirical_alternative_refits <-
    "not_run: no alternative likelihood with separately established identification"
  structural$formal_coverage <- coverage_status
  .atomic_save(structural,
               file.path(out_dir, "structural_misspecification.rds"))
  .write_csv(structural$status,
             file.path(table_dir, "structural_component_status.csv"))

  bundle <- list(
    schema_version = "sw2022-design-misspecification-results-v1",
    profile = cli$profile, requested_scenarios = scenarios,
    requested_replications = B, config = sw_misspecification_config,
    factor_orientation = orientation,
    party_multiplier = stats::setNames(party_multiplier, ids),
    truths = truths, replications = objects, qoi = tables$qoi,
    optimization = tables$optimization, qoi_summary = summary,
    calibration = calibration, truth_summary = truth_summary,
    coverage = coverage_status, structural_sensitivity = structural,
    primary_fit_path = paths$primary,
    primary_fit_md5 = primary_md5,
    prepared_path = paths$prepared,
    prepared_md5 = prepared_md5,
    config_path = config_path, config_md5 = config_md5,
    primary_artifacts_modified = FALSE,
    posterior_summaries_used = FALSE,
    formal_inference_available = FALSE,
    maintained_assumptions_verified = FALSE
  )
  .atomic_save(bundle, file.path(out_dir, "misspecification_results.rds"))
  artifacts <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  artifacts <- artifacts[!file.info(artifacts)$isdir]
  artifacts <- artifacts[!basename(artifacts) %in%
    c("manifest.rds", "sessionInfo.txt", "misspecification_validation.rds")]
  manifest <- list(
    schema_version = "sw2022-design-misspecification-manifest-v1",
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    profile = cli$profile, scenarios = scenarios, replications = B,
    primary_artifacts_modified = FALSE,
    output_root = out_dir,
    artifact_md5 = stats::setNames(unname(tools::md5sum(artifacts)),
      sub(paste0("^", out_dir, "/"), "", artifacts)),
    coverage_evaluated = FALSE, materiality_pass_issued = FALSE,
    maintained_assumptions_verified = FALSE,
    session_info = utils::capture.output(sessionInfo())
  )
  .atomic_save(manifest, file.path(out_dir, "manifest.rds"))
  capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo.txt"))
  message("Saha--Weeks misspecification experiment complete: ", out_dir)
  invisible(bundle)
}

if (sys.nframe() == 0L) .main()
