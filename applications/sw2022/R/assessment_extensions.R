## Saha--Weeks assessment extensions that are specific to the fielded
## three-task application.  This file is sourced by 05_assessment.R; it does
## not alter the package-level estimator or any fitted object.

.sw_grid_for_fold <- function(fit, fold, q, K) {
  grids <- fit$integration_grids_folds
  grid <- if (is.list(grids) && length(grids) == K) grids[[fold]] else
    fit$gh %||% fit$integration_grid
  if (is.null(grid$U) || is.null(grid$w)) {
    stop("A fold integration grid is missing.", call. = FALSE)
  }
  U <- as.matrix(grid$U)
  w <- as.numeric(grid$w)
  if (q == 0L && ncol(U) == 1L && all(U == 0)) {
    U <- matrix(numeric(), nrow(U), 0L)
  }
  if (ncol(U) != q || nrow(U) < 1L || length(w) != nrow(U) ||
      any(!is.finite(U)) || any(!is.finite(w)) || any(w <= 0) ||
      !is.finite(sum(w))) {
    stop("A fold integration grid is incompatible with the fitted rank.",
         call. = FALSE)
  }
  list(U = U, w = w / sum(w))
}

.sw_event_probability <- function(P, w, event) {
  event <- as.integer(event)
  node <- rep(1, ncol(P))
  for (t in seq_len(nrow(P))) {
    node <- node * if (event[[t]] == 1L) P[t, ] else 1 - P[t, ]
  }
  sum(w * node)
}

.sw_exact_joint_predictions <- function(fit, prepared, repeat_tol = 1e-8) {
  required <- c("deltaX", "y", "respondent_id", "fold_id",
                "mu_all_folds", "kappa_folds")
  missing <- required[vapply(required, function(nm) is.null(fit[[nm]]),
                             logical(1L))]
  if (length(missing)) {
    stop("The assembled fit lacks: ", paste(missing, collapse = ", "), ".",
         call. = FALSE)
  }
  dx <- as.matrix(fit$deltaX)
  y <- as.numeric(fit$y)
  rid <- as.character(fit$respondent_id)
  task <- as.integer(prepared$task)
  if (!identical(dim(dx), dim(as.matrix(prepared$deltaX))) ||
      max(abs(dx - as.matrix(prepared$deltaX))) > 1e-12 ||
      !identical(y, as.numeric(prepared$y)) ||
      !identical(rid, as.character(prepared$respondent_id)) ||
      length(task) != nrow(dx)) {
    stop("The assembled fit is not row-aligned with the prepared application.",
         call. = FALSE)
  }
  ids <- unique(rid)
  ri <- match(rid, ids)
  N <- length(ids)
  if (any(tabulate(ri, N) != 3L)) {
    stop("Full-pattern calibration requires exactly three tasks per respondent.",
         call. = FALSE)
  }
  fold_raw <- fit$fold_id
  if (length(fold_raw) == nrow(dx)) {
    fold_task <- as.integer(fold_raw)
  } else if (length(fold_raw) == N) {
    fold_task <- as.integer(fold_raw)[ri]
  } else stop("Fold ids must be recorded per task or per respondent.",
              call. = FALSE)
  K <- max(fold_task)
  if (K < 2L || !setequal(unique(fold_task), seq_len(K)) ||
      length(fit$mu_all_folds) != K || length(fit$kappa_folds) != K) {
    stop("The assembled outer-fold nuisance collection is incomplete.",
         call. = FALSE)
  }
  q <- as.integer(fit$q %||% 0L)
  A_folds <- fit$A_computational_folds %||% fit$A_folds
  if (q > 0L && (!is.list(A_folds) || length(A_folds) != K)) {
    stop("The fold loading collection is incompatible with the fitted rank.",
         call. = FALSE)
  }

  full_events <- expand.grid(y1 = 0:1, y2 = 0:1, y3 = 0:1,
                             KEEP.OUT.ATTRS = FALSE)
  full_events <- full_events[do.call(order, full_events), , drop = FALSE]
  pair_events <- expand.grid(y_first = 0:1, y_second = 0:1,
                             KEEP.OUT.ATTRS = FALSE)
  pair_events <- pair_events[do.call(order, pair_events), , drop = FALSE]
  pair_index <- rbind(c(1L, 2L), c(2L, 3L), c(1L, 3L))
  full <- vector("list", N * nrow(full_events))
  pair <- vector("list", N * nrow(pair_index) * nrow(pair_events))
  repeated <- list()
  fi <- pi <- rpi <- 0L

  for (i in seq_len(N)) {
    rows <- which(ri == i)
    rows <- rows[order(task[rows])]
    if (!identical(task[rows], 1:3)) {
      stop("Task order must be exactly 1, 2, 3 within each respondent.",
           call. = FALSE)
    }
    fold <- unique(fold_task[rows])
    if (length(fold) != 1L) stop("A respondent is split across outer folds.",
                                 call. = FALSE)
    mu_all <- as.matrix(fit$mu_all_folds[[fold]])
    if (nrow(mu_all) == nrow(dx)) {
      mu <- mu_all[rows, , drop = FALSE]
    } else if (nrow(mu_all) == N) {
      mu <- mu_all[rep(i, 3L), , drop = FALSE]
    } else stop("A fold mean prediction has incompatible dimensions.",
                call. = FALSE)
    if (ncol(mu) != ncol(dx) || any(!is.finite(mu)) ||
        any(abs(mu - matrix(mu[1L, ], 3L, ncol(mu), byrow = TRUE)) > 1e-10)) {
      stop("A fold conditional mean is malformed or varies within respondent.",
           call. = FALSE)
    }
    grid <- .sw_grid_for_fold(fit, fold, q, K)
    A <- if (q == 0L) matrix(numeric(), ncol(dx), 0L) else
      as.matrix(A_folds[[fold]])
    if (!identical(dim(A), c(ncol(dx), q)) || any(!is.finite(A))) {
      stop("A fold loading matrix is malformed.", call. = FALSE)
    }
    base <- as.numeric(fit$kappa_folds[[fold]] +
                         rowSums(dx[rows, , drop = FALSE] * mu))
    index <- if (q == 0L) matrix(base, ncol = 1L) else
      sweep(dx[rows, , drop = FALSE] %*% A %*% t(grid$U), 1L, base, `+`)
    P <- stats::plogis(index)

    for (h in seq_len(nrow(full_events))) {
      ev <- as.integer(full_events[h, ])
      fi <- fi + 1L
      full[[fi]] <- data.frame(
        respondent_id = ids[[i]], diagnostic = "full_three_task_pattern",
        pair = "tasks_1_2_3", event = paste0(ev, collapse = ""),
        observed = as.numeric(all(y[rows] == ev)),
        predicted = .sw_event_probability(P, grid$w, ev),
        stringsAsFactors = FALSE
      )
    }
    for (h in seq_len(nrow(pair_index))) {
      which_pair <- pair_index[h, ]
      label <- paste0("tasks_", which_pair[[1L]], "_", which_pair[[2L]])
      for (e in seq_len(nrow(pair_events))) {
        ev <- as.integer(pair_events[e, ])
        pi <- pi + 1L
        pair[[pi]] <- data.frame(
          respondent_id = ids[[i]],
          diagnostic = "prespecified_task_pair_pattern", pair = label,
          event = paste0(ev, collapse = ""),
          observed = as.numeric(all(y[rows[which_pair]] == ev)),
          predicted = .sw_event_probability(P[which_pair, , drop = FALSE],
                                             grid$w, ev),
          stringsAsFactors = FALSE
        )
      }
    }
    cmb <- utils::combn(1:3, 2L)
    for (h in seq_len(ncol(cmb))) {
      which_pair <- cmb[, h]
      if (max(abs(dx[rows[which_pair[[1L]]], ] -
                  dx[rows[which_pair[[2L]]], ])) > repeat_tol) next
      label <- paste0("tasks_", which_pair[[1L]], "_", which_pair[[2L]])
      for (e in seq_len(nrow(pair_events))) {
        ev <- as.integer(pair_events[e, ])
        rpi <- rpi + 1L
        repeated[[rpi]] <- data.frame(
          respondent_id = ids[[i]],
          diagnostic = "exact_repeated_contrast_pattern", pair = label,
          event = paste0(ev, collapse = ""),
          observed = as.numeric(all(y[rows[which_pair]] == ev)),
          predicted = .sw_event_probability(P[which_pair, , drop = FALSE],
                                             grid$w, ev),
          contrast = paste(dx[rows[which_pair[[1L]]], ], collapse = ","),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  full <- do.call(rbind, full)
  pair <- do.call(rbind, pair)
  repeated <- if (length(repeated)) do.call(rbind, repeated) else data.frame()
  check_sum <- function(x, keys) {
    totals <- aggregate(x$predicted, x[keys], sum)
    max(abs(totals$x - 1))
  }
  full_error <- check_sum(full, "respondent_id")
  pair_error <- check_sum(pair, c("respondent_id", "pair"))
  repeated_error <- if (nrow(repeated))
    check_sum(repeated, c("respondent_id", "pair")) else NA_real_
  if (full_error > 1e-10 || pair_error > 1e-10 ||
      (is.finite(repeated_error) && repeated_error > 1e-10)) {
    stop("Integrated joint-event probabilities do not sum to one.",
         call. = FALSE)
  }
  list(
    full = full, pair = pair, repeated = repeated,
    probability_sum_error = c(full = full_error, pair = pair_error,
                              repeated = repeated_error),
    out_of_fold = TRUE,
    training_only_tuning = !isFALSE(fit$eligible_for_ordinary_inference) &&
      !isTRUE(fit$diagnostic_only),
    analysis_signature = fit$analysis_signature %||% NA_character_,
    posterior_summaries_used = FALSE
  )
}

.sw_joint_calibration_table <- function(events) {
  if (!is.data.frame(events) || !nrow(events)) return(data.frame())
  groups <- interaction(events$diagnostic, events$pair, events$event,
                        drop = TRUE, lex.order = TRUE)
  out <- lapply(split(seq_len(nrow(events)), groups), function(ii) {
    gap <- events$observed[ii] - events$predicted[ii]
    data.frame(
      diagnostic = events$diagnostic[ii[[1L]]],
      pair = events$pair[ii[[1L]]], event = events$event[ii[[1L]]],
      observed = mean(events$observed[ii]),
      predicted = mean(events$predicted[ii]), gap = mean(gap),
      respondent_se_gap = if (length(ii) > 1L)
        stats::sd(gap) / sqrt(length(ii)) else NA_real_,
      n_respondents = length(unique(events$respondent_id[ii])),
      n_event_rows = length(ii), stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out[order(out$diagnostic, out$pair, out$event, method = "radix"),,
      drop = FALSE]
}

.sw_joint_extension_status <- function(x, heldout_state) {
  n_repeat <- if (is.data.frame(x$repeated) && nrow(x$repeated))
    length(unique(x$repeated$respondent_id)) else 0L
  data.frame(
    component = c(
      "full eight-pattern three-task calibration",
      "prespecified task-pair joint calibration",
      "exact repeated-contrast joint calibration"
    ),
    status = c(
      if (heldout_state == "not_run") "not_run" else heldout_state,
      if (heldout_state == "not_run") "not_run" else heldout_state,
      if (!n_repeat) "not_supported_in_realized_data" else
        paste0("run_sparse_descriptive_only_under_", heldout_state,
               "; general_calibration_withheld")
    ),
    note = c(
      paste(
        "All eight response patterns for tasks 1--3 use one integrated",
        "respondent factor draw; this is not a product of marginal probabilities."
      ),
      paste(
        "Pairs 1--2, 2--3, and 1--3 were fixed as joint-response checks;",
        "each uses the shared-factor mixed-logit probability."
      ),
      paste0(
        "Only ", n_repeat, " respondent(s) supplied an exact repeated contrast; ",
        "contrasts were not coarsened, so this cannot be a general calibration test."
      )
    ),
    maintained_assumption_verified = FALSE,
    stringsAsFactors = FALSE
  )
}

.sw_completion_comparisons <- function(task_audit) {
  required <- c("respondent_id", "task", "y", "eventual_tasks", "finished",
                "final_analysis_sample", "exclusion_reason")
  if (!is.data.frame(task_audit) || !all(required %in% names(task_audit))) {
    return(list(summary = data.frame(), status = data.frame(
      component = "early response/assignment/order by completion and exclusion",
      status = "not_run", note = "Local raw-universe task audit is unavailable.",
      maintained_assumption_verified = FALSE, stringsAsFactors = FALSE)))
  }
  coordinate <- setdiff(names(task_audit), c(
    required, "progress", "primary_demographics_valid",
    "all_primary_demographics_missing"
  ))
  coordinate <- coordinate[vapply(task_audit[coordinate], is.numeric,
                                  logical(1L))]
  features <- c("y", coordinate)
  definitions <- list(
    eventual_tasks_3_vs_2 = list(flag = task_audit$eventual_tasks == 3L,
                                 label1 = "eventual_T3", label0 = "eventual_T2"),
    finished_vs_unfinished = list(flag = as.logical(task_audit$finished),
                                  label1 = "finished", label0 = "unfinished"),
    primary_included_vs_excluded = list(
      flag = as.logical(task_audit$final_analysis_sample),
      label1 = "primary_included", label0 = "primary_excluded")
  )
  rows <- list(); z <- 0L
  for (comparison in names(definitions)) {
    def <- definitions[[comparison]]
    for (tt in sort(unique(task_audit$task))) {
      at_task <- task_audit$task == tt
      for (feature in features) {
        value <- as.numeric(task_audit[[feature]])
        g1 <- value[at_task & def$flag]
        g0 <- value[at_task & !def$flag]
        z <- z + 1L
        enough <- length(g1) > 0L && length(g0) > 0L
        variance_term <- function(x) if (length(x) > 1L)
          stats::var(x) / length(x) else NA_real_
        zse <- if (enough) sqrt(variance_term(g1) + variance_term(g0)) else
          NA_real_
        rows[[z]] <- data.frame(
          comparison = comparison, task = tt,
          feature = if (feature == "y") "candidate_A_response" else feature,
          feature_role = if (feature == "y") "early_response" else
            "randomized_contrast_coordinate",
          group_1 = def$label1, group_0 = def$label0,
          mean_group_1 = if (length(g1)) mean(g1) else NA_real_,
          mean_group_0 = if (length(g0)) mean(g0) else NA_real_,
          difference_1_minus_0 = if (enough) mean(g1) - mean(g0) else NA_real_,
          descriptive_se = zse, respondents_group_1 = length(g1),
          respondents_group_0 = length(g0),
          status = if (enough) "run_descriptive" else
            "not_observed_for_both_groups",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  summary <- do.call(rbind, rows)
  list(
    summary = summary,
    status = data.frame(
      component = "early response/assignment/order by completion and exclusion",
      status = "run_descriptive",
      note = paste(
        "Candidate-A responses and exact 13-coordinate assignments are",
        "compared separately by task for eventual T=3 versus T=2, survey",
        "finished versus unfinished, and primary inclusion versus exclusion.",
        "These comparisons cannot verify noninformative completion."
      ),
      maintained_assumption_verified = FALSE, stringsAsFactors = FALSE
    )
  )
}

.sw_conditional_randomization_status <- function(design_audit) {
  protocol_verified <- isTRUE(design_audit$design$protocol_verified)
  data.frame(
    proposed_test = c(
      "conditional randomization test of early assignment versus eventual completion",
      "exact ordered-contrast Horvitz--Thompson benchmark"
    ),
    fielded_randomizer_available = FALSE,
    exact_assignment_probabilities_available = FALSE,
    cross_task_restrictions_available = FALSE,
    completion_conditioned_exposure_probabilities_available = FALSE,
    protocol_verified = protocol_verified,
    status = "protocol_unavailable_not_run",
    p_value = NA_real_,
    reason = paste(
      "The public files do not contain the fielded QSF/randomizer, exact",
      "assignment probabilities, cross-task restrictions, or completion-stratum",
      "exposure probabilities. No empirical or illustrative-uniform permutation",
      "distribution is substituted for the missing protocol."
    ),
    maintained_assumption_verified = FALSE,
    stringsAsFactors = FALSE
  )
}
