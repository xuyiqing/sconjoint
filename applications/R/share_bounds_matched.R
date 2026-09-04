## Matched null calibration: run the ANALYSIS's own learner selection inside
## every zero-heterogeneity replication.
##
## WHY THIS EXISTS.  `sb_zero_floor()` takes `hidden`/`weight_decay` as
## arguments and calls `.sc_train_mixed_one()` directly, so the null
## distribution it produces describes ONE FIXED learner.  The analysis does
## not use one fixed learner: it runs inner cross-validation over the
## profile's declared grid and selects by cross-validated sequence log score
## (`scmix_tune_matrix()`), separately in every outer fold.  On gs2020 the
## calibration of record ran `relu8_wd1e-01` while outer folds 1 and 2 both
## SELECTED `relu4_wd1e-01`, and fold 3 selected `relu8_wd1e-01` --- the
## selection is not even constant across folds.
##
## A fixed-learner calibration therefore mismatches the analysis in two ways,
## and swapping relu8 for relu4 repairs only the first:
##   (1) the realized learner differs;
##   (2) SELECTION IS ITSELF PART OF THE ESTIMATOR.  An adaptive procedure
##       manufactures more apparent dispersion than any single member of the
##       grid it chooses from, so ANY fixed-learner null understates the
##       spread the analysis pipeline can invent from noise.
##
## This file addresses both by replicating, per (replication, outer fold),
## exactly the call `scmix_tune_outer_matrix()` makes for that fold --- same
## grid, same inner K, same starts, same epochs, same bounds, same tolerances,
## same common refit integration grid --- with the outcome replaced by a draw
## from the fitted mean under A = 0.
##
## WHAT IT IS NOT.  This is still a NULL DETECTION THRESHOLD, not an upper
## confidence limit for the true dispersion.  Matching the learner does not
## change the kind of object; it only makes the object describe the procedure
## that actually produced the estimate.  See SB_MAINTAINED_CONDITION.
##
## The calibration object records the selected candidate for every cell, and
## the learner spec, which `sb_zero_floor()` did not: `share_bound_floor.rds`
## stored R, gamma, n_epochs and the draws but NOT the learner, so the 28
## August run had to recover `hidden = 8, wd = 0.1` by trial.

## ---------------------------------------------------------------------
## Null outcome draws.  Reproduces `sb_zero_floor()`'s RNG stream exactly:
## one `set.seed(seed)`, then one `runif(n)` per replication, the SAME draw
## shared by every fold within a replication.  Precomputing the whole list
## lets cells run in separate processes without changing a single outcome.
sb_matched_null_draws <- function(assembled, R = 10L, seed = 20260826L) {
  stopifnot(inherits(assembled, "scmix_nested_assembled"))
  deltaX <- as.matrix(assembled$deltaX)
  n <- nrow(deltaX)
  fold_id <- assembled$fold_id
  if (length(fold_id) != n) stop("fold_id must be task-level.")
  mu_task <- as.matrix(assembled$mu_hat)
  kappa_task <- assembled$kappa_folds[fold_id]
  pr <- stats::plogis(kappa_task + rowSums(deltaX * mu_task))
  set.seed(seed)
  out <- vector("list", R)
  for (r in seq_len(R)) out[[r]] <- as.numeric(stats::runif(n) < pr)
  list(pr = pr, ysim = out, R = as.integer(R), seed = seed, n = n)
}

## ---------------------------------------------------------------------
## Fit arguments for one outer-fold training set, mirroring
## `common_fit_args()` in fit_models_common.R and the per-fold call inside
## `scmix_tune_outer_matrix()`.  `profile` is one entry of `cfg$profiles`.
sb_matched_fit_args <- function(profile, optimizer, q = 1L) {
  nodes <- as.integer(profile$n_nodes)
  grid <- lapply(profile$grid, function(spec) {
    spec$q <- NULL
    spec$integration <- "gh"
    spec$n_nodes <- nodes
    spec
  })
  list(
    grid = grid, q = as.integer(q),
    allow_q_tuning = FALSE, allow_integration_tuning = FALSE,
    n_epochs = profile$n_epochs,
    learning_rate = profile$learning_rate,
    n_starts = profile$n_starts,
    mu_bound = optimizer$mu_bound,
    kappa_bound = optimizer$kappa_bound,
    a_bound = optimizer$a_bound,
    weight_bound = optimizer$weight_bound,
    opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
    nested_objective_tol = if (is.null(profile$nested_objective_tol)) 1e-6
                           else profile$nested_objective_tol,
    device = optimizer$device,
    keep_cv_fits = FALSE, verbose = FALSE
  )
}

## ---------------------------------------------------------------------
## One (replication, outer fold) cell.
##
## `k_index` indexes the ASSEMBLED fold ids (1..outer_K), which are positions
## in `nested$outer_folds`, NOT the outer fold labels.  On gs2020 the labels
## are c("2","4","3","1","5"), so k_index 1 is label "2".  Getting this wrong
## silently calibrates a different fold, so it is asserted here.
##
## `y_override` runs the cell on the REAL outcome with the analysis's own
## seed, which is the reproduction gate: the cell must then return the same
## selected candidate and the same loading as the stored analysis refit.
sb_matched_cell <- function(assembled, nested, profile, optimizer,
                            r, k_index, ysim = NULL, q = 1L,
                            seed_base = 20260826L,
                            y_override = NULL, gate_seed = NULL) {
  stopifnot(inherits(assembled, "scmix_nested_assembled"),
            inherits(nested, "scmix_nested_tuning"))
  K <- length(nested$outer_folds)
  if (!(k_index %in% seq_len(K))) {
    stop("k_index must index the assembled fold ids, 1..", K, ".")
  }
  deltaX <- as.matrix(assembled$deltaX)
  Zraw <- as.matrix(assembled$Z)
  rid <- assembled$respondent_id
  fold_id <- assembled$fold_id
  in_k <- fold_id != k_index

  ## The assembled fold index must agree with the tuning entry's training
  ## respondents, or the cell is calibrating a fold the refit never saw.
  expected_train <- setdiff(unique(as.character(rid)),
                            unique(as.character(rid[fold_id == k_index])))
  observed_train <- unique(as.character(
    nested$tuning[[k_index]]$refit$raw_data$respondent_id))
  if (!setequal(expected_train, observed_train)) {
    stop("k_index ", k_index, " does not match nested$tuning[[", k_index,
         "]]: assembled fold ids and tuning order disagree.", call. = FALSE)
  }

  y_use <- if (!is.null(y_override)) y_override else ysim
  if (is.null(y_use) || length(y_use) != nrow(deltaX)) {
    stop("An outcome vector of length nrow(deltaX) is required.")
  }
  fit_seed <- if (!is.null(gate_seed)) gate_seed else
    as.integer(seed_base + 1000L * r + k_index)

  args <- sb_matched_fit_args(profile, optimizer, q = q)
  args$deltaX <- deltaX[in_k, , drop = FALSE]
  args$y <- y_use[in_k]
  args$Z <- Zraw[in_k, , drop = FALSE]
  args$respondent_id <- rid[in_k]
  args$K <- as.integer(profile$inner_K)
  args$refit <- TRUE
  args$seed <- fit_seed
  ## Same shared refit integration grid the outer driver built, so the
  ## selected refit integrates on the identical nodes as the analysis.
  args$refit_integration_grid <- nested$common_refit_integration_grid

  t0 <- proc.time()[["elapsed"]]
  fk <- do.call(scmix_tune_matrix, args)
  elapsed <- proc.time()[["elapsed"]] - t0

  gate <- fk$candidate_selection_gate
  sel <- fk$selected_index
  gate_ok <- is.list(gate) && isTRUE(gate$pass) && length(sel) == 1L &&
    !is.na(sel) && isTRUE(gate$selection_eligible[sel])

  ## `scmix_tune_matrix(..., refit = TRUE)` has already returned the selected
  ## refit's loading on the raw contrast scale.
  A_raw <- as.matrix(fk$refit$A)
  dimnames(A_raw) <- list(assembled$attr_names, NULL)

  list(r = as.integer(r), k_index = as.integer(k_index),
       fold_label = nested$outer_folds[[k_index]],
       selected_name = fk$selected$name, selected_index = sel,
       selection_gate_pass = gate_ok,
       draw = sqrt(rowSums(A_raw^2)),
       A_raw = A_raw, kappa = fk$refit$kappa,
       seed = fit_seed, elapsed_sec = elapsed,
       is_gate_cell = !is.null(y_override))
}

## ---------------------------------------------------------------------
## Assemble completed cells into an object shaped like `sb_zero_floor()`'s
## return, so every downstream consumer (`sb_matched_floor()`,
## `sb_contrast_floor_matched()`, the release gates) works unchanged.
##
## Only COMPLETE replications are used: a replication contributes only if
## every requested fold finished, because the calibration statistic averages
## across folds WITHIN a replication before taking the quantile.  A partial
## overnight run therefore yields a valid calibration at a smaller R rather
## than a malformed one.
sb_assemble_matched_calibration <- function(cells, assembled, folds_use,
                                            gamma = 0.05, n_epochs = NA,
                                            profile_name = NA_character_,
                                            keep_loadings = TRUE) {
  cells <- Filter(function(x) !isTRUE(x$is_gate_cell), cells)
  if (!length(cells)) stop("No non-gate cells to assemble.")
  reps <- sort(unique(vapply(cells, `[[`, integer(1L), "r")))
  complete <- reps[vapply(reps, function(r) {
    ks <- vapply(Filter(function(x) x$r == r, cells), `[[`,
                 integer(1L), "k_index")
    setequal(ks, folds_use)
  }, logical(1L))]
  if (!length(complete)) {
    stop("No replication has every requested fold; cannot assemble.")
  }
  dropped <- setdiff(reps, complete)
  ordered <- do.call(c, lapply(complete, function(r) {
    lapply(folds_use, function(k) {
      hit <- Filter(function(x) x$r == r && x$k_index == k, cells)
      hit[[1L]]
    })
  }))
  p <- length(assembled$attr_names)
  draws <- t(vapply(ordered, `[[`, numeric(p), "draw"))
  colnames(draws) <- assembled$attr_names
  gates <- vapply(ordered, `[[`, logical(1L), "selection_gate_pass")
  if (!all(gates)) {
    stop("Some assembled cells failed the candidate selection gate: ",
         paste(which(!gates), collapse = ", "), ". Do not use this ",
         "calibration.", call. = FALSE)
  }
  cal <- list(
    draws = draws,
    rep_id = vapply(ordered, `[[`, integer(1L), "r"),
    fold_of_draw = vapply(ordered, `[[`, integer(1L), "k_index"),
    R = length(complete), folds_use = folds_use, gamma = gamma,
    n_epochs = n_epochs, attr_names = assembled$attr_names,
    analysis_signature = assembled$analysis_signature,
    ceiling_source = "zero_heterogeneity_null_calibration_matched_selection",
    ceiling_status = "conditional_unverified",
    maintained_condition = SB_MAINTAINED_CONDITION,
    statistic = "within-replication fold average of |A~_j|",
    ## Provenance the fixed-learner calibration did not record.
    learner = list(mode = "matched_selection", profile = profile_name),
    selected_by_cell = data.frame(
      r = vapply(ordered, `[[`, integer(1L), "r"),
      k_index = vapply(ordered, `[[`, integer(1L), "k_index"),
      fold_label = vapply(ordered, `[[`, character(1L), "fold_label"),
      selected = vapply(ordered, `[[`, character(1L), "selected_name"),
      seed = vapply(ordered, `[[`, integer(1L), "seed"),
      elapsed_sec = vapply(ordered, `[[`, numeric(1L), "elapsed_sec"),
      stringsAsFactors = FALSE),
    replications_dropped_incomplete = dropped,
    A_raw_draws = if (isTRUE(keep_loadings))
      lapply(ordered, `[[`, "A_raw") else NULL,
    keep_loadings = isTRUE(keep_loadings))
  cal$floor <- sb_matched_floor(cal, gamma = gamma,
                                attr_names = assembled$attr_names)
  cal$floor_pooled <- stats::setNames(
    apply(draws, 2L, stats::quantile, probs = 1 - gamma, names = FALSE,
          type = 1L), assembled$attr_names)
  cal
}
