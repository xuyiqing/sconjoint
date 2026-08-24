## Fit-aware held-out prediction constructors for the paperps specification
## assessment.  These routines use the same persistent factor draw across every
## task in a respondent sequence.  They never refit on, or split, held-out task
## rows.

.pps_logsumexp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

.pps_prediction_grid <- function(fit, k, q, K) {
  grids <- fit$integration_grids_folds
  if (!is.null(grids)) {
    if (!is.list(grids) || length(grids) != K) {
      stop("`fit$integration_grids_folds` must contain one grid per fold.",
           call. = FALSE)
    }
    grid <- grids[[k]]
  } else {
    grid <- fit$gh %||% fit$integration_grid
  }
  if (is.null(grid$U) || is.null(grid$w)) {
    stop("The fit is missing the finite integration grid used by its fold fits.",
         call. = FALSE)
  }
  U <- as.matrix(grid$U)
  w <- as.numeric(grid$w)
  if (q == 0L && ncol(U) != 0L) {
    ## Some legacy q=0 grids store a dummy one-column zero. Canonicalize only
    ## when it is exactly zero; otherwise the fit is internally inconsistent.
    if (ncol(U) == 1L && all(U == 0)) U <- matrix(numeric(0), nrow(U), 0L)
  }
  if (ncol(U) != q || nrow(U) < 1L || length(w) != nrow(U) ||
      any(!is.finite(U)) || any(!is.finite(w)) || any(w <= 0) ||
      !is.finite(sum(w))) {
    stop("A fold integration grid is incompatible with the maintained rank.",
         call. = FALSE)
  }
  list(U = U, w = w / sum(w))
}

.pps_poisson_binomial <- function(probability) {
  out <- 1
  for (p in probability) {
    next_out <- numeric(length(out) + 1L)
    next_out[seq_along(out)] <- next_out[seq_along(out)] + out * (1 - p)
    next_out[seq_along(out) + 1L] <- next_out[seq_along(out) + 1L] + out * p
    out <- next_out
  }
  out
}

.pps_joint_pattern_rows <- function(P, w, y, respondent_id, pairs, type,
                                    task_order) {
  if (!nrow(pairs)) return(NULL)
  events <- rbind(`00` = c(0, 0), `01` = c(0, 1),
                  `10` = c(1, 0), `11` = c(1, 1))
  rows <- vector("list", nrow(pairs) * 4L)
  z <- 0L
  for (h in seq_len(nrow(pairs))) {
    a <- pairs[h, 1L]; b <- pairs[h, 2L]
    for (ev in rownames(events)) {
      z <- z + 1L
      target <- events[ev, ]
      node_probability <- if (target[1L]) P[a, ] else 1 - P[a, ]
      node_probability <- node_probability *
        if (target[2L]) P[b, ] else 1 - P[b, ]
      rows[[z]] <- data.frame(
        respondent_id = respondent_id,
        type = type,
        stratum = ev,
        observed = as.numeric(y[a] == target[1L] && y[b] == target[2L]),
        predicted = sum(w * node_probability),
        task_1 = task_order[a], task_2 = task_order[b],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' Out-of-fold mixed-logit predictions for specification assessment
#'
#' Computes marginal task probabilities, complete-sequence log scores, the
#' response-count distribution, adjacent-task response-pattern probabilities,
#' and (when realized) exact repeated-contrast response-pattern probabilities.
#' Every prediction for fold k uses only the nuisance fit trained outside fold
#' k, with one shared latent factor draw across the respondent's entire
#' sequence.
#'
#' @param fit A paper-aligned `scmix` fit or `scmix_assemble_nested()` result.
#' @param task_order Optional task order; otherwise task-row order within each
#'   respondent is used.
#' @param include_counts,include_adjacent,include_repeated Which joint
#'   calibration constructors to compute.
#' @param repeat_tol Positive tolerance for exact ordered-contrast repetition.
#' @return A fit-aware prediction object consumable by
#'   [scmix_heldout_sequence_score()] and [scmix_heldout_calibration()], carrying
#'   the fitted analysis's nonempty `analysis_signature`.
#' @export
scmix_heldout_predictions <- function(fit, task_order = NULL,
                                      include_counts = TRUE,
                                      include_adjacent = TRUE,
                                      include_repeated = TRUE,
                                      repeat_tol = 1e-8) {
  needed <- c("deltaX", "y", "respondent_id", "fold_id",
              "mu_all_folds", "kappa_folds")
  missing <- needed[vapply(needed, function(nm) is.null(fit[[nm]]), logical(1L))]
  if (length(missing)) {
    stop("The fit lacks held-out prediction field(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  analysis_signature <- fit$analysis_signature
  if (!is.character(analysis_signature) || length(analysis_signature) != 1L ||
      is.na(analysis_signature) || !nzchar(analysis_signature)) {
    stop("The fit must carry one nonempty `analysis_signature` before fit-aware held-out predictions are constructed.",
         call. = FALSE)
  }
  dx <- as.matrix(fit$deltaX)
  y <- as.numeric(fit$y)
  rid <- as.character(fit$respondent_id)
  n <- nrow(dx)
  if (!is.numeric(dx) || any(!is.finite(dx)) || length(y) != n ||
      any(!y %in% c(0, 1)) || length(rid) != n || anyNA(rid)) {
    stop("The fit's task rows are malformed.", call. = FALSE)
  }
  ids <- unique(rid)
  rindex <- match(rid, ids)
  N <- length(ids)
  fold_raw <- fit$fold_id
  if (!is.numeric(fold_raw) || any(!is.finite(fold_raw)) ||
      any(fold_raw != as.integer(fold_raw))) {
    stop("Fold identifiers must be finite integers.", call. = FALSE)
  }
  if (length(fold_raw) == n) {
    fold_task <- as.integer(fold_raw)
    fold_resp <- vapply(seq_len(N), function(i) {
      u <- unique(fold_task[rindex == i])
      if (length(u) != 1L) stop("A respondent is split across folds.",
                                call. = FALSE)
      u
    }, integer(1L))
  } else if (length(fold_raw) == N) {
    fold_resp <- as.integer(fold_raw)
    fold_task <- fold_resp[rindex]
  } else {
    stop("Fold identifiers must have one value per task or respondent.",
         call. = FALSE)
  }
  K <- max(fold_resp)
  if (K < 2L || !setequal(unique(fold_resp), seq_len(K)) ||
      length(fit$mu_all_folds) != K || length(fit$kappa_folds) != K) {
    stop("The outer-fold nuisance collection is incomplete.", call. = FALSE)
  }
  A_folds <- fit$A_computational_folds %||% fit$A_folds
  q <- as.integer(fit$q %||% if (is.null(A_folds)) 0L else ncol(A_folds[[1L]]))
  if (q < 0L || q > ncol(dx) - 1L ||
      (q > 0L && (!is.list(A_folds) || length(A_folds) != K))) {
    stop("The fold loading collection is incompatible with q.", call. = FALSE)
  }
  if (is.null(task_order)) {
    task_order <- ave(seq_len(n), rid, FUN = seq_along)
  }
  if (!is.numeric(task_order) || length(task_order) != n ||
      any(!is.finite(task_order))) {
    stop("`task_order` must have one finite numeric value per task.",
         call. = FALSE)
  }
  if (any(vapply(split(task_order, rid), anyDuplicated, integer(1L)) > 0L)) {
    stop("Task order must be unique within respondent.", call. = FALSE)
  }
  if (!is.numeric(repeat_tol) || length(repeat_tol) != 1L ||
      !is.finite(repeat_tol) || repeat_tol <= 0) {
    stop("`repeat_tol` must be one finite positive number.", call. = FALSE)
  }

  marginal <- rep(NA_real_, n)
  sequence_loglik <- stats::setNames(rep(NA_real_, N), ids)
  joint_rows <- list(); count_rows <- list(); joint_index <- count_index <- 0L
  for (i in seq_len(N)) {
    rows <- which(rindex == i)
    rows <- rows[order(task_order[rows])]
    k <- unique(fold_task[rows])
    mu_all <- as.matrix(fit$mu_all_folds[[k]])
    if (nrow(mu_all) == n) {
      mu <- mu_all[rows, , drop = FALSE]
      if (any(abs(mu - matrix(mu[1L, ], nrow(mu), ncol(mu), byrow = TRUE)) >
              1e-10)) {
        stop("A fold conditional mean is not constant within respondent.",
             call. = FALSE)
      }
    }
    else if (nrow(mu_all) == N) mu <- mu_all[rep(i, length(rows)), , drop = FALSE]
    else stop("Every fold mean prediction must cover all tasks or respondents.",
              call. = FALSE)
    if (ncol(mu) != ncol(dx) || any(!is.finite(mu))) {
      stop("A fold conditional-mean prediction is malformed.", call. = FALSE)
    }
    grid <- .pps_prediction_grid(fit, k, q, K)
    A <- if (q == 0L) matrix(numeric(0), ncol(dx), 0L) else
      as.matrix(A_folds[[k]])
    if (!identical(dim(A), c(ncol(dx), q)) || any(!is.finite(A))) {
      stop("A fold computational loading is malformed.", call. = FALSE)
    }
    base <- as.numeric(fit$kappa_folds[k] +
                         rowSums(dx[rows, , drop = FALSE] * mu))
    index <- if (q == 0L) matrix(base, ncol = 1L) else
      sweep(dx[rows, , drop = FALSE] %*% A %*% t(grid$U), 1L, base, `+`)
    P <- stats::plogis(index)
    marginal[rows] <- as.numeric(P %*% grid$w)
    log_node <- colSums(y[rows] * log(pmax(P, .Machine$double.xmin)) +
                          (1 - y[rows]) *
                          log(pmax(1 - P, .Machine$double.xmin)))
    sequence_loglik[i] <- .pps_logsumexp(log(grid$w) + log_node)

    if (isTRUE(include_counts)) {
      count_probability <- Reduce(`+`, lapply(seq_len(ncol(P)), function(s) {
        grid$w[s] * .pps_poisson_binomial(P[, s])
      }))
      T_i <- length(rows)
      for (c0 in 0:T_i) {
        count_index <- count_index + 1L
        count_rows[[count_index]] <- data.frame(
          respondent_id = ids[i], type = "response_count",
          stratum = as.character(c0),
          observed = as.numeric(sum(y[rows]) == c0),
          predicted = count_probability[c0 + 1L],
          task_1 = NA_real_, task_2 = NA_real_, stringsAsFactors = FALSE)
      }
    }
    if (isTRUE(include_adjacent) && length(rows) >= 2L) {
      pair_local <- cbind(seq_len(length(rows) - 1L), 2:length(rows))
      part <- .pps_joint_pattern_rows(P, grid$w, y[rows], ids[i], pair_local,
                                      "adjacent_pair", task_order[rows])
      joint_index <- joint_index + 1L; joint_rows[[joint_index]] <- part
    }
    if (isTRUE(include_repeated) && length(rows) >= 2L) {
      cmb <- utils::combn(seq_along(rows), 2L)
      keep <- vapply(seq_len(ncol(cmb)), function(h) {
        max(abs(dx[rows[cmb[1L, h]], ] - dx[rows[cmb[2L, h]], ])) <=
          repeat_tol
      }, logical(1L))
      if (any(keep)) {
        part <- .pps_joint_pattern_rows(P, grid$w, y[rows], ids[i],
                                        t(cmb[, keep, drop = FALSE]),
                                        "repeated_contrast_pair",
                                        task_order[rows])
        joint_index <- joint_index + 1L; joint_rows[[joint_index]] <- part
      }
    }
  }
  joint <- do.call(rbind, c(count_rows, joint_rows))
  out <- list(
    task = data.frame(respondent_id = rid, fold = fold_task,
                      task_order = task_order, observed = y,
                      predicted = marginal, stringsAsFactors = FALSE),
    sequence_loglik = sequence_loglik,
    joint = joint,
    respondent_id = ids,
    analysis_signature = analysis_signature,
    provenance = "fold-specific nuisance fits evaluated out of fold",
    out_of_fold = TRUE,
    training_only_tuning = !isFALSE(fit$eligible_for_ordinary_inference) &&
      !isTRUE(fit$diagnostic_only),
    complete_sequence = TRUE,
    shared_factor_within_sequence = TRUE,
    posterior_summaries_used = FALSE,
    note = paste(
      "Response-count and pair probabilities integrate one common factor draw",
      "over the entire respondent sequence; they are not products of marginal",
      "mixed probabilities."))
  class(out) <- c("scmix_heldout_predictions", "list")
  out
}

#' Convert fit-aware predictions to Section 4 assessment objects
#'
#' @param predictions Result from [scmix_heldout_predictions()].
#' @param design_cell,respondent_group Optional task-row strata.
#' @return Held-out sequence-score and calibration objects carrying the same
#'   nonempty `analysis_signature` as the fit-aware prediction object.
#' @export
scmix_prediction_assessment <- function(predictions, design_cell = NULL,
                                        respondent_group = NULL) {
  if (!inherits(predictions, "scmix_heldout_predictions")) {
    stop("`predictions` must come from `scmix_heldout_predictions()`.",
         call. = FALSE)
  }
  analysis_signature <- predictions$analysis_signature
  if (!is.character(analysis_signature) || length(analysis_signature) != 1L ||
      is.na(analysis_signature) || !nzchar(analysis_signature)) {
    stop("Fit-aware predictions must carry one nonempty `analysis_signature`.",
         call. = FALSE)
  }
  task <- predictions$task
  score <- scmix_heldout_sequence_score(
    predictions$sequence_loglik, predictions$respondent_id,
    out_of_fold = predictions$out_of_fold,
    training_only_tuning = predictions$training_only_tuning,
    provenance = predictions$provenance,
    analysis_signature = analysis_signature)
  calibration <- scmix_heldout_calibration(
    y = task$observed, predicted = task$predicted,
    respondent_id = task$respondent_id,
    design_cell = design_cell, respondent_group = respondent_group,
    task_order = task$task_order, joint = predictions$joint,
    out_of_fold = predictions$out_of_fold,
    training_only_tuning = predictions$training_only_tuning,
    provenance = predictions$provenance,
    analysis_signature = analysis_signature)
  list(score = score, calibration = calibration, predictions = predictions)
}
