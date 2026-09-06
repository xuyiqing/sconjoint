## Explicit loading-scale contract for paperps v2.2.
##
## The likelihood is evaluated after dividing every raw contrast coordinate by
## a training-sample scale d_j.  Consequently the network parameter is
## A_opt = diag(d) A_raw.  Structural quantities use A_raw.  Likelihood and
## network-state calculations use A_opt together with the standardized design.
## These helpers keep both representations explicit and make it impossible for
## downstream code to divide an already-raw loading by d a second time.

.scmix_loading_matrix <- function(x, p, q, what) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!identical(dim(x), c(as.integer(p), as.integer(q))) ||
      any(!is.finite(x))) {
    stop("`", what, "` must be a finite ", p, " by ", q,
         " loading matrix.", call. = FALSE)
  }
  x
}

.scmix_loading_scale <- function(x, p, what) {
  x <- as.numeric(x)
  if (length(x) != p || any(!is.finite(x)) || any(x <= 0)) {
    stop("`", what, "` must contain one finite positive value per loading row.",
         call. = FALSE)
  }
  x
}

.scmix_loading_to_optimization <- function(A_raw, coefficient_scale) {
  out <- sweep(as.matrix(A_raw), 1L, as.numeric(coefficient_scale), `*`)
  dimnames(out) <- dimnames(A_raw)
  out
}

.scmix_loading_to_raw <- function(A_optimization, coefficient_scale) {
  out <- sweep(as.matrix(A_optimization), 1L, as.numeric(coefficient_scale), `/`)
  dimnames(out) <- dimnames(A_optimization)
  out
}

.scmix_add_loading_contract <- function(object, q,
                                        raw_full = NULL,
                                        optimization_full = NULL,
                                        coefficient_scale_full = NULL,
                                        raw_folds = NULL,
                                        optimization_folds = NULL,
                                        coefficient_scale_folds = NULL,
                                        source = NULL) {
  q <- as.integer(q)
  p <- if (!is.null(coefficient_scale_full)) {
    length(coefficient_scale_full)
  } else if (length(coefficient_scale_folds)) {
    length(coefficient_scale_folds[[1L]])
  } else {
    stop("A loading contract requires a full or fold coefficient scale.",
         call. = FALSE)
  }
  tolerance <- 100 * .Machine$double.eps
  full_available <- !is.null(raw_full) || !is.null(optimization_full)
  if (full_available) {
    coefficient_scale_full <- .scmix_loading_scale(
      coefficient_scale_full, p, "coefficient_scale_full")
    if (is.null(raw_full)) {
      optimization_full <- .scmix_loading_matrix(
        optimization_full, p, q, "optimization_full")
      raw_full <- .scmix_loading_to_raw(
        optimization_full, coefficient_scale_full)
    } else {
      raw_full <- .scmix_loading_matrix(raw_full, p, q, "raw_full")
    }
    if (is.null(optimization_full)) {
      optimization_full <- .scmix_loading_to_optimization(
        raw_full, coefficient_scale_full)
    } else {
      optimization_full <- .scmix_loading_matrix(
        optimization_full, p, q, "optimization_full")
    }
  }

  fold_available <- !is.null(raw_folds) || !is.null(optimization_folds)
  if (fold_available) {
    K <- max(length(raw_folds), length(optimization_folds),
             length(coefficient_scale_folds))
    if (!is.list(coefficient_scale_folds) ||
        length(coefficient_scale_folds) != K ||
        (!is.null(raw_folds) && (!is.list(raw_folds) || length(raw_folds) != K)) ||
        (!is.null(optimization_folds) &&
         (!is.list(optimization_folds) || length(optimization_folds) != K))) {
      stop("Fold loading representations and scales must have the same length.",
           call. = FALSE)
    }
    coefficient_scale_folds <- lapply(seq_len(K), function(k) {
      .scmix_loading_scale(coefficient_scale_folds[[k]], p,
                           paste0("coefficient_scale_folds[[", k, "]]"))
    })
    if (is.null(raw_folds)) {
      raw_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_to_raw(
          .scmix_loading_matrix(optimization_folds[[k]], p, q,
                                paste0("optimization_folds[[", k, "]]")),
          coefficient_scale_folds[[k]])
      })
    } else {
      raw_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_matrix(raw_folds[[k]], p, q,
                              paste0("raw_folds[[", k, "]]"))
      })
    }
    if (is.null(optimization_folds)) {
      optimization_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_to_optimization(
          raw_folds[[k]], coefficient_scale_folds[[k]])
      })
    } else {
      optimization_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_matrix(optimization_folds[[k]], p, q,
                              paste0("optimization_folds[[", k, "]]"))
      })
    }
  } else {
    K <- 0L
  }

  errors <- numeric()
  if (full_available) {
    errors <- c(errors, max(abs(
      .scmix_loading_to_optimization(raw_full, coefficient_scale_full) -
        optimization_full), 0))
  }
  if (fold_available) {
    errors <- c(errors, vapply(seq_len(K), function(k) {
      max(abs(.scmix_loading_to_optimization(
        raw_folds[[k]], coefficient_scale_folds[[k]]) -
          optimization_folds[[k]]), 0)
    }, numeric(1L)))
  }
  max_error <- if (length(errors)) max(errors) else NA_real_
  reference <- c(
    if (full_available) as.numeric(optimization_full) else numeric(),
    if (fold_available) unlist(optimization_folds, use.names = FALSE) else numeric())
  round_trip_tolerance <- tolerance * max(1, abs(reference))
  round_trip_pass <- is.finite(max_error) &&
    max_error <= max(round_trip_tolerance)
  if (!round_trip_pass) {
    stop("Raw and optimization-scale loadings do not satisfy A_opt = diag(d) A_raw; ",
         "maximum discrepancy is ", signif(max_error, 8), ".", call. = FALSE)
  }

  object$A_raw_full <- if (full_available) raw_full else NULL
  object$A_optimization_full <- if (full_available) optimization_full else NULL
  object$A_raw_folds <- if (fold_available) raw_folds else NULL
  object$A_optimization_folds <- if (fold_available) optimization_folds else NULL
  object$loading_contract <- list(
    contract_id = "scmix-loading-scale-v1",
    contract_version = 1L,
    canonical_structural_scale = "raw",
    raw_to_optimization = "A_optimization = diag(coefficient_scale) %*% A_raw",
    design_to_optimization =
      "deltaX_optimization = deltaX_raw %*% diag(1 / coefficient_scale)",
    fitted_index_identity = paste(
      "deltaX_raw %*% A_raw equals deltaX_optimization %*%",
      "A_optimization for every integration draw"),
    full = list(
      available = full_available,
      raw_field = if (full_available) "A_raw_full" else NA_character_,
      optimization_field = if (full_available) "A_optimization_full" else
        NA_character_,
      coefficient_scale = if (full_available) coefficient_scale_full else NULL),
    folds = list(
      available = fold_available,
      count = as.integer(K),
      raw_field = if (fold_available) "A_raw_folds" else NA_character_,
      optimization_field = if (fold_available) "A_optimization_folds" else
        NA_character_,
      coefficient_scale = if (fold_available) coefficient_scale_folds else NULL),
    legacy_aliases = list(
      A_hat = "raw full-sample loading in scmix objects",
      A = "raw full-sample loading in scmix_tuned_matrix_fit objects",
      A_folds = "raw outer-fold loadings",
      A_computational_folds = paste(
        "legacy name for raw outer-fold loadings; do not infer scale from",
        "this alias")),
    source = source,
    construction_check = list(
      max_round_trip_error = max_error,
      tolerance = max(round_trip_tolerance),
      pass = round_trip_pass)
  )
  object
}

.scmix_validate_loading_contract <- function(object) {
  contract <- object$loading_contract
  if (!is.list(contract) ||
      !identical(contract$contract_id, "scmix-loading-scale-v1") ||
      !identical(contract$contract_version, 1L)) {
    stop("The object lacks an explicit supported loading-scale contract. ",
         "Legacy loading aliases are not paper-eligible because their scale is ",
         "ambiguous. Refit or explicitly migrate the artifact.", call. = FALSE)
  }
  check_pair <- function(raw, optimization, coefficient_scale, label) {
    raw <- as.matrix(raw)
    optimization <- as.matrix(optimization)
    if (!identical(dim(raw), dim(optimization)) || any(!is.finite(raw)) ||
        any(!is.finite(optimization)) ||
        length(coefficient_scale) != nrow(raw) ||
        any(!is.finite(coefficient_scale)) || any(coefficient_scale <= 0)) {
      stop("Malformed ", label, " loading-scale contract.", call. = FALSE)
    }
    expected <- .scmix_loading_to_optimization(raw, coefficient_scale)
    tolerance <- 100 * .Machine$double.eps *
      max(1, abs(as.numeric(optimization)))
    if (max(abs(expected - optimization), 0) > max(tolerance)) {
      stop("The ", label, " loading contract fails its round-trip identity.",
           call. = FALSE)
    }
  }
  if (isTRUE(contract$full$available)) {
    check_pair(object[[contract$full$raw_field]],
               object[[contract$full$optimization_field]],
               contract$full$coefficient_scale, "full-sample")
  }
  if (isTRUE(contract$folds$available)) {
    raw <- object[[contract$folds$raw_field]]
    optimization <- object[[contract$folds$optimization_field]]
    scales <- contract$folds$coefficient_scale
    K <- contract$folds$count
    if (!is.list(raw) || !is.list(optimization) || !is.list(scales) ||
        length(raw) != K || length(optimization) != K ||
        length(scales) != K) {
      stop("Malformed fold loading-scale contract.", call. = FALSE)
    }
    for (k in seq_len(K)) {
      check_pair(raw[[k]], optimization[[k]], scales[[k]],
                 paste0("fold ", k))
    }
  }
  invisible(contract)
}

#' Extract mixed-logit loadings on an explicit scale
#'
#' Returns the loading paired with either the raw conjoint contrasts or the
#' standardized contrasts used by the optimizer. New fits store both forms and
#' a versioned round-trip contract. Legacy aliases such as `A`, `A_hat`, and
#' `A_computational_folds` are deliberately not guessed by this accessor.
#'
#' @param object An `scmix`, `scmix_tuned_matrix_fit`,
#'   `scmix_nested_assembled`, or portable `scmix_network_state` object.
#' @param scale Either `"raw"` or `"optimization"`.
#' @param scope Either `"full"` or `"fold"`.
#' @param fold Optional positive fold index. When omitted, all fold loadings
#'   are returned as a list.
#' @return A numeric loading matrix, or a list of matrices when all folds are
#'   requested. Attributes record the scale, scope, contract ID, and associated
#'   contrast scale.
#' @export
scmix_loadings <- function(object, scale = c("raw", "optimization"),
                           scope = c("full", "fold"), fold = NULL) {
  scale <- match.arg(scale)
  scope <- match.arg(scope)

  if (inherits(object, "scmix_network_state")) {
    .scmix_validate_network_state(object)
    if (scope != "full") {
      stop("A portable network state contains one loading; use scope = 'full'.",
           call. = FALSE)
    }
    a <- object$architecture
    coefficient_scale <- .scmix_loading_scale(
      a$coefficient_scale, a$p, "architecture$coefficient_scale")
    optimization <- if (a$q == 0L) {
      matrix(numeric(0), a$p, 0L)
    } else {
      .scmix_loading_matrix(object$state_dict$A, a$p, a$q, "state_dict$A")
    }
    value <- if (scale == "optimization") optimization else
      .scmix_loading_to_raw(optimization, coefficient_scale)
    rownames(value) <- object$coefficient_names
    attr(value, "loading_contract_id") <- "scmix-loading-scale-v1"
    attr(value, "loading_scale") <- scale
    attr(value, "loading_scope") <- "network_state"
    attr(value, "coefficient_scale") <- coefficient_scale
    return(value)
  }

  contract <- .scmix_validate_loading_contract(object)
  branch <- contract[[if (scope == "full") "full" else "folds"]]
  if (!isTRUE(branch$available)) {
    stop("The requested loading scope is not available in this object.",
         call. = FALSE)
  }
  field <- branch[[if (scale == "raw") "raw_field" else
    "optimization_field"]]
  value <- object[[field]]
  coefficient_scale <- branch$coefficient_scale
  if (scope == "fold") {
    if (!is.null(fold)) {
      if (!is.numeric(fold) || length(fold) != 1L || is.na(fold) ||
          fold != as.integer(fold) || fold < 1L || fold > length(value)) {
        stop("`fold` must identify one available fold.", call. = FALSE)
      }
      value <- value[[as.integer(fold)]]
      coefficient_scale <- coefficient_scale[[as.integer(fold)]]
    }
  } else if (!is.null(fold)) {
    stop("`fold` is only used with scope = 'fold'.", call. = FALSE)
  }
  attr(value, "loading_contract_id") <- contract$contract_id
  attr(value, "loading_scale") <- scale
  attr(value, "loading_scope") <- scope
  attr(value, "coefficient_scale") <- coefficient_scale
  value
}
