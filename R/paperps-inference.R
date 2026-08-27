## Finite-sieve, respondent-level orthogonal inference for the model in
## paperps.tex.
##
## This module is intentionally separate from the older binned-information
## routines in mixed-inference.R.  It implements a high-level post-estimation
## procedure on an explicitly finite structural tangent:
##
##   (i)   one direction for the alternative/position constant kappa;
##   (ii)  mu(z) = C' phi(z) locally, where phi is a finite basis; and
##   (iii) rotation-horizontal loading directions for A, so vertical
##         directions A Omega, Omega' = -Omega, never enter the information
##         matrix.
##
## Every score is the Fisher score of an actually observed respondent's
## complete response sequence.  This file never resamples task rows.  Riesz
## representers are estimated only on the training respondents of each outer
## fold and evaluated on that fold's held-out respondents.  The returned
## influence contribution includes H_i - E_n H_i, the direct empirical-P_Z
## term, in addition to the likelihood-score correction.
##
## The implementation does not prove DNN approximation, product-rate,
## stochastic-equicontinuity, or numerical-integration conditions.  Its
## inferential interpretation is conditional on those high-level conditions,
## on the finite sieve used here, and on negligible likelihood/derivative
## approximation error.

.scmix_dml_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.scmix_dml_as_matrix <- function(x, nrow_expected = NULL, what = "object") {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1L)
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!is.null(nrow_expected) && nrow(x) != nrow_expected) {
    .scmix_dml_stop(what, " must have ", nrow_expected,
                    " rows; got ", nrow(x), ".")
  }
  if (any(!is.finite(x))) .scmix_dml_stop(what, " contains non-finite values.")
  x
}

.scmix_dml_dim_equal <- function(x, expected) {
  !is.null(dim(x)) && length(dim(x)) == length(expected) &&
    all(dim(x) == as.integer(expected))
}

.scmix_dml_positive_integer <- function(x, what, length_expected = NULL) {
  if (!is.null(length_expected) && length(x) != length_expected) {
    .scmix_dml_stop(what, " must have length ", length_expected, ".")
  }
  if (is.factor(x)) x <- as.character(x)
  xn <- suppressWarnings(as.numeric(x))
  if (length(xn) != length(x) || anyNA(xn) || any(!is.finite(xn)) ||
      any(xn <= 0) || any(xn != round(xn)) ||
      any(xn > .Machine$integer.max)) {
    .scmix_dml_stop(what, " must contain positive integer identifiers; ",
                    "fractional, nonnumeric, missing, and non-finite values ",
                    "are not allowed.")
  }
  as.integer(xn)
}

## Evaluate a stochastic operation without changing the caller's RNG stream.
.scmix_dml_with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

.scmix_dml_offset_seed <- function(seed, offset) {
  as.integer((as.double(seed) + as.double(offset)) %%
               (.Machine$integer.max - 1L))
}

.scmix_dml_layout <- function(fit) {
  needed <- c("deltaX", "y", "respondent_id", "fold_id")
  miss <- needed[vapply(needed, function(nm) is.null(fit[[nm]]), logical(1L))]
  if (length(miss)) {
    .scmix_dml_stop("scmix_dml(): fit is missing required field(s): ",
                    paste(miss, collapse = ", "), ".")
  }
  dx <- .scmix_dml_as_matrix(fit$deltaX, what = "fit$deltaX")
  if (ncol(dx) < 1L) {
    .scmix_dml_stop("scmix_dml(): fit$deltaX must contain at least one ",
                    "identified attribute contrast.")
  }
  n_task <- nrow(dx)
  y <- as.numeric(fit$y)
  if (length(y) != n_task || any(!y %in% c(0, 1))) {
    .scmix_dml_stop("scmix_dml(): fit$y must be a binary vector with one entry ",
                    "per task row.")
  }
  rid <- as.character(fit$respondent_id)
  if (length(rid) != n_task || anyNA(rid)) {
    .scmix_dml_stop("scmix_dml(): fit$respondent_id must have one nonmissing ",
                    "entry per task row.")
  }
  resp <- unique(rid)
  resp_index <- match(rid, resp)
  N <- length(resp)
  first <- match(resp, rid)

  fold_raw <- fit$fold_id
  if (length(fold_raw) == n_task) {
    fold_task <- .scmix_dml_positive_integer(
      fold_raw, "scmix_dml(): fit$fold_id", n_task)
    fold_resp <- integer(N)
    for (i in seq_len(N)) {
      ui <- unique(fold_task[resp_index == i])
      if (length(ui) != 1L || is.na(ui)) {
        .scmix_dml_stop("scmix_dml(): outer fold assignment must be constant ",
                        "within respondent ", resp[i], ".")
      }
      fold_resp[i] <- ui
    }
  } else if (length(fold_raw) == N) {
    fold_resp <- .scmix_dml_positive_integer(
      fold_raw, "scmix_dml(): fit$fold_id", N)
    fold_task <- fold_resp[resp_index]
  } else {
    .scmix_dml_stop("scmix_dml(): fit$fold_id must have either one entry per ",
                    "task or one entry per respondent.")
  }
  K <- if (!is.null(fit$K)) {
    .scmix_dml_positive_integer(fit$K, "scmix_dml(): fit$K", 1L)
  } else max(fold_resp)
  if (K < 2L || !setequal(unique(fold_resp), seq_len(K))) {
    .scmix_dml_stop("scmix_dml(): at least two nonempty respondent-level outer ",
                    "folds numbered 1,...,K are required.")
  }

  Z0 <- if (!is.null(fit$Z_resp)) fit$Z_resp else fit$Z
  if (is.null(Z0)) {
    Z_resp <- matrix(numeric(0), N, 0L)
  } else {
    Z0 <- .scmix_dml_as_matrix(Z0, what = "fit$Z (or fit$Z_resp)")
    if (nrow(Z0) == n_task) {
      Z_resp <- Z0[first, , drop = FALSE]
      for (j in seq_len(ncol(Z_resp))) {
        bad <- vapply(seq_len(N), function(i) {
          zi <- Z0[resp_index == i, j]
          max(abs(zi - zi[1L])) > 1e-10
        }, logical(1L))
        if (any(bad)) {
          .scmix_dml_stop("scmix_dml(): moderator column ", j,
                          " is not constant within respondent.")
        }
      }
    } else if (nrow(Z0) == N) {
      Z_resp <- Z0
    } else {
      .scmix_dml_stop("scmix_dml(): moderator matrix must have one row per ",
                      "respondent or task.")
    }
  }
  if (is.null(colnames(Z_resp)) && ncol(Z_resp)) {
    colnames(Z_resp) <- paste0("z", seq_len(ncol(Z_resp)))
  }
  list(deltaX = dx, y = y, respondent_id = rid, resp = resp,
       resp_index = resp_index, N = N, n_task = n_task, p = ncol(dx),
       first = first, fold_task = fold_task, fold_resp = fold_resp,
       K = K, Z_resp = Z_resp)
}

.scmix_dml_resolve_kappa <- function(fit, K, kappa_folds = NULL) {
  kap <- if (!is.null(kappa_folds)) kappa_folds else fit$kappa_folds
  if (is.null(kap)) {
    .scmix_dml_stop(
      "scmix_dml(): fold-specific kappa estimates are required. The legacy ",
      "scmix fit fixes/omits kappa; refit the paperps model or explicitly pass ",
      "kappa_folds = rep(0, K) only when zero is a maintained normalization."
    )
  }
  kap <- as.numeric(unlist(kap, use.names = FALSE))
  if (length(kap) != K || any(!is.finite(kap))) {
    .scmix_dml_stop("scmix_dml(): kappa_folds must contain one finite scalar ",
                    "for each outer fold.")
  }
  kap
}

.scmix_dml_resolve_A_gh <- function(fit, p, K) {
  q_raw <- if (is.null(fit$q)) {
    A0 <- if (!is.null(fit$A_computational_folds)) {
      fit$A_computational_folds
    } else fit$A_folds
    if (is.null(A0) || !length(A0)) 0L else ncol(A0[[1L]])
  } else fit$q
  if (!is.numeric(q_raw) || length(q_raw) != 1L || !is.finite(q_raw) ||
      q_raw != as.integer(q_raw) || q_raw < 0L || q_raw > p - 1L) {
    .scmix_dml_stop("scmix_dml(): fit$q must be an integer in 0,...,p-1, ",
                    "as required by the maintained model.")
  }
  q <- as.integer(q_raw)
  if (q == 0L) {
    A <- replicate(K, matrix(numeric(0), p, 0L), simplify = FALSE)
    gh <- list(U = matrix(numeric(0), 1L, 0L), w = 1)
    return(list(q = q, A_folds = A, A_full = matrix(numeric(0), p, 0L),
                A_source = "q=0", A_full_source = "q=0",
                gh_folds = replicate(K, gh, simplify = FALSE)))
  }
  ## Scores must use the loading actually optimized with the stored finite
  ## nodes. A post-fit Procrustes rotation generally changes a finite-node
  ## approximation even though it leaves AA' unchanged in the exact model.
  A_source <- if (!is.null(fit$A_computational_folds)) {
    "fit$A_computational_folds"
  } else "fit$A_folds"
  A <- if (!is.null(fit$A_computational_folds)) {
    fit$A_computational_folds
  } else fit$A_folds
  if (!is.list(A) || length(A) != K) {
    .scmix_dml_stop("scmix_dml(): the computational loading field must ",
                    "contain one p-by-q matrix for each outer fold.")
  }
  A <- lapply(seq_len(K), function(k) {
    Ak <- .scmix_dml_as_matrix(A[[k]], what = paste0(A_source, "[[", k, "]]"))
    if (!.scmix_dml_dim_equal(Ak, c(p, q))) {
      .scmix_dml_stop("scmix_dml(): ", A_source, "[[", k, "]] must be ",
                      p, " by ", q, ".")
    }
    Ak
  })
  A_full_source <- if (!is.null(fit$A_computational_full)) {
    "fit$A_computational_full"
  } else if (!is.null(fit$A_full_computational)) {
    "fit$A_full_computational"
  } else if (!is.null(fit$A_hat)) {
    ## In current scmix fits A_hat is the unrotated full-sample optimizer.
    "fit$A_hat"
  } else NA_character_
  A_full <- if (is.na(A_full_source)) NULL else fit[[sub("^fit\\$", "", A_full_source)]]
  if (!is.null(A_full)) {
    A_full <- .scmix_dml_as_matrix(A_full, what = A_full_source)
    if (!.scmix_dml_dim_equal(A_full, c(p, q))) {
      .scmix_dml_stop("scmix_dml(): ", A_full_source, " must be ", p,
                      " by ", q, ".")
    }
  }
  gh0 <- if (!is.null(fit$gh_folds)) {
    fit$gh_folds
  } else if (!is.null(fit$integration_grids_folds)) {
    fit$integration_grids_folds
  } else {
    if (is.null(fit$gh)) NULL else replicate(K, fit$gh, simplify = FALSE)
  }
  if (!is.list(gh0) || length(gh0) != K) {
    .scmix_dml_stop("scmix_dml(): fit$gh_folds must contain one quadrature ",
                    "grid per outer fold (or fit$gh must contain one common grid).")
  }
  gh <- lapply(seq_len(K), function(k) {
    gk <- gh0[[k]]
    if (!is.list(gk) || is.null(gk$U) || is.null(gk$w)) {
      .scmix_dml_stop("scmix_dml(): quadrature grid for fold ", k,
                      " must contain U and w.")
    }
    U <- .scmix_dml_as_matrix(gk$U,
                              what = paste0("quadrature U for fold ", k))
    w <- as.numeric(gk$w)
    if (ncol(U) != q || length(w) != nrow(U) || any(!is.finite(w)) ||
        any(w <= 0) || !is.finite(sum(w)) || sum(w) <= 0) {
      .scmix_dml_stop("scmix_dml(): quadrature nodes/weights for fold ", k,
                      " are incompatible with q or invalid.")
    }
    list(U = U, w = w / sum(w))
  })
  list(q = q, A_folds = A, A_full = A_full, A_source = A_source,
       A_full_source = A_full_source, gh_folds = gh)
}

.scmix_dml_fold_network_Z <- function(fit, Z, k, K) {
  transforms <- if (!is.null(fit$z_preprocess_folds)) {
    fit$z_preprocess_folds
  } else fit$z_transform_folds
  if (is.function(fit$apply_z_preprocess)) {
    out <- fit$apply_z_preprocess(Z = Z, fold = k, source = "training")
    return(.scmix_dml_as_matrix(out, nrow(Z),
                                "fit$apply_z_preprocess() result"))
  }
  if (is.null(transforms) || !is.list(transforms) || length(transforms) != K) {
    .scmix_dml_stop(
      "scmix_dml(): stored fold networks require their fold-specific, ",
      "training-only moderator preprocessors (fit$z_preprocess_folds or ",
      "fit$z_transform_folds). Raw Z is deliberately not passed to a ",
      "network trained on transformed moderators."
    )
  }
  tr <- transforms[[k]]
  if (is.function(tr)) {
    out <- tr(Z)
  } else if (exists(".sc_apply_z_transform", mode = "function")) {
    out <- .sc_apply_z_transform(Z, tr)
  } else if (is.list(tr) && !is.null(tr$center) && !is.null(tr$scale)) {
    if (length(tr$center) != ncol(Z) || length(tr$scale) != ncol(Z)) {
      .scmix_dml_stop("scmix_dml(): fold moderator preprocessor has ",
                      "incompatible dimension.")
    }
    out <- sweep(sweep(Z, 2L, tr$center, `-`), 2L, tr$scale, `/`)
  } else {
    .scmix_dml_stop("scmix_dml(): cannot apply the stored moderator ",
                    "preprocessor for fold ", k, ".")
  }
  .scmix_dml_as_matrix(out, nrow(Z),
                       paste0("fold ", k, " preprocessed moderators"))
}

.scmix_dml_resolve_mu <- function(fit, layout, mu_by_fold = NULL) {
  src <- if (!is.null(mu_by_fold)) mu_by_fold else {
    if (!is.null(fit$mu_by_fold)) fit$mu_by_fold else fit$mu_all_folds
  }
  N <- layout$N; n_task <- layout$n_task; p <- layout$p; K <- layout$K
  normalize_one <- function(x, k) {
    x <- .scmix_dml_as_matrix(x, what = paste0("mu_by_fold[[", k, "]]"))
    if (ncol(x) != p) {
      .scmix_dml_stop("scmix_dml(): mu_by_fold[[", k, "]] must have p columns.")
    }
    if (nrow(x) == n_task) {
      for (i in seq_len(N)) {
        xi <- x[layout$resp_index == i, , drop = FALSE]
        if (any(abs(sweep(xi, 2L, xi[1L, ], `-`)) > 1e-10)) {
          .scmix_dml_stop("scmix_dml(): task-row mean predictions in fold ",
                          k, " are not constant within respondent ",
                          layout$resp[i], ".")
        }
      }
      x <- x[layout$first, , drop = FALSE]
    }
    if (nrow(x) != N) {
      .scmix_dml_stop("scmix_dml(): each all-fold mean prediction must have ",
                      "one row per respondent (or per task).")
    }
    x
  }
  if (!is.null(src)) {
    if (is.array(src) && length(dim(src)) == 3L) {
      if (dim(src)[3L] != K) .scmix_dml_stop("scmix_dml(): mu array third dimension must equal K.")
      return(lapply(seq_len(K), function(k) normalize_one(src[, , k], k)))
    }
    if (!is.list(src) || length(src) != K) {
      .scmix_dml_stop("scmix_dml(): mu_by_fold must be a K-element list or ",
                      "an N-by-p-by-K array.")
    }
    return(lapply(seq_len(K), function(k) normalize_one(src[[k]], k)))
  }

  ## Portable states are the durable source of network predictions. An
  ## nn_module embedded directly in an RDS object contains stale external
  ## pointers after reload, so never prefer it when a state bundle is present.
  network_states <- if (!is.null(fit$network_states)) {
    fit$network_states
  } else fit$network_state_folds
  if (!is.null(network_states)) {
    if (!is.list(network_states) || length(network_states) != K ||
        !all(vapply(network_states, inherits, logical(1L),
                    what = "scmix_network_state"))) {
      .scmix_dml_stop("scmix_dml(): fold portable network states are malformed.")
    }
    if (!ncol(layout$Z_resp)) {
      .scmix_dml_stop("scmix_dml(): stored network states require respondent ",
                      "moderators to construct all-fold mean predictions.")
    }
    return(lapply(seq_len(K), function(k) {
      normalize_one(
        scmix_predict_network(network_states[[k]], layout$Z_resp,
                              input = "raw", output = "raw"), k)
    }))
  }

  ## Graceful compatibility with a fitted network: unlike fit$mu_hat (which
  ## is only out-of-fold), a stored fold network can predict every respondent
  ## under the same training-fold nuisance used for that fold's Riesz fit.
  if (!is.null(fit$nets) && length(fit$nets) == K &&
      exists(".sc_predict_beta", mode = "function")) {
    if (!ncol(layout$Z_resp)) {
      .scmix_dml_stop("scmix_dml(): stored networks require respondent moderators ",
                      "to construct all-fold mean predictions.")
    }
    return(lapply(seq_len(K), function(k) {
      if (is.null(fit$nets[[k]])) {
        .scmix_dml_stop("scmix_dml(): fit$nets[[", k, "]] is missing.")
      }
      sd_dx <- if (!is.null(fit$sd_dx_folds)) {
        fit$sd_dx_folds[[k]]
      } else if (!is.null(fit$sd_dx)) {
        fit$sd_dx
      } else rep(1, p)
      sd_dx <- as.numeric(sd_dx)
      if (length(sd_dx) != p || any(!is.finite(sd_dx)) || any(sd_dx == 0)) {
        .scmix_dml_stop("scmix_dml(): invalid fold-specific DeltaX scale for ",
                        "network prediction in fold ", k, ".")
      }
      Z_k <- .scmix_dml_fold_network_Z(fit, layout$Z_resp, k, K)
      net_k <- .scmix_resolve_network(
        net = fit$nets[[k]], what = paste0("Fold network ", k))
      pred <- .sc_predict_beta(net_k, Z_k)
      normalize_one(sweep(pred, 2L, sd_dx, `/`), k)
    }))
  }
  .scmix_dml_stop(
    "scmix_dml(): all-fold mean predictions are required to estimate each ",
    "Riesz representer on training respondents. Supply mu_by_fold, store ",
    "fit$mu_by_fold/fit$mu_all_folds, or retain usable fold networks. The ",
    "out-of-fold-only fit$mu_hat is deliberately not reused on training folds."
  )
}

#' Rotation-horizontal loading tangent
#'
#' Returns an orthonormal basis for the Frobenius-orthogonal complement of
#' `{A Omega: Omega' = -Omega}`.  These are the loading directions that change
#' `A A'`; pure rotations are excluded.
#' @keywords internal
#' @noRd
.scmix_horizontal_basis <- function(A, tol = 1e-10) {
  A <- as.matrix(A)
  p <- nrow(A); q <- ncol(A); pq <- p * q
  if (q == 0L) return(matrix(numeric(0), 0L, 0L))
  if (q == 1L) return(diag(pq))
  vdim <- q * (q - 1L) / 2L
  V <- matrix(0, pq, vdim)
  cc <- 0L
  for (r in seq_len(q - 1L)) {
    for (s in (r + 1L):q) {
      cc <- cc + 1L
      Om <- matrix(0, q, q)
      Om[r, s] <- 1; Om[s, r] <- -1
      V[, cc] <- as.vector(A %*% Om)
    }
  }
  qv <- qr(V, tol = tol, LAPACK = FALSE)
  if (qv$rank != vdim) {
    .scmix_dml_stop("scmix_dml(): loading is rank-deficient, so the vertical ",
                    "rotation space does not have its regular dimension.")
  }
  Q <- qr.Q(qv, complete = TRUE)
  Q[, (vdim + 1L):pq, drop = FALSE]
}

.scmix_dml_rank_gate <- function(A_folds, q, active_eigenvalue_min,
                                 rank_tolerance, A_full = NULL,
                                 A_full_source = NA_character_) {
  if (q == 0L) {
    ans <- data.frame(component = c(paste0("fold ", seq_along(A_folds)),
                                    if (!is.null(A_full)) "full" else character()),
                      q = 0L, rank = 0L, min_active_eigenvalue = Inf,
                      rank_pass = TRUE, margin_supplied = NA,
                      margin_pass = NA, pass = TRUE,
                      stringsAsFactors = FALSE)
    attr(ans, "scope") <- if (is.null(A_full)) {
      "q=0 fold objects; no full loading was stored"
    } else "q=0 fold and full objects"
    return(ans)
  }
  mats <- A_folds
  names(mats) <- paste0("fold ", seq_along(A_folds))
  if (!is.null(A_full)) {
    mats[["full"]] <- A_full
  }
  out <- lapply(names(mats), function(component) {
    sv <- svd(mats[[component]], nu = 0L, nv = 0L)$d
    ev <- sv^2
    ## Numerical rank is relative to the loading's own scale.  The separate,
    ## prespecified active-eigenvalue margin supplies the substantive lower
    ## bound required for regular inference.
    scale <- max(ev)
    rk <- sum(ev > rank_tolerance * scale)
    mn <- if (length(ev) >= q) ev[q] else 0
    margin_supplied <- !is.null(active_eigenvalue_min)
    margin_pass <- margin_supplied && mn >= active_eigenvalue_min
    data.frame(component = component, q = q, rank = rk,
               min_active_eigenvalue = mn,
               rank_pass = rk == q,
               margin_supplied = margin_supplied,
               margin_pass = margin_pass,
               pass = rk == q && margin_pass,
               stringsAsFactors = FALSE)
  })
  ans <- do.call(rbind, out)
  attr(ans, "scope") <- if (is.null(A_full)) {
    "fold-specific computational loadings only; no full computational loading was stored"
  } else paste0("fold-specific and full computational loadings (full source: ",
                A_full_source, ")")
  ans
}

.scmix_dml_basis <- function(mu_basis, Z, train, fold, respondent_id,
                             tolerance = 1e-10) {
  N <- nrow(Z)
  if (is.function(mu_basis)) {
    B <- mu_basis(Z = Z, train = train, fold = fold,
                  respondent_id = respondent_id)
    B <- .scmix_dml_as_matrix(B, N, "mu_basis() result")
  } else if (is.list(mu_basis)) {
    if (length(mu_basis) < fold) .scmix_dml_stop("scmix_dml(): mu_basis list is shorter than K.")
    B <- .scmix_dml_as_matrix(mu_basis[[fold]], N,
                              paste0("mu_basis[[", fold, "]]"))
  } else if (!is.null(mu_basis)) {
    B <- .scmix_dml_as_matrix(mu_basis, N, "mu_basis")
  } else {
    ## Default finite sieve: an intercept and training-standardized linear
    ## moderator terms.  Centering/scaling and rank selection use training
    ## respondents only; the resulting map is then applied to all respondents.
    if (!ncol(Z)) {
      B <- matrix(1, N, 1L, dimnames = list(NULL, "(Intercept)"))
    } else {
      ctr <- colMeans(Z[train, , drop = FALSE])
      scl <- apply(Z[train, , drop = FALSE], 2L, stats::sd)
      keepz <- is.finite(scl) & scl > tolerance
      Zs <- if (any(keepz)) {
        sweep(sweep(Z[, keepz, drop = FALSE], 2L, ctr[keepz], `-`),
              2L, scl[keepz], `/`)
      } else matrix(numeric(0), N, 0L)
      B0 <- cbind(`(Intercept)` = 1, Zs)
      qB <- qr(B0[train, , drop = FALSE], tol = tolerance, LAPACK = FALSE)
      keep <- sort(qB$pivot[seq_len(qB$rank)])
      B <- B0[, keep, drop = FALSE]
    }
  }
  if (!ncol(B)) .scmix_dml_stop("scmix_dml(): the finite mu basis has no columns.")
  if (qr(B[train, , drop = FALSE], tol = tolerance)$rank < ncol(B)) {
    .scmix_dml_stop("scmix_dml(): the supplied mu basis is rank-deficient on ",
                    "training respondents in fold ", fold, ".")
  }
  if (is.null(colnames(B))) colnames(B) <- paste0("phi", seq_len(ncol(B)))
  B
}

#' Complete-sequence Fisher scores on a finite structural sieve
#'
#' No rows are simulated, resampled, or divided by task count.  Each output
#' row is the derivative of one respondent's integrated complete-sequence log
#' likelihood with respect to kappa, the coefficients of
#' `mu(z) = C' phi(z)`, and rotation-horizontal loading directions.
#'
#' @param deltaX Task-level contrast matrix.
#' @param y Binary task outcomes.
#' @param respondent_index Integer respondent index for every task.
#' @param mu_resp Respondent-level fold-specific conditional means.
#' @param kappa Fold-specific position/alternative constant.
#' @param A Fold-specific p-by-q loading matrix (p-by-0 when q=0).
#' @param gh Quadrature nodes and weights.
#' @param basis Respondent-level finite mu basis.
#' @param horizontal Orthonormal horizontal basis in vec(A) coordinates.
#' @return Respondent score matrix and integrated log likelihoods.
#' @keywords internal
#' @noRd
.scmix_sequence_scores_sieve <- function(deltaX, y, respondent_index,
                                          mu_resp, kappa, A, gh, basis,
                                          horizontal) {
  dx <- as.matrix(deltaX); y <- as.numeric(y)
  ridx <- as.integer(respondent_index)
  N <- nrow(mu_resp); p <- ncol(dx); q <- ncol(A); m <- ncol(basis)
  dA <- ncol(horizontal)
  out <- matrix(0, N, 1L + m * p + dA)
  ll <- numeric(N)
  nm_mu <- unlist(lapply(seq_len(p), function(j)
    paste0("mu:", colnames(basis), ":",
           if (is.null(colnames(dx))) paste0("b", j) else colnames(dx)[j])))
  colnames(out) <- c("kappa", nm_mu,
                     if (dA) paste0("A_horizontal:", seq_len(dA)) else character())
  U <- gh$U; w <- gh$w

  for (i in seq_len(N)) {
    rows <- which(ridx == i)
    if (!length(rows)) .scmix_dml_stop("internal error: respondent without tasks.")
    dxi <- dx[rows, , drop = FALSE]
    yi <- y[rows]
    base <- as.numeric(kappa + dxi %*% mu_resp[i, ])
    idx <- if (q == 0L) {
      matrix(base, ncol = 1L)
    } else {
      matrix(base, nrow = length(rows), ncol = nrow(U)) +
        (dxi %*% A) %*% t(U)
    }
    ymat <- matrix(yi, nrow = length(yi), ncol = ncol(idx))
    lp <- ifelse(ymat == 1, stats::plogis(idx, log.p = TRUE),
                 stats::plogis(-idx, log.p = TRUE))
    log_node <- colSums(lp) + log(w)
    mx <- max(log_node)
    lli <- mx + log(sum(exp(log_node - mx)))
    post <- exp(log_node - lli)
    resid <- ymat - stats::plogis(idx)
    rbar <- as.numeric(resid %*% post)
    s_kappa <- sum(rbar)
    s_mu <- colSums(dxi * rbar)
    s_A_h <- numeric(dA)
    if (q > 0L) {
      s_A <- matrix(0, p, q)
      for (r in seq_len(q)) {
        rbar_u <- as.numeric(resid %*% (post * U[, r]))
        s_A[, r] <- colSums(dxi * rbar_u)
      }
      s_A_h <- as.numeric(crossprod(horizontal, as.vector(s_A)))
    }
    out[i, ] <- c(s_kappa, as.vector(outer(basis[i, ], s_mu)), s_A_h)
    ll[i] <- lli
  }
  list(score = out, loglik = ll)
}

.scmix_dml_target_value <- function(fun, mu, kappa, A, Z, respondent_id,
                                    fold, attr_names) {
  ## The public target API is deliberately expressed through Sigma, never a
  ## particular factor A. This prevents a callback from defining an estimand
  ## that changes under an observationally irrelevant rotation of A.
  ans <- fun(mu = mu, kappa = kappa, Sigma = tcrossprod(A), Z = Z,
             respondent_id = respondent_id, fold = fold,
             attr_names = attr_names)
  if (!is.list(ans) || is.null(ans$value) ||
      !identical(ans$target_type, "rowwise_expectation")) {
    .scmix_dml_stop(
      "scmix_dml(): each plugin callback must return a list with `value` and ",
      "target_type = 'rowwise_expectation'. The implementation supports only ",
      "expectations of respondent-row primitives; generic nonlinear ",
      "functionals of P_Z require a separately derived influence function."
    )
  }
  if (any(c("d_A", "d_A_horizontal") %in% names(ans))) {
    .scmix_dml_stop(
      "scmix_dml(): plugin derivatives may use d_Sigma but not d_A or ",
      "d_A_horizontal; public targets must be invariant to factor rotations."
    )
  }
  val <- ans$value
  val <- .scmix_dml_as_matrix(val, nrow(mu), "plugin target value")
  labels <- if (!is.null(ans$labels)) as.character(ans$labels) else colnames(val)
  if (is.null(labels)) labels <- paste0("target", seq_len(ncol(val)))
  if (length(labels) != ncol(val)) .scmix_dml_stop("plugin target labels have wrong length.")
  list(raw = ans, value = val, labels = labels)
}

.scmix_dml_normalize_derivatives <- function(ans, N, J, p, q, dA,
                                             horizontal, A) {
  derivative_names <- intersect(names(ans),
                                c("d_mu", "d_kappa", "d_Sigma",
                                  "d_A", "d_A_horizontal"))
  if (!length(derivative_names)) return(NULL)
  if (any(c("d_A", "d_A_horizontal") %in% derivative_names)) {
    .scmix_dml_stop("scmix_dml(): raw-loading plugin derivatives are not ",
                    "supported; supply the rotation-invariant d_Sigma.")
  }
  if (is.null(ans$d_mu) || is.null(ans$d_kappa)) {
    .scmix_dml_stop("scmix_dml(): an analytic derivative declaration must ",
                    "include both d_mu and d_kappa.")
  }
  dm <- ans$d_mu
  if (J == 1L && is.matrix(dm) && .scmix_dml_dim_equal(dm, c(N, p))) {
    dm <- array(dm, c(N, 1L, p))
  }
  if (!is.array(dm) || !.scmix_dml_dim_equal(dm, c(N, J, p)) ||
      any(!is.finite(dm))) {
    .scmix_dml_stop("scmix_dml(): d_mu has invalid dimensions or values.")
  }
  dk <- ans$d_kappa
  if (is.null(dim(dk))) dk <- matrix(dk, ncol = J)
  dk <- as.matrix(dk)
  if (!.scmix_dml_dim_equal(dk, c(N, J)) || any(!is.finite(dk))) {
    .scmix_dml_stop("scmix_dml(): d_kappa has invalid dimensions or values.")
  }
  dah <- array(0, c(N, J, dA))
  if (q > 0L) {
    if (!is.null(ans$d_Sigma)) {
      x <- ans$d_Sigma
      if (J == 1L && is.array(x) && .scmix_dml_dim_equal(x, c(N, p, p))) {
        x <- array(x, c(N, 1L, p, p))
      }
      if (!is.array(x) || !.scmix_dml_dim_equal(x, c(N, J, p, p)) ||
          any(!is.finite(x))) {
        .scmix_dml_stop("scmix_dml(): d_Sigma has invalid dimensions or values.")
      }
      for (i in seq_len(N)) for (j in seq_len(J)) {
        Gs <- matrix(x[i, j, , ], p, p)
        dAr <- (Gs + t(Gs)) %*% A
        dah[i, j, ] <- as.numeric(crossprod(horizontal, as.vector(dAr)))
      }
    } else if (!isTRUE(ans$sigma_invariant)) {
      .scmix_dml_stop("scmix_dml(): for q > 0 an analytic callback must ",
                      "supply d_Sigma or explicitly set sigma_invariant = TRUE.")
    }
  }
  list(d_mu = dm, d_kappa = dk, d_A_horizontal = dah,
       source = "analytic", refinement_error = 0)
}

.scmix_dml_callback <- function(fun, mu, kappa, A, Z, respondent_id, fold,
                                attr_names, horizontal, eps,
                                allow_numeric, refinement_factor,
                                refinement_tolerance) {
  base <- .scmix_dml_target_value(fun, mu, kappa, A, Z, respondent_id,
                                  fold, attr_names)
  N <- nrow(mu); p <- ncol(mu); q <- ncol(A); J <- ncol(base$value)
  der <- .scmix_dml_normalize_derivatives(base$raw, N, J, p, q,
                                          ncol(horizontal), horizontal, A)
  if (!is.null(der)) {
    return(c(base[c("value", "labels")], der))
  }
  if (!isTRUE(allow_numeric)) {
    .scmix_dml_stop("scmix_dml(): plugin callback omitted analytic ",
                    "derivatives. Set allow_numeric_derivatives = TRUE to ",
                    "request refined central differences explicitly.")
  }
  differentiate <- function(h) {
    dm <- array(0, c(N, J, p))
    for (j in seq_len(p)) {
      up <- dn <- mu; up[, j] <- up[, j] + h; dn[, j] <- dn[, j] - h
      vp <- .scmix_dml_target_value(fun, up, kappa, A, Z, respondent_id,
                                    fold, attr_names)$value
      vm <- .scmix_dml_target_value(fun, dn, kappa, A, Z, respondent_id,
                                    fold, attr_names)$value
      dm[, , j] <- (vp - vm) / (2 * h)
    }
    vp <- .scmix_dml_target_value(fun, mu, kappa + h, A, Z, respondent_id,
                                  fold, attr_names)$value
    vm <- .scmix_dml_target_value(fun, mu, kappa - h, A, Z, respondent_id,
                                  fold, attr_names)$value
    dk <- (vp - vm) / (2 * h)
    dAh <- array(0, c(N, J, ncol(horizontal)))
    if (q > 0L) {
      for (a in seq_len(ncol(horizontal))) {
        D <- matrix(horizontal[, a], nrow(A), ncol(A))
        vp <- .scmix_dml_target_value(fun, mu, kappa, A + h * D, Z,
                                      respondent_id, fold, attr_names)$value
        vm <- .scmix_dml_target_value(fun, mu, kappa, A - h * D, Z,
                                      respondent_id, fold, attr_names)$value
        dAh[, , a] <- (vp - vm) / (2 * h)
      }
    }
    list(d_mu = dm, d_kappa = dk, d_A_horizontal = dAh)
  }
  coarse <- differentiate(eps)
  fine <- differentiate(eps / refinement_factor)
  vec <- function(x) unlist(x, recursive = TRUE, use.names = FALSE)
  den <- pmax(abs(vec(fine)), 1)
  err <- max(abs(vec(coarse) - vec(fine)) / den)
  list(value = base$value, labels = base$labels,
       d_mu = fine$d_mu, d_kappa = fine$d_kappa,
       d_A_horizontal = fine$d_A_horizontal, source = "numeric_refined",
       refinement_error = err,
       refinement_pass = is.finite(err) && err <= refinement_tolerance)
}

.scmix_dml_targets <- function(targets, plugin_targets, mu, kappa, A, Z,
                               respondent_id, fold, attr_names, basis,
                               horizontal, derivative_eps,
                               allow_numeric_derivatives,
                               derivative_refinement_factor,
                               derivative_refinement_tolerance) {
  N <- nrow(mu); p <- ncol(mu); m <- ncol(basis); dA <- ncol(horizontal)
  values <- list(); grads <- list(); labels <- character(); sources <- character()
  refinement_error <- numeric(); refinement_pass <- logical()
  if (length(targets)) {
    bad <- setdiff(targets, "theta")
    if (length(bad)) .scmix_dml_stop("scmix_dml(): unknown built-in target(s): ",
                                     paste(bad, collapse = ", "), ".")
  }
  if ("theta" %in% targets) {
    for (j in seq_len(p)) {
      values[[length(values) + 1L]] <- mu[, j]
      gj <- matrix(0, N, 1L + m * p + dA)
      for (i in seq_len(N)) {
        dm <- numeric(p); dm[j] <- 1
        gj[i, ] <- c(0, as.vector(outer(basis[i, ], dm)), numeric(dA))
      }
      grads[[length(grads) + 1L]] <- gj
      labels <- c(labels, paste0("theta:", attr_names[j]))
      sources <- c(sources, "analytic")
      refinement_error <- c(refinement_error, 0)
      refinement_pass <- c(refinement_pass, TRUE)
    }
  }
  if (!is.null(plugin_targets)) {
    if (is.function(plugin_targets)) plugin_targets <- list(plugin = plugin_targets)
    if (!is.list(plugin_targets) || !length(plugin_targets) ||
        !all(vapply(plugin_targets, is.function, logical(1L)))) {
      .scmix_dml_stop("scmix_dml(): plugin_targets must be a named list of ",
                      "smooth callback functions.")
    }
    if (is.null(names(plugin_targets)) || any(!nzchar(names(plugin_targets)))) {
      names(plugin_targets) <- paste0("plugin", seq_along(plugin_targets))
    }
    for (nm in names(plugin_targets)) {
      cb <- .scmix_dml_callback(plugin_targets[[nm]], mu, kappa, A, Z,
                                respondent_id, fold, attr_names, horizontal,
                                derivative_eps, allow_numeric_derivatives,
                                derivative_refinement_factor,
                                derivative_refinement_tolerance)
      J <- ncol(cb$value)
      for (j in seq_len(J)) {
        values[[length(values) + 1L]] <- cb$value[, j]
        gj <- matrix(0, N, 1L + m * p + dA)
        for (i in seq_len(N)) {
          gj[i, ] <- c(cb$d_kappa[i, j],
                       as.vector(outer(basis[i, ], cb$d_mu[i, j, ])),
                       if (dA) cb$d_A_horizontal[i, j, ] else numeric())
        }
        grads[[length(grads) + 1L]] <- gj
        labj <- if (J == 1L) nm else paste0(nm, ":", cb$labels[j])
        labels <- c(labels, labj)
        sources <- c(sources, cb$source)
        refinement_error <- c(refinement_error, cb$refinement_error)
        refinement_pass <- c(refinement_pass,
                             if (is.null(cb$refinement_pass)) TRUE else
                               cb$refinement_pass)
      }
    }
  }
  if (!length(values)) .scmix_dml_stop("scmix_dml(): at least one target is required.")
  list(value = do.call(cbind, values), gradient = grads,
       labels = make.unique(labels), derivative_source = sources,
       derivative_refinement_error = refinement_error,
       derivative_refinement_pass = refinement_pass)
}

.scmix_dml_contrast <- function(x, attr_names, what) {
  p <- length(attr_names)
  if (!is.numeric(x) || !length(x) || any(!is.finite(x))) {
    .scmix_dml_stop("scmix_inference_target(): ", what,
                    " must be a finite numeric contrast.")
  }
  if (!is.null(names(x))) {
    if (anyDuplicated(names(x)) || any(!nzchar(names(x))) ||
        length(setdiff(names(x), attr_names))) {
      .scmix_dml_stop("scmix_inference_target(): named ", what,
                      " contains duplicate, empty, or unknown coefficients.")
    }
    out <- stats::setNames(numeric(p), attr_names)
    out[names(x)] <- x
    return(unname(out))
  }
  if (length(x) != p) {
    .scmix_dml_stop("scmix_inference_target(): ", what,
                    " must have one entry per coefficient.")
  }
  as.numeric(x)
}

.scmix_dml_gh1 <- function(n_nodes) {
  if (!is.numeric(n_nodes) || length(n_nodes) != 1L || !is.finite(n_nodes) ||
      n_nodes < 3L || n_nodes != as.integer(n_nodes)) {
    .scmix_dml_stop("scmix_inference_target(): n_nodes must be an integer at least 3.")
  }
  n <- as.integer(n_nodes)
  J <- matrix(0, n, n)
  off <- sqrt(seq_len(n - 1L))
  J[cbind(seq_len(n - 1L), 2L:n)] <- off
  J[cbind(2L:n, seq_len(n - 1L))] <- off
  ee <- eigen(J, symmetric = TRUE)
  ord <- order(ee$values)
  list(x = ee$values[ord], w = ee$vectors[1L, ord]^2)
}

#' Typed rowwise primitives for the paper's quantities of interest
#'
#' Constructs rotation-invariant callbacks for `scmix_dml()`. The returned
#' callback targets only respondent-row expectations. Ratios and variance
#' shares are represented by primitive moments and must subsequently be mapped
#' with `scmix_delta_transform()`.
#'
#' @param type One of `"tau"`, `"choice"`, `"sign"`, `"compensating"`,
#'   `"subgroup_tau_primitives"`, `"mrs_primitives"`,
#'   `"heterogeneity_primitives"`, or `"covariance_primitives"`.
#' @param contrast A coefficient contrast for tau, choice, sign, subgroup, or
#'   directional heterogeneity.
#' @param numerator,denominator Contrasts for MRS primitives.
#' @param subgroup A respondent-level binary indicator for subgroup primitives.
#' @param penalty,benefit,amount Inputs defining the compensating contrast.
#' @param position_neutral Whether choice averages both display positions.
#' @param n_nodes One-dimensional normal quadrature nodes for choice.
#' @param variance_floor Explicit prespecified strictly positive
#'   directional-residual-variance gate required for regular sign and
#'   compensating-share primitives. Choice probabilities and heterogeneity
#'   moments remain defined at zero directional residual variance and may leave
#'   this argument `NULL`.
#' @param label Optional primitive label stem.
#' @return A typed callback accepted by `plugin_targets` in `scmix_dml()`.
#' @rdname scmix_dml
#' @export
scmix_inference_target <- function(
    type = c("tau", "choice", "sign", "compensating",
             "subgroup_tau_primitives", "mrs_primitives",
             "heterogeneity_primitives", "covariance_primitives"),
    contrast = NULL, numerator = NULL, denominator = NULL, subgroup = NULL,
    penalty = NULL, benefit = NULL, amount = NULL,
    position_neutral = FALSE, n_nodes = 31L,
    variance_floor = NULL, label = NULL) {
  type <- match.arg(type)
  if (!is.logical(position_neutral) || length(position_neutral) != 1L ||
      is.na(position_neutral)) {
    .scmix_dml_stop("scmix_inference_target(): position_neutral must be logical.")
  }
  threshold_type <- type %in% c("sign", "compensating")
  if (threshold_type && is.null(variance_floor)) {
    .scmix_dml_stop("scmix_inference_target(): sign and compensating targets ",
                    "require an explicit prespecified positive variance_floor.")
  }
  if (!is.null(variance_floor) &&
      (!is.numeric(variance_floor) || length(variance_floor) != 1L ||
       !is.finite(variance_floor) || variance_floor <= 0)) {
    .scmix_dml_stop("scmix_inference_target(): variance_floor must be positive ",
                    "when supplied.")
  }
  if (!is.null(label) && !.scmix_dml_nonempty_text(label)) {
    .scmix_dml_stop("scmix_inference_target(): label must be nonempty.")
  }
  gh <- if (identical(type, "choice")) .scmix_dml_gh1(n_nodes) else NULL
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    N <- nrow(mu); p <- ncol(mu)
    if (length(attr_names) != p) .scmix_dml_stop("typed target: attr_names mismatch.")
    zero_dm <- function(J) array(0, c(N, J, p))
    zero_ds <- function(J) array(0, c(N, J, p, p))
    finish <- function(value, dm, dk, ds = NULL, labels,
                       sigma_invariant = FALSE) {
      ans <- list(target_type = "rowwise_expectation", value = value,
                  d_mu = dm, d_kappa = dk, labels = labels)
      if (!is.null(ds)) ans$d_Sigma <- ds
      if (isTRUE(sigma_invariant)) ans$sigma_invariant <- TRUE
      ans
    }
    if (identical(type, "covariance_primitives")) {
      ij <- which(lower.tri(matrix(0, p, p), diag = TRUE), arr.ind = TRUE)
      s <- nrow(ij)
      value <- matrix(0, N, p + 2L * s)
      dm <- zero_dm(p + 2L * s)
      ds <- zero_ds(p + 2L * s)
      labels <- c(
        paste0("mean[", attr_names, "]"),
        paste0("second[", attr_names[ij[, 1L]], ",", attr_names[ij[, 2L]], "]"),
        paste0("residual[", attr_names[ij[, 1L]], ",", attr_names[ij[, 2L]], "]")
      )
      colnames(value) <- labels
      value[, seq_len(p)] <- mu
      for (j in seq_len(p)) dm[, j, j] <- 1
      for (h in seq_len(s)) {
        a <- ij[h, 1L]; b <- ij[h, 2L]
        second_col <- p + h
        residual_col <- p + s + h
        value[, second_col] <- mu[, a] * mu[, b]
        dm[, second_col, a] <- dm[, second_col, a] + mu[, b]
        dm[, second_col, b] <- dm[, second_col, b] + mu[, a]
        value[, residual_col] <- Sigma[a, b]
        if (a == b) {
          ds[, residual_col, a, b] <- 1
        } else {
          ds[, residual_col, a, b] <- 0.5
          ds[, residual_col, b, a] <- 0.5
        }
      }
      return(finish(value, dm, matrix(0, N, p + 2L * s), ds, labels))
    }
    if (identical(type, "mrs_primitives")) {
      ca <- .scmix_dml_contrast(numerator, attr_names, "numerator")
      cb <- .scmix_dml_contrast(denominator, attr_names, "denominator")
      value <- cbind(numerator = as.numeric(mu %*% ca),
                     denominator = as.numeric(mu %*% cb))
      dm <- zero_dm(2L)
      for (i in seq_len(N)) { dm[i, 1L, ] <- ca; dm[i, 2L, ] <- cb }
      return(finish(value, dm, matrix(0, N, 2L), labels = colnames(value),
                    sigma_invariant = TRUE))
    }
    d <- if (identical(type, "compensating")) {
      cp <- .scmix_dml_contrast(penalty, attr_names, "penalty")
      cb <- .scmix_dml_contrast(benefit, attr_names, "benefit")
      if (!is.numeric(amount) || length(amount) != 1L || !is.finite(amount)) {
        .scmix_dml_stop("scmix_inference_target(): amount must be finite.")
      }
      cp + amount * cb
    } else .scmix_dml_contrast(contrast, attr_names, "contrast")
    m <- as.numeric(mu %*% d)
    if (identical(type, "tau")) {
      dm <- zero_dm(1L)
      for (i in seq_len(N)) dm[i, 1L, ] <- d
      return(finish(matrix(m, ncol = 1L), dm, matrix(0, N, 1L),
                    labels = if (is.null(label)) "tau" else label,
                    sigma_invariant = TRUE))
    }
    if (identical(type, "subgroup_tau_primitives")) {
      g <- subgroup
      if (!is.null(names(g))) {
        if (anyDuplicated(names(g)) || !setequal(names(g), respondent_id)) {
          .scmix_dml_stop("subgroup names must uniquely match respondent_id.")
        }
        g <- g[respondent_id]
      }
      if (length(g) != N || anyNA(g) || any(!g %in% c(0, 1, FALSE, TRUE))) {
        .scmix_dml_stop("subgroup must be a respondent-level binary indicator.")
      }
      g <- as.numeric(g)
      value <- cbind(weighted_tau = g * m, subgroup_probability = g)
      dm <- zero_dm(2L)
      for (i in seq_len(N)) dm[i, 1L, ] <- g[i] * d
      return(finish(value, dm, matrix(0, N, 2L), labels = colnames(value),
                    sigma_invariant = TRUE))
    }
    v <- as.numeric(crossprod(d, Sigma %*% d))
    if (!is.finite(v) || v < -sqrt(.Machine$double.eps)) {
      .scmix_dml_stop("typed target received an invalid directional variance.")
    }
    v <- max(v, 0)
    cc <- tcrossprod(d)
    if (identical(type, "heterogeneity_primitives")) {
      value <- cbind(mean = m, second_moment = m^2,
                     residual_variance = rep(v, N))
      dm <- zero_dm(3L); ds <- zero_ds(3L)
      for (i in seq_len(N)) {
        dm[i, 1L, ] <- d; dm[i, 2L, ] <- 2 * m[i] * d
        ds[i, 3L, , ] <- cc
      }
      return(finish(value, dm, matrix(0, N, 3L), ds, colnames(value)))
    }
    if ((identical(type, "sign") || identical(type, "compensating")) &&
        v < variance_floor) {
      .scmix_dml_stop("typed threshold-share target directional variance is ",
                      "below its prespecified positive floor.")
    }
    sdv <- sqrt(v)
    ds <- zero_ds(1L); dm <- zero_dm(1L); dk <- matrix(0, N, 1L)
    if (identical(type, "choice")) {
      one_position <- function(sign_kappa) {
        if (v == 0) {
          index <- sign_kappa * kappa + m
          prob <- stats::plogis(index)
          slope <- prob * (1 - prob)
          return(list(value = prob, dm = slope, dk = sign_kappa * slope,
                      dv = 0.5 * slope * (1 - 2 * prob)))
        }
        index <- outer(sign_kappa * kappa + m, rep(1, length(gh$x))) +
          outer(rep(1, N), sdv * gh$x)
        prob <- stats::plogis(index); slope <- prob * (1 - prob)
        list(value = as.numeric(prob %*% gh$w),
             dm = as.numeric(slope %*% gh$w),
             dk = sign_kappa * as.numeric(slope %*% gh$w),
             dv = as.numeric((slope *
               matrix(gh$x / (2 * sdv), N, length(gh$x), byrow = TRUE)) %*%
               gh$w))
      }
      a <- one_position(1)
      if (isTRUE(position_neutral)) {
        b <- one_position(-1)
        anames <- names(a)
        a <- lapply(anames, function(nm) (a[[nm]] + b[[nm]]) / 2)
        names(a) <- anames
      }
      value <- a$value; dmean <- a$dm; dk[, 1L] <- a$dk; dv <- a$dv
      target_label <- if (is.null(label)) "choice" else label
    } else {
      z <- m / sdv; density <- stats::dnorm(z)
      value <- stats::pnorm(z); dmean <- density / sdv
      dv <- -density * m / (2 * v^(3 / 2))
      target_label <- if (is.null(label)) {
        if (identical(type, "compensating")) "compensating" else "sign"
      } else label
    }
    for (i in seq_len(N)) {
      dm[i, 1L, ] <- dmean[i] * d
      ds[i, 1L, , ] <- dv[i] * cc
    }
    finish(matrix(value, ncol = 1L), dm, dk, ds, target_label)
  }
  attr(callback, "scmix_target_spec") <- list(
    type = type, contrast = contrast, numerator = numerator,
    denominator = denominator, subgroup = subgroup, penalty = penalty,
    benefit = benefit, amount = amount, position_neutral = position_neutral,
    n_nodes = n_nodes, variance_floor = variance_floor, label = label)
  class(callback) <- c("scmix_rowwise_target", "function")
  callback
}

#' Gated delta transformations of paper-QOI primitive estimates
#'
#' @param inference A `scmix_dml` object containing the requested primitives.
#' @param type `"subgroup_ratio"`, `"mrs"`,
#'   `"directional_heterogeneity"`, or `"covariance_decomposition"`.
#' @param primitives Exact names of the required primitive targets in
#'   `inference` in the order documented by `scmix_inference_target()`.
#' @param denominator_margin Positive prespecified denominator margin for
#'   subgroup ratios or MRS.
#' @param total_margin Positive prespecified total-heterogeneity margin for the
#'   heterogeneity share.
#' @param level Confidence level.
#' @return A gated delta-method object. Formal intervals are returned only for
#'   transformations whose primitive inference and reporting gates are valid.
#' @rdname scmix_dml
#' @export
scmix_delta_transform <- function(
    inference, type = c("subgroup_ratio", "mrs",
                        "directional_heterogeneity",
                        "covariance_decomposition"), primitives,
    denominator_margin = NULL, total_margin = NULL, level = 0.95) {
  type <- match.arg(type)
  if (!inherits(inference, "scmix_dml") || is.null(inference$estimate) ||
      is.null(inference$diagnostic_covariance)) {
    .scmix_dml_stop("scmix_delta_transform(): inference must be a computed scmix_dml object.")
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    .scmix_dml_stop("scmix_delta_transform(): level must lie strictly between zero and one.")
  }
  need <- if (identical(type, "directional_heterogeneity")) 3L else if (
    identical(type, "covariance_decomposition")) NA_integer_ else 2L
  if (!is.character(primitives) || !length(primitives) ||
      (!is.na(need) && length(primitives) != need) ||
      anyDuplicated(primitives) || !all(primitives %in% names(inference$estimate))) {
    .scmix_dml_stop("scmix_delta_transform(): primitives must name the exact ",
                    "available primitive vector required by the transformation.")
  }
  est <- as.numeric(inference$estimate[primitives]); names(est) <- primitives
  V0 <- inference$diagnostic_covariance[primitives, primitives, drop = FALSE]
  if (any(!is.finite(est)) || any(!is.finite(V0))) {
    .scmix_dml_stop("scmix_delta_transform(): primitive estimates/covariance are non-finite.")
  }
  gate <- list(pass = TRUE, reason = "not required")
  if (identical(type, "subgroup_ratio") || identical(type, "mrs")) {
    margin <- denominator_margin
    if (!is.null(margin) && (!is.numeric(margin) || length(margin) != 1L ||
                            !is.finite(margin) || margin <= 0)) {
      .scmix_dml_stop("scmix_delta_transform(): denominator_margin must be positive.")
    }
    den <- est[2L]
    gate$pass <- !is.null(margin) &&
      if (identical(type, "subgroup_ratio")) den >= margin else abs(den) >= margin
    gate$reason <- if (gate$pass) "passed" else "denominator margin failed or missing"
    if (den == 0) {
      value <- NA_real_
      J <- matrix(NA_real_, 1L, 2L)
    } else {
      value <- if (identical(type, "mrs")) -est[1L] / den else est[1L] / den
      J <- if (identical(type, "mrs")) {
        matrix(c(-1 / den, est[1L] / den^2), 1L)
      } else matrix(c(1 / den, -est[1L] / den^2), 1L)
    }
    names(value) <- type
    rownames(J) <- type
  } else if (identical(type, "directional_heterogeneity")) {
    margin <- total_margin
    if (!is.null(margin) && (!is.numeric(margin) || length(margin) != 1L ||
                            !is.finite(margin) || margin <= 0)) {
      .scmix_dml_stop("scmix_delta_transform(): total_margin must be positive.")
    }
    m <- unname(est[1L]); second <- unname(est[2L]); HR <- unname(est[3L])
    HZ <- second - m^2; HT <- HZ + HR
    value <- c(H_Z = HZ, H_R = HR, H_T = HT,
               share_Z = if (HT != 0) HZ / HT else NA_real_)
    dHZ <- c(-2 * m, 1, 0); dHT <- dHZ + c(0, 0, 1)
    dshare <- if (HT != 0) (dHZ * HT - HZ * dHT) / HT^2 else rep(NA_real_, 3L)
    J <- rbind(H_Z = dHZ, H_R = c(0, 0, 1), H_T = dHT,
               share_Z = dshare)
    gate$pass <- !is.null(margin) && is.finite(HT) && HT >= margin
    gate$reason <- if (gate$pass) "passed" else
      "total-heterogeneity margin failed or missing"
  } else {
    p_float <- sqrt(length(est) + 1) - 1
    p <- as.integer(round(p_float))
    if (p < 1L || abs(p_float - p) > 1e-8) {
      .scmix_dml_stop("scmix_delta_transform(): covariance primitives must ",
                      "have p + p(p+1) entries for an integer p.")
    }
    s <- p * (p + 1L) / 2L
    mean_index <- seq_len(p)
    second_index <- p + seq_len(s)
    residual_index <- p + s + seq_len(s)
    means <- est[mean_index]
    second <- est[second_index]
    residual <- est[residual_index]
    ij <- which(lower.tri(matrix(0, p, p), diag = TRUE), arr.ind = TRUE)
    omega_z <- omega_r <- omega_t <- numeric(s)
    J <- matrix(0, 3L * s, length(est))
    for (h in seq_len(s)) {
      a <- ij[h, 1L]; b <- ij[h, 2L]
      omega_z[h] <- second[h] - means[a] * means[b]
      omega_r[h] <- residual[h]
      omega_t[h] <- omega_z[h] + omega_r[h]
      dz <- numeric(length(est))
      dz[a] <- dz[a] - means[b]
      dz[b] <- dz[b] - means[a]
      dz[second_index[h]] <- 1
      J[h, ] <- dz
      J[s + h, residual_index[h]] <- 1
      J[2L * s + h, ] <- dz
      J[2L * s + h, residual_index[h]] <- 1
    }
    entry_names <- paste0("[", ij[, 1L], ",", ij[, 2L], "]")
    value <- c(stats::setNames(omega_z, paste0("Omega_Z", entry_names)),
               stats::setNames(omega_r, paste0("Omega_R", entry_names)),
               stats::setNames(omega_t, paste0("Omega_T", entry_names)))
    rownames(J) <- names(value)
  }
  V <- if (all(is.finite(J))) J %*% V0 %*% t(J) else
    matrix(NA_real_, nrow(J), nrow(J), dimnames = list(rownames(J), rownames(J)))
  se <- sqrt(pmax(diag(V), 0)); names(se) <- names(value)
  ## A zero-variance primitive can be a known component of a regular
  ## transformation (for example Omega_R = 0 in a prespecified q = 0 model).
  ## Require the fit-level verification record and finite joint diagnostic
  ## covariance, then decide availability from each transformed target's own
  ## standard error and reporting gate rather than requiring every primitive
  ## to have a positive standard error separately.
  primitive_ok <- inference$status %in%
    c("available", "conditional_available") &&
    isTRUE(inference$inference_available)
  target_ok <- rep(primitive_ok, length(value))
  if (identical(type, "directional_heterogeneity")) {
    target_ok[4L] <- target_ok[4L] && gate$pass
  } else if (!identical(type, "covariance_decomposition")) {
    target_ok[] <- target_ok & gate$pass
  }
  target_ok <- target_ok & is.finite(se) & se > 0
  names(target_ok) <- names(value)
  status <- if (primitive_ok && any(target_ok)) {
    if (identical(inference$status, "available")) "available" else
      "conditional_available"
  } else "conditional_unverified"
  formal_available <- status %in% c("available", "conditional_available")
  zcrit <- stats::qnorm(1 - (1 - level) / 2)
  lo <- hi <- stats::setNames(rep(NA_real_, length(value)), names(value))
  if (formal_available) {
    lo[target_ok] <- value[target_ok] - zcrit * se[target_ok]
    hi[target_ok] <- value[target_ok] + zcrit * se[target_ok]
  }
  formal_covariance <- matrix(NA_real_, nrow(V), ncol(V),
                              dimnames = dimnames(V))
  if (formal_available) {
    formal_covariance[target_ok, target_ok] <- V[target_ok, target_ok,
                                                  drop = FALSE]
  }
  formal_se <- stats::setNames(rep(NA_real_, length(value)), names(value))
  if (formal_available) formal_se[target_ok] <- se[target_ok]
  out <- list(type = type, estimate = value,
              diagnostic_covariance = V, diagnostic_se = se,
              covariance = formal_covariance, se = formal_se,
              ci_lower = lo, ci_upper = hi, jacobian = J,
              primitives = primitives, gate = gate,
              target_inference_available = target_ok &
                formal_available,
              inference_available = formal_available &&
                any(target_ok), status = status,
              analysis_signature = inference$analysis_signature,
              inference_claim = if (identical(status, "conditional_available")) {
                "conditional_on_documented_high_level_assumptions"
              } else if (identical(status, "available")) {
                inference$inference_claim
              } else "not_available",
              reason = if (formal_available) NULL else
                "primitive inference or a quantity-specific gate is unverified")
  class(out) <- c("scmix_delta_transform", "list")
  out
}

.scmix_dml_penalty <- function(spec, d) {
  if (is.character(spec) && length(spec) == 1L && spec == "identity") {
    return(diag(d))
  }
  if (!is.numeric(spec)) {
    .scmix_dml_stop("scmix_dml(): riesz_penalty must be 'identity' or numeric.")
  }
  if (is.numeric(spec) && is.null(dim(spec)) && length(spec) == d) {
    P <- diag(as.numeric(spec), d)
  } else {
    P <- as.matrix(spec)
  }
  if (!.scmix_dml_dim_equal(P, c(d, d)) || any(!is.finite(P))) {
    .scmix_dml_stop("scmix_dml(): riesz_penalty must be 'identity', a length-d ",
                    "diagonal, or a finite d-by-d matrix.")
  }
  if (max(abs(P - t(P))) > 1e-10 || min(eigen(P, symmetric = TRUE,
                                             only.values = TRUE)$values) < -1e-10) {
    .scmix_dml_stop("scmix_dml(): riesz_penalty must be symmetric positive semidefinite.")
  }
  P
}

.scmix_dml_structural_gram <- function(basis, train, p, dA,
                                       tolerance = 1e-12) {
  GB <- crossprod(basis[train, , drop = FALSE]) / length(train)
  G <- matrix(0, 1L + p * ncol(basis) + dA,
              1L + p * ncol(basis) + dA)
  G[1L, 1L] <- 1
  mu_idx <- 1L + seq_len(p * ncol(basis))
  G[mu_idx, mu_idx] <- kronecker(diag(p), GB)
  if (dA) {
    a_idx <- (max(mu_idx) + 1L):nrow(G)
    G[a_idx, a_idx] <- diag(dA)
  }
  ev <- eigen((G + t(G)) / 2, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) <= tolerance * max(max(ev), 1)) {
    .scmix_dml_stop("scmix_dml(): structural tangent Gram matrix is not ",
                    "positive definite on the training respondents.")
  }
  G
}

.scmix_dml_inverse_sqrt <- function(G) {
  eg <- eigen((G + t(G)) / 2, symmetric = TRUE)
  eg$vectors %*% ((1 / sqrt(eg$values)) * t(eg$vectors))
}

.scmix_dml_nu_grid <- function(spec, fold, n_train, K) {
  x <- if (is.null(spec)) {
    c(0, 1 / n_train, 1 / sqrt(n_train))
  } else if (is.list(spec)) {
    if (length(spec) != K) {
      .scmix_dml_stop("scmix_dml(): a list-valued nu_grid must contain one ",
                      "grid per outer fold.")
    }
    spec[[fold]]
  } else spec
  x <- sort(unique(as.numeric(x)))
  if (!length(x) || any(!is.finite(x)) || any(x < 0)) {
    .scmix_dml_stop("scmix_dml(): every nu_grid must be a finite nonnegative vector.")
  }
  x
}

.scmix_dml_solve <- function(I, g, nu, P, tolerance = 1e-10) {
  M <- (I + t(I)) / 2 + nu * P
  ee <- eigen(M, symmetric = TRUE)
  cutoff <- tolerance * max(abs(ee$values), 1)
  inv <- ifelse(ee$values > cutoff, 1 / ee$values, 0)
  ee$vectors %*% (inv * crossprod(ee$vectors, g))
}

.scmix_dml_riesz <- function(score, gradient, train, nu_grid, penalty,
                             structural_gram, validation_fraction, seed,
                             solve_tolerance) {
  W <- .scmix_dml_inverse_sqrt(structural_gram)
  score_w <- score %*% W
  gradient_w <- lapply(gradient, function(g) g %*% W)
  grad_matrix <- function(rows) {
    do.call(cbind, lapply(gradient_w, function(g)
      colMeans(g[rows, , drop = FALSE])))
  }
  fit_alpha <- function(rows, nu) {
    I <- crossprod(score_w[rows, , drop = FALSE]) / length(rows)
    g <- grad_matrix(rows)
    .scmix_dml_solve(I, g, nu, penalty, solve_tolerance)
  }
  selected <- nu_grid[1L]
  risk <- rep(NA_real_, length(nu_grid))
  if (length(nu_grid) > 1L && validation_fraction > 0 && length(train) >= 20L) {
    selected_validation <- .scmix_dml_with_seed(seed, {
      n_val <- max(5L, min(length(train) - 5L,
                           floor(validation_fraction * length(train))))
      val <- sample(train, n_val, replace = FALSE)
      subtrain <- setdiff(train, val)
      Ival <- crossprod(score_w[val, , drop = FALSE]) / length(val)
      gval <- grad_matrix(val)
      for (r in seq_along(nu_grid)) {
        aa <- fit_alpha(subtrain, nu_grid[r])
        risk[r] <- sum(diag(crossprod(aa, Ival %*% aa))) -
          2 * sum(aa * gval)
      }
      list(selected = nu_grid[which.min(risk)], risk = risk)
    })
    selected <- selected_validation$selected
    risk <- selected_validation$risk
  }
  Iall <- crossprod(score_w[train, , drop = FALSE]) / length(train)
  alpha <- fit_alpha(train, selected)
  alpha_zero <- fit_alpha(train, 0)
  gall <- grad_matrix(train)
  eq <- Iall %*% alpha - gall
  eq_rel <- sqrt(colSums(eq^2)) / pmax(sqrt(colSums(gall^2)), 1e-12)
  corr <- score_w[train, , drop = FALSE] %*% alpha
  corr0 <- score_w[train, , drop = FALSE] %*% alpha_zero
  ridge_rel <- sqrt(colMeans((corr - corr0)^2)) /
    pmax(sqrt(colMeans(corr0^2)), 1e-12)
  eig <- eigen((Iall + t(Iall)) / 2, symmetric = TRUE, only.values = TRUE)$values
  list(alpha = alpha, alpha_raw = W %*% alpha,
       score_whitened = score_w, selected_nu = selected,
       validation = data.frame(nu = nu_grid, risk = risk), information = Iall,
       information_eigenvalues = eig, structural_gram = structural_gram,
       structural_inverse_sqrt = W,
       riesz_equation_relative_residual = eq_rel,
       ridge_relative_sensitivity = ridge_rel,
       grid_contains_zero = any(nu_grid == 0),
       grid_max_positive = if (any(nu_grid > 0)) max(nu_grid) else 0)
}

.scmix_dml_multiplier <- function(IF, estimate, se, R, level, seed,
                                  multiplier) {
  if (R <= 0L) return(NULL)
  N <- nrow(IF); J <- ncol(IF)
  valid <- is.finite(se) & se > 0
  valid_names <- colnames(IF)[valid]
  invalid_names <- colnames(IF)[!valid]
  if (!any(valid)) {
    return(list(status = "withheld", reason = "all targets have zero or non-finite standard errors",
                included_targets = character(), withheld_targets = invalid_names,
                n_draws = 0L))
  }
  draws <- .scmix_dml_with_seed(seed, {
    ans <- matrix(0, R, J)
    for (r in seq_len(R)) {
      xi <- if (multiplier == "normal") stats::rnorm(N) else
        sample(c(-1, 1), N, replace = TRUE)
      ans[r, ] <- as.numeric(crossprod(xi, IF)) / N
    }
    ans
  })
  colnames(draws) <- colnames(IF)
  studentized <- sweep(draws[, valid, drop = FALSE], 2L, se[valid], `/`)
  crit <- stats::quantile(apply(abs(studentized), 1L, max), probs = level,
                          names = FALSE, type = 8)
  alpha <- 1 - level
  ## Invert the bootstrap error distribution; do not rely on its finite-draw
  ## empirical quantiles being exactly symmetric.
  point_lo <- point_hi <- sim_lo <- sim_hi <- rep(NA_real_, J)
  names(point_lo) <- names(point_hi) <- names(sim_lo) <- names(sim_hi) <-
    colnames(IF)
  point_lo[valid] <- estimate[valid] - apply(
    draws[, valid, drop = FALSE], 2L, stats::quantile,
    probs = 1 - alpha / 2, names = FALSE, type = 8)
  point_hi[valid] <- estimate[valid] - apply(
    draws[, valid, drop = FALSE], 2L, stats::quantile,
    probs = alpha / 2, names = FALSE, type = 8)
  sim_lo[valid] <- estimate[valid] - crit * se[valid]
  sim_hi[valid] <- estimate[valid] + crit * se[valid]
  list(status = if (all(valid)) "available" else "partially_withheld",
       centered_draws = draws,
       estimate_draws = sweep(draws, 2L, estimate, `+`),
       pointwise_lower = point_lo, pointwise_upper = point_hi,
       simultaneous_critical_value = crit,
       simultaneous_lower = sim_lo, simultaneous_upper = sim_hi,
       included_targets = valid_names, withheld_targets = invalid_names,
       multiplier = multiplier, level = level, n_draws = R)
}

.scmix_dml_withheld <- function(reason, rank_gate = NULL, call = NULL,
                                analysis_signature = NA_character_) {
  out <- list(status = "withheld", inference_available = FALSE,
              inference_claim = "not_available",
              reason = reason, rank_gate = rank_gate, estimate = NA_real_,
              se = NA_real_, covariance = NA_real_, influence = NULL,
              analysis_signature = analysis_signature,
              rank_gate_scope = attr(rank_gate, "scope"),
              call = call,
              theory_scope = paste(
                "Ordinary inference is withheld. This finite-sieve procedure",
                "does not verify DNN rate or likelihood-approximation conditions."))
  class(out) <- c("scmix_dml", "list")
  out
}

.scmix_dml_nonempty_text <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
}

.scmix_dml_object_fingerprint <- function(x) {
  path <- tempfile("scmix-fingerprint-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 3, compress = FALSE)
  unname(as.character(tools::md5sum(path)))
}

.scmix_dml_signature <- function(x) {
  sig <- if (is.list(x)) x$analysis_signature else NULL
  if (.scmix_dml_nonempty_text(sig)) as.character(sig) else NA_character_
}

.scmix_dml_fit_eligibility <- function(fit) {
  inspect <- function(value, favorable, field) {
    if (is.null(value)) {
      return(list(pass = TRUE, state = "absent_legacy"))
    }
    valid <- is.logical(value) && length(value) == 1L && !is.na(value)
    pass <- valid && identical(value, favorable)
    list(pass = pass,
         state = if (!valid) "malformed" else if (pass) "favorable" else
           "adverse",
         field = field)
  }
  diagnostic <- inspect(fit$diagnostic_only, FALSE, "diagnostic_only")
  ordinary <- inspect(fit$eligible_for_ordinary_inference, TRUE,
                      "eligible_for_ordinary_inference")
  pass <- diagnostic$pass && ordinary$pass
  reasons <- character()
  if (!diagnostic$pass) {
    reasons <- c(reasons, paste0("fit$diagnostic_only is ", diagnostic$state))
  }
  if (!ordinary$pass) {
    reasons <- c(reasons, paste0("fit$eligible_for_ordinary_inference is ",
                                 ordinary$state))
  }
  list(pass = pass, diagnostic_only = diagnostic,
       eligible_for_ordinary_inference = ordinary,
       reason = if (pass) "passed or absent on a legacy fit" else
         paste(reasons, collapse = "; "))
}

.scmix_dml_numerical_battery <- function(checks) {
  nm <- names(checks)
  patterns <- c(
    qoi = "^(qoi)([._]|$)",
    likelihood = "^(likelihood|loglik)([._]|$)",
    score_derivative = "^(score|derivative)([._]|$)",
    riesz = "^(riesz)([._]|$)",
    influence_l2 = "^(if_l2|influence_l2)([._]|$)",
    se = "^(se)([._]|$)"
  )
  stats::setNames(vapply(patterns, function(rx) any(grepl(rx, nm,
                                                          ignore.case = TRUE)),
                         logical(1L)), names(patterns))
}

#' Construct auditable high-level evidence for ordinary inference
#'
#' This constructor prevents an arbitrary list of booleans from promoting a
#' diagnostic calculation to ordinary inference. It validates the empirical
#' numerical-refinement and optimization artifacts and requires explicit,
#' traceable arguments for the high-level rate conditions those finite-sample
#' diagnostics cannot prove. The constructor validates the record, not the
#' substantive truth of an approximation or product-rate argument.
#'
#' @param fit The exact fit to which every artifact must be linked through its
#'   `analysis_signature`. An explicitly diagnostic-only or
#'   inference-ineligible fit is rejected; legacy fits without either
#'   eligibility field are allowed.
#' @param mu_basis The nondefault growing or fold-fitted sieve supplied to
#'   `scmix_dml()`.
#' @param tangent A list with `type = "growing_sieve"` or `"fitted_sieve"`,
#'   `prespecified = TRUE`, `identified_directions = TRUE`, and nonempty
#'   `approximation_argument`, `product_rate_argument`, and `provenance` fields.
#' @param numerical A list with an `artifact` produced by
#'   `scmix_integration_refinement()` plus nonempty `rate_argument` and
#'   `provenance`. The artifact must contain at least two fresh-refit settings
#'   and a passing numerical gate.
#' @param optimization A list with an `artifact` produced by
#'   `scmix_optimization_audit()` plus nonempty `gap_argument` and `provenance`.
#'   All attained-solution gates must pass and no output bound may be active.
#' @param ridge Optional list for a caller-supplied positive ridge sequence,
#'   with `vanishing_sequence = TRUE` and nonempty `rate_argument` and
#'   `provenance`.
#' @return A classed verification record accepted by `scmix_dml()`.
#' @rdname scmix_dml
#' @export
scmix_inference_verification <- function(fit, mu_basis, tangent, numerical,
                                         optimization, ridge = NULL) {
  analysis_signature <- .scmix_dml_signature(fit)
  if (is.na(analysis_signature)) {
    .scmix_dml_stop("scmix_inference_verification(): fit must contain a ",
                    "nonempty analysis_signature.")
  }
  fit_eligibility <- .scmix_dml_fit_eligibility(fit)
  if (!isTRUE(fit_eligibility$pass)) {
    .scmix_dml_stop("scmix_inference_verification(): the fit is not eligible ",
                    "for conditional inference: ", fit_eligibility$reason, ".")
  }
  if (is.null(mu_basis) || is.function(mu_basis)) {
    .scmix_dml_stop("scmix_inference_verification(): available inference ",
                    "requires an explicit matrix or fold-list growing/fitted ",
                    "sieve; the default fixed linear tangent and opaque basis ",
                    "callbacks remain diagnostic.")
  }
  tangent_type <- if (is.list(tangent)) tangent$type else NULL
  if (!is.list(tangent) || !isTRUE(tangent$prespecified) ||
      !isTRUE(tangent$identified_directions) ||
      !tangent_type %in% c("growing_sieve", "fitted_sieve") ||
      !.scmix_dml_nonempty_text(tangent$approximation_argument) ||
      !.scmix_dml_nonempty_text(tangent$product_rate_argument) ||
      !.scmix_dml_nonempty_text(tangent$provenance)) {
    .scmix_dml_stop(
      "scmix_inference_verification(): tangent must record prespecified and ",
      "identified growing/fitted-sieve directions plus nonempty ",
      "approximation, product-rate, and provenance arguments."
    )
  }
  K <- if (is.list(fit) && length(fit$K) == 1L) as.integer(fit$K) else NA_integer_
  basis_dims <- if (is.list(mu_basis)) {
    vapply(mu_basis, function(B) ncol(as.matrix(B)), integer(1L))
  } else ncol(as.matrix(mu_basis))
  tangent_structure_pass <- if (identical(tangent_type, "fitted_sieve")) {
    is.list(mu_basis) && is.finite(K) && length(mu_basis) == K &&
      isTRUE(tangent$training_only) && isTRUE(tangent$outer_fold_specific) &&
      all(basis_dims > 0L)
  } else {
    ns <- tangent$sample_sizes; ds <- tangent$dimensions
    is.numeric(ns) && is.numeric(ds) && length(ns) >= 2L &&
      length(ns) == length(ds) && all(is.finite(ns)) && all(is.finite(ds)) &&
      all(diff(ns) > 0) && all(diff(ds) >= 0) &&
      utils::tail(ds, 1L) == max(basis_dims) && all(basis_dims > 0L)
  }
  if (!tangent_structure_pass) {
    .scmix_dml_stop("scmix_inference_verification(): tangent evidence does ",
                    "not match the supplied growing or outer-fold fitted sieve.")
  }
  num_artifact <- if (is.list(numerical)) numerical$artifact else NULL
  num_checks <- if (inherits(num_artifact, "scmix_integration_refinement")) {
    num_artifact$checks
  } else NULL
  battery <- if (is.data.frame(num_checks)) {
    .scmix_dml_numerical_battery(num_checks)
  } else stats::setNames(rep(FALSE, 6L),
                         c("qoi", "likelihood", "score_derivative", "riesz",
                           "influence_l2", "se"))
  if (!is.list(numerical) ||
      !inherits(num_artifact, "scmix_integration_refinement") ||
      !isTRUE(num_artifact$gate$pass) || !is.data.frame(num_checks) ||
      nrow(num_checks) < 2L ||
      !identical(as.integer(num_artifact$refit_count), as.integer(nrow(num_checks))) ||
      length(num_artifact$refit_analysis_signatures) != nrow(num_checks) ||
      !isTRUE(all(!is.na(num_artifact$refit_analysis_signatures) &
                  num_artifact$refit_analysis_signatures == analysis_signature)) ||
      !all(battery) ||
      !identical(.scmix_dml_signature(num_artifact), analysis_signature) ||
      !isTRUE(num_artifact$signature_match) ||
      !.scmix_dml_nonempty_text(numerical$rate_argument) ||
      !.scmix_dml_nonempty_text(numerical$provenance)) {
    .scmix_dml_stop(
      "scmix_inference_verification(): numerical must contain a passing ",
      "fit-linked scmix_integration_refinement artifact with at least two ",
      "fresh-refit settings; QOI, likelihood, score/derivative, Riesz, IF-L2, ",
      "and SE checks; and nonempty rate/provenance arguments."
    )
  }
  opt_artifact <- if (is.list(optimization)) optimization$artifact else NULL
  opt_pass <- inherits(opt_artifact, "scmix_optimization_audit") &&
    identical(.scmix_dml_signature(opt_artifact), analysis_signature) &&
    isTRUE(opt_artifact$signature_match) &&
    isTRUE(opt_artifact$all_selected_tolerances_met) &&
    isTRUE(opt_artifact$all_objectives_finite) &&
    isTRUE(opt_artifact$all_computational_gates_pass) &&
    !isTRUE(opt_artifact$any_bound_activity)
  if (!is.list(optimization) || !opt_pass ||
      !.scmix_dml_nonempty_text(optimization$gap_argument) ||
      !.scmix_dml_nonempty_text(optimization$provenance)) {
    .scmix_dml_stop(
      "scmix_inference_verification(): optimization must contain a passing ",
      "scmix_optimization_audit artifact, no active output bound, and ",
      "nonempty approximate-gap/provenance arguments."
    )
  }
  ridge_record <- NULL
  if (!is.null(ridge)) {
    if (!is.list(ridge) || !isTRUE(ridge$vanishing_sequence) ||
        !.scmix_dml_nonempty_text(ridge$rate_argument) ||
        !.scmix_dml_nonempty_text(ridge$provenance)) {
      .scmix_dml_stop(
        "scmix_inference_verification(): ridge must affirm a vanishing ",
        "sequence and provide nonempty rate/provenance arguments."
      )
    }
    ridge_record <- list(pass = TRUE, provenance = ridge$provenance,
                         record = ridge)
  }
  out <- list(
    analysis_signature = analysis_signature,
    basis_fingerprint = .scmix_dml_object_fingerprint(mu_basis),
    fit_eligibility = list(pass = TRUE,
                           provenance = "fit eligibility fields audited",
                           record = fit_eligibility),
    tangent = list(pass = TRUE, provenance = tangent$provenance,
                   record = tangent, structure_verified = TRUE,
                   automated_rate_verified = FALSE,
                   conditional_argument_accepted = TRUE,
                   reason = paste(
                     "Growing/fitted-sieve structure is fit-linked and the",
                     "high-level approximation/product-rate arguments are",
                     "documented; those rates are assumptions, not empirically",
                     "verified facts.")),
    numerical = list(pass = TRUE, provenance = numerical$provenance,
                     artifact = num_artifact,
                     rate_argument = numerical$rate_argument,
                     battery = battery),
    optimization = list(pass = TRUE, provenance = optimization$provenance,
                        artifact = opt_artifact,
                        gap_argument = optimization$gap_argument),
    ridge = ridge_record,
    scope = paste(
      "Fit-linked audit record. Numerical and optimization components are",
      "machine checked. Inference supported by this record remains explicitly",
      "conditional on its documented tangent approximation and product-rate",
      "arguments and is never labeled empirically verified."
    )
  )
  class(out) <- c("scmix_inference_verification", "list")
  out
}

.scmix_dml_verification <- function(fit, verification, mu_basis) {
  x <- if (!is.null(verification)) verification else fit$inference_verification
  fit_signature <- .scmix_dml_signature(fit)
  basis_fingerprint <- if (is.null(mu_basis) || is.function(mu_basis)) {
    NA_character_
  } else .scmix_dml_object_fingerprint(mu_basis)
  num <- if (is.list(x)) x$numerical$artifact else NULL
  opt <- if (is.list(x)) x$optimization$artifact else NULL
  battery <- if (inherits(num, "scmix_integration_refinement") &&
                 is.data.frame(num$checks)) {
    .scmix_dml_numerical_battery(num$checks)
  } else rep(FALSE, 6L)
  signature_linked <- .scmix_dml_nonempty_text(fit_signature) &&
    identical(x$analysis_signature, fit_signature) &&
    identical(.scmix_dml_signature(num), fit_signature) &&
    identical(.scmix_dml_signature(opt), fit_signature)
  basis_linked <- .scmix_dml_nonempty_text(basis_fingerprint) &&
    identical(x$basis_fingerprint, basis_fingerprint)
  numerical_live <- inherits(num, "scmix_integration_refinement") &&
    isTRUE(num$gate$pass) && is.data.frame(num$checks) &&
    nrow(num$checks) >= 2L &&
    identical(as.integer(num$refit_count), as.integer(nrow(num$checks))) &&
    length(num$refit_analysis_signatures) == nrow(num$checks) &&
    isTRUE(all(!is.na(num$refit_analysis_signatures) &
               num$refit_analysis_signatures == fit_signature)) &&
    all(battery) &&
    isTRUE(num$signature_match)
  optimization_live <- inherits(opt, "scmix_optimization_audit") &&
    isTRUE(opt$signature_match) &&
    isTRUE(opt$all_selected_tolerances_met) &&
    isTRUE(opt$all_objectives_finite) &&
    isTRUE(opt$all_computational_gates_pass) &&
    !isTRUE(opt$any_bound_activity)
  fit_eligibility <- .scmix_dml_fit_eligibility(fit)
  record_valid <- inherits(x, "scmix_inference_verification") &&
    signature_linked && basis_linked
  required <- c("fit_eligibility", "tangent", "numerical", "optimization")
  one <- function(nm) {
    z <- if (is.list(x)) x[[nm]] else NULL
    provenance <- if (is.list(z) && length(z$provenance) == 1L) {
      as.character(z$provenance)
    } else ""
    live <- switch(nm,
                   fit_eligibility = isTRUE(fit_eligibility$pass),
                   tangent = isTRUE(x$tangent$structure_verified) &&
                     isTRUE(x$tangent$conditional_argument_accepted) &&
                     .scmix_dml_nonempty_text(x$tangent$record$approximation_argument) &&
                     .scmix_dml_nonempty_text(x$tangent$record$product_rate_argument),
                   numerical = numerical_live,
                   optimization = optimization_live)
    pass <- record_valid && live && is.list(z) && identical(z$pass, TRUE) &&
      !is.na(provenance) && nzchar(trimws(provenance))
    evidence_type <- if (identical(nm, "tangent")) {
      "documented_high_level_assumption"
    } else if (identical(nm, "fit_eligibility")) {
      "live_fit_eligibility_gate"
    } else "fit_linked_empirical_artifact"
    data.frame(component = nm, pass = pass, evidence_type = evidence_type,
               provenance = provenance, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, lapply(required, one))
  attr(out, "ridge") <- if (is.list(x)) x$ridge else NULL
  attr(out, "fit_eligibility") <- fit_eligibility
  out
}

#' Respondent-level finite-sieve orthogonal inference for `scmix`
#'
#' Implements a high-level, outer-cross-fitted one-step procedure for a
#' prespecified finite vector of regular targets.  Built-in `"theta"` targets
#' `E[mu(Z)]`. Additional smooth rowwise plug-in targets may be supplied as
#' callbacks. A callback receives `mu`, `kappa`, `Sigma`, `Z`,
#' `respondent_id`, `fold`, and `attr_names`. It must return a list declaring
#' `target_type = "rowwise_expectation"`, an N-row `value`, and analytic
#' derivatives `d_mu`, `d_kappa`, and (when relevant) `d_Sigma`. Raw loading
#' factors are not exposed. Numerical derivatives are available only by an
#' explicit opt-in and are checked by step-size refinement.
#'
#' The default finite mean tangent is an intercept plus training-standardized
#' linear moderator terms, with training-rank-redundant columns removed. It is
#' a local structural sieve, not the tangent of the raw DNN weights. Loading
#' scores are projected onto the rotation-horizontal tangent. Respondents are
#' the sampling, fold, Riesz-validation, and multiplier units throughout.
#'
#' This function does not establish the high-level DNN approximation and
#' product-rate conditions assumed by the paper. For q > 0, ordinary inference
#' is withheld unless every fold loading has exact rank q and its smallest
#' active eigenvalue exceeds `active_eigenvalue_min`.
#' A complete classed verification record supports the explicitly conditional
#' status `"conditional_available"`; it never represents empirical
#' verification of the documented high-level rate assumptions. Raw lists and
#' the default linear tangent remain `"conditional_unverified"`.
#'
#' @param fit An `scmix`-like fit containing complete task sequences and outer
#'   respondent folds. Explicit diagnostic-only or inference-ineligible flags
#'   prevent conditional inference; absent legacy flags are permitted.
#' @param targets Built-in targets; currently `"theta"`.
#' @param plugin_targets Optional named list of smooth callback functions.
#' @param mu_by_fold Optional K-list of all-respondent fold-specific mean
#'   predictions. Required unless stored on the fit or recoverable from nets.
#' @param kappa_folds Optional fold-specific kappa estimates.
#' @param mu_basis Optional fixed matrix, K-list, or basis callback. NULL uses
#'   the documented intercept-plus-linear-moderator basis.
#' @param nu_grid NULL for the fold-size-dependent grid containing zero,
#'   N_train^-1, and N_train^-1/2; a common finite nonnegative grid; or a
#'   K-list of fold-specific grids.
#' @param riesz_penalty Positive-semidefinite penalty in the declared
#'   structural-orthonormal coordinates.
#' @param riesz_validation_fraction Training-respondent validation fraction.
#' @param active_eigenvalue_min Explicit prespecified positive minimum active
#'   eigenvalue of the loading covariance in raw coded coefficient units. `NULL` permits a
#'   diagnostic calculation but cannot support conditional inference when
#'   `q > 0`.
#' @param rank_tolerance Relative numerical rank tolerance.
#' @param information_eigenvalue_min Explicit prespecified strictly positive
#'   minimum generalized complete-sequence information eigenvalue in the
#'   declared structural norm. `NULL` permits a diagnostic calculation but
#'   cannot support conditional inference.
#' @param derivative_eps Initial central-difference step for callback derivatives.
#' @param allow_numeric_derivatives Whether callbacks may omit analytic
#'   derivatives and use refined numerical derivatives.
#' @param derivative_refinement_factor Step-size refinement factor.
#' @param derivative_refinement_tolerance Maximum relative refinement discrepancy.
#' @param riesz_equation_tolerance Maximum relative unpenalized Riesz-equation
#'   residual for conditionally supported inference.
#' @param ridge_sensitivity_tolerance Maximum relative correction sensitivity
#'   to the selected ridge versus zero ridge.
#' @param verification Classed evidence record returned by
#'   [scmix_inference_verification()]. Raw boolean lists are not accepted.
#' @param multiplier_draws Number of respondent multiplier draws.
#' @param multiplier `"normal"` or `"rademacher"`.
#' @param level Confidence level.
#' @param seed Reproducibility seed.
#' @return An `scmix_dml` object containing estimates, respondent influence
#'   vectors and their direct-P_Z/correction decomposition, covariance, standard
#'   errors, fold Riesz diagnostics, and multiplier simultaneous intervals.
#' @export
scmix_dml <- function(fit, targets = "theta", plugin_targets = NULL,
                      mu_by_fold = NULL, kappa_folds = NULL,
                      mu_basis = NULL,
                      nu_grid = NULL,
                      riesz_penalty = "identity",
                      riesz_validation_fraction = 0.2,
                      active_eigenvalue_min = NULL,
                      rank_tolerance = 1e-8,
                      information_eigenvalue_min = NULL,
                      derivative_eps = 1e-5,
                      allow_numeric_derivatives = FALSE,
                      derivative_refinement_factor = 2,
                      derivative_refinement_tolerance = 1e-3,
                      riesz_equation_tolerance = 5e-2,
                      ridge_sensitivity_tolerance = 1e-1,
                      verification = NULL,
                      multiplier_draws = 999L,
                      multiplier = c("normal", "rademacher"),
                      level = 0.95, seed = 1L) {
  call <- match.call()
  multiplier <- match.arg(multiplier)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    .scmix_dml_stop("scmix_dml(): level must lie strictly between zero and one.")
  }
  if (!is.numeric(derivative_eps) || length(derivative_eps) != 1L ||
      !is.finite(derivative_eps) || derivative_eps <= 0) {
    .scmix_dml_stop("scmix_dml(): derivative_eps must be positive.")
  }
  if (!is.logical(allow_numeric_derivatives) ||
      length(allow_numeric_derivatives) != 1L ||
      is.na(allow_numeric_derivatives)) {
    .scmix_dml_stop("scmix_dml(): allow_numeric_derivatives must be TRUE or FALSE.")
  }
  positive_scalar <- function(x, what, lower = 0) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= lower) {
      .scmix_dml_stop("scmix_dml(): ", what, " must be a finite scalar greater than ",
                      lower, ".")
    }
  }
  positive_scalar(derivative_refinement_factor,
                  "derivative_refinement_factor", 1)
  positive_scalar(derivative_refinement_tolerance,
                  "derivative_refinement_tolerance")
  positive_scalar(riesz_equation_tolerance, "riesz_equation_tolerance")
  positive_scalar(ridge_sensitivity_tolerance, "ridge_sensitivity_tolerance")
  if (!is.null(active_eigenvalue_min) &&
      (!is.numeric(active_eigenvalue_min) ||
       length(active_eigenvalue_min) != 1L ||
       !is.finite(active_eigenvalue_min) || active_eigenvalue_min <= 0)) {
    .scmix_dml_stop("scmix_dml(): active_eigenvalue_min must be a finite ",
                    "strictly positive scalar when supplied.")
  }
  if (!is.numeric(rank_tolerance) || length(rank_tolerance) != 1L ||
      !is.finite(rank_tolerance) || rank_tolerance <= 0) {
    .scmix_dml_stop("scmix_dml(): rank_tolerance must be a finite positive scalar.")
  }
  if (!is.null(information_eigenvalue_min) &&
      (!is.numeric(information_eigenvalue_min) ||
       length(information_eigenvalue_min) != 1L ||
       !is.finite(information_eigenvalue_min) || information_eigenvalue_min <= 0)) {
    .scmix_dml_stop("scmix_dml(): information_eigenvalue_min must be a finite ",
                    "strictly positive scalar when supplied.")
  }
  if (!is.numeric(riesz_validation_fraction) ||
      length(riesz_validation_fraction) != 1L ||
      !is.finite(riesz_validation_fraction) ||
      riesz_validation_fraction < 0 || riesz_validation_fraction >= 1) {
    .scmix_dml_stop("scmix_dml(): riesz_validation_fraction must lie in [0,1).")
  }
  if (!is.numeric(multiplier_draws) || length(multiplier_draws) != 1L ||
      is.na(multiplier_draws) || multiplier_draws < 0 ||
      multiplier_draws > .Machine$integer.max ||
      multiplier_draws != as.integer(multiplier_draws)) {
    .scmix_dml_stop("scmix_dml(): multiplier_draws must be a nonnegative integer.")
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != as.integer(seed)) {
    .scmix_dml_stop("scmix_dml(): seed must be a nonnegative integer.")
  }
  seed <- as.integer(seed)
  layout <- .scmix_dml_layout(fit)
  attr_names <- fit$attr_names
  if (is.null(attr_names)) attr_names <- colnames(layout$deltaX)
  if (is.null(attr_names)) attr_names <- paste0("beta", seq_len(layout$p))
  if (length(attr_names) != layout$p) {
    .scmix_dml_stop("scmix_dml(): fit$attr_names has the wrong length.")
  }
  colnames(layout$deltaX) <- attr_names
  kap <- .scmix_dml_resolve_kappa(fit, layout$K, kappa_folds)
  ag <- .scmix_dml_resolve_A_gh(fit, layout$p, layout$K)
  mu_fold <- .scmix_dml_resolve_mu(fit, layout, mu_by_fold)

  rank_gate <- .scmix_dml_rank_gate(ag$A_folds, ag$q,
                                    active_eigenvalue_min, rank_tolerance,
                                    A_full = ag$A_full,
                                    A_full_source = ag$A_full_source)
  rank_failed <- any(!rank_gate$rank_pass)
  active_margin_failed <- ag$q > 0L && !is.null(active_eigenvalue_min) &&
    any(!rank_gate$margin_pass)
  if (rank_failed || active_margin_failed) {
    reason <- paste0(
      "rank boundary: q > 0 requires rank(A_k)=q",
      if (!is.null(active_eigenvalue_min)) paste0(
        " and minimum active eigenvalue >= ", format(active_eigenvalue_min))
      else "",
      " in every stored computational loading"
    )
    warning("scmix_dml(): ", reason, ". Ordinary Hessian/delta/bootstrap ",
            "inference is withheld.", call. = FALSE)
    return(.scmix_dml_withheld(reason, rank_gate, call,
                               .scmix_dml_signature(fit)))
  }

  N <- layout$N; p <- layout$p; K <- layout$K
  plugin <- correction <- direct_pz <- NULL
  labels <- deriv_sources <- deriv_refinement <- NULL
  fold_details <- vector("list", K)
  info_gate_failed <- character()

  for (k in seq_len(K)) {
    held <- which(layout$fold_resp == k)
    train <- which(layout$fold_resp != k)
    B <- .scmix_dml_basis(mu_basis, layout$Z_resp, train, k, layout$resp)
    H <- .scmix_horizontal_basis(ag$A_folds[[k]], rank_tolerance)
    seq_score <- .scmix_sequence_scores_sieve(
      deltaX = layout$deltaX, y = layout$y,
      respondent_index = layout$resp_index,
      mu_resp = mu_fold[[k]], kappa = kap[k], A = ag$A_folds[[k]],
      gh = ag$gh_folds[[k]], basis = B, horizontal = H
    )
    tg <- .scmix_dml_targets(targets, plugin_targets, mu_fold[[k]], kap[k],
                             ag$A_folds[[k]], layout$Z_resp, layout$resp, k,
                             attr_names, B, H, derivative_eps,
                             allow_numeric_derivatives,
                             derivative_refinement_factor,
                             derivative_refinement_tolerance)
    if (is.null(labels)) {
      labels <- tg$labels
      plugin <- correction <- matrix(NA_real_, N, length(labels),
                                      dimnames = list(layout$resp, labels))
      direct_pz <- matrix(NA_real_, N, length(labels),
                          dimnames = list(layout$resp, labels))
      deriv_sources <- matrix(NA_character_, K, length(labels),
                              dimnames = list(paste0("fold", seq_len(K)), labels))
      deriv_refinement <- matrix(NA_real_, K, length(labels),
                                 dimnames = dimnames(deriv_sources))
    } else if (!identical(labels, tg$labels)) {
      .scmix_dml_stop("scmix_dml(): target labels changed across folds.")
    }
    deriv_sources[k, ] <- tg$derivative_source
    deriv_refinement[k, ] <- tg$derivative_refinement_error
    training_plugin_mean <- colMeans(tg$value[train, , drop = FALSE])
    direct_pz[held, ] <- sweep(tg$value[held, , drop = FALSE], 2L,
                               training_plugin_mean, `-`)
    G <- .scmix_dml_structural_gram(B, train, p, ncol(H))
    P <- .scmix_dml_penalty(riesz_penalty, ncol(seq_score$score))
    nu_k <- .scmix_dml_nu_grid(nu_grid, k, length(train), K)
    fold_seed <- .scmix_dml_offset_seed(seed, 1000 * k)
    rz <- .scmix_dml_riesz(seq_score$score, tg$gradient, train, nu_k, P, G,
                           riesz_validation_fraction, fold_seed,
                           solve_tolerance = 1e-10)
    ev <- rz$information_eigenvalues
    generalized_min <- min(ev)
    if (!is.null(information_eigenvalue_min) &&
        generalized_min < information_eigenvalue_min) {
      info_gate_failed <- c(info_gate_failed,
                            paste0("fold ", k, " (minimum structural-norm ",
                                   "information eigenvalue ",
                                   format(generalized_min, digits = 3), ")"))
    }
    plugin[held, ] <- tg$value[held, , drop = FALSE]
    correction[held, ] <- rz$score_whitened[held, , drop = FALSE] %*% rz$alpha
    fold_details[[k]] <- list(
      heldout_respondents = layout$resp[held],
      training_n = length(train), heldout_n = length(held),
      basis_names = colnames(B), tangent_dimension = ncol(seq_score$score),
      loading_horizontal_dimension = ncol(H), selected_nu = rz$selected_nu,
      training_plugin_mean = training_plugin_mean,
      nu_validation = rz$validation,
      information_eigenvalues = ev,
      information_structural_min = generalized_min,
      structural_norm = paste0("delta-kappa^2 + E_train||delta-mu(Z)||^2 + ",
                               "||delta-A-horizontal||_F^2"),
      structural_norm_gram = rz$structural_gram,
      riesz_coefficients_whitened = rz$alpha,
      riesz_coefficients_raw = rz$alpha_raw,
      riesz_equation_relative_residual =
        rz$riesz_equation_relative_residual,
      ridge_relative_sensitivity = rz$ridge_relative_sensitivity,
      grid_contains_zero = rz$grid_contains_zero,
      derivative_refinement_error = tg$derivative_refinement_error,
      mean_training_loglik = mean(seq_score$loglik[train]),
      mean_heldout_loglik = mean(seq_score$loglik[held])
    )
  }

  if (length(info_gate_failed)) {
    reason <- paste0("weak or unidentified finite-sieve information: ",
                     paste(info_gate_failed, collapse = "; "))
    warning("scmix_dml(): ", reason,
            ". Ordinary inference is withheld; reduce/prespecify a smaller ",
            "identified basis or strengthen the design.", call. = FALSE)
    out <- .scmix_dml_withheld(reason, rank_gate, call,
                               .scmix_dml_signature(fit))
    out$fold_details <- fold_details
    return(out)
  }
  if (anyNA(plugin) || anyNA(correction) || anyNA(direct_pz)) {
    .scmix_dml_stop("internal error: some respondents were not evaluated out of fold.")
  }
  psi <- plugin + correction
  estimate <- colMeans(psi)
  plugin_estimate <- colMeans(plugin)
  ## The empirical-P_Z derivative is fold-specific: the same training
  ## empirical distribution used to estimate the fold's Riesz gradient
  ## centers the held-out plug-in contribution. Only the sum is centered
  ## globally for covariance and multiplier calculations.
  uncentered_IF <- direct_pz + correction
  IF <- sweep(uncentered_IF, 2L, colMeans(uncentered_IF), `-`)
  colnames(IF) <- labels
  variance_of_influence <- crossprod(IF) / N
  covariance_computed <- variance_of_influence / N
  se_computed <- sqrt(pmax(diag(covariance_computed), 0))
  names(estimate) <- names(plugin_estimate) <- names(se_computed) <- labels
  dimnames(covariance_computed) <- list(labels, labels)
  z <- stats::qnorm(1 - (1 - level) / 2)

  evidence <- .scmix_dml_verification(fit, verification, mu_basis)
  verification_issues <- character()
  if (ag$q > 0L && is.null(active_eigenvalue_min)) {
    verification_issues <- c(
      verification_issues,
      "missing explicit prespecified active-eigenvalue margin")
  }
  if (is.null(information_eigenvalue_min)) {
    verification_issues <- c(
      verification_issues,
      "missing explicit prespecified generalized-information margin")
  }
  if (!all(evidence$pass)) {
    verification_issues <- c(
      verification_issues,
      paste0("missing admissible ", evidence$component[!evidence$pass],
             " evidence"))
  }
  live_fit_eligibility <- attr(evidence, "fit_eligibility")
  if (is.list(live_fit_eligibility) &&
      !isTRUE(live_fit_eligibility$pass)) {
    verification_issues <- c(
      verification_issues,
      paste0("fit is diagnostic-only or otherwise ineligible: ",
             live_fit_eligibility$reason))
  }
  refine_ok <- all(vapply(fold_details, function(x)
    all(x$derivative_refinement_error <= derivative_refinement_tolerance),
    logical(1L)))
  if (!refine_ok) {
    verification_issues <- c(verification_issues,
                             "numerical derivative refinement failed")
  }
  riesz_resid <- max(unlist(lapply(fold_details,
                                   `[[`, "riesz_equation_relative_residual")))
  ridge_sens <- max(unlist(lapply(fold_details,
                                  `[[`, "ridge_relative_sensitivity")))
  if (!is.finite(riesz_resid) || riesz_resid > riesz_equation_tolerance) {
    verification_issues <- c(verification_issues,
                             "Riesz equation residual exceeds its gate")
  }
  if (!is.finite(ridge_sens) || ridge_sens > ridge_sensitivity_tolerance) {
    verification_issues <- c(verification_issues,
                             "Riesz correction is ridge-sensitive")
  }
  custom_positive_ridge <- !is.null(nu_grid) &&
    any(vapply(fold_details, function(x) x$selected_nu > 0, logical(1L)))
  ridge_evidence <- attr(evidence, "ridge")
  ridge_provenance <- if (is.list(ridge_evidence) &&
                           length(ridge_evidence$provenance) == 1L) {
    as.character(ridge_evidence$provenance)
  } else ""
  ridge_sequence_verified <- !custom_positive_ridge ||
    (is.list(ridge_evidence) && identical(ridge_evidence$pass, TRUE) &&
       !is.na(ridge_provenance) && nzchar(trimws(ridge_provenance)))
  if (!ridge_sequence_verified) {
    verification_issues <- c(
      verification_issues,
      "a user-supplied positive ridge was selected without evidence that its sequence vanishes")
  }
  valid_se <- is.finite(se_computed) & se_computed > 0
  if (!any(valid_se)) {
    verification_issues <- c(verification_issues,
                             "no target has a positive finite standard error")
  }
  conditional_ready <- !length(verification_issues) && any(valid_se)
  status <- if (conditional_ready) "conditional_available" else
    "conditional_unverified"
  target_available <- conditional_ready & valid_se
  se <- stats::setNames(rep(NA_real_, length(labels)), labels)
  se[target_available] <- se_computed[target_available]
  formal_covariance <- matrix(NA_real_, length(labels), length(labels),
                              dimnames = list(labels, labels))
  if (conditional_ready) {
    formal_covariance[target_available, target_available] <-
      covariance_computed[target_available, target_available, drop = FALSE]
  }
  ci_lower <- ci_upper <- stats::setNames(rep(NA_real_, length(labels)), labels)
  ci_lower[target_available] <- estimate[target_available] - z * se_computed[target_available]
  ci_upper[target_available] <- estimate[target_available] + z * se_computed[target_available]
  mult <- if (conditional_ready) {
    .scmix_dml_multiplier(IF, estimate, se_computed,
                          as.integer(multiplier_draws), level,
                          .scmix_dml_offset_seed(seed, 99991L), multiplier)
  } else {
    list(status = "withheld", reason = paste(verification_issues,
                                              collapse = "; "),
         n_draws = 0L)
  }

  out <- list(
    status = status,
    inference_available = conditional_ready && any(target_available),
    inference_claim = if (conditional_ready) {
      "conditional_on_documented_high_level_assumptions"
    } else "not_available",
    target_inference_available = stats::setNames(target_available, labels),
    reason = if (conditional_ready) NULL else
      paste(verification_issues, collapse = "; "),
    analysis_signature = .scmix_dml_signature(fit),
    estimate = estimate, plugin_estimate = plugin_estimate,
    one_step_adjustment = estimate - plugin_estimate,
    se = se, covariance = formal_covariance,
    diagnostic_se = se_computed, diagnostic_covariance = covariance_computed,
    variance_of_influence = variance_of_influence,
    ci_lower = ci_lower, ci_upper = ci_upper,
    influence = IF, signal = psi, plugin_contribution = plugin,
    score_correction = correction,
    direct_empirical_PZ = direct_pz,
    uncentered_influence = uncentered_IF,
    influence_global_center = colMeans(uncentered_IF),
    multiplier = mult, rank_gate = rank_gate,
    rank_gate_scope = attr(rank_gate, "scope"),
    computational_loading_source = ag$A_source,
    fold_details = fold_details, derivative_source = deriv_sources,
    derivative_refinement_error = deriv_refinement,
    verification_evidence = evidence,
    riesz_equation_max_relative_residual = riesz_resid,
    ridge_max_relative_sensitivity = ridge_sens,
    n_respondents = N, outer_folds = K, q = ag$q,
    call = call,
    theory_scope = paste(
      "Finite-sieve/high-level respondent-sequence DML. Validity additionally",
      "uses the declared empirical-L2/Frobenius structural norm. The default",
      "linear tangent is diagnostic. Fit-linked numerical and optimization",
      "artifacts are audited; any intervals remain explicitly conditional on",
      "the documented growing/fitted-sieve approximation and product-rate",
      "assumptions, which the data do not verify."
    )
  )
  class(out) <- c("scmix_dml", "list")
  out
}

#' @export
print.scmix_dml <- function(x, ...) {
  cat("paperps finite-sieve respondent-level DML\n")
  cat("  status:", x$status, "\n")
  if (!isTRUE(x$inference_available)) {
    cat("  reason:", x$reason, "\n")
    return(invisible(x))
  }
  cat("  respondents:", x$n_respondents, " outer folds:", x$outer_folds,
      " q:", x$q, "\n")
  print(data.frame(estimate = x$estimate, se = x$se,
                   lower = x$ci_lower, upper = x$ci_upper))
  cat("  scope:", x$theory_scope, "\n")
  invisible(x)
}
