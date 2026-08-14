## Debiased sign shares for arbitrary coefficient contrasts (P8).
##
## The estimand generalizes the coordinate sign share pi_k of
## scmix_polarization() to any contrast delta:
##
##   pi(delta) = E_Z[ Pr(delta' beta_i > 0 | Z_i) ]
##             = E_Z[ Phi( delta' mu(Z) / sigma_delta ) ],
##   sigma_delta^2 = delta' A A' delta,
##
## under the maintained Gaussian factor law.  One .scmix_prep() serves
## every contrast in the call: the information simulation is the
## expensive step, and each additional contrast costs only one
## loading-influence adjustment, so majority-preference sweeps should
## pass their whole grid in a single call.

#' Parse a contrasts argument into a J x p matrix plus labels
#'
#' Accepts a named numeric vector (expanded against `attr_names`, as in
#' [scmix_counterfactual()]), a full length-p vector, a numeric matrix
#' with one contrast per row, or a list of such vectors.
#' @keywords internal
#' @noRd
.scmix_parse_contrasts <- function(fit, contrasts) {
  p <- ncol(fit$deltaX)
  expand_one <- function(cv, lab) {
    if (!is.null(names(cv))) {
      bad <- setdiff(names(cv), fit$attr_names)
      if (length(bad) > 0L) {
        stop("unknown contrast names: ", paste(bad, collapse = ", "),
             call. = FALSE)
      }
      out <- stats::setNames(numeric(p), fit$attr_names)
      out[names(cv)] <- as.numeric(cv)
      out <- unname(out)
    } else {
      if (length(cv) != p) {
        stop("a contrast must have length ", p,
             " or use names from attr_names.", call. = FALSE)
      }
      out <- as.numeric(cv)
    }
    if (all(out == 0)) stop("a contrast of all zeros is not allowed.",
                            call. = FALSE)
    if (is.null(lab)) {
      nz <- which(out != 0)
      lab <- if (length(nz) > 4L) "contrast" else
        paste(ifelse(out[nz] == 1, fit$attr_names[nz],
                     sprintf("%g*%s", out[nz], fit$attr_names[nz])),
              collapse = " + ")
    }
    list(v = out, label = lab)
  }

  if (is.matrix(contrasts)) {
    if (ncol(contrasts) != p) {
      stop("contrast matrix must have ", p, " columns (one row per",
           " contrast).", call. = FALSE)
    }
    labs <- rownames(contrasts)
    parsed <- lapply(seq_len(nrow(contrasts)), function(j)
      expand_one(contrasts[j, ],
                 if (is.null(labs)) sprintf("contrast_%d", j) else labs[j]))
  } else if (is.list(contrasts)) {
    labs <- names(contrasts)
    parsed <- lapply(seq_along(contrasts), function(j)
      expand_one(contrasts[[j]],
                 if (is.null(labs) || labs[j] == "")
                   sprintf("contrast_%d", j) else labs[j]))
  } else {
    parsed <- list(expand_one(contrasts, NULL))
  }
  list(D = do.call(rbind, lapply(parsed, `[[`, "v")),
       labels = vapply(parsed, `[[`, character(1L), "label"))
}

#' Reporting gates for one contrast sign share
#'
#' Three gates, all decided on scale-invariant quantities so that
#' `pi(delta)` and `pi(c * delta)` gate identically:
#' floor -- the standardized residual SD along delta,
#' `sigma_delta / ||delta / sd_dx||`, sits below `sd_floor` in some fold
#' (reduces to the coordinate rule `sigma_k * sd_dx_k < sd_floor` at
#' `delta = e_k`); rank -- the heterogeneity t-ratio
#' `delta' Sigma_bar delta / se` from the loading influence falls below
#' `t_min`; projection -- more than 1 percent of the (standardized)
#' gradient mass lies in loading directions the truncating
#' pseudo-inverse projected out, which silently zeroes their variance
#' contribution and makes the rank t overconfident.  `t_min = 0`
#' disables the rank and projection gates (escape hatch); the floor
#' gate always applies.
#' @keywords internal
#' @noRd
.sc_signshare_gates <- function(sig2_bar, sig_by_fold, norm_d_std, g_raw,
                                sd_dx, q, I_AAeff_inv, eigA, N,
                                sd_floor, t_min) {
  sigma_std_min <- min(sig_by_fold) / norm_d_std
  floor_gate <- any(sig_by_fold / norm_d_std < sd_floor)

  var_g <- as.numeric(t(g_raw) %*% I_AAeff_inv %*% g_raw) / N
  t_het <- sig2_bar / sqrt(max(var_g, 1e-300))

  g_std <- g_raw / rep(sd_dx, q)
  denom <- sum(g_std^2)
  proj_mass <- 0
  if (denom > 0 && any(!eigA$keep)) {
    V_out <- eigA$vectors[, !eigA$keep, drop = FALSE]
    proj_mass <- sum((crossprod(V_out, g_std))^2) / denom
  }

  gates_on <- is.finite(t_min) && t_min > 0
  rank_gate <- gates_on && t_het < t_min
  proj_gate <- gates_on && proj_mass > 0.01
  list(floor = floor_gate, rank = rank_gate, proj = proj_gate,
       gated = floor_gate || rank_gate || proj_gate,
       t_het = t_het, proj_mass = proj_mass,
       sigma_std_min = sigma_std_min)
}

#' Debiased sign shares for arbitrary coefficient contrasts
#'
#' Estimates `pi(delta) = E_Z[Pr(delta' beta_i > 0 | Z_i)]` for one or
#' many contrasts `delta`, with influence-function confidence intervals
#' from the same fit.  Under the maintained Gaussian factor law the
#' conditional share is `Phi(delta' mu(Z) / sigma_delta)` with
#' `sigma_delta^2 = delta' A A' delta` (ties at zero have probability
#' zero, so `>` and `>=` coincide).  The orthogonal signal carries the
#' location correction and the completed loading-influence term, with
#' both the indirect and the direct `dH/dA` channel.
#'
#' Three recipes cover the paper's contrast estimands:
#' a compensating differential `Pr(beta_j + beta_k >= 0)` is
#' `contrasts = c(attr_j = 1, attr_k = 1)`; the majority preference
#' function `Pr((X_A - X_B)' beta > 0)` takes a matrix whose rows are
#' the profile differences `X_A - X_B`; a slope share such as the tax
#' application's "share with a progressive schedule" puts the slope
#' weights on the rate coordinates -- centered log bracket midpoints
#' `log(c(5, 22.5, 60, 130, 275, 500))`, divided by their sum of
#' squares -- and reads off `pi(delta)` for the resulting linear
#' combination.
#'
#' Reporting is gated per contrast (NA estimate, SE, and CI) by three
#' checks decided on scale-invariant quantities: a residual-SD floor on
#' the standardized index scale (any fold; reduces exactly to the
#' [scmix_polarization()] rule for a coordinate contrast); a
#' heterogeneity t-ratio `delta' Sigma_bar delta / se >= t_min` from
#' the loading influence (a contrast whose dispersion rests on loading
#' coordinates this design does not identify cannot support a
#' population sign share); and a projection check that fails the
#' contrast when a material share of its gradient lies in loading
#' directions the correction projected out.  `t_min = 0` disables the
#' latter two.  `$extra$gates` tabulates every gate input per contrast.
#'
#' One `.scmix_prep()` (the information simulation, the expensive step)
#' serves the whole call, so batch the contrasts of a sweep into one
#' call rather than looping.
#'
#' @inheritParams scmix_theta
#' @param contrasts One contrast or many: a named numeric vector
#'   (expanded against `fit$attr_names`), a full length-p vector, a
#'   numeric matrix with one contrast per row (rownames become labels),
#'   or a list of such vectors (names become labels).
#' @param by Optional respondent grouping as in [scmix_average()]: a
#'   respondent-length vector, a task-row-length vector, or a moderator
#'   column name (median split).  Group estimates re-average the same
#'   full-sample orthogonal signal; gated contrasts stay NA in every
#'   group.
#' @param sd_floor Floor on the standardized residual SD along the
#'   contrast (see [scmix_polarization()]).
#' @param t_min Threshold for the heterogeneity t-ratio gate; `0`
#'   disables the rank and projection gates.
#' @return An `scmix_quantity` with one entry per contrast (times
#'   groups when `by` is given).  `$extra` carries the parsed contrast
#'   matrix, the per-fold `sigma_delta` values and their range, the
#'   per-contrast gate table, and the labels gated to NA.
#' @export
scmix_signshare <- function(fit, contrasts, by = NULL,
                            n_bins = 40L, M = 2000L, seed = 1L,
                            sd_floor = 0.05, t_min = 2) {
  stopifnot(inherits(fit, "scmix"))
  fit <- .scmix_canon(fit)
  pc <- .scmix_parse_contrasts(fit, contrasts)
  D <- pc$D
  J <- nrow(D)
  labels <- pc$labels

  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  fold_resp <- pr$sc$fold_resp
  K <- length(fit$A_folds)
  q <- ncol(fit$A_folds[[1L]])
  sd_dx <- .scmix_sd_dx(fit)
  A_bar <- Reduce(`+`, fit$A_folds) / K
  Sig_bar <- tcrossprod(A_bar)

  psi <- matrix(NA_real_, pr$N, J)
  sigma_by_fold <- matrix(NA_real_, J, K)
  gate_rows <- vector("list", J)

  for (j in seq_len(J)) {
    d <- D[j, ]
    ## per-fold loading projections v_f = A_f' d and sigma_delta
    V_f <- vapply(fit$A_folds, function(A) as.numeric(crossprod(A, d)),
                  numeric(q))
    if (is.null(dim(V_f))) V_f <- matrix(V_f, nrow = q)
    sig_f <- sqrt(pmax(colSums(V_f^2), 1e-12))
    sigma_by_fold[j, ] <- sig_f

    norm_d_std <- sqrt(sum((d / sd_dx)^2))
    ## gradient of sigma^2_delta = d' A A' d in vec(A) at A_bar:
    ## g[(r-1)p + i] = 2 d_i (A_bar' d)_r
    v_bar <- as.numeric(crossprod(A_bar, d))
    g_raw <- 2 * as.numeric(outer(d, v_bar))
    gates <- .sc_signshare_gates(
      sig2_bar = as.numeric(t(d) %*% Sig_bar %*% d),
      sig_by_fold = sig_f, norm_d_std = norm_d_std, g_raw = g_raw,
      sd_dx = sd_dx, q = q, I_AAeff_inv = pr$I_AAeff_inv,
      eigA = pr$eigA, N = pr$N, sd_floor = sd_floor, t_min = t_min)
    gate_rows[[j]] <- data.frame(
      label = labels[j], sigma_std_min = gates$sigma_std_min,
      t_het = gates$t_het, proj_mass = gates$proj_mass,
      gate_floor = gates$floor, gate_rank = gates$rank,
      gate_proj = gates$proj, reported = !gates$gated,
      stringsAsFactors = FALSE)

    ## floored sigma per fold (keeps psi finite; gated columns go NA)
    sig_flr <- pmax(sig_f, sd_floor * norm_d_std)

    m <- as.numeric(pr$mu_resp %*% d)
    s_i <- sig_flr[fold_resp]
    z <- m / s_i
    phi_z <- stats::dnorm(z)
    H <- stats::pnorm(z)
    ## mu-gradient rows a_i = phi(z_i)/sigma_i * d
    a_rows <- (phi_z / s_i) %o% d
    ## direct channel dH/dA_{ir} = -phi(z) z d_i (A_f'd)_r / sigma^2,
    ## vec layout (r-1)p + i matches outer(d, v_f) flattened col-major
    kron_f <- t(vapply(seq_len(K), function(f)
      as.numeric(outer(d, V_f[, f])), numeric(pr$pq)))
    dA_rows <- (-phi_z * z / s_i^2) * kron_f[fold_resp, , drop = FALSE]

    col <- H + rowSums(a_rows * pr$C) -
      .scmix_A_adjust(pr, a_rows, dA_rows = dA_rows)
    if (!gates$gated) psi[, j] <- col
  }

  gate_tab <- do.call(rbind, gate_rows)
  gated_labels <- gate_tab$label[!gate_tab$reported]
  if (length(gated_labels) > 0L) {
    why <- vapply(which(!gate_tab$reported), function(j) {
      g <- gate_tab[j, ]
      paste(c(if (g$gate_floor) "floor", if (g$gate_rank) "rank",
              if (g$gate_proj) "projection"), collapse = "+")
    }, character(1L))
    warning("scmix_signshare(): reported NA for ",
            paste(sprintf("%s [%s]", gated_labels, why), collapse = ", "),
            ". The design does not identify the residual dispersion",
            " these sign shares rest on (see $extra$gates).",
            call. = FALSE)
  }

  extra <- list(contrasts = D, contrast_labels = labels,
                sigma_delta_by_fold = sigma_by_fold,
                sigma_delta_range = t(apply(sigma_by_fold, 1L, range)),
                gates = gate_tab, gated = gated_labels)

  if (is.null(by)) {
    out <- .scmix_wrap(psi, labels, "pi(delta) (share with delta'beta > 0)",
                       fit, extra = extra)
  } else {
    g <- .scmix_resolve_by(fit, by, fn = "scmix_signshare")
    out <- .scmix_wrap_by(psi, g, col_labels = labels,
                          quantity = "pi(delta) by subgroup",
                          fit = fit, fn = "scmix_signshare", extra = extra)
  }
  oob <- !is.na(out$estimate) & (out$estimate < 0 | out$estimate > 1)
  if (any(oob)) {
    warning("scmix_signshare(): estimate outside [0, 1] for: ",
            paste(names(out$estimate)[oob], collapse = ", "),
            ". The additive correction is unreliable for shares this",
            " close to the boundary; interpret with caution.",
            call. = FALSE)
    out$extra$out_of_range <- names(out$estimate)[oob]
  }
  out
}
