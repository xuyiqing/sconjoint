# Nonlinear compensator columns for the Graham--Svolik (gs2020) compensating-
# differentials exhibit.
#
# Implements Section 8 ("Nonlinear compensator columns") of the algorithms memo,
#   .../2608_issues/Yiqing/applications/compensator_columns_section_2026-08-27.tex
# as revised after the two adversarial audits
#   compensator_columns_audit_math_2026-08-27.md      (math)
#   compensator_columns_audit_fidelity_2026-08-27.md  (construct provenance)
# whose required fixes are load-bearing here and are marked [AUDIT] below.
#
# Estimand. For a benefit function b : R^p -> R,
#     C_0(c_p; b) = Pr{ c_p' beta_i + b(beta_i) >= 0 }
#                 = E_Z[ Pr{ h(mu(Z) + A u) >= 0 | Z } ],   u ~ N(0, I_q),
#     h(beta) = c_p' beta + b(beta).
# Five columns, all one engine:
#     None         b = 0
#     Co-party     b(beta) = a * c_b' beta                       (a = 1)
#     Full policy  b_pol(beta) = 3(|c_e' beta| + |c_s' beta|)
#     Gov feature  b_gov(beta) = max{0, max_g c_g' beta}         (positive part)
#     Any          b_any(beta) = max{c_b' beta, b_pol(beta), b_gov(beta)}
# Tie convention is `>=` everywhere, matching paperps.tex eq. compensating_share
# and the archived `compensate <- function(benefit) beta_u + benefit >= 0`.
#
# Method (q = 1, exact). Every one of the five h is CONVEX piecewise linear in
# u: each is an affine term plus a maximum of affine functions, so
#     h(mu + A u) = max_k (alpha_k + gamma_k u),  alpha_k = c_k' mu, gamma_k = c_k' A,
# over an explicit finite list of "support contrasts" c_k. Hence
#     {h >= 0} = union_k {alpha_k + gamma_k u >= 0},
# a union of half-lines: right ray, left ray, all of R, or empty per line. The
# acceptance set is therefore always (-inf, L] U [R, inf) (possibly all of R),
# never a bounded interval, and the cell is Phi(L) + 1 - Phi(R). No envelope
# construction, no breakpoint bookkeeping, no 0/0. [AUDIT math, Finding 3]
# `cc_cell_breakpoints()` implements the memo's literal breakpoint route as an
# independent second engine for verification.
#
# [AUDIT math, Finding 2] The Any column is the UNION of the three acceptance
# sets of Co-party, Full policy, and Gov feature -- the same event as the
# archived three-way logical OR. It is NOT the per-column breakpoint recipe
# rerun on h_any: that misses the kinks where h_any switches benefit branch
# (audited counterexamples: 0.564263 true vs 0.545472 literal reuse on a generic
# interior config; 0.076564 vs 0.031645 on a minimal one).
#
# [AUDIT math, Finding 3, edit 1] Breakpoint roots are collected ONLY where the
# line's slope is nonzero. The literal root -alpha/gamma is 0/0 on an
# identically-zero line, a case that occurs in the memo's own check (ii).
#
# [AUDIT math, Finding 6] The dispersion-floor gate is one-sided, so the engine
# also returns the per-respondent supplement: the slope of the support line
# generating each acceptance endpoint, and the cell's exact sensitivity to a
# level shift of h, sum_j phi(e_j)/|gamma_(j)|.
#
# [AUDIT math, Finding 6.2 / Finding 5] A floored Full-policy, Gov-feature, or
# Any cell inherits the None cell's certified lower bound by pointwise
# domination (b >= 0), so it is released as "at least x", never as NA. Co-party
# gets no such inheritance: its benefit takes both signs.
#
# Interface. Fit-facing entry points follow the conventions of
# 2608_issues/Yiqing/bound_for_share/code/share_bounds.R: raw-scale loadings
# A_folds[[k]] / sd_dx_folds[[k]] (as in sb_raw_A_folds), respondent-level means
# by !duplicated(respondent_id) (as in sb_respondent_means).
#
# Dependencies: base R + stats only. Standalone; source() it, no package load
# required to run the engine on synthetic (mu, A).


## ---------------------------------------------------------------------------
## Constants: archived column labels and factor order (35b:342-345 = refreeze:579-580)
## ---------------------------------------------------------------------------

cc_column_labels <- function() {
  c(none = "None", party = "Co-party", pol = "Full policy",
    gov = "Gov feature", any = "Any")
}

cc_specs <- function() c("none", "party", "pol", "gov", "any")

## Ideology partition, 35_run:162-164 / refreeze:68 (same partition on integer
## ideo7 in 1..7).
cc_ideology_group <- function(ideo7) {
  cut(as.numeric(ideo7), breaks = c(0.5, 3.5, 4.5, 7.5),
      labels = c("Liberal (1-3)", "Moderate (4)", "Conservative (5-7)"))
}


## ---------------------------------------------------------------------------
## Selectors
## ---------------------------------------------------------------------------

#' Coerce one selector to a numeric contrast vector of length p
#'
#' @param x Either a coordinate name present in `attr_names`, a coordinate
#'   index, or a numeric vector of length `p`.
#' @param attr_names Character vector of coordinate names (length p).
#' @param what Label used in error messages.
#' @return A numeric vector of length `p`.
.cc_as_contrast <- function(x, attr_names, what = "selector") {
  p <- length(attr_names)
  if (is.character(x)) {
    if (length(x) != 1L) stop(what, ": give one coordinate name.", call. = FALSE)
    j <- match(x, attr_names)
    if (is.na(j)) stop(what, ": coordinate '", x, "' not in attr_names.",
                       call. = FALSE)
    v <- numeric(p); v[j] <- 1; return(v)
  }
  if (is.numeric(x) && length(x) == 1L && x == round(x) && x >= 1 && x <= p) {
    v <- numeric(p); v[as.integer(x)] <- 1; return(v)
  }
  if (is.numeric(x) && length(x) == p) return(as.numeric(x))
  stop(what, ": must be a coordinate name, a coordinate index, or a numeric ",
       "vector of length ", p, ".", call. = FALSE)
}

#' Build the selector list consumed by the engine
#'
#' @param attr_names Character vector of coordinate names (length p).
#' @param action Undemocratic-action coordinate `c_p` (name, index, or vector).
#' @param coparty Co-partisanship coordinate `c_b`.
#' @param econ Economic-policy coordinate `c_e`.
#' @param social Social-policy coordinate `c_s`.
#' @param governance Character/numeric vector of the six good-governance
#'   coordinates `G`, or a list of contrast vectors. The elections-board
#'   reference level is NOT among them; it enters as the zero floor.
#' @return A list with components `action`, `coparty`, `econ`, `social`,
#'   `governance` (a list), and `attr_names`.
cc_selectors <- function(attr_names, action, coparty, econ, social, governance) {
  attr_names <- as.character(attr_names)
  gov <- if (is.list(governance)) governance else as.list(governance)
  sel <- list(
    action  = .cc_as_contrast(action,  attr_names, "action"),
    coparty = .cc_as_contrast(coparty, attr_names, "coparty"),
    econ    = .cc_as_contrast(econ,    attr_names, "econ"),
    social  = .cc_as_contrast(social,  attr_names, "social"),
    governance = lapply(seq_along(gov), function(i)
      .cc_as_contrast(gov[[i]], attr_names, paste0("governance[[", i, "]]"))),
    attr_names = attr_names)
  if (length(sel$governance) < 1L) {
    stop("At least one governance selector is required.", call. = FALSE)
  }
  sel
}

#' Support contrasts c_k with h(mu + Au) = max_k (c_k'mu + c_k'A u)
#'
#' none:  {c_p}
#' party: {c_p + a c_b}
#' pol:   {c_p + 3 s1 c_e + 3 s2 c_s : s1, s2 in {+1,-1}}          (4 lines)
#' gov:   {c_p} U {c_p + c_g : g in G}                             (1 + |G| lines)
#' any:   party U pol U gov                                        (12 lines)
#'
#' These are exactly the slope sets named by the memo's floor gate: the four
#' values (c_p +- 3c_e +- 3c_s)'A for b_pol, and c_p'A together with
#' (c_p + c_g)'A for b_gov.
#'
#' @return A p x K numeric matrix whose columns are the support contrasts.
.cc_support_contrasts <- function(sel, spec, policy_steps = 3,
                                  coparty_amount = 1) {
  cp <- sel$action
  party <- cp + coparty_amount * sel$coparty
  pol <- NULL
  if (spec %in% c("pol", "any")) {
    pol <- cbind(cp + policy_steps * sel$econ + policy_steps * sel$social,
                 cp + policy_steps * sel$econ - policy_steps * sel$social,
                 cp - policy_steps * sel$econ + policy_steps * sel$social,
                 cp - policy_steps * sel$econ - policy_steps * sel$social)
  }
  gov <- NULL
  if (spec %in% c("gov", "any")) {
    gov <- cbind(cp, vapply(sel$governance, function(cg) cp + cg, numeric(length(cp))))
  }
  out <- switch(spec,
    none  = matrix(cp, ncol = 1L),
    party = matrix(party, ncol = 1L),
    pol   = pol,
    gov   = gov,
    any   = cbind(matrix(party, ncol = 1L), pol, gov),
    stop("Unknown spec: ", spec, call. = FALSE))
  out <- as.matrix(out)
  dimnames(out) <- NULL
  out
}


## ---------------------------------------------------------------------------
## Core exact engine (q = 1): acceptance set as a union of half-lines
## ---------------------------------------------------------------------------

.cc_acceptance_new <- function(n) {
  list(L = rep(-Inf, n), R = rep(Inf, n),
       gL = rep(NA_real_, n), gR = rep(NA_real_, n),
       whole = rep(FALSE, n), tied = rep(FALSE, n),
       zero_line = rep(FALSE, n))
}

## Endpoint bookkeeping. On an exact tie between two support lines (a
## root-at-kink collision) we keep the SMALLER |slope|. That is not arbitrary:
## raising the level of h by t moves R(t) = min_k -(alpha_k + t)/gamma_k, whose
## fastest-decreasing branch is the smallest gamma_k, so the retained slope is
## the correct right-derivative branch -- and it is the conservative one for the
## floor flag. Ties are also recorded in `tied`.
.cc_update_L <- function(acc, cand, cand_g) {
  better <- !is.na(cand) & cand > acc$L
  tie <- !is.na(cand) & is.finite(cand) & (cand == acc$L)
  gcur <- ifelse(is.na(acc$gL), Inf, abs(acc$gL))
  tie_win <- tie & (abs(cand_g) < gcur)
  take <- better | tie_win
  take[is.na(take)] <- FALSE
  acc$L[take] <- cand[take]
  acc$gL[take] <- cand_g[take]
  acc$tied <- acc$tied | tie
  acc
}

.cc_update_R <- function(acc, cand, cand_g) {
  better <- !is.na(cand) & cand < acc$R
  tie <- !is.na(cand) & is.finite(cand) & (cand == acc$R)
  gcur <- ifelse(is.na(acc$gR), Inf, abs(acc$gR))
  tie_win <- tie & (abs(cand_g) < gcur)
  take <- better | tie_win
  take[is.na(take)] <- FALSE
  acc$R[take] <- cand[take]
  acc$gR[take] <- cand_g[take]
  acc$tied <- acc$tied | tie
  acc
}

#' Acceptance set of {max_k (alpha_k + gamma_k u) >= 0}, vectorised over rows
#'
#' @param Alpha n x K matrix of support-line intercepts.
#' @param Gamma n x K matrix of support-line slopes.
#' @return An acceptance object: `L`, `R` (endpoints; acceptance is
#'   (-inf, L] U [R, inf)), `gL`, `gR` (generating slopes, NA at an infinite
#'   endpoint), `whole` (a zero-slope line at a nonnegative level covers R),
#'   `tied` (an endpoint was attained by two support lines at once).
.cc_acceptance_from_lines <- function(Alpha, Gamma) {
  Alpha <- as.matrix(Alpha); Gamma <- as.matrix(Gamma)
  n <- nrow(Alpha); K <- ncol(Alpha)
  acc <- .cc_acceptance_new(n)
  for (k in seq_len(K)) {
    al <- Alpha[, k]; ga <- Gamma[, k]
    ## [AUDIT] roots collected ONLY where the slope is nonzero: -alpha/gamma is
    ## 0/0 on an identically-zero line.
    nz <- ga != 0
    root <- rep(NA_real_, n)
    if (any(nz)) root[nz] <- -al[nz] / ga[nz]
    neg <- ga < 0
    if (any(neg)) {
      acc <- .cc_update_L(acc, ifelse(neg, root, NA_real_),
                          ifelse(neg, ga, NA_real_))
    }
    pos <- ga > 0
    if (any(pos)) {
      acc <- .cc_update_R(acc, ifelse(pos, root, NA_real_),
                          ifelse(pos, ga, NA_real_))
    }
    zer <- !nz
    ## `>=` at the boundary: a flat line sitting exactly at zero accepts.
    if (any(zer)) {
      acc$whole <- acc$whole | (zer & al >= 0)
      ## The memo's exact boundary-mass condition: {h = 0} carries probability
      ## zero precisely when NO linear piece is identically zero, i.e. no
      ## support contrast has both c'A = 0 and c'mu = 0. A zero slope at a
      ## nonzero level is harmless; the dispersion floor does not by itself rule
      ## out this measure-zero coincidence, so flag it rather than assume it away.
      acc$zero_line <- acc$zero_line | (zer & al == 0)
    }
  }
  acc
}

#' Union of acceptance sets (used for the Any column)
#'
#' (-inf, L1] U [R1, inf) union (-inf, L2] U [R2, inf)
#'   = (-inf, max L] U [min R, inf).
.cc_union_acceptance <- function(accs) {
  out <- accs[[1L]]
  if (length(accs) > 1L) {
    for (k in seq_along(accs)[-1L]) {
      a <- accs[[k]]
      out <- .cc_update_L(out, a$L, a$gL)
      out <- .cc_update_R(out, a$R, a$gR)
      out$whole <- out$whole | a$whole
      out$tied <- out$tied | a$tied
      out$zero_line <- out$zero_line | a$zero_line
    }
  }
  out
}

#' Standard-normal mass, endpoint sensitivities, and floor flags
#'
#' @param acc An acceptance object.
#' @param floor Scalar dispersion floor on the raw contrast scale, or NA to
#'   skip the per-respondent binding-slope flag.
#' @return A list with `share`, `sensitivity` (0 for a cell with no finite
#'   endpoint -- correct for an upward level shift; a `zero_line` cell is
#'   nonetheless discontinuous downward, which is why that flag is carried),
#'   `binding_min_slope`, `binding_below_floor`, `n_endpoints`, `tied`,
#'   `zero_line`, plus the acceptance fields.
.cc_summarise_acceptance <- function(acc, floor = NA_real_) {
  full <- acc$whole | (acc$L >= acc$R)          # `>=`, not `>`
  share <- stats::pnorm(acc$L) + stats::pnorm(acc$R, lower.tail = FALSE)
  share[full] <- 1
  finL <- is.finite(acc$L) & !full
  finR <- is.finite(acc$R) & !full
  ## Exact d(share)/d(level shift of h): sum_j phi(e_j) / |gamma_(j)|.
  sens <- numeric(length(share))
  sens[finL] <- sens[finL] + stats::dnorm(acc$L[finL]) / abs(acc$gL[finL])
  sens[finR] <- sens[finR] + stats::dnorm(acc$R[finR]) / abs(acc$gR[finR])
  slopeL <- ifelse(finL, abs(acc$gL), NA_real_)
  slopeR <- ifelse(finR, abs(acc$gR), NA_real_)
  ## The binding slope: the smaller of the two endpoint slopes, i.e. the one
  ## the floor gate must clear for this respondent's cell to be more than an
  ## indicator.
  bmin <- pmin(slopeL, slopeR, na.rm = TRUE)
  below <- if (is.na(floor)) rep(NA, length(share)) else
    !is.na(bmin) & bmin < floor
  list(share = share, sensitivity = sens,
       binding_min_slope = bmin, binding_below_floor = below,
       n_endpoints = as.integer(finL) + as.integer(finR),
       L = acc$L, R = acc$R, gL = acc$gL, gR = acc$gR,
       whole = full, tied = acc$tied, zero_line = acc$zero_line)
}

#' Normalise a loading argument to one q = 1 row per respondent
#'
#' @param A One of: an n x p matrix (one loading row per respondent, q = 1); a
#'   numeric vector of length p (common loading, q = 1); a p x q matrix
#'   (common loading, q = ncol).
#' @param form "auto", "rows", or "common". "auto" reads an n x p matrix as
#'   respondent rows and a p x q matrix as a common loading; pass an explicit
#'   form when n == p.
#' @return A list with `Aq` (n x p, q = 1) or `A_common` (p x q) and `q`.
.cc_normalize_loading <- function(A, n, p, form = c("auto", "rows", "common")) {
  form <- match.arg(form)
  if (is.null(dim(A))) {
    if (length(A) != p) stop("Loading vector must have length p = ", p, ".",
                             call. = FALSE)
    return(list(Aq = matrix(rep(as.numeric(A), each = n), nrow = n), q = 1L,
                A_common = matrix(as.numeric(A), ncol = 1L)))
  }
  A <- as.matrix(A)
  is_rows <- switch(form,
    rows = TRUE, common = FALSE,
    auto = (nrow(A) == n && ncol(A) == p))
  if (is_rows) {
    if (nrow(A) != n || ncol(A) != p) {
      stop("Respondent-row loading must be n x p = ", n, " x ", p, ".",
           call. = FALSE)
    }
    return(list(Aq = A, q = 1L, A_common = NULL))
  }
  if (nrow(A) != p) stop("Common loading must have p = ", p, " rows.",
                         call. = FALSE)
  q <- ncol(A)
  if (q == 1L) {
    return(list(Aq = matrix(rep(as.numeric(A[, 1L]), each = n), nrow = n),
                q = 1L, A_common = A))
  }
  list(Aq = NULL, q = q, A_common = A)
}

#' Exact conditional shares for one column, all respondents (q = 1)
#'
#' @param mu n x p matrix of respondent means (raw contrast scale).
#' @param A Loading; see `.cc_normalize_loading`. Must reduce to q = 1.
#' @param sel Selector list from `cc_selectors()`.
#' @param spec One of "none", "party", "pol", "gov", "any".
#' @param policy_steps Full policy swing in ordinal steps (3; see the units
#'   assertion in `cc_assert_policy_units`).
#' @param coparty_amount The manuscript's amount `a` in `C_0(c_p, c_b; a)`.
#' @param floor Scalar dispersion floor on the raw contrast scale, or NA.
#' @param verify_any If TRUE (default) the Any column is computed as the union
#'   of the three acceptance sets AND cross-checked against the direct
#'   twelve-support-line construction; they are provably identical, so a
#'   mismatch is an implementation error.
#' @return A list: `share` (length n), `sensitivity`, `binding_min_slope`,
#'   `binding_below_floor`, `n_endpoints`, `L`, `R`, `gL`, `gR`, `whole`,
#'   `tied` (an endpoint attained by two support lines -- a root-at-kink
#'   collision, where only one-sided derivatives exist), `zero_line` (a support
#'   line is identically zero, so the `>=` boundary carries real mass),
#'   `slopes` (n x K support-line slopes), `max_abs_slope` (length n).
cc_cell_exact <- function(mu, A, sel, spec, policy_steps = 3,
                          coparty_amount = 1, floor = NA_real_,
                          loading_form = "auto", verify_any = TRUE) {
  mu <- as.matrix(mu)
  n <- nrow(mu); p <- ncol(mu)
  ld <- .cc_normalize_loading(A, n, p, loading_form)
  if (ld$q != 1L) {
    stop("cc_cell_exact() requires q = 1; got q = ", ld$q,
         ". Use cc_cell_mc() for the q > 1 fallback.", call. = FALSE)
  }
  Aq <- ld$Aq
  build <- function(sp) {
    C <- .cc_support_contrasts(sel, sp, policy_steps, coparty_amount)
    list(acc = .cc_acceptance_from_lines(mu %*% C, Aq %*% C),
         Gamma = Aq %*% C)
  }
  if (spec == "any") {
    ## [AUDIT Finding 2] union of the three acceptance sets, never a rerun of
    ## the per-column breakpoint recipe on h_any.
    parts <- lapply(c("party", "pol", "gov"), build)
    acc <- .cc_union_acceptance(lapply(parts, `[[`, "acc"))
    Gamma <- do.call(cbind, lapply(parts, `[[`, "Gamma"))
    if (isTRUE(verify_any)) {
      direct <- build("any")
      s_u <- .cc_summarise_acceptance(acc)$share
      s_d <- .cc_summarise_acceptance(direct$acc)$share
      if (max(abs(s_u - s_d)) > 1e-12) {
        stop("Internal error: Any-column union disagrees with the direct ",
             "twelve-line construction (max ", max(abs(s_u - s_d)), ").",
             call. = FALSE)
      }
    }
  } else {
    one <- build(spec)
    acc <- one$acc
    Gamma <- one$Gamma
  }
  out <- .cc_summarise_acceptance(acc, floor)
  out$slopes <- Gamma
  out$max_abs_slope <- apply(abs(Gamma), 1L, max)
  out$spec <- spec
  out
}


## ---------------------------------------------------------------------------
## Definitional h and the independent breakpoint engine (verification path)
## ---------------------------------------------------------------------------

#' h(mu_i + a_i u) evaluated straight from the definition (abs / max form)
#'
#' Mirrors the archived lines: policy benefit `3 * (abs(.) + abs(.))`,
#' governance benefit `pmax(apply(., 1, max), 0)`, any = three-way max.
#' Uses no support-line algebra, so it is an independent check of it.
cc_h_eval <- function(mu_i, a_i, sel, spec, u, policy_steps = 3,
                      coparty_amount = 1) {
  u <- as.numeric(u)
  ln <- function(cv) sum(cv * mu_i) + sum(cv * a_i) * u
  lp <- ln(sel$action)
  if (spec == "none") return(lp)
  if (spec == "party") return(lp + coparty_amount * ln(sel$coparty))
  bpol <- policy_steps * (abs(ln(sel$econ)) + abs(ln(sel$social)))
  gl <- matrix(vapply(sel$governance, ln, numeric(length(u))), nrow = length(u))
  bgov <- pmax(apply(gl, 1L, max), 0)
  if (spec == "pol") return(lp + bpol)
  if (spec == "gov") return(lp + bgov)
  if (spec == "any") {
    return(lp + pmax(coparty_amount * ln(sel$coparty), bpol, bgov))
  }
  stop("Unknown spec: ", spec, call. = FALSE)
}

#' Independent second engine: superset breakpoint enumeration for one respondent
#'
#' Follows the memo's steps 2-3 literally, but with the superset principle the
#' audit requires: breakpoints are all support-line roots (nonzero slope only)
#' AND all pairwise support-line crossings, so no branch-switch kink can be
#' missed for any of the five columns, Any included. On each piece h is
#' evaluated from the DEFINITION at two interior points, so the piece's slope
#' and level are exact.
#'
#' @param merge_tol Endpoints within this relative distance are treated as the
#'   same point when counting components. Two representations of one shared
#'   kink -- the grid breakpoint `-alpha/gamma` and the chord root
#'   `x1 - v1/slope` -- agree only to a few ulps, so an exact merge would report
#'   spurious extra components. The reported `share` is summed over the RAW
#'   per-piece intervals and does not depend on this tolerance.
#' @return A list with `share`, `intervals` (a 2-column matrix of merged closed
#'   acceptance intervals), `n_components`, `bounded_component` (TRUE if any
#'   merged component is bounded -- a convexity violation, always FALSE for a
#'   correct run), `n_breakpoints`.
cc_cell_breakpoints <- function(mu_i, a_i, sel, spec, policy_steps = 3,
                                coparty_amount = 1, merge_tol = 1e-9) {
  C <- .cc_support_contrasts(sel, spec, policy_steps, coparty_amount)
  al <- as.numeric(crossprod(C, mu_i))
  ga <- as.numeric(crossprod(C, a_i))
  K <- length(al)
  pts <- numeric(0)
  for (k in seq_len(K)) {
    if (ga[k] != 0) pts <- c(pts, -al[k] / ga[k])   # [AUDIT] nonzero slope only
    if (k < K) for (j in (k + 1L):K) {
      if (ga[k] != ga[j]) pts <- c(pts, (al[j] - al[k]) / (ga[k] - ga[j]))
    }
  }
  pts <- pts[is.finite(pts)]
  pts <- sort(unique(pts))
  grid <- c(-Inf, pts, Inf)
  lo_v <- grid[-length(grid)]; hi_v <- grid[-1L]
  ivs <- list()
  for (m in seq_along(lo_v)) {
    lo <- lo_v[m]; hi <- hi_v[m]
    if (!(hi > lo)) next
    if (!is.finite(lo) && !is.finite(hi)) { x1 <- -1; x2 <- 1 }
    else if (!is.finite(lo)) { x1 <- hi - 2; x2 <- hi - 1 }
    else if (!is.finite(hi)) { x1 <- lo + 1; x2 <- lo + 2 }
    else { w <- hi - lo; x1 <- lo + 0.25 * w; x2 <- lo + 0.75 * w }
    if (!(x2 > x1)) next
    v1 <- cc_h_eval(mu_i, a_i, sel, spec, x1, policy_steps, coparty_amount)
    v2 <- cc_h_eval(mu_i, a_i, sel, spec, x2, policy_steps, coparty_amount)
    slope <- (v2 - v1) / (x2 - x1)
    if (slope == 0) {
      if (v1 >= 0) ivs[[length(ivs) + 1L]] <- c(lo, hi)
    } else {
      r <- x1 - v1 / slope
      if (slope > 0) { a_ <- max(lo, r); b_ <- hi } else { a_ <- lo; b_ <- min(hi, r) }
      if (b_ > a_) ivs[[length(ivs) + 1L]] <- c(a_, b_)
    }
  }
  if (!length(ivs)) {
    return(list(share = 0, intervals = matrix(numeric(0), ncol = 2L),
                n_components = 0L, bounded_component = FALSE,
                n_breakpoints = length(pts)))
  }
  M <- do.call(rbind, ivs)
  ## The pieces are disjoint by construction (each interval lies inside its own
  ## grid cell), so the raw sum is the exact mass; merging below is only for the
  ## structural component count.
  share <- sum(stats::pnorm(M[, 2L]) - stats::pnorm(M[, 1L]))
  M <- M[order(M[, 1L]), , drop = FALSE]
  merged <- M[1L, , drop = FALSE]
  if (nrow(M) > 1L) for (m in 2:nrow(M)) {
    last <- nrow(merged)
    slack <- merge_tol * max(1, abs(merged[last, 2L]))
    if (M[m, 1L] <= merged[last, 2L] + slack) {
      merged[last, 2L] <- max(merged[last, 2L], M[m, 2L])
    } else merged <- rbind(merged, M[m, , drop = FALSE])
  }
  width <- merged[, 2L] - merged[, 1L]
  bounded <- any(is.finite(merged[, 1L]) & is.finite(merged[, 2L]) &
                   width > merge_tol * pmax(1, abs(merged[, 1L])))
  list(share = share, intervals = merged, n_components = nrow(merged),
       bounded_component = bounded, n_breakpoints = length(pts))
}


## ---------------------------------------------------------------------------
## q > 1 fallback: seeded Monte Carlo (memo step 7)
## ---------------------------------------------------------------------------

#' Seeded Monte Carlo cell value for q >= 1
#'
#' Memo step 7. `M` defaults to the smallest draw count whose worst-case
#' standard error, sqrt(0.25 / M), sits below `se_target_frac` of the printed
#' cell resolution (0.1 * 0.01 = 0.001, so M = 250,000). A doubled-M replicate
#' is run and reported. The gate bounds the Monte Carlo standard error, not the
#' last printed digit, so the distance to the nearest rounding boundary is
#' reported too.
#'
#' @param mu n x p matrix of respondent means.
#' @param A p x q common loading, or a list of n such matrices.
#' @param sel Selector list.
#' @param spec One of the five specs.
#' @return A list with `share`, `share_double`, `mc_se`, `M`,
#'   `rounding_distance`, `printed_digit_stable`.
cc_cell_mc <- function(mu, A, sel, spec, policy_steps = 3, coparty_amount = 1,
                       M = NULL, seed = 20260827L, resolution = 0.01,
                       se_target_frac = 0.1, chunk = 50000L) {
  mu <- as.matrix(mu); n <- nrow(mu); p <- ncol(mu)
  if (is.null(M)) M <- ceiling(0.25 / (se_target_frac * resolution)^2)
  M <- as.integer(M)
  A_list <- if (is.list(A)) A else replicate(n, as.matrix(A), simplify = FALSE)
  if (length(A_list) != n) stop("Loading list must have n = ", n, " entries.",
                                call. = FALSE)
  q <- ncol(A_list[[1L]])
  ## Fixed seed per the memo, but restore the caller's RNG stream on exit so a
  ## driver script's other random draws are unaffected.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }
  ## One shared draw block per replicate: the estimator is the sample mean over
  ## draws of f(u) = (1/n) sum_i 1{h_i(mu_i + A_i u) >= 0}, so the Monte Carlo
  ## standard error is sd(f)/sqrt(M) across draws -- NOT a binomial formula,
  ## which would ignore the cross-respondent correlation induced by sharing u.
  run <- function(draws, sd) {
    set.seed(sd)
    tot <- 0; tot2 <- 0; done <- 0L
    while (done < draws) {
      nb <- min(chunk, draws - done)
      U <- matrix(stats::rnorm(nb * q), nrow = nb, ncol = q)
      acc <- numeric(nb)
      for (i in seq_len(n)) {
        B <- matrix(mu[i, ], nrow = nb, ncol = p, byrow = TRUE) +
          U %*% t(A_list[[i]])
        acc <- acc + as.numeric(.cc_h_from_beta(B, sel, spec, policy_steps,
                                                coparty_amount) >= 0)
      }
      f <- acc / n
      tot <- tot + sum(f); tot2 <- tot2 + sum(f^2)
      done <- done + nb
    }
    mn <- tot / draws
    v <- max(tot2 / draws - mn^2, 0) * draws / max(draws - 1L, 1L)
    list(mean = mn, se = sqrt(v / draws))
  }
  r1 <- run(M, seed)
  r2 <- run(2L * M, seed + 1L)
  rd <- abs((r1$mean / resolution) - (floor(r1$mean / resolution) + 0.5)) *
    resolution
  list(share = r1$mean, share_double = r2$mean, mc_se = r1$se, M = M,
       se_gate_pass = r1$se <= se_target_frac * resolution,
       replicate_gap = abs(r1$mean - r2$mean), rounding_distance = rd,
       printed_digit_stable = rd > 2 * r1$se)
}

## h evaluated on a matrix of beta draws (definitional form; used by the MC path).
.cc_h_from_beta <- function(B, sel, spec, policy_steps = 3, coparty_amount = 1) {
  pr <- function(cv) as.numeric(B %*% cv)
  lp <- pr(sel$action)
  if (spec == "none") return(lp)
  if (spec == "party") return(lp + coparty_amount * pr(sel$coparty))
  bpol <- policy_steps * (abs(pr(sel$econ)) + abs(pr(sel$social)))
  gl <- vapply(sel$governance, pr, numeric(nrow(B)))
  bgov <- pmax(apply(matrix(gl, nrow = nrow(B)), 1L, max), 0)
  if (spec == "pol") return(lp + bpol)
  if (spec == "gov") return(lp + bgov)
  if (spec == "any") return(lp + pmax(coparty_amount * pr(sel$coparty), bpol, bgov))
  stop("Unknown spec: ", spec, call. = FALSE)
}


## ---------------------------------------------------------------------------
## Assertions and floor helpers (memo step 1; Uncertainty gate b)
## ---------------------------------------------------------------------------

#' Assert the prepared policy coordinates are in raw ordinal units
#'
#' Memo step 1: every fielded profile difference on each policy coordinate must
#' be an integer in [-3, 3]. If preparation ever rescales, fail rather than
#' silently mismatch the `policy_steps = 3` multiplier. Verified in the raw
#' Graham--Svolik data: `diff_p1_num`, `diff_p2_num` take exactly {-3,...,3}.
#'
#' @param deltaX Task-level contrast matrix.
#' @param econ_idx,social_idx Column indices (or names) of the two policy
#'   coordinates.
#' @param policy_steps Expected full swing (3).
#' @return Invisibly TRUE; stops on violation.
cc_assert_policy_units <- function(deltaX, econ_idx, social_idx,
                                   policy_steps = 3, tol = 1e-9) {
  deltaX <- as.matrix(deltaX)
  for (nm in c(econ_idx, social_idx)) {
    v <- as.numeric(deltaX[, nm])
    if (any(!is.finite(v))) {
      stop("Policy coordinate '", nm, "' has non-finite entries.", call. = FALSE)
    }
    if (max(abs(v - round(v))) > tol) {
      stop("Policy coordinate '", nm, "' is not in raw ordinal units ",
           "(non-integer differences found). Preparation appears to rescale; ",
           "the ", policy_steps, "-step multiplier would silently mismatch.",
           call. = FALSE)
    }
    if (max(abs(v)) > policy_steps + tol) {
      stop("Policy coordinate '", nm, "' has |difference| > ", policy_steps,
           "; the ordinal scale is not 1..", policy_steps + 1, ".", call. = FALSE)
    }
    if (max(abs(v)) < policy_steps - tol) {
      warning("Policy coordinate '", nm, "' never attains the full ",
              policy_steps, "-step swing in the fielded design.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Reduce a per-coordinate dispersion floor to one scalar for a column
#'
#' The bound memo's floor (`sb_zero_floor()$floor`) is per coordinate, i.e. it
#' calibrates |A_j| for a UNIT coordinate contrast. The compensator slopes are
#' composite contrasts c'A, for which no per-coordinate floor exists. This
#' helper takes the conservative (largest) floor over the coordinates that enter
#' any support line of the column, making the gate harder to clear. Judgment
#' call, recorded rather than hidden: use it explicitly, or pass your own scalar.
cc_reduce_floor <- function(floor_vec, sel, spec, policy_steps = 3,
                            coparty_amount = 1) {
  if (length(floor_vec) == 1L) return(as.numeric(floor_vec))
  C <- .cc_support_contrasts(sel, spec, policy_steps, coparty_amount)
  used <- apply(C != 0, 1L, any)
  if (!any(used)) return(NA_real_)
  max(as.numeric(floor_vec)[used])
}


## ---------------------------------------------------------------------------
## Respondent-level and cell-level assembly
## ---------------------------------------------------------------------------

#' Per-respondent conditional shares for all five columns
#'
#' @param mu n x p matrix of respondent means (raw contrast scale).
#' @param A Loading; see `.cc_normalize_loading`.
#' @param sel Selector list from `cc_selectors()`.
#' @param specs Which columns to compute (default all five).
#' @param floors Named numeric of scalar floors per spec, a single scalar, or
#'   NA.
#' @return A named list of `cc_cell_exact()` results, one per spec.
cc_respondent_shares <- function(mu, A, sel, specs = cc_specs(),
                                 policy_steps = 3, coparty_amount = 1,
                                 floors = NA_real_, loading_form = "auto",
                                 verify_any = TRUE) {
  get_floor <- function(sp) {
    if (length(floors) == 1L) return(as.numeric(floors))
    if (!is.null(names(floors)) && sp %in% names(floors)) {
      return(as.numeric(floors[[sp]]))
    }
    NA_real_
  }
  out <- lapply(specs, function(sp)
    cc_cell_exact(mu, A, sel, sp, policy_steps, coparty_amount,
                  floor = get_floor(sp), loading_form = loading_form,
                  verify_any = verify_any))
  names(out) <- specs
  out
}

#' Aggregate per-respondent shares into one exhibit cell with gate metadata
#'
#' @param res A `cc_cell_exact()` result.
#' @param idx Logical or integer index of the respondents in this cell.
#' @param floor Scalar floor used for the column gate, or NA.
#' @return A one-row data frame.
.cc_aggregate_cell <- function(res, idx, floor = NA_real_) {
  sh <- res$share[idx]
  sens <- res$sensitivity[idx]
  mas <- res$max_abs_slope[idx]
  bmin <- res$binding_min_slope[idx]
  below <- if (is.na(floor)) rep(NA, length(sh)) else
    !is.na(bmin) & bmin < floor
  data.frame(
    n_respondents = length(sh),
    share = mean(sh),
    ## Column gate (memo, Uncertainty (b)): largest support-line slope magnitude.
    max_abs_slope = if (length(mas)) max(mas) else NA_real_,
    ## Same quantity per respondent, minimised: exposes the gate's one-sidedness.
    min_respondent_max_slope = if (length(mas)) min(mas) else NA_real_,
    floor = floor,
    floored = if (is.na(floor) || !length(mas)) NA else max(mas) < floor,
    ## Exact cell sensitivity to a common level shift of h: the mean of the
    ## per-respondent sum_j phi(e_j)/|gamma_(j)|.
    sensitivity = if (length(sens)) mean(sens) else NA_real_,
    sensitivity_max = if (length(sens)) max(sens) else NA_real_,
    n_binding_below_floor = if (all(is.na(below))) NA_integer_ else
      as.integer(sum(below, na.rm = TRUE)),
    frac_binding_below_floor = if (all(is.na(below))) NA_real_ else
      mean(below, na.rm = TRUE),
    ## Root-at-kink collisions (only one-sided derivatives exist there) and
    ## identically-zero pieces (the tie convention carries real mass).
    n_endpoint_tied = as.integer(sum(res$tied[idx])),
    n_zero_line = as.integer(sum(res$zero_line[idx])),
    stringsAsFactors = FALSE)
}

#' Build the full compensator-column table from respondent-level inputs
#'
#' Fit-free core: everything downstream of (mu, A, selectors, groups) lives
#' here, so the engine is testable without a fit.
#'
#' @param mu n x p matrix of respondent means (raw contrast scale).
#' @param A Loading; see `.cc_normalize_loading`.
#' @param actions Named list of action selectors (`c_p`), one per undemocratic
#'   action; names become the exhibit's row labels.
#' @param sel_template Selector list from `cc_selectors()`; its `action` entry
#'   is replaced per action.
#' @param group Factor of ideology groups, length n, or NULL for overall only.
#'   `cc_ideology_group()` produces the archived partition.
#' @param floors Scalar floor, a named per-spec vector, or NA.
#' @param none_bounds Certified lower bounds for the None cell, from the bound
#'   memo. Either a scalar applied to every cell, or a data frame with columns
#'   `action`, `group`, `bound`. Floored `pol`, `gov`, and `any` cells inherit
#'   it by pointwise domination; floored `party` cells do NOT (its benefit takes
#'   both signs) and are withheld.
#' @return A data frame, one row per (action, group, column), with `share` (the
#'   engine value, pre-release, used by the domination check), `released_value`,
#'   `release_kind`, and the gate metadata. The per-respondent detail is
#'   attached as `attr(x, "respondent")`.
cc_columns <- function(mu, A, actions, sel_template, group = NULL,
                       specs = cc_specs(), policy_steps = 3,
                       coparty_amount = 1, floors = NA_real_,
                       none_bounds = NULL, loading_form = "auto",
                       verify_any = TRUE) {
  mu <- as.matrix(mu); n <- nrow(mu)
  if (is.null(group)) {
    grp <- factor(rep("Overall", n), levels = "Overall")
    groups <- "Overall"
  } else {
    grp <- as.factor(group)
    if (length(grp) != n) stop("`group` must have length n = ", n, ".",
                               call. = FALSE)
    groups <- c("Overall", levels(grp))
  }
  labels <- cc_column_labels()
  bound_for <- function(act, gp) {
    if (is.null(none_bounds)) return(NA_real_)
    if (is.data.frame(none_bounds)) {
      hit <- none_bounds$action == act & as.character(none_bounds$group) == gp
      if (!any(hit)) return(NA_real_)
      return(as.numeric(none_bounds$bound[which(hit)[1L]]))
    }
    as.numeric(none_bounds)[1L]
  }
  rows <- list(); detail <- list()
  for (act_name in names(actions)) {
    sel <- sel_template
    sel$action <- .cc_as_contrast(actions[[act_name]], sel$attr_names,
                                  paste0("action '", act_name, "'"))
    res <- cc_respondent_shares(mu, A, sel, specs, policy_steps,
                                coparty_amount, floors, loading_form,
                                verify_any)
    detail[[act_name]] <- res
    for (sp in specs) {
      floor_sp <- if (length(floors) == 1L) as.numeric(floors) else
        if (!is.null(names(floors)) && sp %in% names(floors))
          as.numeric(floors[[sp]]) else NA_real_
      for (gp in groups) {
        idx <- if (gp == "Overall") seq_len(n) else which(as.character(grp) == gp)
        if (!length(idx)) next
        cell <- .cc_aggregate_cell(res[[sp]], idx, floor_sp)
        b <- bound_for(act_name, gp)
        floored <- isTRUE(cell$floored)
        inherits_ok <- sp %in% c("none", "pol", "gov", "any")
        if (!floored) {
          rel <- cell$share; kind <- "point"
        } else if (inherits_ok && is.finite(b)) {
          rel <- b; kind <- "lower_bound"
        } else {
          rel <- NA_real_
          kind <- if (!inherits_ok) "withheld (no domination inheritance)" else
            "withheld (no None bound supplied)"
          warning("Floored cell withheld: action '", act_name, "', group '", gp,
                  "', column '", labels[[sp]], "'.", call. = FALSE)
        }
        rows[[length(rows) + 1L]] <- cbind(
          data.frame(action = act_name, group = gp,
                     column = unname(labels[[sp]]), spec = sp,
                     stringsAsFactors = FALSE),
          cell,
          data.frame(inherited_bound = b, released_value = rel,
                     release_kind = kind, stringsAsFactors = FALSE))
      }
    }
  }
  out <- do.call(rbind, rows)
  out$column <- factor(out$column, levels = unname(labels[specs]))
  rownames(out) <- NULL
  attr(out, "respondent") <- detail
  attr(out, "policy_steps") <- policy_steps
  attr(out, "coparty_amount") <- coparty_amount
  out
}

#' Check the domination theorems on engine values (memo check iii)
#'
#' Full policy >= None, Gov feature >= None, Any >= each of Co-party, Full
#' policy, Gov feature, None. Compares ENGINE values, before the release rules:
#' under the v2.1 fits the printed None cell is a bound, not the engine value.
#' No gate is applied to Co-party, whose benefit takes both signs.
#'
#' @return A data frame of violations (empty when the theorems hold).
cc_check_domination <- function(tab, tol = 1e-12) {
  pairs <- list(c("none", "pol"), c("none", "gov"), c("none", "any"),
                c("party", "any"), c("pol", "any"), c("gov", "any"))
  bad <- list()
  key <- paste(tab$action, tab$group, sep = "\r")
  for (pr in pairs) {
    lo <- tab[tab$spec == pr[1L], ]; hi <- tab[tab$spec == pr[2L], ]
    if (!nrow(lo) || !nrow(hi)) next
    m <- match(paste(lo$action, lo$group, sep = "\r"),
               paste(hi$action, hi$group, sep = "\r"))
    ok <- !is.na(m)
    d <- lo$share[ok] - hi$share[m[ok]]
    viol <- which(d > tol)
    if (length(viol)) {
      bad[[length(bad) + 1L]] <- data.frame(
        action = lo$action[ok][viol], group = lo$group[ok][viol],
        lower = pr[1L], upper = pr[2L],
        lower_share = lo$share[ok][viol], upper_share = hi$share[m[ok]][viol],
        excess = d[viol], stringsAsFactors = FALSE)
    }
  }
  if (!length(bad)) {
    return(data.frame(action = character(0), group = character(0),
                      lower = character(0), upper = character(0),
                      lower_share = numeric(0), upper_share = numeric(0),
                      excess = numeric(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, bad)
}


## ---------------------------------------------------------------------------
## Fit-facing entry points
## ---------------------------------------------------------------------------

#' Extract respondent-level means and raw-scale loadings from an assembled fit
#'
#' Conventions copied from `share_bounds.R`:
#'   - raw-scale loading  A_folds[[k]] / sd_dx_folds[[k]]   (sb_raw_A_folds)
#'   - respondent means   mu_hat[!duplicated(respondent_id), ] (sb_respondent_means)
#'
#' Judgment call, recorded: each respondent is paired with the loading of the
#' fold in which that respondent was HELD OUT, matching how `mu_hat` itself is
#' assembled (out-of-fold rows). Fold-averaging the loading is inadmissible at
#' q = 1 -- the loading's sign is fold-local, and this engine reads the RELATIVE
#' signs of the loading entries across coordinates (the memo's "on the
#' corresponding loading entries up to a common sign"), which
#' `sb_fitted_dispersion()`'s per-coordinate magnitude average destroys. Because
#' every cell is invariant to A -> -A respondent by respondent (check iv), the
#' fold-local signs are harmless.
#'
#' @param assembled An `scmix_nested_assembled` fit.
#' @return A list with `mu` (n_resp x p), `A` (n_resp x p, q = 1), `respondents`,
#'   `fold`, `attr_names`, `q`, `K`.
cc_fit_inputs <- function(assembled) {
  if (!inherits(assembled, "scmix_nested_assembled")) {
    warning("`assembled` is not an scmix_nested_assembled object; proceeding ",
            "on its fields.", call. = FALSE)
  }
  need <- c("mu_hat", "A_folds", "sd_dx_folds", "respondent_id", "fold_id", "K")
  miss <- need[!need %in% names(assembled)]
  if (length(miss)) {
    stop("Assembled fit is missing: ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  rid <- as.character(assembled$respondent_id)
  keep <- !duplicated(rid)
  mu <- as.matrix(assembled$mu_hat)[keep, , drop = FALSE]
  fold <- as.integer(assembled$fold_id)[keep]
  p <- ncol(mu)
  A_raw <- lapply(seq_len(assembled$K), function(k) {
    Ak <- as.matrix(assembled$A_folds[[k]])
    Ak / as.numeric(assembled$sd_dx_folds[[k]])
  })
  q <- ncol(A_raw[[1L]])
  attr_names <- assembled$attr_names
  if (is.null(attr_names)) attr_names <- colnames(mu)
  if (is.null(attr_names)) attr_names <- paste0("b", seq_len(p))
  out <- list(mu = mu, respondents = rid[keep], fold = fold,
              attr_names = as.character(attr_names), q = q,
              K = as.integer(assembled$K), A_raw_folds = A_raw)
  if (q == 1L) {
    out$A <- t(vapply(fold, function(k) as.numeric(A_raw[[k]][, 1L]),
                      numeric(p)))
  } else {
    out$A <- lapply(fold, function(k) A_raw[[k]])
  }
  out
}

#' Compensator columns for an assembled fit (the exhibit entry point)
#'
#' Memo steps 1--6 at q = 1 (exact), step 7 at q > 1 (seeded Monte Carlo).
#'
#' @param assembled An `scmix_nested_assembled` fit.
#' @param actions Named list/vector of the seven undemocratic-action
#'   coordinates. Names become row labels.
#' @param coparty,econ,social Coordinate names/indices/vectors.
#' @param governance The six good-governance coordinates.
#' @param ideology Seven-point ideology, either respondent-level (in the order
#'   of `cc_fit_inputs()$respondents`) or task-level (collapsed the same way).
#'   NULL gives the overall table only.
#' @param floors Scalar dispersion floor on the raw contrast scale, a per-spec
#'   named vector, or NA. See `cc_reduce_floor()` for turning the bound memo's
#'   per-coordinate floor into one scalar.
#' @param none_bounds Certified lower bounds for the None cells (scalar or a
#'   data frame with `action`, `group`, `bound`).
#' @param assert_units If TRUE (default) run the memo's raw-ordinal-units
#'   assertion on the fit's `deltaX` policy columns.
#' @return The `cc_columns()` data frame, with `attr(x, "inputs")` carrying the
#'   extracted respondent-level means, loadings, and fold map.
cc_compensator_columns <- function(assembled, actions, coparty, econ, social,
                                   governance, ideology = NULL,
                                   policy_steps = 3, coparty_amount = 1,
                                   floors = NA_real_, none_bounds = NULL,
                                   specs = cc_specs(), assert_units = TRUE,
                                   mc_M = NULL, mc_seed = 20260827L) {
  inp <- cc_fit_inputs(assembled)
  if (isTRUE(assert_units) && !is.null(assembled$deltaX)) {
    dx <- as.matrix(assembled$deltaX)
    if (is.null(colnames(dx))) colnames(dx) <- inp$attr_names
    e_nm <- if (is.character(econ)) econ else inp$attr_names[econ]
    s_nm <- if (is.character(social)) social else inp$attr_names[social]
    if (is.character(e_nm) && is.character(s_nm) &&
        all(c(e_nm, s_nm) %in% colnames(dx))) {
      cc_assert_policy_units(dx, e_nm, s_nm, policy_steps)
    } else {
      warning("Could not locate the policy columns in `deltaX`; the raw ",
              "ordinal-units assertion was skipped.", call. = FALSE)
    }
  }
  if (inp$q != 1L) {
    stop("This fit has q = ", inp$q, ". The exact path applies only at q = 1; ",
         "use cc_cell_mc() per cell for the seeded Monte Carlo fallback ",
         "(memo step 7). Recommended M = ",
         if (is.null(mc_M)) 250000L else mc_M, ", seed = ", mc_seed, ".",
         call. = FALSE)
  }
  acts <- if (is.list(actions)) actions else
    as.list(stats::setNames(actions, as.character(actions)))
  if (is.null(names(acts)) || any(!nzchar(names(acts)))) {
    stop("`actions` must be named; the names become the exhibit row labels.",
         call. = FALSE)
  }
  sel <- cc_selectors(inp$attr_names, action = acts[[1L]],
                      coparty = coparty, econ = econ, social = social,
                      governance = governance)
  grp <- NULL
  if (!is.null(ideology)) {
    ideo <- as.numeric(ideology)
    if (length(ideo) == nrow(inp$mu)) {
      grp <- cc_ideology_group(ideo)
    } else if (length(ideo) == length(assembled$respondent_id)) {
      grp <- cc_ideology_group(ideo[!duplicated(as.character(assembled$respondent_id))])
    } else {
      stop("`ideology` must be respondent-level or task-level.", call. = FALSE)
    }
  }
  out <- cc_columns(inp$mu, inp$A, acts, sel, group = grp, specs = specs,
                    policy_steps = policy_steps,
                    coparty_amount = coparty_amount, floors = floors,
                    none_bounds = none_bounds, loading_form = "rows")
  attr(out, "inputs") <- inp[c("respondents", "fold", "attr_names", "q", "K")]
  out
}

#' Closed-form C_0 for a linear benefit (memo check i, capstone)
#'
#' `Phi((c_p + a c_b)'mu / s)` with `s = sqrt(c'Sigma c)`, Sigma = A A'. Agrees
#' with the piecewise engine to order 1e-13, not bitwise: the closed form
#' evaluates sqrt(c'Sigma c) where the engine uses |c'A|, and the two round
#' differently in the last bits.
cc_closed_form_linear <- function(mu, A, c_vec, loading_form = "auto") {
  mu <- as.matrix(mu); n <- nrow(mu); p <- ncol(mu)
  ld <- .cc_normalize_loading(A, n, p, loading_form)
  if (ld$q != 1L) stop("Closed form requires q = 1.", call. = FALSE)
  m <- as.numeric(mu %*% c_vec)
  ## Deliberately the Sigma route, not |c'A|: form Sigma = A A' and evaluate
  ## sqrt(c' Sigma c). Algebraically identical at q = 1, different in the last
  ## bits -- which is exactly what the capstone's 1e-13 tolerance covers.
  s <- numeric(n)
  key <- apply(ld$Aq, 1L, function(z) paste(sprintf("%.17g", z), collapse = "|"))
  for (k in unique(key)) {
    i0 <- which(key == k)[1L]
    a <- as.numeric(ld$Aq[i0, ])
    Sigma <- tcrossprod(a)
    s[key == k] <- sqrt(max(as.numeric(crossprod(c_vec, Sigma %*% c_vec)), 0))
  }
  out <- numeric(n)
  pos <- s > 0
  out[pos] <- stats::pnorm(m[pos] / s[pos])
  out[!pos] <- as.numeric(m[!pos] >= 0)
  out
}
