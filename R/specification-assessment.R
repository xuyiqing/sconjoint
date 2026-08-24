## Specification-assessment tools for paperps.tex.
##
## These diagnostics can reveal lack of support, poor fit, weak information,
## numerical instability, and sensitivity.  They cannot verify normality,
## common covariance, noninformative completion, or independent logit shocks.

.pps_assessment_disclaimer <- paste(
  "Specification assessments can reveal poor fit, weak information,",
  "numerical instability, and sensitivity; they cannot verify normality,",
  "common residual covariance, noninformative completion, or independent",
  "logit shocks."
)

.pps_match_contrast <- function(deltaX, d, tol = 1e-8) {
  rowSums(abs(sweep(deltaX, 2L, d, `-`))) <= tol
}

.pps_protocol_vector <- function(x, respondent_id) {
  ids <- unique(as.character(respondent_id))
  key <- as.character(respondent_id)
  N <- length(ids)
  if (is.function(x)) x <- x(ids)
  if (!is.numeric(x) || any(!is.finite(x))) {
    stop("`protocol_probability` must be finite numeric values or a function ",
         "returning them.", call. = FALSE)
  }
  if (!is.null(names(x))) {
    if (anyDuplicated(names(x))) {
      stop("Named protocol probabilities may not have duplicate respondent ids.",
           call. = FALSE)
    }
    if (!all(ids %in% names(x))) {
      stop("Named protocol probabilities must cover every respondent id.",
           call. = FALSE)
    }
    x <- unname(x[ids])
  } else if (length(x) == 1L) {
    x <- rep(x, N)
  } else if (length(x) == length(key)) {
    split_x <- split(x, factor(key, levels = ids))
    if (any(vapply(split_x, function(v) max(v) - min(v) > 1e-12,
                   logical(1L)))) {
      stop("Task-row protocol probabilities must be constant within respondent.",
           call. = FALSE)
    }
    x <- vapply(split_x, `[`, numeric(1L), 1L)
  } else if (length(x) != N) {
    stop("`protocol_probability` must have length 1, N respondents, or the ",
         "number of task rows.", call. = FALSE)
  }
  if (any(x <= 0 | x > 1)) {
    stop("Every protocol probability must lie in (0, 1].",
         call. = FALSE)
  }
  stats::setNames(as.numeric(x), ids)
}

#' Protocol-weighted design benchmark for an on-support ordered contrast
#'
#' Implements the respondent Horvitz--Thompson contribution in the paper.
#' Respondents with no realized matching task are retained with a zero
#' numerator. `protocol_probability` is the known conditional expectation of
#' the respondent's matching-task fraction, including all assignment and
#' completion strata used by the protocol.
#'
#' @param deltaX Task-row contrast matrix.
#' @param y Task-row binary choices for alternative 1.
#' @param respondent_id Task-row respondent identifiers.
#' @param contrast Ordered contrast `d`; `-d` is not silently pooled with it.
#' @param protocol_probability Known `bar(pi)_i(d)`, as one value, one per
#'   respondent, one constant-within-respondent value per task, or a function
#'   of respondent ids.
#' @param tol Matching tolerance.
#' @return An internal object containing the estimate, respondent influence
#'   contributions, standard error, match counts, and protocol probabilities.
#' @export
scmix_design_benchmark <- function(deltaX, y, respondent_id, contrast,
                                   protocol_probability, tol = 1e-8) {
  deltaX <- as.matrix(deltaX)
  if (!is.numeric(deltaX) || any(!is.finite(deltaX))) {
    stop("`deltaX` must be a finite numeric matrix.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("`tol` must be one finite positive number.", call. = FALSE)
  }
  n <- nrow(deltaX)
  if (length(y) != n || length(respondent_id) != n ||
      anyNA(y) || anyNA(respondent_id) || !all(y %in% c(0, 1))) {
    stop("`y` and `respondent_id` must match the task rows, and `y` must be ",
         "binary.", call. = FALSE)
  }
  d <- .pps_contrast(contrast, ncol(deltaX), colnames(deltaX))
  if (!is.null(colnames(deltaX))) names(d) <- colnames(deltaX)
  key <- as.character(respondent_id)
  ids <- unique(key)
  r <- match(key, ids)
  N <- length(ids)
  T_i <- tabulate(r, nbins = N)
  hit <- .pps_match_contrast(deltaX, d, tol)
  numerator <- rowsum(as.numeric(hit) * as.numeric(y) / T_i[r], r,
                      reorder = FALSE)[, 1L]
  matches <- rowsum(as.numeric(hit), r, reorder = FALSE)[, 1L]
  pi_i <- .pps_protocol_vector(protocol_probability, respondent_id)
  U <- numerator / unname(pi_i)
  estimate <- mean(U)
  se <- if (N > 1L) {
    sqrt(sum((U - estimate)^2) / (N * (N - 1L)))
  } else NA_real_
  respondent <- data.frame(
    respondent_id = ids, T_i = T_i, matches = as.integer(matches),
    numerator = numerator, protocol_probability = unname(pi_i), U = U,
    influence = U - estimate, stringsAsFactors = FALSE
  )
  out <- list(estimate = estimate, se = se, contrast = d,
              respondent = respondent, n_respondents = N,
              n_matching_tasks = sum(hit), on_support = TRUE,
              estimand = "on-support ordered-contrast choice probability",
              disclaimer = paste(
                "This is respondent-superpopulation inference.",
                "Its validity depends on the supplied protocol probabilities",
                "and the randomized-assignment/noninformative-completion conditions."
              ))
  class(out) <- c("scmix_design_benchmark", "list")
  out
}

#' Position-neutral design benchmark
#'
#' Combines separately estimated benchmarks for `d` and `-d` as
#' `0.5 * {V(d) + 1 - V(-d)}` while retaining their respondent covariance.
#'
#' @param positive,negative Benchmark objects for `d` and `-d`.
#' @return An internal `scmix_design_benchmark` object.
#' @export
scmix_design_benchmark_neutral <- function(positive, negative) {
  if (!inherits(positive, "scmix_design_benchmark") ||
      !inherits(negative, "scmix_design_benchmark")) {
    stop("Both inputs must be design benchmark objects.", call. = FALSE)
  }
  id1 <- positive$respondent$respondent_id
  id2 <- negative$respondent$respondent_id
  if (!setequal(id1, id2)) {
    stop("The two benchmarks must cover the same respondents.", call. = FALSE)
  }
  d_pos <- as.numeric(positive$contrast)
  d_neg <- as.numeric(negative$contrast)
  names_ok <- (is.null(names(positive$contrast)) &&
                 is.null(names(negative$contrast))) ||
    identical(names(positive$contrast), names(negative$contrast))
  if (!length(d_pos) || length(d_pos) != length(d_neg) ||
      any(!is.finite(d_pos)) || any(!is.finite(d_neg)) ||
      !names_ok || !all(d_neg == -d_pos)) {
    stop("`negative` must use exactly the negative of `positive`'s ordered contrast.",
         call. = FALSE)
  }
  neg <- negative$respondent[match(id1, id2), , drop = FALSE]
  U <- 0.5 * (positive$respondent$U + 1 - neg$U)
  estimate <- mean(U)
  N <- length(U)
  se <- if (N > 1L) stats::sd(U) / sqrt(N) else NA_real_
  respondent <- positive$respondent
  respondent$U <- U
  respondent$influence <- U - estimate
  respondent$matches_negative <- neg$matches
  out <- list(estimate = estimate, se = se,
              contrast = positive$contrast,
              respondent = respondent, n_respondents = N,
              n_matching_tasks = positive$n_matching_tasks +
                negative$n_matching_tasks,
              on_support = TRUE,
              estimand = "position-neutral on-support choice probability",
              disclaimer = positive$disclaimer)
  class(out) <- c("scmix_design_benchmark", "list")
  out
}

#' Joint structural--design discrepancy
#'
#' Subtracts a design benchmark from a structural one-step estimate. When the
#' structural respondent influence contributions are supplied, uncertainty is
#' computed from the respondent-by-respondent difference, preserving their
#' covariance. It never treats the estimates as independent.
#'
#' @param structural_estimate Scalar structural estimate or a paper-aligned
#'   `scmix_dml` result.
#' @param design A `scmix_design_benchmark` object.
#' @param structural_influence Optional centered influence contribution per
#'   respondent, named by respondent id or ordered as in `design`.
#' @param target_label Required when `structural_estimate` is a `scmix_dml`
#'   result; identifies the structural target and its confidence interval.
#' @param structural_contrast Ordered contrast used by the structural target.
#'   When supplied it must equal the design benchmark's contrast.
#' @param require_verified If true, a `scmix_dml` result contributes joint
#'   inference only when its global status is `"available"` or
#'   `"conditional_available"`. The latter remains explicitly conditional on
#'   documented high-level assumptions. In every case, the selected target must
#'   itself be inference-available with a finite, strictly positive standard
#'   error; otherwise the discrepancy is descriptive.
#' @return An internal discrepancy object; without structural influence the
#'   comparison is explicitly descriptive.
#' @export
scmix_structural_design_discrepancy <- function(structural_estimate, design,
                                                structural_influence = NULL,
                                                target_label = NULL,
                                                structural_contrast = NULL,
                                                require_verified = TRUE) {
  if (!inherits(design, "scmix_design_benchmark")) {
    stop("`design` must be a design benchmark object.", call. = FALSE)
  }
  dml_source <- inherits(structural_estimate, "scmix_dml")
  inference_status <- "manual scalar"
  structural_inference_claim <- "not applicable"
  structural_target_inference_available <- NA
  if (dml_source) {
    if (!is.null(structural_influence)) {
      stop("Do not supply `structural_influence` when passing a `scmix_dml` ",
           "object; its respondent influence vector is used directly.",
           call. = FALSE)
    }
    if (is.null(target_label) || length(target_label) != 1L ||
        !target_label %in% names(structural_estimate$estimate)) {
      stop("`target_label` must select one target from the `scmix_dml` result.",
           call. = FALSE)
    }
    if (is.null(structural_estimate$influence) ||
        !target_label %in% colnames(structural_estimate$influence)) {
      stop("The selected DML target has no respondent influence contribution.",
           call. = FALSE)
    }
    influence_ids <- rownames(structural_estimate$influence)
    if (is.null(influence_ids) || anyNA(influence_ids) ||
        anyDuplicated(influence_ids)) {
      stop("The DML influence matrix must carry unique respondent row names.",
           call. = FALSE)
    }
    structural_influence <- structural_estimate$influence[, target_label]
    names(structural_influence) <- influence_ids
    inference_status <- structural_estimate$status %||% "unknown"
    structural_inference_claim <- if (identical(inference_status,
                                                "conditional_available")) {
      "conditional_on_documented_high_level_assumptions"
    } else {
      structural_estimate$inference_claim %||%
        "available_under_stated_regular_inference_conditions"
    }
    target_flags <- structural_estimate$target_inference_available
    target_se <- structural_estimate$se
    structural_target_inference_available <-
      !is.null(target_flags) && !is.null(names(target_flags)) &&
      target_label %in% names(target_flags) && isTRUE(target_flags[[target_label]]) &&
      !is.null(target_se) && !is.null(names(target_se)) &&
      target_label %in% names(target_se) &&
      is.finite(target_se[[target_label]]) && target_se[[target_label]] > 0
    global_inference_available <- isTRUE(structural_estimate$inference_available) &&
      (identical(inference_status, "available") ||
         identical(inference_status, "conditional_available"))
    if (!isTRUE(structural_target_inference_available) ||
        (isTRUE(require_verified) && !global_inference_available)) {
      structural_influence <- NULL
    }
    structural_estimate <- unname(structural_estimate$estimate[target_label])
  }
  if (!is.numeric(structural_estimate) || length(structural_estimate) != 1L ||
      !is.finite(structural_estimate)) {
    stop("`structural_estimate` must resolve to one finite number.",
         call. = FALSE)
  }
  if (!is.null(structural_contrast)) {
    design_names <- names(design$contrast)
    if (!is.null(names(structural_contrast)) && is.null(design_names)) {
      if (length(structural_contrast) != length(design$contrast)) {
        stop("A named structural contrast can be matched to an unnamed legacy design benchmark only when it supplies every coefficient in benchmark order.",
             call. = FALSE)
      }
      d <- as.numeric(structural_contrast)
    } else {
      d <- .pps_contrast(structural_contrast, length(design$contrast),
                         design_names)
    }
    if (max(abs(d - as.numeric(design$contrast))) > 1e-10) {
      stop("The structural and design targets use different ordered contrasts.",
           call. = FALSE)
    }
  } else if (isTRUE(require_verified) && !is.null(structural_influence)) {
    ## A covariance calculation is meaningful only after verifying that both
    ## estimators target the same ordered contrast. Keep the point discrepancy
    ## but fail closed on ordinary joint inference.
    structural_influence <- NULL
  }
  delta <- structural_estimate - design$estimate
  ids <- design$respondent$respondent_id
  diff_if <- NULL
  se <- NA_real_
  if (!is.null(structural_influence)) {
    if (!is.numeric(structural_influence) || any(!is.finite(structural_influence))) {
      stop("`structural_influence` must be finite and numeric.", call. = FALSE)
    }
    if (!is.null(names(structural_influence))) {
      if (!all(ids %in% names(structural_influence))) {
        stop("Named structural influence values must cover every design ",
             "respondent.", call. = FALSE)
      }
      structural_influence <- unname(structural_influence[ids])
    }
    if (length(structural_influence) != length(ids)) {
      stop("`structural_influence` must have one value per respondent.",
           call. = FALSE)
    }
    structural_influence <- structural_influence - mean(structural_influence)
    diff_if <- structural_influence - design$respondent$influence
    diff_if <- diff_if - mean(diff_if)
    se <- if (length(diff_if) > 1L) stats::sd(diff_if) / sqrt(length(diff_if)) else NA_real_
    names(diff_if) <- ids
  }
  if (dml_source && is.null(diff_if)) {
    structural_inference_claim <- "not_available_for_selected_discrepancy"
  }
  out <- list(estimate = delta, se = se, structural = structural_estimate,
              design = design$estimate, influence = diff_if,
              inference = if (is.null(diff_if)) "descriptive only" else
                "joint respondent influence",
              target_label = target_label,
              structural_source = if (dml_source) "scmix_dml" else
                "manual scalar",
              structural_contrast = structural_contrast,
              structural_inference_status = inference_status,
              structural_inference_claim = structural_inference_claim,
              structural_target_inference_available =
                structural_target_inference_available,
              same_target_verified = !is.null(structural_contrast),
              disclaimer = paste(
                "Agreement does not verify the maintained structural assumptions;",
                "disagreement on randomized support is evidence against the fitted specification."
              ))
  class(out) <- c("scmix_structural_design_discrepancy", "list")
  out
}

.pps_matrix_rank <- function(x, tol = NULL) {
  s <- svd(as.matrix(x), nu = 0L, nv = 0L)$d
  if (!length(s)) return(0L)
  cutoff <- if (is.null(tol)) {
    max(dim(x)) * max(s) * .Machine$double.eps^0.5
  } else {
    if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
      stop("`tol` must be one finite positive relative tolerance.", call. = FALSE)
    }
    tol * max(s)
  }
  sum(s > cutoff)
}

.pps_vech <- function(x) x[lower.tri(x, diag = TRUE)]

.pps_protocol_audit <- function(protocol_support, ids,
                                expected_strata = NULL) {
  if (is.null(protocol_support)) {
    return(list(established = NA, table = NULL,
                note = "protocol support was not supplied"))
  }
  if (is.null(expected_strata)) {
    stop("`protocol_strata` must explicitly enumerate the complete design/completion-stratum universe; it is not inferred from supplied support rows.",
         call. = FALSE)
  }
  if (is.matrix(protocol_support) && is.numeric(protocol_support)) {
    if (nrow(protocol_support) != length(ids)) {
      stop("A protocol-support matrix must have one row per contrast.",
           call. = FALSE)
    }
    tab <- data.frame(
      contrast_id = rep(ids, times = ncol(protocol_support)),
      stratum = rep(colnames(protocol_support) %||%
                      paste0("stratum_", seq_len(ncol(protocol_support))),
                    each = length(ids)),
      probability = as.numeric(protocol_support),
      event = "repeated_ordered_contrast", stringsAsFactors = FALSE
    )
  } else if (is.data.frame(protocol_support)) {
    req <- c("contrast_id", "stratum", "event", "probability")
    if (!all(req %in% names(protocol_support))) {
      stop("A protocol-support data frame needs contrast_id, stratum, event, ",
           "and probability columns.",
           call. = FALSE)
    }
    tab <- protocol_support
  } else {
    stop("`protocol_support` must be a numeric matrix or data frame.",
         call. = FALSE)
  }
  tab$contrast_id <- as.character(tab$contrast_id)
  if (!is.numeric(tab$probability) || any(!is.finite(tab$probability)) ||
      any(tab$probability <= 0 | tab$probability > 1)) {
    stop("Protocol-support probabilities must lie in (0, 1].",
         call. = FALSE)
  }
  if (anyNA(tab$stratum) || anyNA(tab$event) ||
      any(as.character(tab$event) != "repeated_ordered_contrast")) {
    stop("Every protocol row must describe the repeated_ordered_contrast event ",
         "in a nonmissing design/completion stratum.", call. = FALSE)
  }
  tab$stratum <- as.character(tab$stratum)
  if (!length(expected_strata) || anyNA(expected_strata) ||
      anyDuplicated(as.character(expected_strata))) {
    stop("`expected_strata` must uniquely enumerate all required design and ",
         "completion strata.", call. = FALSE)
  }
  if (any(!tab$contrast_id %in% ids) ||
      any(!tab$stratum %in% as.character(expected_strata))) {
    stop("Protocol support contains a contrast or stratum outside the explicitly enumerated universe.",
         call. = FALSE)
  }
  expected <- expand.grid(contrast_id = ids,
                          stratum = as.character(expected_strata),
                          stringsAsFactors = FALSE)
  key <- paste(tab$contrast_id, tab$stratum, sep = "\r")
  if (anyDuplicated(key)) {
    stop("Protocol support has duplicate contrast-by-stratum rows.",
         call. = FALSE)
  }
  covered_key <- paste(expected$contrast_id, expected$stratum, sep = "\r")
  covered <- covered_key %in% key
  positive <- vapply(ids, function(id) {
    v <- tab$probability[tab$contrast_id == id]
    length(v) > 0L && all(v > 0)
  }, logical(1L))
  established <- all(covered) && all(positive)
  list(established = established, table = tab,
       expected_strata = as.character(expected_strata),
       event = "repeated_ordered_contrast",
       note = if (established)
         "protocol probabilities cover the repeated ordered-contrast event in every enumerated stratum" else
         "at least one required contrast-by-stratum repeated event is not covered")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Audit the paper's sufficient finite-design identification conditions
#'
#' Checks affine rank, the transparent unrestricted-vech sufficient condition
#' for covariance injectivity, user-supplied protocol support, and realized
#' first-two-task repeat counts. A failed or incomplete audit makes the current
#' theorem silent; it is never reported as proof of nonidentification.
#'
#' @param contrasts Matrix with one required ordered contrast per row.
#' @param q Maintained maximum covariance rank.
#' @param protocol_support Data frame with `contrast_id`, `stratum`, `event`,
#'   and known `probability`, or a contrast-by-stratum numeric matrix. The event
#'   must be `"repeated_ordered_contrast"`.
#' @param protocol_strata Complete prespecified list of design/completion strata
#'   that the protocol-support input must cover.
#' @param deltaX,respondent_id,task_order Optional realized task data used only
#'   for finite-sample repeat counts.
#' @param covariance_injective Optional classed verifier artifact for
#'   rank-constrained injectivity. It must inherit from
#'   `scmix_covariance_injectivity_certificate`, contain `established = TRUE`
#'   and `verified = TRUE`, record nonempty `method`, `domain`, and `provenance`
#'   fields, state a finite positive numeric `tolerance`, and contain the exact
#'   `q` and contrast matrix verified. An unclassed list or bare logical
#'   assertion is recorded but cannot establish the condition.
#' @param injectivity_certificate Legacy text field; text alone is not a
#'   certificate and cannot establish injectivity.
#' @param tol Positive relative singular-value tolerance for rank calculations
#'   and absolute tolerance for realized contrast matching.
#' @return An internal design-audit object.
#' @export
scmix_design_audit <- function(contrasts, q, protocol_support = NULL,
                               protocol_strata = NULL,
                               deltaX = NULL, respondent_id = NULL,
                               task_order = NULL,
                               covariance_injective = NA,
                               injectivity_certificate = NULL,
                               tol = 1e-8) {
  D <- as.matrix(contrasts)
  if (!is.numeric(D) || any(!is.finite(D)) || nrow(D) < 1L) {
    stop("`contrasts` must be a nonempty finite numeric matrix.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("`tol` must be one finite positive number.", call. = FALSE)
  }
  p <- ncol(D)
  if (!is.numeric(q) || length(q) != 1L || is.na(q) || !is.finite(q) ||
      q < 0L || q > max(0L, p - 1L) ||
      q != as.integer(q)) {
    stop("`q` must be an integer between zero and p - 1, as in the maintained model.",
         call. = FALSE)
  }
  ids <- rownames(D)
  if (is.null(ids) || any(ids == "") || anyDuplicated(ids)) {
    ids <- paste0("d", seq_len(nrow(D)))
  }
  affine_rank <- .pps_matrix_rank(cbind(1, D), tol = tol)
  affine_ok <- affine_rank == p + 1L
  V <- t(vapply(seq_len(nrow(D)), function(i) .pps_vech(tcrossprod(D[i, ])),
                numeric(p * (p + 1L) / 2L)))
  vech_rank <- .pps_matrix_rank(V, tol = tol)
  vech_ok <- vech_rank == ncol(V)
  required_certificate_text <- c("method", "domain", "provenance")
  certificate_contrasts <- if (is.list(covariance_injective))
    tryCatch(as.matrix(covariance_injective$contrasts),
             error = function(e) NULL) else NULL
  external_ok <- inherits(covariance_injective,
                          "scmix_covariance_injectivity_certificate") &&
    is.list(covariance_injective) &&
    isTRUE(covariance_injective$established) &&
    isTRUE(covariance_injective$verified) &&
    all(vapply(required_certificate_text, function(nm) {
      value <- covariance_injective[[nm]]
      !is.null(value) && length(value) == 1L && !is.na(value) &&
        nzchar(as.character(value))
    }, logical(1L))) &&
    is.numeric(covariance_injective$tolerance) &&
    length(covariance_injective$tolerance) == 1L &&
    is.finite(covariance_injective$tolerance) &&
    covariance_injective$tolerance > 0 &&
    is.numeric(covariance_injective$q) &&
    length(covariance_injective$q) == 1L &&
    !is.na(covariance_injective$q) &&
    is.finite(covariance_injective$q) &&
    covariance_injective$q == as.integer(q) &&
    is.numeric(certificate_contrasts) && identical(dim(certificate_contrasts),
                                                   dim(D)) &&
    all(is.finite(certificate_contrasts)) &&
    all(certificate_contrasts == D)
  external_asserted <- !is.null(injectivity_certificate) ||
    isTRUE(covariance_injective) ||
    (is.list(covariance_injective) && isTRUE(covariance_injective$established) &&
       !external_ok)
  covariance_ok <- if (q == 0L) TRUE else vech_ok || external_ok
  covariance_method <- if (q == 0L) "q=0 covariance fixed at zero" else
    if (vech_ok) "full unrestricted-vech rank" else
      if (external_ok) paste0("verified external certificate: ",
                              covariance_injective$method) else
        if (external_asserted) "user asserted; not independently certified" else
        "not established"
  protocol <- .pps_protocol_audit(protocol_support, ids, protocol_strata)

  realized <- stats::setNames(rep(NA_integer_, nrow(D)), ids)
  if (!is.null(deltaX) || !is.null(respondent_id) || !is.null(task_order)) {
    if (is.null(deltaX) || is.null(respondent_id) || is.null(task_order)) {
      stop("Supply `deltaX`, `respondent_id`, and `task_order` together.",
           call. = FALSE)
    }
    deltaX <- as.matrix(deltaX)
    if (!is.numeric(deltaX) || any(!is.finite(deltaX)) ||
        !identical(ncol(deltaX), p) ||
        nrow(deltaX) != length(respondent_id) ||
        length(task_order) != nrow(deltaX) || anyNA(respondent_id) ||
        anyNA(task_order) || !is.numeric(task_order) ||
        any(!is.finite(task_order))) {
      stop("Realized design inputs have incompatible dimensions.", call. = FALSE)
    }
    spl <- split(seq_len(nrow(deltaX)),
                 factor(as.character(respondent_id),
                        levels = unique(as.character(respondent_id))))
    duplicate_order <- vapply(spl, function(ii) anyDuplicated(task_order[ii]) > 0L,
                               logical(1L))
    if (any(duplicate_order)) {
      stop("`task_order` must be unique within every respondent.",
           call. = FALSE)
    }
    first_two <- lapply(spl, function(ii) {
      ii <- ii[order(task_order[ii])]
      if (length(ii) < 2L) integer() else ii[1:2]
    })
    for (ell in seq_len(nrow(D))) {
      realized[ell] <- sum(vapply(first_two, function(ii) {
        length(ii) == 2L && all(.pps_match_contrast(
          deltaX[ii, , drop = FALSE], D[ell, ], tol))
      }, logical(1L)))
    }
  }

  conditions <- c(protocol_support = isTRUE(protocol$established),
                  affine_rank = affine_ok,
                  covariance_injectivity = covariance_ok)
  established <- isTRUE(protocol$established) && affine_ok && covariance_ok
  status <- if (established) "sufficient conditions established" else
    "current theorem does not establish identification"
  out <- list(status = status, established = established, q = as.integer(q),
              contrast_ids = ids, contrasts = D,
              protocol = protocol, affine_rank = affine_rank,
              affine_required = p + 1L, vech_rank = vech_rank,
              vech_required = ncol(V), covariance_method = covariance_method,
              covariance_certificate = if (external_ok) covariance_injective else NULL,
              covariance_user_asserted = external_asserted,
              conditions = conditions, realized_repeat_counts = realized,
              disclaimer = paste(
                "Realized counts diagnose precision and do not replace protocol support.",
                "Failure of a sufficient condition leaves this theorem silent and is not proof of nonidentification."
              ))
  class(out) <- c("scmix_design_audit", "list")
  out
}

#' Held-out respondent-sequence log scores
#'
#' Accepts one already-integrated complete-sequence log likelihood per
#' respondent and model. Duplicate respondent ids are rejected so task-level
#' log scores cannot be silently substituted.
#'
#' @param loglik Numeric vector or respondent-by-model matrix.
#' @param respondent_id One unique id per row.
#' @param out_of_fold Whether every score was computed from a fit that excluded
#'   that respondent.
#' @param training_only_tuning Whether preprocessing and tuning for every fold
#'   used only its training respondents.
#' @param provenance Nonempty description of the constructor, fit, or file that
#'   produced the scores.
#' @param analysis_signature Optional nonempty signature linking a fit-aware
#'   score to the fitted analysis. The official prediction pipeline supplies it.
#' @return Means, respondent standard errors, and paired model differences.
#' @export
scmix_heldout_sequence_score <- function(loglik, respondent_id = NULL,
                                         out_of_fold = FALSE,
                                         training_only_tuning = FALSE,
                                         provenance = NULL,
                                         analysis_signature = NULL) {
  ll <- as.matrix(loglik)
  if (!is.numeric(ll) || any(!is.finite(ll)) || nrow(ll) < 2L) {
    stop("`loglik` must contain finite complete-sequence scores for at least ",
         "two respondents.", call. = FALSE)
  }
  if (is.null(colnames(ll))) colnames(ll) <- paste0("model_", seq_len(ncol(ll)))
  if (is.null(respondent_id)) respondent_id <- seq_len(nrow(ll))
  if (length(respondent_id) != nrow(ll) || anyNA(respondent_id) ||
      anyDuplicated(as.character(respondent_id))) {
    stop("There must be exactly one complete-sequence log score per unique ",
         "respondent; task-level scores are not accepted.", call. = FALSE)
  }
  estimate <- colMeans(ll)
  se <- apply(ll, 2L, stats::sd) / sqrt(nrow(ll))
  pairs <- if (ncol(ll) > 1L) {
    cmb <- utils::combn(seq_len(ncol(ll)), 2L)
    do.call(rbind, lapply(seq_len(ncol(cmb)), function(k) {
      d <- ll[, cmb[1L, k]] - ll[, cmb[2L, k]]
      data.frame(model_1 = colnames(ll)[cmb[1L, k]],
                 model_2 = colnames(ll)[cmb[2L, k]],
                 difference = mean(d), se = stats::sd(d) / sqrt(length(d)),
                 stringsAsFactors = FALSE)
    }))
  } else data.frame()
  verified <- isTRUE(out_of_fold) && isTRUE(training_only_tuning) &&
    !is.null(provenance) && length(provenance) == 1L &&
    !is.na(provenance) && nzchar(as.character(provenance))
  if (!is.null(analysis_signature) &&
      (!is.character(analysis_signature) || length(analysis_signature) != 1L ||
       is.na(analysis_signature) || !nzchar(analysis_signature))) {
    stop("`analysis_signature` must be NULL or one nonempty character value.",
         call. = FALSE)
  }
  out <- list(estimate = estimate, se = se, paired_differences = pairs,
              respondent_id = respondent_id, loglik = ll,
              unit = "complete respondent sequence",
              provenance = provenance,
              out_of_fold = isTRUE(out_of_fold),
              training_only_tuning = isTRUE(training_only_tuning),
              analysis_signature = analysis_signature,
              verified_heldout = verified,
              status = if (verified) "verified held-out construction" else
                "user-supplied scores; held-out provenance not established",
              disclaimer = .pps_assessment_disclaimer)
  class(out) <- c("scmix_heldout_score", "list")
  out
}

.pps_calibration_table <- function(observed, predicted, respondent_id,
                                   stratum, type) {
  lev <- unique(as.character(stratum))
  rows <- lapply(lev, function(g) {
    take <- !is.na(stratum) & as.character(stratum) == g
    ids <- unique(as.character(respondent_id[take]))
    ri <- match(as.character(respondent_id[take]), ids)
    obs_i <- rowsum(observed[take], ri, reorder = FALSE)[, 1L] /
      tabulate(ri, length(ids))
    pred_i <- rowsum(predicted[take], ri, reorder = FALSE)[, 1L] /
      tabulate(ri, length(ids))
    gap_i <- obs_i - pred_i
    data.frame(type = type, stratum = g, observed = mean(obs_i),
               predicted = mean(pred_i), gap = mean(gap_i),
               se_gap = if (length(gap_i) > 1L)
                 stats::sd(gap_i) / sqrt(length(gap_i)) else NA_real_,
               n_respondents = length(ids), n_rows = sum(take),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Held-out marginal and joint calibration diagnostics
#'
#' Every summary averages within respondent before averaging across
#' respondents. Optional `joint` rows allow response-pattern, transition, or
#' repeated-contrast probabilities to be assessed without pretending they can
#' be reconstructed from marginal predictions alone.
#'
#' @param y,predicted Held-out task outcomes and marginal probabilities.
#' @param respondent_id Task-row respondent ids.
#' @param probability_breaks Prespecified bin boundaries from zero to one.
#' @param design_cell,respondent_group,task_order Optional task-row strata.
#' @param joint Optional data frame with `respondent_id`, `type`, `stratum`,
#'   `observed`, and `predicted` columns for genuine joint checks.
#' @param out_of_fold,training_only_tuning Logical provenance gates.
#' @param provenance Nonempty description of how predictions were constructed.
#' @param analysis_signature Optional nonempty signature linking fit-aware
#'   calibration to the fitted analysis. The official prediction pipeline
#'   supplies it.
#' @return An internal calibration object.
#' @export
scmix_heldout_calibration <- function(y, predicted, respondent_id,
                                      probability_breaks = seq(0, 1, by = 0.1),
                                      design_cell = NULL,
                                      respondent_group = NULL,
                                      task_order = NULL,
                                      joint = NULL,
                                      out_of_fold = FALSE,
                                      training_only_tuning = FALSE,
                                      provenance = NULL,
                                      analysis_signature = NULL) {
  n <- length(y)
  if (!is.numeric(predicted) || length(predicted) != n ||
      length(respondent_id) != n || anyNA(respondent_id) ||
      anyNA(y) || !all(y %in% c(0, 1)) ||
      any(!is.finite(predicted)) || any(predicted < 0 | predicted > 1)) {
    stop("Held-out outcomes, probabilities, and respondent ids are invalid.",
         call. = FALSE)
  }
  if (!is.numeric(probability_breaks) || any(!is.finite(probability_breaks)) ||
      length(probability_breaks) < 2L || any(diff(probability_breaks) <= 0) ||
      probability_breaks[1L] != 0 || tail(probability_breaks, 1L) != 1) {
    stop("`probability_breaks` must be strictly increasing from zero to one.",
         call. = FALSE)
  }
  br <- probability_breaks
  bins <- cut(predicted, br, include.lowest = TRUE, right = TRUE)
  tables <- list(.pps_calibration_table(y, predicted, respondent_id, bins,
                                        "probability_bin"))
  add <- function(x, nm) {
    if (is.null(x)) return(NULL)
    if (length(x) != n || anyNA(x)) {
      stop("`", nm, "` must have one nonmissing value per task.",
           call. = FALSE)
    }
    .pps_calibration_table(y, predicted, respondent_id, x, nm)
  }
  if (!is.null(design_cell)) tables[[length(tables) + 1L]] <- add(design_cell, "design_cell")
  if (!is.null(respondent_group)) tables[[length(tables) + 1L]] <- add(respondent_group, "respondent_group")
  if (!is.null(task_order)) tables[[length(tables) + 1L]] <- add(task_order, "task_order")

  ids <- unique(as.character(respondent_id))
  ri <- match(as.character(respondent_id), ids)
  count_obs <- rowsum(y, ri, reorder = FALSE)[, 1L]
  count_pred <- rowsum(predicted, ri, reorder = FALSE)[, 1L]
  response_count <- data.frame(respondent_id = ids,
                               observed = count_obs, predicted = count_pred,
                               gap = count_obs - count_pred,
                               stringsAsFactors = FALSE)

  joint_table <- NULL
  if (!is.null(joint)) {
    req <- c("respondent_id", "type", "stratum", "observed", "predicted")
    if (!is.data.frame(joint) || !all(req %in% names(joint))) {
      stop("`joint` must contain respondent_id, type, stratum, observed, and ",
           "predicted columns.", call. = FALSE)
    }
    if (!is.numeric(joint$observed) || !is.numeric(joint$predicted) ||
        anyNA(joint[c("respondent_id", "type", "stratum")]) ||
        any(!is.finite(joint$observed)) || any(!is.finite(joint$predicted)) ||
        any(joint$observed < 0 | joint$observed > 1) ||
        any(joint$predicted < 0 | joint$predicted > 1)) {
      stop("Joint calibration values must be probabilities or binary event ",
           "indicators in [0,1].", call. = FALSE)
    }
    groups <- interaction(joint$type, joint$stratum, drop = TRUE, lex.order = TRUE)
    joint_table <- do.call(rbind, lapply(split(seq_len(nrow(joint)), groups),
      function(ii) .pps_calibration_table(
        joint$observed[ii], joint$predicted[ii], joint$respondent_id[ii],
        joint$stratum[ii], as.character(joint$type[ii[1L]]))))
  }
  verified <- isTRUE(out_of_fold) && isTRUE(training_only_tuning) &&
    !is.null(provenance) && length(provenance) == 1L &&
    !is.na(provenance) && nzchar(as.character(provenance))
  if (!is.null(analysis_signature) &&
      (!is.character(analysis_signature) || length(analysis_signature) != 1L ||
       is.na(analysis_signature) || !nzchar(analysis_signature))) {
    stop("`analysis_signature` must be NULL or one nonempty character value.",
         call. = FALSE)
  }
  out <- list(marginal = do.call(rbind, tables),
              response_count = response_count, joint = joint_table,
              joint_checks_supplied = !is.null(joint_table),
              unit = "respondent-first held-out calibration",
              provenance = provenance,
              out_of_fold = isTRUE(out_of_fold),
              training_only_tuning = isTRUE(training_only_tuning),
              analysis_signature = analysis_signature,
              verified_heldout = verified,
              status = if (verified) "verified held-out construction" else
                "user-supplied predictions; held-out provenance not established",
              disclaimer = paste(
                .pps_assessment_disclaimer,
                "Marginal calibration alone does not assess within-respondent dependence."
              ))
  class(out) <- c("scmix_calibration_assessment", "list")
  out
}

#' Completion and attrition diagnostics
#'
#' Describes associations between completed-task counts and observed
#' respondent predictors, completion patterns, and early responses. These are
#' diagnostics for observed selection and cannot verify independence from
#' latent preferences or utility shocks.
#'
#' @param completed_tasks Completed task count, one per respondent.
#' @param predictors Optional respondent-level data frame.
#' @param early_response Optional respondent-level early-task response summary.
#' @param completion_pattern Optional respondent-level pattern label.
#' @param respondent_id Optional unique respondent ids.
#' @return An internal completion-assessment object.
#' @export
scmix_completion_diagnostics <- function(completed_tasks, predictors = NULL,
                                          early_response = NULL,
                                          completion_pattern = NULL,
                                          respondent_id = NULL) {
  N <- length(completed_tasks)
  if (!is.numeric(completed_tasks) || N < 2L || any(!is.finite(completed_tasks)) ||
      any(completed_tasks < 0) || any(completed_tasks != as.integer(completed_tasks))) {
    stop("`completed_tasks` must be finite nonnegative respondent-level counts.",
         call. = FALSE)
  }
  if (is.null(respondent_id)) respondent_id <- seq_len(N)
  if (length(respondent_id) != N || anyDuplicated(as.character(respondent_id))) {
    stop("`respondent_id` must contain one unique id per completed-task count.",
         call. = FALSE)
  }
  associations <- data.frame()
  if (!is.null(predictors)) {
    predictors <- as.data.frame(predictors)
    if (nrow(predictors) != N) stop("`predictors` must have one row per respondent.",
                                    call. = FALSE)
    associations <- do.call(rbind, lapply(names(predictors), function(nm) {
      x <- predictors[[nm]]
      if (anyNA(x)) {
        return(data.frame(predictor = nm, type = "missing", statistic = NA_real_,
                          p_value = NA_real_, stringsAsFactors = FALSE))
      }
      if (is.numeric(x) && stats::sd(x) > 0) {
        f <- stats::lm(completed_tasks ~ x)
        co <- summary(f)$coefficients[2L, ]
        data.frame(predictor = nm, type = "linear slope", statistic = co[1L],
                   p_value = co[4L], stringsAsFactors = FALSE)
      } else {
        g <- factor(x)
        if (nlevels(g) < 2L) {
          data.frame(predictor = nm, type = "constant", statistic = NA_real_,
                     p_value = NA_real_, stringsAsFactors = FALSE)
        } else {
          f <- stats::lm(completed_tasks ~ g)
          av <- stats::anova(f)
          means <- tapply(completed_tasks, g, mean)
          data.frame(predictor = nm, type = "factor mean range",
                     statistic = diff(range(means)), p_value = av$`Pr(>F)`[1L],
                     stringsAsFactors = FALSE)
        }
      }
    }))
  }
  early <- NULL
  if (!is.null(early_response)) {
    if (!is.numeric(early_response) || length(early_response) != N ||
        any(!is.finite(early_response))) {
      stop("`early_response` must be finite and have one value per respondent.",
           call. = FALSE)
    }
    f <- stats::lm(early_response ~ completed_tasks)
    co <- summary(f)$coefficients[2L, ]
    early <- c(slope = unname(co[1L]), se = unname(co[2L]),
               p_value = unname(co[4L]))
  }
  patterns <- NULL
  if (!is.null(completion_pattern)) {
    if (length(completion_pattern) != N || anyNA(completion_pattern)) {
      stop("`completion_pattern` must have one nonmissing value per respondent.",
           call. = FALSE)
    }
    patterns <- as.data.frame(table(completion_pattern), stringsAsFactors = FALSE)
  }
  out <- list(summary = c(N = N, mean = mean(completed_tasks),
                          sd = stats::sd(completed_tasks),
                          min = min(completed_tasks), max = max(completed_tasks)),
              associations = associations, early_response = early,
              completion_patterns = patterns,
              disclaimer = paste(
                "These checks can expose observed selection but cannot verify",
                "independence from latent preferences or unobserved utility shocks."
              ))
  class(out) <- c("scmix_completion_assessment", "list")
  out
}

#' Rank-interiority reporting gate
#'
#' Checks whether a fitted covariance lies on the prespecified exact-rank
#' stratum and whether its smallest active eigenvalue exceeds a prespecified
#' margin. This is a warning rule, not a regular rank test.
#'
#' @param Sigma Fitted covariance.
#' @param q Prespecified maximum/active rank for the reported specification.
#' @param eigenvalue_margin Prespecified positive separation margin; `NULL`
#'   leaves regular inference unapproved.
#' @param rank_tol Relative numerical rank tolerance on the declared
#'   structurally scaled covariance.
#' @param structural_scale Positive coefficient multipliers defining the
#'   structural numerical scale, for example respondent-weighted contrast RMS
#'   values. Required for `q > 0`.
#' @param absolute_floor Machine-level absolute eigenvalue floor after scaling.
#' @return An internal gate object.
#' @export
scmix_rank_gate <- function(Sigma, q, eigenvalue_margin = NULL,
                            rank_tol = 1e-8, structural_scale = NULL,
                            absolute_floor = sqrt(.Machine$double.eps)) {
  Sigma <- as.matrix(Sigma)
  p <- nrow(Sigma)
  Sigma <- .pps_validate_sigma(Sigma, p)
  if (!is.numeric(q) || length(q) != 1L || q < 0L || q > max(0L, p - 1L) ||
      q != as.integer(q)) {
    stop("`q` must be an integer between zero and p - 1, as in the maintained model.",
         call. = FALSE)
  }
  if (!is.numeric(rank_tol) || length(rank_tol) != 1L ||
      !is.finite(rank_tol) || rank_tol <= 0 ||
      !is.numeric(absolute_floor) || length(absolute_floor) != 1L ||
      !is.finite(absolute_floor) || absolute_floor < 0) {
    stop("Rank tolerances must be finite, with rank_tol > 0 and ",
         "absolute_floor >= 0.", call. = FALSE)
  }
  if (is.null(structural_scale) && q == 0L) structural_scale <- rep(1, p)
  if (!is.numeric(structural_scale) || length(structural_scale) != p ||
      any(!is.finite(structural_scale)) || any(structural_scale <= 0)) {
    stop("`structural_scale` must supply p finite positive coefficient ",
         "multipliers", if (q > 0L) " when q > 0." else ".",
         call. = FALSE)
  }
  D <- diag(as.numeric(structural_scale), p)
  Sigma_scaled <- D %*% Sigma %*% D
  eig <- sort(eigen(Sigma_scaled, symmetric = TRUE, only.values = TRUE)$values,
              decreasing = TRUE)
  scale <- max(eig[1L], 0)
  cutoff <- absolute_floor + rank_tol * scale
  rank_est <- sum(eig > cutoff)
  if (q == 0L) {
    ## The homogeneous submodel is regular when it was fixed in advance; it
    ## has no active covariance eigenvalue requiring a separation margin.
    separated <- rank_est == 0L
  } else if (is.null(eigenvalue_margin)) {
    separated <- NA
  } else {
    if (!is.numeric(eigenvalue_margin) || length(eigenvalue_margin) != 1L ||
        !is.finite(eigenvalue_margin) || eigenvalue_margin <= 0) {
      stop("`eigenvalue_margin` must be finite and strictly positive.",
           call. = FALSE)
    }
    separated <- rank_est == q && eig[q] >= eigenvalue_margin
  }
  out <- list(q = as.integer(q), eigenvalues = eig, numerical_rank = rank_est,
              scaled_covariance = Sigma_scaled,
              structural_scale = structural_scale,
              numerical_rank_cutoff = cutoff,
              smallest_active = if (q == 0L) NA_real_ else eig[q],
              margin = eigenvalue_margin,
              regular_inference = isTRUE(separated),
              status = if (isTRUE(separated)) "rank-interiority gate passed" else
                if (is.na(separated)) "rank margin not prespecified" else
                  "boundary/rank warning: regular inference withheld",
              disclaimer = paste(
                "This is a reporting gate, not a regular test of rank.",
                "Data-dependent rank selection requires separate theory."
              ))
  class(out) <- c("scmix_rank_gate", "list")
  out
}

#' Numerical-stability reporting gate
#'
#' Compares estimates, standard errors, held-out scores, and covariance
#' eigenvalues across integration refinements, independent scrambles,
#' rotations, or starts. Tolerances must be prespecified by column.
#'
#' @param checks Data frame with one row per numerical replication/refinement.
#' @param tolerances Named nonnegative tolerances for assessed numeric columns.
#' @param reference Row used as the reference, by default the last row.
#' @return An internal numerical gate object.
#' @export
scmix_numerical_gate <- function(checks, tolerances,
                                 reference = nrow(checks)) {
  checks <- as.data.frame(checks)
  if (nrow(checks) < 2L) stop("At least two numerical checks are required.",
                              call. = FALSE)
  if (!is.numeric(reference) || length(reference) != 1L ||
      reference < 1L || reference > nrow(checks)) {
    stop("`reference` must identify one row of `checks`.", call. = FALSE)
  }
  if (is.null(names(tolerances)) || any(!is.finite(tolerances)) ||
      any(tolerances < 0)) {
    stop("`tolerances` must be a named finite nonnegative vector.",
         call. = FALSE)
  }
  missing <- setdiff(names(tolerances), names(checks))
  if (length(missing)) stop("Missing numerical-check column(s): ",
                            paste(missing, collapse = ", "), call. = FALSE)
  rows <- lapply(names(tolerances), function(nm) {
    x <- checks[[nm]]
    if (!is.numeric(x) || any(!is.finite(x))) {
      stop("Numerical-check columns must be finite numeric values.",
           call. = FALSE)
    }
    dev <- max(abs(x - x[reference]))
    data.frame(metric = nm, max_deviation = dev,
               tolerance = unname(tolerances[nm]), pass = dev <= tolerances[nm],
               stringsAsFactors = FALSE)
  })
  comparison <- do.call(rbind, rows)
  pass <- all(comparison$pass)
  out <- list(pass = pass, comparison = comparison, checks = checks,
              reference = reference,
              status = if (pass) "numerical-stability gate passed" else
                "numerical instability: affected claims should be reported or withheld",
              disclaimer = paste(
                "Empirical stability does not prove the asymptotic numerical-error rates",
                "required by the inference theory."
              ))
  class(out) <- c("scmix_numerical_gate", "list")
  out
}

#' Local information on identified structural directions
#'
#' Computes generalized information eigenvalues from caller-supplied
#' complete-sequence directional scores and a structural-norm Gram matrix. The
#' caller must affirm that columns are identified structural function/covariance
#' directions with network reparameterizations and factor rotations already
#' quotiented out. Raw neural-weight or loading-factor Hessians are rejected by
#' construction because this interface accepts scores, not a Hessian.
#'
#' @param score Respondent-by-direction matrix of complete-sequence directional
#'   scores.
#' @param structural_norm Positive-definite Gram matrix for the fixed
#'   structural norm used to normalize directions.
#' @param respondent_id Optional unique id for every score row.
#' @param direction_labels Optional labels for score columns.
#' @param identified_directions Must be explicitly `TRUE`.
#' @param provenance Nonempty description of the identified tangent
#'   construction and score source.
#' @param norm_tol Numerical positive-definiteness tolerance.
#' @return Generalized eigenvalues/eigenvectors and the empirical structural
#'   information matrix. This is a local information diagnostic, not a global
#'   identification test.
#' @export
scmix_local_information <- function(score, structural_norm,
                                    respondent_id = NULL,
                                    direction_labels = colnames(score),
                                    identified_directions = FALSE,
                                    provenance = NULL,
                                    norm_tol = 1e-10) {
  if (!isTRUE(identified_directions)) {
    stop("Set `identified_directions = TRUE` only after quotienting score-null, ",
         "network-reparameterization, and factor-rotation directions.",
         call. = FALSE)
  }
  S <- as.matrix(score)
  if (!is.numeric(S) || any(!is.finite(S)) || nrow(S) < 2L || ncol(S) < 1L) {
    stop("`score` must be a finite respondent-by-direction matrix.",
         call. = FALSE)
  }
  if (!is.null(respondent_id) &&
      (length(respondent_id) != nrow(S) ||
       anyDuplicated(as.character(respondent_id)))) {
    stop("There must be one score row per unique respondent.", call. = FALSE)
  }
  K <- ncol(S)
  G <- as.matrix(structural_norm)
  if (!is.numeric(G) || !identical(dim(G), c(K, K)) ||
      any(!is.finite(G)) || max(abs(G - t(G))) > 1e-8) {
    stop("`structural_norm` must be a finite symmetric K by K Gram matrix.",
         call. = FALSE)
  }
  eg <- eigen((G + t(G)) / 2, symmetric = TRUE)
  if (min(eg$values) <= norm_tol * max(1, max(eg$values))) {
    stop("`structural_norm` must be positive definite on the supplied ",
         "identified directions.", call. = FALSE)
  }
  G_mhalf <- eg$vectors %*% diag(1 / sqrt(eg$values), K) %*% t(eg$vectors)
  I_hat <- crossprod(S) / nrow(S)
  standardized <- G_mhalf %*% I_hat %*% G_mhalf
  standardized <- (standardized + t(standardized)) / 2
  ee <- eigen(standardized, symmetric = TRUE)
  labels <- direction_labels
  if (is.null(labels)) labels <- paste0("direction_", seq_len(K))
  if (length(labels) != K || anyDuplicated(labels)) {
    stop("`direction_labels` must uniquely label every score column.",
         call. = FALSE)
  }
  rownames(I_hat) <- colnames(I_hat) <- labels
  rownames(G) <- colnames(G) <- labels
  verified_source <- !is.null(provenance) && length(provenance) == 1L &&
    !is.na(provenance) && nzchar(as.character(provenance))
  out <- list(information = I_hat, structural_norm = G,
              generalized_eigenvalues = ee$values,
              standardized_eigenvectors = ee$vectors,
              smallest = min(ee$values), directions = labels,
              respondent_id = respondent_id,
              provenance = provenance,
              verified_source = verified_source,
              status = if (verified_source)
                "identified-direction information supplied with provenance" else
                "identified-direction assertion supplied without provenance",
              input = "complete-sequence scores on identified structural directions",
              raw_parameter_hessian_used = FALSE,
              disclaimer = paste(
                "Small generalized eigenvalues diagnose weak local information;",
                "they do not prove global nonidentification."
              ))
  class(out) <- c("scmix_local_information", "list")
  out
}

#' Descriptive profile sequence-likelihood table
#'
#' Summarizes one complete-sequence log likelihood per respondent and grid
#' point after the caller has reoptimized the remaining nuisance components.
#' The output is descriptive; it assigns no likelihood-ratio critical values,
#' particularly at a covariance-rank boundary.
#'
#' @param grid Numeric profile values.
#' @param loglik Respondent-by-grid matrix of integrated complete-sequence log
#'   likelihoods.
#' @param respondent_id Optional unique respondent ids.
#' @param direction Description of the structural or quantity direction.
#' @param nuisance_reoptimized Must be explicitly true if the table is to be
#'   labeled a profile rather than a slice.
#' @param rank_boundary Whether the profile touches a covariance-rank boundary.
#' @param sieve_tuning_fixed Whether the sieve architecture, tuning rule, and
#'   penalty were fixed over the profile grid.
#' @param provenance Nonempty description of the fit and profiling routine.
#' @return A descriptive profile table.
#' @export
scmix_profile_sequence_likelihood <- function(grid, loglik,
                                              respondent_id = NULL,
                                              direction = "structural direction",
                                              nuisance_reoptimized = FALSE,
                                              rank_boundary = FALSE,
                                              sieve_tuning_fixed = FALSE,
                                              provenance = NULL) {
  ll <- as.matrix(loglik)
  if (!is.numeric(grid) || any(!is.finite(grid)) || length(grid) != ncol(ll) ||
      !is.numeric(ll) || any(!is.finite(ll)) || nrow(ll) < 2L) {
    stop("Supply finite grid values and a respondent-by-grid sequence-loglik ",
         "matrix.", call. = FALSE)
  }
  if (!is.null(respondent_id) &&
      (length(respondent_id) != nrow(ll) ||
       anyDuplicated(as.character(respondent_id)))) {
    stop("There must be one complete-sequence likelihood row per respondent.",
         call. = FALSE)
  }
  mean_ll <- colMeans(ll)
  tab <- data.frame(grid = grid, mean_sequence_loglik = mean_ll,
                    total_loglik_difference = nrow(ll) *
                      (mean_ll - max(mean_ll)),
                    stringsAsFactors = FALSE)
  verified_profile <- isTRUE(nuisance_reoptimized) &&
    isTRUE(sieve_tuning_fixed) && !is.null(provenance) &&
    length(provenance) == 1L && !is.na(provenance) &&
    nzchar(as.character(provenance))
  out <- list(table = tab, direction = direction,
              kind = if (isTRUE(nuisance_reoptimized))
                "profile sequence likelihood" else "sequence-likelihood slice",
              nuisance_reoptimized = isTRUE(nuisance_reoptimized),
              sieve_tuning_fixed = isTRUE(sieve_tuning_fixed),
              provenance = provenance,
              verified_profile = verified_profile,
              rank_boundary = isTRUE(rank_boundary),
              likelihood_ratio_critical_values = FALSE,
              disclaimer = if (isTRUE(rank_boundary))
                "No regular likelihood-ratio critical values are assigned at a rank boundary." else
                "This descriptive profile diagnoses curvature and is not a global identification test.")
  class(out) <- c("scmix_profile_sequence_likelihood", "list")
  out
}

#' Common respondent multiplier for structural--design discrepancies
#'
#' Applies the same multiplier draw to each respondent across every
#' prespecified contrast, preserving both structural--design covariance and
#' cross-contrast covariance. Inputs must be discrepancy objects with joint
#' respondent influence contributions.
#'
#' @param discrepancies One discrepancy object or a named list of them.
#' @param R Number of multiplier replications.
#' @param level Simultaneous confidence level.
#' @param seed Optional seed.
#' @param multipliers Optional precomputed `R`-by-`N` mean-zero multiplier
#'   matrix; useful for deterministic tests.
#' @return Multiplier draws, simultaneous critical value, and intervals.
#' @export
scmix_discrepancy_multiplier <- function(discrepancies, R = 999L,
                                         level = 0.95, seed = NULL,
                                         multipliers = NULL) {
  if (inherits(discrepancies, "scmix_structural_design_discrepancy")) {
    discrepancies <- list(discrepancy = discrepancies)
  }
  if (!is.list(discrepancies) || !length(discrepancies)) {
    stop("`discrepancies` must contain at least one discrepancy object.",
         call. = FALSE)
  }
  if (is.null(names(discrepancies)) || any(names(discrepancies) == "")) {
    names(discrepancies) <- paste0("contrast_", seq_along(discrepancies))
  }
  valid <- vapply(discrepancies, function(x) {
    inherits(x, "scmix_structural_design_discrepancy") &&
      !is.null(x$influence) && !is.null(names(x$influence))
  }, logical(1L))
  if (!all(valid)) {
    stop("Every discrepancy needs named joint respondent influence ",
         "contributions; descriptive discrepancies cannot be bootstrapped.",
         call. = FALSE)
  }
  ids <- names(discrepancies[[1L]]$influence)
  if (any(vapply(discrepancies, function(x)
    !setequal(names(x$influence), ids), logical(1L)))) {
    stop("All discrepancies must cover the same respondents.", call. = FALSE)
  }
  IF <- vapply(discrepancies, function(x)
    unname(x$influence[ids]), numeric(length(ids)))
  IF <- sweep(IF, 2L, colMeans(IF), `-`)
  N <- nrow(IF)
  estimates <- vapply(discrepancies, `[[`, numeric(1L), "estimate")
  root_sd <- sqrt(colMeans(IF^2))
  se <- root_sd / sqrt(N)
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("`level` must lie strictly between zero and one.", call. = FALSE)
  }
  if (is.null(multipliers)) {
    if (!is.numeric(R) || length(R) != 1L || R < 100L) {
      stop("`R` must be at least 100.", call. = FALSE)
    }
    if (!is.null(seed)) {
      old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        get(".Random.seed", envir = .GlobalEnv) else NULL
      on.exit({
        if (is.null(old)) {
          if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv)
        } else assign(".Random.seed", old, envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(seed)
    }
    multipliers <- matrix(sample(c(-1, 1), as.integer(R) * N, replace = TRUE),
                          nrow = as.integer(R), ncol = N)
  } else {
    multipliers <- as.matrix(multipliers)
    if (!is.numeric(multipliers) || ncol(multipliers) != N ||
        any(!is.finite(multipliers)) || nrow(multipliers) < 2L) {
      stop("`multipliers` must be a finite R by N matrix.", call. = FALSE)
    }
  }
  G <- multipliers %*% IF / sqrt(N)
  colnames(G) <- names(discrepancies)
  if (any(root_sd <= sqrt(.Machine$double.eps))) {
    stop("Every simultaneous contrast must have nonzero respondent influence ",
         "variance.", call. = FALSE)
  }
  max_t <- apply(abs(sweep(G, 2L, root_sd, `/`)), 1L, max)
  critical <- unname(stats::quantile(max_t, probs = level, type = 8))
  intervals <- data.frame(
    contrast = names(discrepancies), estimate = estimates, se = se,
    lower = estimates - critical * se,
    upper = estimates + critical * se,
    stringsAsFactors = FALSE
  )
  out <- list(intervals = intervals, critical_value = critical,
              level = level, draws = G, multipliers = multipliers,
              respondent_id = ids,
              unit = "one common multiplier per respondent across contrasts")
  class(out) <- c("scmix_discrepancy_multiplier", "list")
  out
}

#' Prespecified structural-sensitivity plan and result collector
#'
#' Records whether every sensitivity required by the paper was implemented,
#' run, or omitted. Supplied alternative-model results remain sensitivity
#' analyses unless a separate identification result and description are
#' explicitly provided; they are never promoted to coequal identified models
#' by this collector.
#'
#' @param results Named list keyed by the required component names shown in the
#'   returned status table. Each supplied component must contain an enumerated
#'   `status`, nonempty `provenance`, and (when run) a nonnull `result`.
#' @param q_values Prespecified primary and alternative rank values.
#' @param materiality_tolerances Prespecified nonnegative tolerance named for
#'   every implemented component. For substantive readiness, each result must
#'   record `tolerance_applied = TRUE`, the matching `materiality_tolerance`, a
#'   finite nonnegative scalar `materiality_value`, and `passed = TRUE` exactly
#'   when that value does not exceed the tolerance.
#' @param prespecified Whether the plan was fixed before viewing outcomes.
#' @return A fail-closed status table, supplied results, protocol-completion
#'   status, and a separate substantive-pass indicator. Completion requires at
#'   least two distinct prespecified rank values; substantive passage also
#'   requires auditable component-level materiality evidence. Neither verifies
#'   a maintained assumption.
#' @export
scmix_structural_sensitivity <- function(results = list(), q_values = NULL,
                                         materiality_tolerances = NULL,
                                         prespecified = FALSE) {
  required <- c(
    "rank_q_stability",
    "shape_skewed_simulation", "shape_skewed_refit",
    "shape_bimodal_simulation", "shape_bimodal_refit",
    "shape_heavy_tail_simulation", "shape_heavy_tail_refit",
    "covariance_by_Z",
    "task_order_fatigue_learning", "serial_shocks", "position",
    "scale", "completion"
  )
  if (!is.list(results) ||
      (length(results) && (is.null(names(results)) || any(names(results) == "")))) {
    stop("`results` must be a named list.", call. = FALSE)
  }
  unknown <- setdiff(names(results), required)
  if (length(unknown)) {
    stop("Unknown sensitivity component(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  if (!is.null(q_values) &&
      (!is.numeric(q_values) || any(!is.finite(q_values)) ||
       any(q_values < 0) || any(q_values != as.integer(q_values)))) {
    stop("`q_values` must contain nonnegative integer ranks.", call. = FALSE)
  }
  if (!is.null(materiality_tolerances) &&
      (is.null(names(materiality_tolerances)) ||
       any(!is.finite(materiality_tolerances)) ||
       any(materiality_tolerances < 0))) {
    stop("Materiality tolerances must be named, finite, and nonnegative.",
         call. = FALSE)
  }
  if (!is.null(materiality_tolerances)) {
    unknown_tolerances <- setdiff(names(materiality_tolerances), required)
    if (length(unknown_tolerances)) {
      stop("Unknown materiality-tolerance component(s): ",
           paste(unknown_tolerances, collapse = ", "), call. = FALSE)
    }
  }
  rows <- lapply(required, function(nm) {
    x <- results[[nm]]
    allowed <- c("run_pass", "run_sensitive", "run_fail", "not_run",
                 "not_applicable")
    valid_schema <- is.list(x) && !is.null(x$status) &&
      length(x$status) == 1L && as.character(x$status) %in% allowed &&
      !is.null(x$provenance) && length(x$provenance) == 1L &&
      !is.na(x$provenance) && nzchar(as.character(x$provenance))
    if (!is.null(x) && !valid_schema) {
      stop("Sensitivity component '", nm, "' must contain an allowed status ",
           "and nonempty provenance.", call. = FALSE)
    }
    status_value <- if (is.null(x)) "not_run" else as.character(x$status)
    implemented <- valid_schema && startsWith(status_value, "run_") &&
      !is.null(x$result)
    justified_na <- valid_schema && identical(status_value, "not_applicable") &&
      !is.null(x$justification) && nzchar(as.character(x$justification)[1L])
    id_ok <- implemented &&
      isTRUE(x$identification_established) &&
      !is.null(x$identification_note) && nzchar(x$identification_note)
    planned_tolerance <- if (!is.null(materiality_tolerances) &&
                             nm %in% names(materiality_tolerances))
      unname(materiality_tolerances[[nm]]) else NA_real_
    result_meta <- if (implemented && is.list(x$result)) x$result else list()
    tolerance_matches <- is.numeric(result_meta$materiality_tolerance) &&
      length(result_meta$materiality_tolerance) == 1L &&
      is.finite(result_meta$materiality_tolerance) &&
      is.finite(planned_tolerance) &&
      abs(result_meta$materiality_tolerance - planned_tolerance) <=
        sqrt(.Machine$double.eps) * max(1, abs(planned_tolerance))
    value_ok <- is.numeric(result_meta$materiality_value) &&
      length(result_meta$materiality_value) == 1L &&
      is.finite(result_meta$materiality_value) &&
      result_meta$materiality_value >= 0
    pass_recorded <- is.logical(result_meta$passed) &&
      length(result_meta$passed) == 1L && !is.na(result_meta$passed)
    materiality_evidence <- implemented &&
      isTRUE(result_meta$tolerance_applied) && tolerance_matches &&
      value_ok && pass_recorded
    materiality_pass <- materiality_evidence &&
      isTRUE(result_meta$passed) &&
      result_meta$materiality_value <= planned_tolerance
    data.frame(component = nm, implemented = implemented,
               justified_not_applicable = justified_na,
               status = status_value,
               materiality_tolerance = planned_tolerance,
               materiality_evidence = materiality_evidence,
               materiality_pass = materiality_pass,
               separately_identified = id_ok,
               interpretation = if (id_ok)
                 "separate identification claim supplied; review its proof" else
                 "sensitivity only; not claimed identified",
               stringsAsFactors = FALSE)
  })
  status <- do.call(rbind, rows)
  shape_names <- c("skewed", "bimodal", "heavy_tail")
  shape_standardization <- lapply(shape_names, function(shape) {
    candidates <- list(
      results[[paste0("shape_", shape, "_simulation")]],
      results[[paste0("shape_", shape, "_refit")]])
    ran <- vapply(candidates, function(x) is.list(x) &&
                    startsWith(as.character(x$status %||% ""), "run_") &&
                    !is.null(x$result), logical(1L))
    x <- if (any(ran)) candidates[[which(ran)[1L]]] else NULL
    if (is.null(x) || !is.list(x)) {
      return(list(mean_zero = FALSE, unit_covariance = FALSE,
                  finite_covariance = FALSE,
                  factor_orientation_prespecified = FALSE))
    }
    meta <- if (is.list(x$result)) x$result else x
    list(mean_zero = isTRUE(meta$mean_zero),
         unit_covariance = isTRUE(meta$unit_covariance),
         finite_covariance = isTRUE(meta$finite_covariance),
         factor_orientation_prespecified =
           isTRUE(meta$factor_orientation_prespecified))
  })
  names(shape_standardization) <- shape_names
  distinct_q <- !is.null(q_values) && length(unique(as.integer(q_values))) >= 2L
  complete <- isTRUE(prespecified) && distinct_q &&
    all(status$implemented | status$justified_not_applicable)
  shape_standardized <- vapply(shape_standardization, function(x) {
    all(vapply(x, isTRUE, logical(1L)))
  }, logical(1L))
  substantive_pass <- complete &&
    !any(status$status %in% c("run_sensitive", "run_fail")) &&
    all(shape_standardized) &&
    all(!status$implemented |
          (status$materiality_evidence & status$materiality_pass))
  out <- list(status = status, results = results, q_values = q_values,
              shape_standardization = shape_standardization,
              materiality_tolerances = materiality_tolerances,
              distinct_q_values = distinct_q,
              materiality_complete =
                all(!status$implemented | status$materiality_evidence),
              prespecified = isTRUE(prespecified),
              complete = complete,
              substantive_pass = substantive_pass,
              disclaimer = paste(
                "Alternative residual shapes and process specifications are sensitivity analyses,",
                "not coequal identified models. A supplied fit is not an identification result."
              ))
  class(out) <- c("scmix_structural_sensitivity", "list")
  out
}

#' Execute a prespecified structural-sensitivity battery
#'
#' Runs application-specific callbacks and converts their outputs into the
#' fail-closed schema used by [scmix_structural_sensitivity()]. The primary
#' normal low-rank mixed logit remains the sole baseline model; callbacks are
#' assessment tools and do not inherit its identification theorem.
#'
#' @param runners Named list of functions keyed by sensitivity component. Each
#'   function receives `context` and returns a result. A result may include
#'   `materially_sensitive = TRUE` or `passed = FALSE` to set its status.
#' @param provenance Named character vector/list giving a nonempty provenance
#'   record for every supplied runner.
#' @param context Read-only application inputs and fitted objects passed to each
#'   callback.
#' @param q_values,materiality_tolerances,prespecified Passed to
#'   [scmix_structural_sensitivity()].
#' @return A `scmix_structural_sensitivity` object containing every result or
#'   captured error. Missing runners remain visibly `not_run`.
#' @export
scmix_run_structural_sensitivity <- function(
    runners, provenance, context = list(), q_values = NULL,
    materiality_tolerances = NULL, prespecified = FALSE) {
  if (!is.list(runners) ||
      (length(runners) && (is.null(names(runners)) ||
                           any(!nzchar(names(runners))))) ||
      any(!vapply(runners, is.function, logical(1L)))) {
    stop("`runners` must be a named list of functions.", call. = FALSE)
  }
  if (length(runners)) {
    if (is.null(names(provenance)) ||
        !all(names(runners) %in% names(provenance)) ||
        any(vapply(provenance[names(runners)], function(x)
          length(x) != 1L || is.na(x) || !nzchar(as.character(x)), logical(1L)))) {
      stop("`provenance` must provide one nonempty named record per runner.",
           call. = FALSE)
    }
  }
  results <- lapply(names(runners), function(nm) {
    value <- tryCatch(runners[[nm]](context), error = identity)
    if (inherits(value, "error")) {
      return(list(status = "run_fail",
                  provenance = as.character(provenance[[nm]]),
                  result = list(error = conditionMessage(value))))
    }
    status <- if (is.list(value) && isTRUE(value$materially_sensitive)) {
      "run_sensitive"
    } else if (is.list(value) && identical(value$passed, FALSE)) {
      "run_fail"
    } else "run_pass"
    list(status = status, provenance = as.character(provenance[[nm]]),
         result = value,
         identification_established = is.list(value) &&
           isTRUE(value$identification_established),
         identification_note = if (is.list(value))
           value$identification_note else NULL)
  })
  names(results) <- names(runners)
  scmix_structural_sensitivity(
    results = results, q_values = q_values,
    materiality_tolerances = materiality_tolerances,
    prespecified = prespecified)
}

#' Quantity-specific reporting gates
#'
#' Applies the paper's MRS-denominator, total-heterogeneity,
#' threshold-variance, majority, support, weak-information, and numerical/rank
#' rules. Missing margins fail safely by leaving the corresponding claim
#' unapproved.
#'
#' @param mrs_denominator,total_heterogeneity,residual_variance Optional values.
#' @param mrs_margin,total_margin,residual_variance_margin Prespecified margins.
#' @param confidence_interval Optional interval used descriptively. A regular
#'   majority claim is generated only from `inference` and `target_label`.
#' @param on_support Whether the target is on randomized support.
#' @param rank_gate,numerical_gate Optional gate objects.
#' @param weak_information Deprecated logical warning. It is recorded for
#'   compatibility but cannot establish adequate information by itself.
#' @param local_information A verified `scmix_local_information` object.
#' @param information_eigenvalue_margin Prespecified strictly positive lower
#'   margin for its smallest structural-norm generalized information eigenvalue.
#' @param target Target type determining the evidence that must be supplied.
#' @param inference Paper-aligned `scmix_dml` result.
#' @param target_label Name of the target column in `inference`.
#' @param optimization_gate Output of `scmix_optimization_audit()`.
#' @param design_audit Optional design audit used to label theorem applicability.
#' @return A gate table and allowed majority label.
#' @export
scmix_reporting_gates <- function(mrs_denominator = NULL, mrs_margin = NULL,
                                  total_heterogeneity = NULL,
                                  total_margin = NULL,
                                  residual_variance = NULL,
                                  residual_variance_margin = NULL,
                                  confidence_interval = NULL,
                                  on_support = NA,
                                  rank_gate = NULL,
                                  numerical_gate = NULL,
                                  weak_information = NULL,
                                  local_information = NULL,
                                  information_eigenvalue_margin = NULL,
                                  target = c("generic_regular", "mrs",
                                             "variance_share",
                                             "threshold_share",
                                             "structural_choice",
                                             "descriptive"),
                                  inference = NULL,
                                  target_label = NULL,
                                  optimization_gate = NULL,
                                  design_audit = NULL) {
  target <- match.arg(target)
  rows <- list()
  inf_ok <- FALSE
  add_gate <- function(name, g) {
    rows[[length(rows) + 1L]] <<- data.frame(
      gate = name, pass = if (is.na(g$pass)) NA else isTRUE(g$pass),
      value = g$value, margin = g$margin, reason = g$reason,
      stringsAsFactors = FALSE)
  }
  if (!is.null(mrs_denominator)) {
    add_gate("MRS denominator", .pps_gate(mrs_denominator, mrs_margin,
                                           "MRS denominator", TRUE))
  }
  if (!is.null(total_heterogeneity)) {
    add_gate("total directional heterogeneity",
             .pps_gate(total_heterogeneity, total_margin,
                       "total directional heterogeneity"))
  }
  if (!is.null(residual_variance)) {
    add_gate("threshold directional residual variance",
             .pps_gate(residual_variance, residual_variance_margin,
                       "directional residual variance"))
  }
  add_evidence <- function(name, pass, reason) {
    rows[[length(rows) + 1L]] <<- data.frame(
      gate = name, pass = if (is.na(pass)) NA else isTRUE(pass),
      value = NA_real_, margin = NA_real_, reason = reason,
      stringsAsFactors = FALSE)
  }
  if (!identical(target, "descriptive")) {
    target_flags <- if (inherits(inference, "scmix_dml"))
      inference$target_inference_available else NULL
    target_ok <- inherits(inference, "scmix_dml") &&
      !is.null(target_label) && length(target_label) == 1L &&
      target_label %in% names(inference$estimate) &&
      target_label %in% names(inference$se) &&
      !is.null(target_flags) && !is.null(names(target_flags)) &&
      target_label %in% names(target_flags) &&
      isTRUE(target_flags[[target_label]]) &&
      is.finite(inference$estimate[target_label]) &&
      is.finite(inference$se[target_label]) && inference$se[target_label] > 0
    inf_ok <- inherits(inference, "scmix_dml") &&
      isTRUE(inference$inference_available) &&
      (identical(inference$status, "available") ||
         identical(inference$status, "conditional_available")) && target_ok
    add_evidence("paper-aligned inference", inf_ok,
                 if (inf_ok) "regular-inference gates passed" else
                   "an inference-available scmix_dml result was not supplied")
    add_evidence("rank interiority evidence", !is.null(rank_gate) &&
                   isTRUE(rank_gate$regular_inference),
                 if (is.null(rank_gate)) "rank gate missing" else rank_gate$status)
    add_evidence("numerical refinement evidence", !is.null(numerical_gate) &&
                   isTRUE(numerical_gate$pass),
                 if (is.null(numerical_gate)) "numerical gate missing" else
                   numerical_gate$status)
    info_margin_ok <- is.numeric(information_eigenvalue_margin) &&
      length(information_eigenvalue_margin) == 1L &&
      is.finite(information_eigenvalue_margin) &&
      information_eigenvalue_margin > 0
    info_value <- if (inherits(local_information, "scmix_local_information") &&
                      length(local_information$smallest) == 1L)
      local_information$smallest else NA_real_
    info_ok <- inherits(local_information, "scmix_local_information") &&
      isTRUE(local_information$verified_source) && info_margin_ok &&
      is.finite(info_value) && info_value >= information_eigenvalue_margin
    rows[[length(rows) + 1L]] <- data.frame(
      gate = "local information evidence", pass = info_ok,
      value = info_value,
      margin = if (info_margin_ok) information_eigenvalue_margin else NA_real_,
      reason = if (info_ok) {
        "smallest structural-norm generalized eigenvalue exceeds the prespecified margin"
      } else if (!inherits(local_information, "scmix_local_information")) {
        if (!is.null(weak_information))
          "legacy weak-information assertion is insufficient; verified local-information evidence is required" else
          "verified local-information assessment missing"
      } else if (!isTRUE(local_information$verified_source)) {
        "local-information source or identified structural directions were not verified"
      } else if (!info_margin_ok) {
        "strictly positive prespecified information margin missing"
      } else {
        "smallest structural-norm generalized eigenvalue is below the prespecified margin"
      },
      stringsAsFactors = FALSE)
    opt_ok <- !is.null(optimization_gate) &&
      isTRUE(optimization_gate$all_selected_tolerances_met) &&
      isTRUE(optimization_gate$all_computational_gates_pass) &&
      !isTRUE(optimization_gate$any_bound_activity)
    add_evidence("optimization evidence", opt_ok,
                 if (is.null(optimization_gate)) "optimization audit missing" else
                   if (opt_ok) "attained-solution diagnostics passed" else
                     "optimization tolerance or bound-activity warning")
  }
  if (identical(target, "mrs") && is.null(mrs_denominator)) {
    add_evidence("MRS denominator", FALSE, "MRS denominator evidence missing")
  }
  if (identical(target, "variance_share") && is.null(total_heterogeneity)) {
    add_evidence("total directional heterogeneity", FALSE,
                 "total-heterogeneity evidence missing")
  }
  if (identical(target, "threshold_share") && is.null(residual_variance)) {
    add_evidence("threshold directional residual variance", FALSE,
                 "directional residual-variance evidence missing")
  }
  if (!identical(target, "descriptive")) {
    add_evidence("structural identification audit",
                 inherits(design_audit, "scmix_design_audit") &&
                   isTRUE(design_audit$established),
                 if (is.null(design_audit))
                   "structural identification audit missing" else
                   design_audit$status %||% "malformed design audit")
  }
  majority <- "no regular majority claim"
  ci <- confidence_interval
  ci_source <- if (is.null(ci)) "none" else "user supplied; descriptive only"
  if (inherits(inference, "scmix_dml") && !is.null(target_label) &&
      length(target_label) == 1L && target_label %in% names(inference$estimate)) {
    ci <- c(inference$ci_lower[target_label], inference$ci_upper[target_label])
    ci_source <- "scmix_dml"
  }
  if (!is.null(ci)) {
    if (!is.numeric(ci) || length(ci) != 2L || any(!is.finite(ci)) || ci[1L] > ci[2L]) {
      stop("`confidence_interval` must be an ordered finite interval.",
           call. = FALSE)
    }
  }
  support <- if (isTRUE(on_support)) "on randomized support" else
    if (identical(on_support, FALSE)) "structural extrapolation" else
      "support not audited"
  table <- if (length(rows)) do.call(rbind, rows) else data.frame()
  approved <- nrow(table) > 0L && all(!is.na(table$pass) & table$pass)
  if (identical(target, "threshold_share") && approved &&
      identical(ci_source, "scmix_dml")) {
    if (ci[1L] > 0.5) majority <- "above one-half"
    if (ci[2L] < 0.5) majority <- "below one-half"
  }
  out <- list(gates = table, majority_claim = majority, support = support,
              target = target, confidence_interval = ci,
              confidence_interval_source = ci_source,
              inference_status = if (inherits(inference, "scmix_dml"))
                inference$status %||% "unknown" else "not supplied",
              inference_claim = if (isTRUE(inf_ok) &&
                                     inherits(inference, "scmix_dml") &&
                                     identical(inference$status,
                                               "conditional_available")) {
                "conditional_on_documented_high_level_assumptions"
              } else if (isTRUE(inf_ok) && inherits(inference, "scmix_dml")) {
                inference$inference_claim %||%
                  "available_under_stated_regular_inference_conditions"
              } else "not available",
              regular_reporting_approved = approved &&
                !identical(target, "descriptive"),
              disclaimer = .pps_assessment_disclaimer)
  class(out) <- c("scmix_reporting_gates", "list")
  out
}

#' Collect prespecified specification assessments
#'
#' Assembles independently computed design, benchmark, predictive,
#' calibration, completion, numerical, and reporting objects. If a fitted
#' covariance is available, it also constructs the rank-interiority gate.
#' The collector deliberately does not convert diagnostic success into a claim
#' that maintained structural assumptions have been verified.
#'
#' @param fit Optional mixed-logit fit used only to obtain covariance and q for
#'   a rank gate.
#' @param design_audit,design_benchmarks,discrepancies,heldout_scores,
#'   calibration,completion,local_information,profiles,numerical,sensitivity,
#'   reporting Precomputed assessment objects.
#' @param inference Paper-aligned inference result.
#' @param optimization Optimization audit.
#' @param q,eigenvalue_margin Optional rank-gate settings.
#' @param information_eigenvalue_margin Prespecified strictly positive minimum
#'   acceptable structural-norm generalized information eigenvalue.
#' @param calibration_margins Named positive materiality thresholds `marginal`
#'   and `joint` for the maximum absolute held-out calibration gaps.
#' @param required_reporting Prespecified nonempty names of all quantities for
#'   which regular structural reporting is requested.
#' @param required_discrepancies Prespecified names of all on-support
#'   structural--design comparisons. `character(0)` explicitly declares none;
#'   `NULL` leaves scope undeclared and fails closed.
#' @param discrepancy_tolerances Named positive materiality thresholds for every
#'   required structural--design discrepancy.
#' @details When `fit` is supplied, its nonempty `analysis_signature` must match
#'   the inference, optimization, and numerical artifacts. Every supplied
#'   held-out score and calibration object must carry the same nonempty
#'   signature. The status
#'   `"conditional_available"` authorizes only explicitly conditional claims;
#'   it never says the high-level assumptions were verified.
#' @return An internal `scmix_assessment` object. `protocol_complete` records
#'   execution only; `structural_reporting_ready` additionally applies all
#'   prespecified materiality and reporting gates. Neither verifies maintained
#'   assumptions.
#' @export
scmix_assess <- function(fit = NULL, design_audit = NULL,
                         design_benchmarks = NULL, discrepancies = NULL,
                         heldout_scores = NULL, calibration = NULL,
                         completion = NULL, local_information = NULL,
                         profiles = NULL, numerical = NULL,
                         sensitivity = NULL, reporting = NULL,
                         inference = NULL, optimization = NULL, q = NULL,
                         eigenvalue_margin = NULL,
                         information_eigenvalue_margin = NULL,
                         calibration_margins = NULL,
                         required_reporting = NULL,
                         required_discrepancies = NULL,
                         discrepancy_tolerances = NULL) {
  rank <- NULL
  if (!is.null(fit)) {
    mm <- tryCatch(.pps_extract_mu(fit), error = function(e) NULL)
    if (!is.null(mm)) {
      ss <- tryCatch(.pps_extract_sigma(fit, ncol(mm$value)),
                     error = function(e) NULL)
      q_use <- q %||% fit$q
      if (!is.null(ss) && !is.null(q_use)) {
        scale <- fit$sd_dx_full %||% fit$sd_dx
        rank <- tryCatch(
          scmix_rank_gate(ss$value, q_use, eigenvalue_margin,
                          structural_scale = scale),
          error = function(e) list(regular_inference = FALSE,
                                   status = conditionMessage(e),
                                   error = TRUE))
      }
    }
  }
  components <- list(
    design_audit = design_audit,
    design_benchmarks = design_benchmarks,
    structural_design_discrepancies = discrepancies,
    heldout_scores = heldout_scores,
    calibration = calibration,
    completion = completion,
    local_information = local_information,
    profiles = profiles,
    rank = rank,
    optimization = optimization,
    numerical = numerical,
    sensitivity = sensitivity,
    reporting = reporting,
    inference = inference
  )
  every_inherits <- function(x, cls) {
    if (inherits(x, cls)) return(TRUE)
    is.list(x) && length(x) > 0L &&
      all(vapply(x, inherits, logical(1L), what = cls))
  }
  as_object_list <- function(x, cls, required_names = NULL) {
    if (inherits(x, cls)) {
      ans <- list(x)
      if (!is.null(required_names) && length(required_names) == 1L) {
        names(ans) <- required_names
      } else {
        names(ans) <- ".unnamed"
      }
      return(ans)
    }
    if (is.list(x)) return(x)
    list()
  }
  valid_scope <- function(x, allow_empty = FALSE) {
    is.character(x) && !anyNA(x) && !anyDuplicated(x) &&
      all(nzchar(x)) && (allow_empty || length(x) > 0L)
  }
  if (!is.null(calibration_margins) &&
      (is.null(names(calibration_margins)) ||
       !all(c("marginal", "joint") %in% names(calibration_margins)) ||
       any(!is.finite(calibration_margins[c("marginal", "joint")])) ||
       any(calibration_margins[c("marginal", "joint")] <= 0))) {
    stop("`calibration_margins` must contain named, finite, strictly positive `marginal` and `joint` thresholds.",
         call. = FALSE)
  }
  if (!is.null(discrepancy_tolerances) &&
      (is.null(names(discrepancy_tolerances)) ||
       any(!is.finite(discrepancy_tolerances)) ||
       any(discrepancy_tolerances <= 0))) {
    stop("`discrepancy_tolerances` must be named, finite, and strictly positive.",
         call. = FALSE)
  }
  reporting_objects <- as_object_list(reporting, "scmix_reporting_gates",
                                      required_reporting)
  discrepancy_objects <- as_object_list(
    discrepancies, "scmix_structural_design_discrepancy",
    required_discrepancies)
  information_objects <- as_object_list(local_information,
                                        "scmix_local_information")
  no_discrepancy_scope <- valid_scope(required_discrepancies,
                                      allow_empty = TRUE) &&
    length(required_discrepancies) == 0L
  executed <- c(
    design_audit = inherits(design_audit, "scmix_design_audit"),
    design_benchmarks = no_discrepancy_scope ||
      every_inherits(design_benchmarks, "scmix_design_benchmark"),
    structural_design_discrepancies = no_discrepancy_scope ||
      every_inherits(discrepancies, "scmix_structural_design_discrepancy"),
    heldout_scores = every_inherits(heldout_scores, "scmix_heldout_score") &&
      all(vapply(if (inherits(heldout_scores, "scmix_heldout_score"))
        list(heldout_scores) else heldout_scores,
        function(x) isTRUE(x$verified_heldout), logical(1L))),
    calibration = inherits(calibration, "scmix_calibration_assessment") &&
      isTRUE(calibration$verified_heldout) &&
      isTRUE(calibration$joint_checks_supplied),
    completion = inherits(completion, "scmix_completion_assessment"),
    local_information = every_inherits(local_information,
                                       "scmix_local_information") &&
      all(vapply(information_objects,
                 function(x) isTRUE(x$verified_source), logical(1L))),
    profiles = every_inherits(profiles,
                              "scmix_profile_sequence_likelihood") &&
      all(vapply(if (inherits(profiles, "scmix_profile_sequence_likelihood"))
        list(profiles) else profiles,
        function(x) isTRUE(x$verified_profile), logical(1L))),
    rank = inherits(rank, "scmix_rank_gate"),
    optimization = inherits(optimization, "scmix_optimization_audit"),
    numerical = inherits(numerical, "scmix_numerical_gate") ||
      (inherits(numerical, "scmix_integration_refinement") &&
         inherits(numerical$gate, "scmix_numerical_gate")),
    sensitivity = inherits(sensitivity, "scmix_structural_sensitivity") &&
      isTRUE(sensitivity$complete),
    reporting = every_inherits(reporting, "scmix_reporting_gates"),
    inference = inherits(inference, "scmix_dml")
  )
  status <- data.frame(
    component = names(components),
    supplied = !vapply(components, is.null, logical(1L)),
    executed = unname(executed[names(components)]),
    stringsAsFactors = FALSE
  )
  status$state <- ifelse(status$executed, "executed with required provenance",
                         ifelse(status$supplied,
                                "supplied but incomplete or unverified",
                                "not run"))
  protocol_complete <- all(status$executed)
  inference_available <- inherits(inference, "scmix_dml") &&
    isTRUE(inference$inference_available) &&
    (identical(inference$status, "available") ||
       identical(inference$status, "conditional_available"))
  inference_claim <- if (inherits(inference, "scmix_dml") &&
                          identical(inference$status,
                                    "conditional_available")) {
    "conditional_on_documented_high_level_assumptions"
  } else if (inherits(inference, "scmix_dml")) {
    inference$inference_claim %||%
      "available_under_stated_regular_inference_conditions"
  } else "not available"
  optimization_ok <- !is.null(optimization) &&
    isTRUE(optimization$all_selected_tolerances_met) &&
    isTRUE(optimization$all_computational_gates_pass) &&
    !isTRUE(optimization$any_bound_activity)
  numerical_gate <- if (inherits(numerical, "scmix_integration_refinement"))
    numerical$gate else numerical
  gate_rows <- list()
  add_assessment_gate <- function(gate, pass, reason, value = NA_real_,
                                  margin = NA_real_) {
    gate_rows[[length(gate_rows) + 1L]] <<- data.frame(
      gate = gate, pass = isTRUE(pass), value = value, margin = margin,
      reason = reason, stringsAsFactors = FALSE)
  }
  add_assessment_gate(
    "paper-aligned inference", inference_available,
    if (identical(inference$status, "conditional_available") &&
        inference_available) {
      "conditionally available under documented high-level assumptions; those assumptions are not verified by this assessment"
    } else if (inference_available) {
      "regular-inference status available under the stated conditions"
    } else "paper-aligned regular inference unavailable")

  nonempty_signature <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  fit_supplied <- !is.null(fit)
  fit_signature <- if (is.list(fit)) fit$analysis_signature else NULL
  required_signature_values <- list(
    fit = fit_signature,
    inference = if (is.list(inference)) inference$analysis_signature else NULL,
    optimization = if (is.list(optimization)) optimization$analysis_signature else NULL,
    numerical = if (is.list(numerical)) numerical$analysis_signature else NULL
  )
  missing_signature <- names(required_signature_values)[
    !vapply(required_signature_values, nonempty_signature, logical(1L))]
  mismatched_signature <- if (nonempty_signature(fit_signature)) {
    names(required_signature_values)[vapply(required_signature_values, function(x) {
      nonempty_signature(x) && !identical(x, fit_signature)
    }, logical(1L))]
  } else character()
  score_objects <- if (inherits(heldout_scores, "scmix_heldout_score")) {
    list(heldout_scores)
  } else if (is.list(heldout_scores)) heldout_scores else list()
  if (length(score_objects) &&
      (is.null(names(score_objects)) || any(names(score_objects) == ""))) {
    names(score_objects) <- paste0("heldout_score_", seq_along(score_objects))
  }
  linked_artifacts <- score_objects
  if (!is.null(calibration)) {
    calibration_name <- "calibration"
    if (calibration_name %in% names(linked_artifacts)) {
      calibration_name <- ".calibration"
    }
    linked_artifacts[[calibration_name]] <- calibration
  }
  linked_signatures <- lapply(linked_artifacts, function(x) {
    if (is.list(x)) x$analysis_signature else NULL
  })
  missing_linked_signature <- names(linked_signatures)[
    !vapply(linked_signatures, nonempty_signature, logical(1L))]
  mismatched_linked_signature <- if (nonempty_signature(fit_signature)) {
    names(linked_signatures)[vapply(linked_signatures, function(x) {
      nonempty_signature(x) && !identical(x, fit_signature)
    }, logical(1L))]
  } else character()
  optional_signature_ok <- !fit_supplied ||
    (!length(missing_linked_signature) &&
       !length(mismatched_linked_signature))
  nested_signature_ok <- (!inherits(numerical, "scmix_integration_refinement") ||
    (isTRUE(numerical$signature_match) &&
       !is.null(numerical$refit_analysis_signatures) &&
       length(numerical$refit_analysis_signatures) > 0L &&
       all(vapply(as.list(numerical$refit_analysis_signatures),
                  nonempty_signature, logical(1L))) &&
       all(numerical$refit_analysis_signatures == fit_signature))) &&
    (!inherits(optimization, "scmix_optimization_audit") ||
       isTRUE(optimization$signature_match))
  signature_ok <- !fit_supplied ||
    (!length(missing_signature) && !length(mismatched_signature) &&
       optional_signature_ok && nested_signature_ok)
  signature_reason <- if (!fit_supplied) {
    "no fitted analysis was supplied; signature linkage is not applicable"
  } else if (length(missing_signature)) {
    paste0("missing nonempty analysis_signature for: ",
           paste(missing_signature, collapse = ", "))
  } else if (length(mismatched_signature)) {
    paste0("analysis_signature mismatch for: ",
           paste(mismatched_signature, collapse = ", "))
  } else if (length(missing_linked_signature)) {
    paste0("missing nonempty analysis_signature for supplied fit-aware artifact(s): ",
           paste(missing_linked_signature, collapse = ", "))
  } else if (length(mismatched_linked_signature)) {
    paste0("fit-aware artifact analysis_signature mismatch for: ",
           paste(mismatched_linked_signature, collapse = ", "))
  } else if (!nested_signature_ok) {
    "an optimization or integration-refinement signature record is incomplete or mismatched"
  } else "all required analysis artifacts share the fitted analysis_signature"
  add_assessment_gate("analysis artifact signatures", signature_ok,
                      signature_reason)
  add_assessment_gate(
    "identification-audit applicability",
    inherits(design_audit, "scmix_design_audit") && isTRUE(design_audit$established),
    if (inherits(design_audit, "scmix_design_audit")) design_audit$status else
      "design audit missing")
  add_assessment_gate(
    "rank interiority", inherits(rank, "scmix_rank_gate") &&
      isTRUE(rank$regular_inference),
    if (inherits(rank, "scmix_rank_gate")) rank$status else "rank gate missing")
  add_assessment_gate(
    "optimization", optimization_ok,
    if (optimization_ok) "optimization diagnostics passed" else
      "optimization diagnostics missing or failed")
  add_assessment_gate(
    "numerical refinement", inherits(numerical_gate, "scmix_numerical_gate") &&
      isTRUE(numerical_gate$pass),
    if (inherits(numerical_gate, "scmix_numerical_gate")) numerical_gate$status else
      "numerical-refinement gate missing")

  info_margin_ok <- is.numeric(information_eigenvalue_margin) &&
    length(information_eigenvalue_margin) == 1L &&
    is.finite(information_eigenvalue_margin) &&
    information_eigenvalue_margin > 0
  info_values <- if (length(information_objects))
    vapply(information_objects, function(x) x$smallest %||% NA_real_, numeric(1L)) else
      numeric()
  info_ok <- length(info_values) > 0L && info_margin_ok &&
    all(is.finite(info_values)) &&
    all(vapply(information_objects, function(x) isTRUE(x$verified_source),
               logical(1L))) &&
    min(info_values) >= information_eigenvalue_margin
  add_assessment_gate(
    "local structural information", info_ok,
    if (info_ok) "all verified generalized information eigenvalues exceed the prespecified margin" else
      "verified local-information objects and a positive prespecified margin are required",
    value = if (length(info_values) && any(is.finite(info_values)))
      min(info_values[is.finite(info_values)]) else NA_real_,
    margin = if (info_margin_ok) information_eigenvalue_margin else NA_real_)

  cal_margin_ok <- !is.null(calibration_margins) &&
    all(c("marginal", "joint") %in% names(calibration_margins))
  max_gap <- function(tab) {
    if (!is.data.frame(tab) || !"gap" %in% names(tab) || !nrow(tab) ||
        any(!is.finite(tab$gap))) return(NA_real_)
    max(abs(tab$gap))
  }
  marginal_gap <- if (inherits(calibration, "scmix_calibration_assessment"))
    max_gap(calibration$marginal) else NA_real_
  joint_gap <- if (inherits(calibration, "scmix_calibration_assessment"))
    max_gap(calibration$joint) else NA_real_
  calibration_ok <- inherits(calibration, "scmix_calibration_assessment") &&
    isTRUE(calibration$verified_heldout) &&
    isTRUE(calibration$joint_checks_supplied) && cal_margin_ok &&
    is.finite(marginal_gap) && is.finite(joint_gap) &&
    marginal_gap <= calibration_margins[["marginal"]] &&
    joint_gap <= calibration_margins[["joint"]]
  add_assessment_gate(
    "held-out calibration materiality", calibration_ok,
    if (calibration_ok) "marginal and joint gaps are within prespecified thresholds" else
      "verified held-out marginal and joint calibration must satisfy prespecified thresholds",
    value = if (any(is.finite(c(marginal_gap, joint_gap))))
      max(c(marginal_gap, joint_gap), na.rm = TRUE) else NA_real_,
    margin = if (cal_margin_ok) max(calibration_margins[c("marginal", "joint")]) else
      NA_real_)

  sensitivity_status_ok <- inherits(sensitivity, "scmix_structural_sensitivity") &&
    is.data.frame(sensitivity$status) && nrow(sensitivity$status) == 13L &&
    all(c("implemented", "justified_not_applicable", "status",
          "materiality_evidence", "materiality_pass") %in%
          names(sensitivity$status)) &&
    all(sensitivity$status$implemented |
          sensitivity$status$justified_not_applicable) &&
    !any(sensitivity$status$status %in% c("run_sensitive", "run_fail")) &&
    isTRUE(sensitivity$distinct_q_values) &&
    isTRUE(sensitivity$materiality_complete) &&
    all(!sensitivity$status$implemented |
          (sensitivity$status$materiality_evidence &
             sensitivity$status$materiality_pass))
  sensitivity_shape_ok <- inherits(sensitivity, "scmix_structural_sensitivity") &&
    is.list(sensitivity$shape_standardization) &&
    all(c("skewed", "bimodal", "heavy_tail") %in%
          names(sensitivity$shape_standardization)) &&
    all(vapply(sensitivity$shape_standardization[
      c("skewed", "bimodal", "heavy_tail")], function(x) {
        is.list(x) && all(c("mean_zero", "unit_covariance",
                            "finite_covariance",
                            "factor_orientation_prespecified") %in% names(x)) &&
          all(vapply(x[c("mean_zero", "unit_covariance", "finite_covariance",
                         "factor_orientation_prespecified")],
                     isTRUE, logical(1L)))
      }, logical(1L)))
  sensitivity_ok <- sensitivity_status_ok && sensitivity_shape_ok &&
    isTRUE(sensitivity$prespecified) && isTRUE(sensitivity$complete) &&
    isTRUE(sensitivity$substantive_pass)
  add_assessment_gate(
    "prespecified structural sensitivity", sensitivity_ok,
    if (isTRUE(sensitivity_ok)) "complete battery has no failed or materially sensitive result" else
      "battery is incomplete, failed, materially sensitive, or lacks required shape standardization")

  reporting_scope_ok <- valid_scope(required_reporting, allow_empty = FALSE)
  reporting_ok <- reporting_scope_ok &&
    all(required_reporting %in% names(reporting_objects)) &&
    all(vapply(reporting_objects[required_reporting], function(x)
      inherits(x, "scmix_reporting_gates") &&
        isTRUE(x$regular_reporting_approved), logical(1L)))
  add_assessment_gate(
    "all requested quantity-specific reporting gates", reporting_ok,
    if (reporting_ok) "every prespecified quantity passed its reporting gate" else
      "required quantity scope is undeclared, incomplete, or contains a failed gate")

  discrepancy_scope_ok <- valid_scope(required_discrepancies, allow_empty = TRUE)
  if (discrepancy_scope_ok && length(required_discrepancies) == 0L) {
    discrepancy_ok <- TRUE
    discrepancy_reason <- "protocol declares no applicable on-support structural comparisons"
  } else {
    tolerance_scope_ok <- discrepancy_scope_ok &&
      !is.null(discrepancy_tolerances) &&
      all(required_discrepancies %in% names(discrepancy_tolerances))
    discrepancy_ok <- tolerance_scope_ok &&
      all(required_discrepancies %in% names(discrepancy_objects)) &&
      all(vapply(required_discrepancies, function(nm) {
        x <- discrepancy_objects[[nm]]
        inherits(x, "scmix_structural_design_discrepancy") &&
          identical(x$structural_source, "scmix_dml") &&
          (identical(x$structural_inference_status, "available") ||
             identical(x$structural_inference_status,
                       "conditional_available")) &&
          isTRUE(x$structural_target_inference_available) &&
          identical(x$inference, "joint respondent influence") &&
          is.finite(x$estimate) && is.finite(x$se) && x$se > 0 &&
          abs(x$estimate) <= discrepancy_tolerances[[nm]]
      }, logical(1L)))
    discrepancy_reason <- if (discrepancy_ok) {
      "all prespecified joint discrepancies are within materiality thresholds"
    } else {
      "discrepancy scope/tolerances are undeclared, joint inference is unavailable, or a material discrepancy is present"
    }
  }
  add_assessment_gate("on-support structural--design discrepancies",
                      discrepancy_ok, discrepancy_reason)
  assessment_gates <- do.call(rbind, gate_rows)
  structural_reporting_ready <- protocol_complete &&
    all(assessment_gates$pass)
  out <- list(design_audit = design_audit,
              design_benchmarks = design_benchmarks,
              structural_design_discrepancies = discrepancies,
              heldout_scores = heldout_scores,
              calibration = calibration,
              completion = completion,
              local_information = local_information,
              profiles = profiles,
              rank = rank,
              optimization = optimization,
              numerical = numerical,
              sensitivity = sensitivity,
              reporting = reporting,
              inference = inference,
              component_status = status,
              assessment_gates = assessment_gates,
              analysis_signature = if (nonempty_signature(fit_signature))
                fit_signature else NA_character_,
              signature_match = signature_ok,
              inference_status = if (inherits(inference, "scmix_dml"))
                inference$status %||% "unknown" else "not supplied",
              inference_claim = inference_claim,
              protocol_complete = protocol_complete,
              complete = protocol_complete,
              structural_reporting_ready = structural_reporting_ready,
              maintained_assumptions_verified = FALSE,
              disclaimer = .pps_assessment_disclaimer)
  class(out) <- c("scmix_assessment", "list")
  out
}

#' @export
print.scmix_design_audit <- function(x, ...) {
  cat("paperps finite-design identification audit\n")
  cat("  status:", x$status, "\n")
  cat("  affine rank:", x$affine_rank, "/", x$affine_required, "\n")
  cat("  covariance check:", x$covariance_method, "\n")
  cat("  protocol support:", x$protocol$note, "\n")
  invisible(x)
}

#' @export
as.data.frame.scmix_design_audit <- function(x, ...) {
  data.frame(condition = names(x$conditions),
             established = unname(x$conditions),
             stringsAsFactors = FALSE)
}

#' @export
print.scmix_reporting_gates <- function(x, ...) {
  cat("paperps quantity-specific reporting gates\n")
  cat("  target:", x$target, "\n")
  cat("  regular reporting:",
      if (isTRUE(x$regular_reporting_approved)) "approved" else "withheld",
      "\n")
  if (nrow(x$gates)) print(x$gates, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.scmix_reporting_gates <- function(x, ...) x$gates

#' @export
print.scmix_assessment <- function(x, ...) {
  cat("paperps specification assessment\n")
  cat("  protocol complete:",
      if (isTRUE(x$protocol_complete)) "yes" else "no", "\n")
  cat("  structural reporting ready:",
      if (isTRUE(x$structural_reporting_ready)) "yes" else "no", "\n")
  cat("  inference claim:", x$inference_claim %||% "not available", "\n")
  print(x$component_status, row.names = FALSE)
  if (!is.null(x$assessment_gates)) print(x$assessment_gates, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.scmix_assessment <- function(x, ...) x$component_status
