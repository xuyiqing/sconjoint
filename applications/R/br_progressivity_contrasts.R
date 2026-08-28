## Ballard-Rosa (br2017) tax-progressivity contrasts for
## applications/R/contrast_bounds.R.
##
## PROVENANCE OF THE COORDINATE NAMES. Not invented, and not positional.
## The six rate-bracket coordinates and the revenue coordinate are named
## identically in four independent places in this repo and its archive:
##   - man/br2017.Rd / R/data.R: "rate_L10, rate_10_35, rate_35_85,
##     rate_85_175, rate_175_375, rate_375P: Marginal tax rate (percent)
##     for the six income brackets."
##   - data-raw/build_br2017.R (`rate_cols`), and the model formula in
##     tutorial/04-example-br.qmd and tutorial/10-mixed-logit.qmd:
##     choice ~ rate_L10 + rate_10_35 + rate_35_85 + rate_85_175 +
##              rate_175_375 + rate_375P + revenue_score | ...
##   - the archived per-coordinate bounds table
##     2608_issues/Yiqing/bound_for_share/results/share_bounds_br2017.csv,
##     whose `coordinate` column is exactly these seven strings.
## Bracket order is low income -> high income, matching the v2.1 one-step
## thetas -0.049 / -0.035 / -0.021 / -0.001 / +0.006 / +0.011.
##
## Each coordinate is the effect of a ONE-PERCENTAGE-POINT increase in
## that bracket's marginal rate on the choice index. A progressive
## respondent dislikes rate increases at the bottom and tolerates or
## favours them at the top, so a progressivity contrast is INCREASING in
## bracket rank.
##
## Every contrast below is built by name lookup against the fit's
## attr_names at runtime, with a hard stopifnot on any missing name.

## Low -> high. This ordering is load-bearing for the slope contrasts.
BR_RATE_BRACKETS <- c("rate_L10", "rate_10_35", "rate_35_85",
                      "rate_85_175", "rate_175_375", "rate_375P")

## Bracket midpoints in thousands of dollars, for the optional
## log-midpoint spacing. The top bracket is open-ended ("Over $375k"), so
## its midpoint is a modelling choice, not a datum --- which is why the
## DEFAULT spacing is the bracket RANK, which needs no such choice.
BR_RATE_MIDPOINT_K <- c(rate_L10 = 5, rate_10_35 = 22.5, rate_35_85 = 60,
                        rate_85_175 = 130, rate_175_375 = 275,
                        rate_375P = 500)

## Hard name check. Returns the requested names, in the requested order.
br_require_coords <- function(attr_names, needed = BR_RATE_BRACKETS) {
  attr_names <- as.character(attr_names)
  missing_nm <- setdiff(needed, attr_names)
  if (length(missing_nm)) {
    stop("br2017 contrast construction: coordinate(s) not found in the ",
         "fit's attr_names: ", paste(missing_nm, collapse = ", "),
         ".\n  Fit has: ", paste(attr_names, collapse = ", "),
         call. = FALSE)
  }
  stopifnot(`every br rate-bracket coordinate must be present in the fit` =
              all(needed %in% attr_names))
  needed
}

## The bracket positions used by the slope contrasts.
##   "rank"          x = 1..6, the bracket's ordinal position. Default.
##                   No assumption about the open-ended top bracket.
##   "log_midpoint"  x = log10(midpoint in $k). Records an arbitrary
##                   $500k midpoint for the open top bracket.
br_bracket_positions <- function(spacing = c("rank", "log_midpoint"),
                                 brackets = BR_RATE_BRACKETS) {
  spacing <- match.arg(spacing)
  if (spacing == "rank") {
    stats::setNames(seq_along(brackets), brackets)
  } else {
    missing_nm <- setdiff(brackets, names(BR_RATE_MIDPOINT_K))
    if (length(missing_nm)) {
      stop("No midpoint recorded for: ", paste(missing_nm, collapse = ", "),
           call. = FALSE)
    }
    stats::setNames(log10(BR_RATE_MIDPOINT_K[brackets]), brackets)
  }
}

## Build the contrast list.
##
## Returned contrasts (all named numeric vectors over the six brackets;
## all sum to zero except `mean_rate`):
##
##   top_minus_bottom   rate_375P - rate_L10. The simplest progressivity
##                      statement: does the respondent prefer a higher
##                      marginal rate on the top bracket than on the
##                      bottom one? Positive = progressive.
##
##   slope              OLS slope of the respondent's six coefficients on
##                      bracket position: w_j = (x_j - xbar) / sum_l
##                      (x_l - xbar)^2. c' beta_i is then literally the
##                      per-bracket-step slope of respondent i's fitted
##                      rate schedule, in coefficient units per step.
##                      Positive = progressive.
##
##   slope_unit         The same DIRECTION, rescaled to ||c||_2 = 1. The
##                      sign share is invariant to positive rescaling of
##                      c, so `slope` and `slope_unit` must return
##                      identical bounds; they differ only in the reported
##                      mean / dispersion / ceiling columns, which are in
##                      interpretable units for `slope` and in comparable
##                      units for `slope_unit`. Included as a live
##                      invariance check on real data.
##
##   mean_rate          Equal-weight average of the six brackets: the
##                      LEVEL of the rate schedule, the companion to its
##                      slope. Not a progressivity contrast; supplied so
##                      level and slope can be read together.
##
## `extra` accepts further named numeric vectors (sparse, named by
## coordinate) to append --- e.g. list(top_minus_middle = c(rate_375P = 1,
## rate_35_85 = -1)).
br_progressivity_contrasts <- function(attr_names,
                                       spacing = c("rank", "log_midpoint"),
                                       brackets = BR_RATE_BRACKETS,
                                       include = c("top_minus_bottom",
                                                   "slope", "slope_unit",
                                                   "mean_rate"),
                                       extra = NULL) {
  spacing <- match.arg(spacing)
  brackets <- br_require_coords(attr_names, brackets)
  nb <- length(brackets)
  if (nb < 2L) stop("Need at least two brackets.", call. = FALSE)

  x <- br_bracket_positions(spacing, brackets)
  dev <- x - mean(x)
  ss <- sum(dev^2)
  if (!is.finite(ss) || ss <= 0) {
    stop("Bracket positions are degenerate; no slope contrast exists.",
         call. = FALSE)
  }
  w_slope <- stats::setNames(as.numeric(dev / ss), brackets)
  w_unit <- stats::setNames(as.numeric(dev / sqrt(ss)), brackets)

  all_contrasts <- list(
    top_minus_bottom = stats::setNames(
      c(-1, 1), c(brackets[[1L]], brackets[[nb]])),
    slope = w_slope,
    slope_unit = w_unit,
    mean_rate = stats::setNames(rep(1 / nb, nb), brackets))

  unknown <- setdiff(include, names(all_contrasts))
  if (length(unknown)) {
    stop("Unknown contrast(s) requested: ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  out <- all_contrasts[include]

  if (!is.null(extra)) {
    if (!is.list(extra) || is.null(names(extra)) ||
        any(!nzchar(names(extra)))) {
      stop("`extra` must be a named list of named numeric vectors.",
           call. = FALSE)
    }
    for (nm in names(extra)) {
      v <- extra[[nm]]
      if (!is.numeric(v) || is.null(names(v))) {
        stop("extra[['", nm, "']] must be a NAMED numeric vector.",
             call. = FALSE)
      }
      br_require_coords(attr_names, names(v))
    }
    dup <- intersect(names(extra), names(out))
    if (length(dup)) {
      stop("`extra` re-defines built-in contrast(s): ",
           paste(dup, collapse = ", "), call. = FALSE)
    }
    out <- c(out, extra)
  }

  attr(out, "spacing") <- spacing
  attr(out, "bracket_positions") <- x
  out
}
