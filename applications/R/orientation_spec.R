## Prespecified reporting orientation for bound rows (audit work package 3).
##
## THE PROBLEM THIS REMOVES. A one-sided share bound reports the mass on
## ONE side of zero. Until now the runners picked that side from the data:
## from the sign of the one-step theta where one existed, and otherwise
## from the modal sign of the fitted respondent means. Both are functions
## of the same fit the bound is computed from, so the reported side was
## selected on the data the interval is meant to cover, with no sample
## split and no selective-inference adjustment. That is a defect no
## relabelling fixes.
##
## THE FIX. The side comes from the substantive definition of the
## contrast, written down here, before any fit is consulted. A row whose
## side is not prespecified is not displayed --- prespecification is a
## precondition for release, never a consequence of the estimate.
##
## WHAT IS AND IS NOT PRESPECIFIABLE. A direction is prespecifiable when
## the experiment's own design fixes it. Opposition to an undemocratic
## action is prespecifiable: the design builds those levels as violations.
## Progressivity is prespecifiable: `br_progressivity_contrasts()` defines
## its contrasts so that positive means progressive. A per-bracket rate
## coefficient is NOT: the paper's substantive claim is about the SHAPE of
## the schedule across brackets, not about the sign of any one bracket.
## Whether people prefer more revenue raised is the empirical question
## itself, so it has no prior direction either. Those rows are left
## unspecified and therefore withheld. Adding them later is an author
## decision about substance, not a code change.
##
## `orientation_side` is the side of zero whose share is reported:
##   "negative" -> report P(c' beta < 0)
##   "positive" -> report P(c' beta > 0)
##
## `orientation_source` is one of:
##   "prespecified"        from the definitions below. The only source a
##                         displayed bound may have.
##   "onestep_theta_sign"  legacy, data-selected. Diagnostic only.
##   "fitted_mean_mode"    legacy, data-selected. Diagnostic only.
##   "unspecified"         no side was declared. Withheld.

ORIENT_SOURCES <- c("prespecified", "onestep_theta_sign",
                    "fitted_mean_mode", "unspecified")

#' Build an orientation specification table
#'
#' @param ... one or more `orient_row()` results, or a data frame with the
#'   same columns.
#' @return A data frame with `name`, `orientation_side`,
#'   `orientation_source`, `sign_margin`, `rationale`.
orient_row <- function(name, side, rationale, sign_margin = NA_real_,
                       source = "prespecified") {
  side <- match.arg(side, c("negative", "positive"))
  if (!source %in% ORIENT_SOURCES) {
    stop("Unknown orientation_source: ", source, call. = FALSE)
  }
  if (!nzchar(rationale)) {
    stop("Every prespecified orientation needs a rationale naming the ",
         "substantive definition it comes from.", call. = FALSE)
  }
  data.frame(name = name, orientation_side = side,
             orientation_source = source,
             sign_margin = as.numeric(sign_margin),
             rationale = rationale, stringsAsFactors = FALSE)
}

orient_spec <- function(...) {
  rows <- list(...)
  rows <- lapply(rows, function(r) if (is.data.frame(r)) r else
    stop("orient_spec() takes orient_row() results.", call. = FALSE))
  out <- do.call(rbind, rows)
  if (anyDuplicated(out$name)) {
    stop("Duplicate orientation entries: ",
         paste(unique(out$name[duplicated(out$name)]), collapse = ", "),
         call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

#' Look a name up in a specification, returning the unspecified row when
#' it is absent. Never guesses.
orient_lookup <- function(spec, name) {
  unspec <- list(orientation_side = NA_character_,
                 orientation_source = "unspecified",
                 sign_margin = NA_real_,
                 rationale = NA_character_)
  if (is.null(spec) || !nrow(spec)) return(unspec)
  i <- match(name, spec$name)
  if (is.na(i)) return(unspec)
  list(orientation_side = as.character(spec$orientation_side[i]),
       orientation_source = as.character(spec$orientation_source[i]),
       sign_margin = as.numeric(spec$sign_margin[i]),
       rationale = as.character(spec$rationale[i]))
}

#' Hard gate: a runner calls this before writing a manuscript-facing
#' artifact. Any row that would be DISPLAYED must carry a prespecified
#' orientation.
orient_require_prespecified <- function(tb, displayed = NULL,
                                        what = "artifact") {
  if (is.null(tb) || !nrow(tb)) return(invisible(TRUE))
  if (is.null(displayed)) {
    displayed <- if ("bound_release" %in% names(tb))
      tb$bound_release == "conditional_sensitivity" else
        rep(TRUE, nrow(tb))
    displayed[is.na(displayed)] <- FALSE
  }
  if (!"orientation_source" %in% names(tb)) {
    stop("Cannot write ", what, ": the table has no `orientation_source` ",
         "column, so prespecification cannot be checked.", call. = FALSE)
  }
  bad <- which(displayed & tb$orientation_source != "prespecified")
  if (length(bad)) {
    nm <- if ("coordinate" %in% names(tb)) tb$coordinate[bad] else bad
    stop("Cannot write ", what, ": ", length(bad), " displayed row(s) lack ",
         "a prespecified orientation (", paste(nm, collapse = ", "),
         "). Declare the side in the application's orientation spec, or ",
         "the row must not be displayed.", call. = FALSE)
  }
  invisible(TRUE)
}


## ---------------------------------------------------------------------
## Ballard-Rosa (br2017)
## ---------------------------------------------------------------------
##
## `br_progressivity_contrasts()` already builds its contrasts so that a
## POSITIVE value means a more progressive schedule: top_minus_bottom is
## rate_375P - rate_L10, and the two slope contrasts are the OLS slope of
## the six coefficients on bracket rank. The submitted draft's claims are
## exactly these two directions ("93 percent have a positive progressivity
## slope"; "91 percent tax the top bracket more than the bottom"), so the
## side is fixed by the contrast definition, not by the fit.
##
## `mean_rate` is the LEVEL of the schedule, not a progressivity contrast.
## The draft makes no directional claim about it, so it stays unspecified.
BR_CONTRAST_ORIENTATION <- orient_spec(
  orient_row("top_minus_bottom", "positive",
             paste("br_progressivity_contrasts() defines it as",
                   "rate_375P - rate_L10; positive = taxes the top bracket",
                   "more than the bottom, the submitted draft's claim")),
  orient_row("slope", "positive",
             paste("OLS slope of the six bracket coefficients on bracket",
                   "rank; positive = progressive, the submitted draft's",
                   "claim")),
  orient_row("slope_unit", "positive",
             paste("the same direction as `slope`, rescaled to unit norm;",
                   "the share is invariant to positive rescaling")))

## Native BR coordinates: none. The progressivity story constrains the
## SHAPE across brackets, not the sign of any single bracket, and whether
## respondents prefer more revenue raised is the empirical question. Left
## empty deliberately, so those rows are withheld rather than oriented
## from their own estimates.
BR_COORDINATE_ORIENTATION <- BR_CONTRAST_ORIENTATION[0L, , drop = FALSE]


## ---------------------------------------------------------------------
## Graham--Svolik (gs2020)
## ---------------------------------------------------------------------
##
## The experiment builds the `u_*` levels as undemocratic actions and the
## `v_*` levels as valence violations (an extramarital affair, a tax
## problem). The design's premise is that respondents oppose both, and the
## paper's question is how much co-partisanship offsets that opposition.
## Opposition is therefore prespecifiable: report the share whose
## coefficient on the violation is NEGATIVE.
##
## `diff_respParty` is prespecified positive for the same reason: the
## paper's premise is that voters favour the co-partisan. (It is
## point-identified on these fits, so it is not displayed as a bound.)
##
## The `g_*` good-governance codes are NOT prespecified: each compares one
## good-governance code with another, so the design fixes no direction
## between them. Policy positions depend on the respondent's own ideology.
## Sex, race, and profession have no design-level prior direction.
.gs_u <- c("banProtest", "court", "execRule", "gerry2", "gerry10",
           "journalists", "limitVote")
.gs_v <- c("affair", "tax")

GS_COORDINATE_ORIENTATION <- do.call(orient_spec, c(
  lapply(.gs_u, function(a)
    orient_row(paste0("diff_dem_code_u_", a), "negative",
               paste0("the experiment builds '", a, "' as an undemocratic ",
                      "action; the reported share is opposition to it"))),
  lapply(.gs_v, function(a)
    orient_row(paste0("diff_dem_code_v_", a), "negative",
               paste0("the experiment builds '", a, "' as a valence ",
                      "violation; the reported share is opposition to it"))),
  list(orient_row("diff_respParty", "positive",
                  paste("the paper's premise is that voters favour the",
                        "co-partisan candidate")))))

#' The orientation spec for one application, by name.
orient_spec_for <- function(app, kind = c("coordinate", "contrast")) {
  kind <- match.arg(kind)
  key <- paste(app, kind, sep = ":")
  switch(key,
         "br2017:contrast" = BR_CONTRAST_ORIENTATION,
         "br2017:coordinate" = BR_COORDINATE_ORIENTATION,
         "gs2020:coordinate" = GS_COORDINATE_ORIENTATION,
         ## No spec on file. Returning an empty spec withholds every row,
         ## which is the correct default for an application nobody has
         ## prespecified yet.
         BR_CONTRAST_ORIENTATION[0L, , drop = FALSE])
}
