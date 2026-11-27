# Bansak, Hainmueller and Hangartner (2016, Science) asylum-seeker conjoint: design law
# and generator for the RUM conditional information (same API as R/rum-design.R; the
# spec comes from .rum_design_spec_bhh()).
#
# Data: the 2016 wave as released with Bansak, Hainmueller and Hangartner (2023, Nature),
# conjoint_data_2016.csv. 18,030 respondents in 15 countries, five pairs each, nine
# attributes. The articles describe a "fully randomized paired profiles conjoint
# design": each attribute uniform on its levels, independent across attributes,
# profiles and pairs (Nature 2023, Methods and Extended Data Fig. 1). Checked against the
# data in rum_bhh2016_2026-10-23/scripts/D1_design_check.R: every level's share is
# within 0.4 points of 1/L, and sides and attributes look independent.
# Attribute row order (display) was randomized per respondent; it is not in d.
#
# Levels in the authors' order (results_for_figure2.R); the first level is the
# reference. d = x(profile 1) - x(profile 2), y = 1 if profile 1 was chosen.

.RUM_BHH_LEVELS <- list(
  consist = c("No inconsistencies", "Minor inconsistencies", "Major inconsistencies"),
  gender = c("Female", "Male"),
  origin = c("Syria", "Afghanistan", "Kosovo", "Eritrea", "Pakistan", "Ukraine", "Iraq"),
  age = c("21 Years", "38 Years", "62 Years"),
  job = c("Unemployed", "Cleaner", "Farmer", "Accountant", "Teacher", "Doctor"),
  vulner = c("None", "PTSD", "Victim of torture", "No surviving family", "Handicapped"),
  reason = c("Political persecution", "Religious persecution", "Ethnic persecution", "Economic opportunities"),
  religion = c("Christian", "Agnostic", "Muslim"),
  lang = c("Fluent", "Broken", "None"))
.RUM_BHH_FEATURES <- c(
  "consist_Minor", "consist_Major", "gender_Male",
  "origin_Afghanistan", "origin_Kosovo", "origin_Eritrea", "origin_Pakistan", "origin_Ukraine", "origin_Iraq",
  "age_38", "age_62",
  "job_Cleaner", "job_Farmer", "job_Accountant", "job_Teacher", "job_Doctor",
  "vulner_PTSD", "vulner_Torture", "vulner_NoFamily", "vulner_Handicapped",
  "reason_Religious", "reason_Ethnic", "reason_Economic",
  "religion_Agnostic", "religion_Muslim",
  "lang_Broken", "lang_None")
.RUM_BHH_ATTR_OF <- rep(seq_along(.RUM_BHH_LEVELS), lengths(.RUM_BHH_LEVELS) - 1L)
.RUM_BHH_LU <- lapply(.RUM_BHH_LEVELS, function(l) diag(length(l))[, -1L, drop = FALSE])

.rum_bhh_draw <- function(M, n_task = 5L) {
  n2 <- M * 5L * 2L
  raw <- lapply(.RUM_BHH_LEVELS, function(l) array(sample.int(length(l), n2, replace = TRUE), c(M, 5L, 2L)))
  .rum_raw_head(raw, n_task)
}

.rum_bhh_x <- function(raw, side) {
  do.call(cbind, lapply(names(.RUM_BHH_LEVELS), function(a)
    .RUM_BHH_LU[[a]][as.vector(raw[[a]][, , side]), , drop = FALSE]))
}

.rum_bhh_code <- function(raw, key = NULL) {
  M <- dim(raw$consist)[1L]; n_task <- dim(raw$consist)[2L]
  d <- .rum_bhh_x(raw, 1L) - .rum_bhh_x(raw, 2L)
  array(d, c(M, n_task, length(.RUM_BHH_FEATURES)), dimnames = list(NULL, NULL, .RUM_BHH_FEATURES))
}

.rum_bhh_profiles_to_raw <- function(profA, profB) {
  n <- nrow(profA)
  out <- lapply(names(.RUM_BHH_LEVELS), function(a) {
    f <- function(p) { ix <- match(as.character(p[[a]]), .RUM_BHH_LEVELS[[a]])
      if (anyNA(ix)) stop("unknown level in ", a); ix }
    .rum_side_array(f(profA), f(profB), n, 1L)
  })
  names(out) <- names(.RUM_BHH_LEVELS)
  out
}

.rum_bhh_raw_to_profiles <- function(raw) {
  M <- dim(raw$consist)[1L]; n_task <- dim(raw$consist)[2L]
  one <- function(side) {
    out <- data.frame(draw = rep(seq_len(M), each = n_task), task = rep(seq_len(n_task), M))
    for (a in names(.RUM_BHH_LEVELS)) out[[a]] <- .RUM_BHH_LEVELS[[a]][as.vector(t(raw[[a]][, , side]))]
    out
  }
  list(profA = one(1L), profB = one(2L))
}

# The BHH 2016 spec (class rum_design_spec; pass it wherever a spec or study string is taken).
.rum_design_spec_bhh <- function() {
  structure(list(
    study = "bhh2016", T = 5L, p = length(.RUM_BHH_FEATURES), features = .RUM_BHH_FEATURES,
    uses_resp = FALSE, draw = .rum_bhh_draw, code = .rum_bhh_code, key = function(resp) "all",
    to_raw = .rum_bhh_profiles_to_raw, to_profiles = .rum_bhh_raw_to_profiles,
    law = list(
      source = "Bansak, Hainmueller and Hangartner (2016) Science; 2023 Nature Methods ('fully randomized paired profiles'); rum_bhh2016_2026-10-23/report/D1_design.md",
      tasks = "5 pairs in order (A-E); side 1 = profile 1 of the pair",
      levels = .RUM_BHH_LEVELS,
      rule = "each attribute uniform, independent across attributes, profiles and pairs",
      respondent = "none")),
    class = "rum_design_spec")
}
