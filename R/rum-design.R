# Conjoint design laws and design generators for the RUM conditional information.
#
# The one-step correction needs I(z) = E[s s' | Z = z], an expectation over the
# respondent's choices and over the experiment's design distribution given z
# (paperps Appendix D, "Scores and Target Information"). These internal helpers
# draw fresh task sequences from each application's randomization law and code
# them into the fitted `d` columns with the respondent's own covariates.
#
# Laws and codings follow agent F's specs (rum_inference_2026-10-11/report/
# F_design_specs.md and scripts/F_spec_{gs,br,sw}.R, F_br_revenue_rule.R), which
# rebuild every fitted d row exactly (GS 20,657 tasks, BR 15,928, SW 3,573 x 2).
#
# API (internal):
#   .rum_design_spec(study)                     the law, feature names and coder
#   .rum_draw_designs(spec, resp, M, seed, T)   M sequences for one respondent:
#                                               array M x T x p (attr "raw" if raw = TRUE)
#   .rum_draw_designs_crn(spec, resp, M, seed, T) one set of M raw sequences
#                                               coded for many respondents
#   .rum_crn_designs(crn, i)                    the M x T x p array for respondent i
#   .rum_design_draw_raw(spec, M, T, seed)      raw sequences only
#   .rum_design_code(spec, raw, resp)           code raw sequences (array M x T x p)
#   .rum_design_code_profiles(spec, profA, profB, resp)  code data-frame profiles (n x p)
#   .rum_design_profiles(spec, raw)             raw sequences as data-frame profiles
#
# Raw sequences are lists of integer arrays. Profile-level attributes have
# dim c(M, T, 2), where side 1 is the profile coded first in d (GS: the displayed
# LEFT candidate; BR: Plan A; SW: Candidate A) and side 2 the other one.
# d = x(side 1) - x(side 2), and y = 1 means side 1 was chosen.

# ---------------------------------------------------------------------------
# Graham and Svolik (2020)
# ---------------------------------------------------------------------------

.RUM_GS_DEM <- c("g_schedule", "g_committee", "g_boardElect", "g_record",
                 "g_officestructure", "g_procedure", "g_progEval",
                 "u_journalists", "u_banProtest", "u_execRule", "u_court",
                 "u_limitVote", "u_gerry2", "u_gerry10", "v_tax", "v_affair")
.RUM_GS_RACE <- c("White", "Black", "Hispanic", "Asian")
.RUM_GS_RACE_P <- c(.65, .15, .15, .05)
.RUM_GS_PRO <- c("Business executive", "Small business owner", "Lawyer", "Farmer",
                 "Legislative staffer", "Police officer", "Teacher",
                 "Served in the army", "Served in the navy")
.RUM_GS_PRO_P <- c(.15, .15, .20, .10, .10, .10, .10, .05, .05)
.RUM_GS_FEATURES <- c(
  "diff_respParty", "diff_p1_num", "diff_p2_num",
  "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
  "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
  "diff_dem_code_g_record", "diff_dem_code_g_schedule",
  "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
  "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
  "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
  "diff_dem_code_u_limitVote", "diff_dem_code_v_affair",
  "diff_dem_code_v_tax", "diff_sex_Female", "diff_race_Asian",
  "diff_race_Black", "diff_race_Hispanic", "diff_pro_Farmer",
  "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
  "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
  "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
  "diff_pro_Teacher")
# Party pairs (left, right): 1 = (D,R), 2 = (R,D), 3 = (D,D), 4 = (R,R).
.RUM_GS_PAIR_LEFT <- c(1L, 2L, 1L, 2L)    # 1 = D, 2 = R
.RUM_GS_PAIR_RIGHT <- c(2L, 1L, 1L, 2L)
# Area pairs (economic, social): 1 = (E,I), 2 = (E,M), 3 = (T,I), 4 = (T,M).
.RUM_GS_AREA_P1 <- c("E", "E", "T", "T")
.RUM_GS_AREA_P2 <- c("I", "M", "I", "M")

.rum_lookup <- function(levels, columns) {
  out <- matrix(0, length(levels), length(columns))
  out[cbind(match(columns, levels), seq_along(columns))] <- 1
  out
}
.RUM_GS_LU_DEM <- .rum_lookup(.RUM_GS_DEM, sub("^diff_dem_code_", "", .RUM_GS_FEATURES[4:18]))
.RUM_GS_LU_RACE <- .rum_lookup(.RUM_GS_RACE, c("Asian", "Black", "Hispanic"))
.RUM_GS_LU_PRO <- .rum_lookup(gsub(" ", "_", .RUM_GS_PRO), sub("^diff_pro_", "", .RUM_GS_FEATURES[23:30]))

# Each row of the result is an independent random permutation of `items`.
.rum_row_perm <- function(M, items) {
  n <- length(items)
  key <- matrix(stats::runif(M * n), M, n)
  o <- order(row(key), key)
  matrix(items[col(key)[o]], M, n, byrow = TRUE)
}

.rum_side_array <- function(left, right, M, T) {
  a <- array(0L, c(M, T, 2L))
  a[, , 1L] <- left
  a[, , 2L] <- right
  a
}

# One full respondent sequence of scenarios 1-13 per row. Blocking (GS online
# appendix, "Candidate choices and the randomization procedure"; F_design_specs.md
# section 1): 4 generic-vs-generic scenarios and each of the 9 negative positions
# once, permuted; 14 party pairs (D,R) x4, (R,D) x4, (D,D) x3, (R,R) x3 over
# scenarios 1-13 and 16, with 16 opposite-party; 16 area-pair slots (4 each),
# permuted; 12 women among the 32 candidates of scenarios 1-16. Everything else
# is independent, except the side of the negative candidate (below). Rows are
# scenario IDs 1-13 (the fitted task order); the law is exchangeable over IDs, so
# the first T rows have the law of any T scenarios.
#
# Negative candidate's side. The appendix table says "a coin flip", but the data
# balance it within respondent: among the 9 negative scenarios the number shown
# on the left has variance 1.08 (1,682 respondents), against 2.25 for 9 coin
# flips, and its distribution matches a hypergeometric draw of 9 from a block of
# 16 sides with 8 left (variance 1.05; chi-square p = 0.13); every pair of
# scenarios has correlation about -1/15 (G1_design_fix.md). So each sequence
# gets a shuffled block of 8 left and 8 right over scenario IDs 1-16, and the
# negative candidate in scenario s sits on the left iff slot s is "left". The
# block is built from 16 iid uniforms per row: the 13 per-scenario uniforms the
# old coin flip used plus 3 drawn last, the 8 smallest being "left", so a seed
# gives the same draws as before for every other attribute.
.rum_gs_side_block <- function(u13, M, S) {
  U <- cbind(matrix(u13, M, S), matrix(stats::runif(M * (16L - S)), M, 16L - S))
  rk <- integer(M * 16L)
  rk[order(row(U), U)] <- rep(seq_len(16L), M)
  matrix(rk, M, 16L)[, seq_len(S), drop = FALSE] <= 8L
}

.rum_gs_draw <- function(M, T = 13L) {
  S <- 13L
  # democracy: 0 = generic-vs-generic, 8..16 = the negative position's code
  type <- .rum_row_perm(M, c(rep(0L, 4L), 8:16))
  neg <- type > 0L
  u_side <- stats::runif(M * S)                 # side uniforms; the block is set below
  gen1 <- sample.int(7L, M * S, replace = TRUE)
  gen2 <- ((gen1 - 1L + sample.int(6L, M * S, replace = TRUE)) %% 7L) + 1L  # distinct from gen1
  # party pairs: scenario 16 takes (D,R) or (R,D); scenarios 1-13 permute the rest
  s16_dr <- stats::runif(M) < .5
  pair <- .rum_row_perm(M, c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 0L))
  z <- which(pair == 0L)                        # one per row; the 13th opposite pair is the one 16 did not take
  pair[z] <- ifelse(s16_dr[(z - 1L) %% M + 1L], 2L, 1L)
  # area pairs: 13 of the 16 slots
  area <- .rum_row_perm(M, rep(1:4, each = 4L))[, seq_len(S), drop = FALSE]
  # sex: 12 women among 32 slots; slots 1..26 are (scenario, side) of scenarios 1-13
  fem <- .rum_row_perm(M, c(rep(1L, 12L), rep(0L, 20L)))[, seq_len(2L * S), drop = FALSE]
  femL <- fem[, 2L * seq_len(S) - 1L, drop = FALSE]
  femR <- fem[, 2L * seq_len(S), drop = FALSE]
  n2 <- M * S * 2L
  arr <- function(x) array(as.integer(x), c(M, S, 2L))
  p1 <- arr(sample.int(4L, n2, replace = TRUE))
  p2 <- arr(sample.int(4L, n2, replace = TRUE))
  race <- arr(sample.int(4L, n2, replace = TRUE, prob = .RUM_GS_RACE_P))
  pro <- arr(sample.int(9L, n2, replace = TRUE, prob = .RUM_GS_PRO_P))
  neg_left <- .rum_gs_side_block(u_side, M, S)  # drawn last: 3 more uniforms per row
  demL <- ifelse(neg, ifelse(neg_left, type, gen1), gen1)
  demR <- ifelse(neg, ifelse(neg_left, gen1, type), gen2)
  raw <- list(
    party = .rum_side_array(.RUM_GS_PAIR_LEFT[pair], .RUM_GS_PAIR_RIGHT[pair], M, S),
    dem = .rum_side_array(demL, demR, M, S),
    p1 = p1, p2 = p2,
    female = .rum_side_array(femL, femR, M, S),
    race = race, pro = pro,
    area = matrix(as.integer(area), M, S))
  .rum_raw_head(raw, T)
}

.rum_raw_head <- function(raw, T) {
  S <- dim(raw[[1L]])[2L]
  T <- as.integer(T)
  if (length(T) != 1L || is.na(T) || T < 1L || T > S)
    stop(sprintf("T must be an integer between 1 and %d.", S))
  if (T == S) return(raw)
  lapply(raw, function(a) if (length(dim(a)) == 3L) a[, seq_len(T), , drop = FALSE]
                          else a[, seq_len(T), drop = FALSE])
}

# pid_lean: "D" if pid7 < 0 (strong, weak, leaning Democrats), "R" if pid7 > 0,
# "I" if pid7 == 0 (pure independents; their co-partisan column is always 0).
.rum_gs_key <- function(resp) {
  if (is.null(resp)) stop("GS coding needs the respondent's party (pid7 or pid_lean).")
  if (is.character(resp) || is.factor(resp)) {
    k <- as.character(resp)
  } else if (is.numeric(resp)) {
    p <- as.numeric(resp)
    if (anyNA(p) || any(!p %in% -3:3)) stop("pid7 must be in -3..3.")
    k <- ifelse(p < 0, "D", ifelse(p > 0, "R", "I"))
  } else if (!is.null(resp$pid_lean)) {
    k <- as.character(resp$pid_lean)
  } else if (!is.null(resp$pid7)) {
    return(.rum_gs_key(as.numeric(resp$pid7)))
  } else stop("resp must supply pid7 or pid_lean.")
  if (anyNA(k) || any(!k %in% c("D", "R", "I"))) stop("pid_lean must be D, R or I.")
  k
}

# x(side) for all draws and tasks: (M*T) x 30, rows in array order (m fastest).
.rum_gs_x <- function(raw, side, key) {
  g <- function(a) as.vector(a[, , side])
  pk <- match(key, c("D", "R", "I"))
  M <- dim(raw$party)[1L]; T <- dim(raw$party)[2L]
  if (length(pk) == 1L) pk <- rep(pk, M * T) else if (length(pk) == M) pk <- rep(pk, T)
  cbind(as.numeric(g(raw$party) == pk), g(raw$p1), g(raw$p2),
        .RUM_GS_LU_DEM[g(raw$dem), , drop = FALSE], g(raw$female),
        .RUM_GS_LU_RACE[g(raw$race), , drop = FALSE],
        .RUM_GS_LU_PRO[g(raw$pro), , drop = FALSE])
}

.rum_gs_code <- function(raw, key) {
  M <- dim(raw$party)[1L]; T <- dim(raw$party)[2L]
  if (!length(key) %in% c(1L, M)) stop("resp must have one row or one row per draw.")
  d <- .rum_gs_x(raw, 1L, key) - .rum_gs_x(raw, 2L, key)
  array(d, c(M, T, length(.RUM_GS_FEATURES)),
        dimnames = list(NULL, NULL, .RUM_GS_FEATURES))
}

.rum_gs_profiles_to_raw <- function(profA, profB) {
  n <- nrow(profA)
  enc <- function(f) .rum_side_array(f(profA), f(profB), n, 1L)
  chk <- function(x, what) { if (anyNA(x)) stop("unknown ", what, " level"); x }
  list(
    party = enc(function(p) chk(match(p$party, c("D", "R")), "party")),
    dem = enc(function(p) chk(match(p$dem_code, .RUM_GS_DEM), "dem_code")),
    p1 = enc(function(p) chk(match(as.numeric(p$p1_num), 1:4), "p1_num")),
    p2 = enc(function(p) chk(match(as.numeric(p$p2_num), 1:4), "p2_num")),
    female = enc(function(p) chk(match(p$sex, c("Male", "Female")), "sex") - 1L),
    race = enc(function(p) chk(match(p$race, .RUM_GS_RACE), "race")),
    pro = enc(function(p) chk(match(gsub("_", " ", p$pro), .RUM_GS_PRO), "pro")),
    area = matrix(if (!is.null(profA$p1_area))
      match(paste0(profA$p1_area, profA$p2_area), paste0(.RUM_GS_AREA_P1, .RUM_GS_AREA_P2))
      else NA_integer_, n, 1L))
}

.rum_gs_raw_to_profiles <- function(raw) {
  M <- dim(raw$party)[1L]; T <- dim(raw$party)[2L]
  one <- function(side) {
    g <- function(a) as.vector(t(a[, , side]))   # draw-major, task-minor
    ar <- as.vector(t(raw$area))
    data.frame(draw = rep(seq_len(M), each = T), task = rep(seq_len(T), M),
               party = c("D", "R")[g(raw$party)], p1_area = .RUM_GS_AREA_P1[ar],
               p1_num = g(raw$p1), p2_area = .RUM_GS_AREA_P2[ar], p2_num = g(raw$p2),
               dem_code = .RUM_GS_DEM[g(raw$dem)],
               sex = c("Male", "Female")[g(raw$female) + 1L],
               race = .RUM_GS_RACE[g(raw$race)], pro = .RUM_GS_PRO[g(raw$pro)],
               stringsAsFactors = FALSE)
  }
  list(profA = one(1L), profB = one(2L))
}

# ---------------------------------------------------------------------------
# Ballard-Rosa, Martin and Scheve (2017)
# ---------------------------------------------------------------------------

.RUM_BR_MENUS <- list(c(0, 5, 15, 25), c(5, 15, 25, 35), c(5, 15, 25, 35),
                      c(5, 15, 25, 35), c(5, 15, 25, 35, 45), c(5, 15, 25, 35, 45, 55))
# Displayed revenue (taxrev: 1 = much more ... 5 = much less) = category of
# w'rates + N(0, sigma^2) at the article's cutpoints (JOP 79(1) p. 6, notes 20-21).
# w and sigma are maximum-likelihood estimates from the 36,000 released profiles
# (F_br_revenue_rule.R): the noiseless rule matches 97.56% of displayed labels.
.RUM_BR_REV_CUTS <- c(.75, .95, 1.05, 1.25)
.RUM_BR_REV_W <- c(0.01133501, 0.02014928, 0.00910247, 0.00305561, 0.00250765, 0.00538833)
.RUM_BR_REV_SIGMA <- 0.00682408

.rum_br_recipe <- function() {
  maps <- lapply(seq_along(.RUM_BR_MENUS), function(j) {
    x <- (.RUM_BR_MENUS[[j]] - mean(.RUM_BR_MENUS[[j]])) / 10
    linear <- x / sqrt(mean(x^2))
    residual_square <- x^2 - mean(x^2) - linear * mean(linear * x^2)
    quadratic <- residual_square / sqrt(mean(residual_square^2))
    ans <- cbind(linear, quadratic)
    colnames(ans) <- paste0("tax", j, c("_linear", "_quadratic"))
    ans
  })
  revenue <- stats::contr.helmert(5)
  revenue <- sweep(revenue, 2L, sqrt(colMeans(revenue^2)), "/")
  colnames(revenue) <- paste0("revenue_helmert", 1:4)
  list(tax_maps = maps, revenue_map = unname(revenue),
       features = c(unlist(lapply(maps, colnames)), colnames(revenue)))
}
.RUM_BR_RECIPE <- .rum_br_recipe()

.rum_br_revenue_ratio <- function(rates) drop(as.matrix(rates) %*% .RUM_BR_REV_W)
# findInterval(r, cuts) is 0 at or below .75 (much less = 5) ... 4 above 1.25
# (much more = 1); left.open matches cut()'s right-closed intervals in F's rule.
.rum_br_revenue_code <- function(ratio) 5L - findInterval(ratio, .RUM_BR_REV_CUTS, left.open = TRUE)
.rum_br_revenue_draw <- function(rates, sigma = .RUM_BR_REV_SIGMA) {
  r <- .rum_br_revenue_ratio(rates)
  .rum_br_revenue_code(r + stats::rnorm(length(r), 0, sigma))
}
# Category probabilities, n x 5 (columns taxrev 1..5).
.rum_br_revenue_probs <- function(rates, sigma = .RUM_BR_REV_SIGMA) {
  r <- .rum_br_revenue_ratio(rates)
  cu <- c(-Inf, .RUM_BR_REV_CUTS, Inf)
  P <- vapply(1:5, function(k) stats::pnorm((cu[k + 1L] - r) / sigma) -
                stats::pnorm((cu[k] - r) / sigma), numeric(length(r)))
  P <- matrix(P, ncol = 5L)[, 5:1, drop = FALSE]
  colnames(P) <- paste0("taxrev", 1:5)
  P
}

# Each plan's six rates are independent uniform draws from the bracket menus,
# independent across brackets, plans and tasks; revenue is drawn per plan from
# the noisy rule. 8 tasks per respondent, kept in table order.
.rum_br_draw <- function(M, T = 8L) {
  n2 <- M * 8L * 2L
  rate <- lapply(.RUM_BR_MENUS, function(m) array(sample.int(length(m), n2, replace = TRUE), c(M, 8L, 2L)))
  pct <- vapply(1:6, function(j) .RUM_BR_MENUS[[j]][as.vector(rate[[j]])], numeric(n2))
  rev <- array(.rum_br_revenue_draw(pct), c(M, 8L, 2L))
  names(rate) <- paste0("r", 1:6)
  .rum_raw_head(c(rate, list(taxrev = rev)), T)
}

.rum_br_x <- function(raw, side) {
  g <- function(a) as.vector(a[, , side])
  X <- do.call(cbind, lapply(1:6, function(j) .RUM_BR_RECIPE$tax_maps[[j]][g(raw[[paste0("r", j)]]), , drop = FALSE]))
  cbind(X, .RUM_BR_RECIPE$revenue_map[g(raw$taxrev), , drop = FALSE])
}

.rum_br_code <- function(raw, key = NULL) {
  M <- dim(raw$r1)[1L]; T <- dim(raw$r1)[2L]
  d <- .rum_br_x(raw, 1L) - .rum_br_x(raw, 2L)
  array(d, c(M, T, length(.RUM_BR_RECIPE$features)),
        dimnames = list(NULL, NULL, .RUM_BR_RECIPE$features))
}

.rum_br_profiles_to_raw <- function(profA, profB) {
  n <- nrow(profA)
  rc <- if (all(paste0("r", 1:6) %in% names(profA))) paste0("r", 1:6) else paste0("tax_rate_", 1:6, "_pct")
  raw <- lapply(1:6, function(j) {
    f <- function(p) { ix <- match(as.numeric(p[[rc[j]]]), .RUM_BR_MENUS[[j]])
      if (anyNA(ix)) stop("rate not on the bracket ", j, " menu"); ix }
    .rum_side_array(f(profA), f(profB), n, 1L)
  })
  names(raw) <- paste0("r", 1:6)
  f <- function(p) { ix <- match(as.numeric(p$taxrev), 1:5)
    if (anyNA(ix)) stop("taxrev not in 1-5"); ix }
  c(raw, list(taxrev = .rum_side_array(f(profA), f(profB), n, 1L)))
}

.rum_br_raw_to_profiles <- function(raw) {
  M <- dim(raw$r1)[1L]; T <- dim(raw$r1)[2L]
  one <- function(side) {
    g <- function(a) as.vector(t(a[, , side]))
    out <- data.frame(draw = rep(seq_len(M), each = T), task = rep(seq_len(T), M))
    for (j in 1:6) out[[paste0("r", j)]] <- .RUM_BR_MENUS[[j]][g(raw[[paste0("r", j)]])]
    out$taxrev <- g(raw$taxrev)
    out
  }
  list(profA = one(1L), profB = one(2L))
}

# ---------------------------------------------------------------------------
# Saha and Weeks (2022), SSI survey
# ---------------------------------------------------------------------------

.RUM_SW_LEVELS <- list(
  gender = c("Female", "Male"), run = c("No", "Yes"),
  talent = c("Assertive", "Collaborative", "Determined to Succeed", "Empathetic",
             "Good Communicator", "Hard-Working", "Tough Negotiator"),
  agenda = c("Very Few Changes", "Moderate Changes", "Complete Overhaul"),
  child = c("No children", "1 child", "2 children", "3 children"))
.RUM_SW_FEATURES <- c("cand_genderMale", "cand_runYes", "cand_talentCollaborative",
  "cand_talentDetermined.to.Succeed", "cand_talentEmpathetic",
  "cand_talentGood.Communicator", "cand_talentHard.Working",
  "cand_talentTough.Negotiator", "cand_agendaModerate.Changes",
  "cand_agendaComplete.Overhaul", "cand_child1.child", "cand_child2.children",
  "cand_child3.children")
.RUM_SW_ATTR_OF <- rep(1:5, c(1, 1, 6, 2, 3))
# Explicit arm: products of dummies from different attributes, combn(13, 2) order
# (scripts_C/B0b_sw_extra_inputs.R L41-44).
.RUM_SW_PAIRS <- local({
  p <- t(utils::combn(13, 2))
  p[.RUM_SW_ATTR_OF[p[, 1]] != .RUM_SW_ATTR_OF[p[, 2]], , drop = FALSE]
})
.RUM_SW_FEATURES_EXPLICIT <- c(.RUM_SW_FEATURES,
  paste0(.RUM_SW_FEATURES[.RUM_SW_PAIRS[, 1]], ":", .RUM_SW_FEATURES[.RUM_SW_PAIRS[, 2]]))
.RUM_SW_LU <- lapply(.RUM_SW_LEVELS, function(l) diag(length(l))[, -1L, drop = FALSE])

# Five attributes per candidate, each uniform and independent, independent across
# candidates and the 3 elections (article p. 7; uniform is read off the marginals).
.rum_sw_draw <- function(M, T = 3L) {
  n2 <- M * 3L * 2L
  raw <- lapply(.RUM_SW_LEVELS, function(l) array(sample.int(length(l), n2, replace = TRUE), c(M, 3L, 2L)))
  .rum_raw_head(raw, T)
}

.rum_sw_x <- function(raw, side, explicit) {
  X <- do.call(cbind, lapply(names(.RUM_SW_LEVELS), function(a)
    .RUM_SW_LU[[a]][as.vector(raw[[a]][, , side]), , drop = FALSE]))
  if (explicit) X <- cbind(X, X[, .RUM_SW_PAIRS[, 1], drop = FALSE] * X[, .RUM_SW_PAIRS[, 2], drop = FALSE])
  X
}

.rum_sw_code <- function(raw, key = NULL, explicit = FALSE) {
  M <- dim(raw$gender)[1L]; T <- dim(raw$gender)[2L]
  nm <- if (explicit) .RUM_SW_FEATURES_EXPLICIT else .RUM_SW_FEATURES
  d <- .rum_sw_x(raw, 1L, explicit) - .rum_sw_x(raw, 2L, explicit)
  array(d, c(M, T, length(nm)), dimnames = list(NULL, NULL, nm))
}

.rum_sw_profiles_to_raw <- function(profA, profB) {
  n <- nrow(profA)
  out <- lapply(names(.RUM_SW_LEVELS), function(a) {
    f <- function(p) { ix <- match(p[[a]], .RUM_SW_LEVELS[[a]])
      if (anyNA(ix)) stop("unknown level in ", a); ix }
    .rum_side_array(f(profA), f(profB), n, 1L)
  })
  names(out) <- names(.RUM_SW_LEVELS)
  out
}

.rum_sw_raw_to_profiles <- function(raw) {
  M <- dim(raw$gender)[1L]; T <- dim(raw$gender)[2L]
  one <- function(side) {
    out <- data.frame(draw = rep(seq_len(M), each = T), task = rep(seq_len(T), M))
    for (a in names(.RUM_SW_LEVELS)) out[[a]] <- .RUM_SW_LEVELS[[a]][as.vector(t(raw[[a]][, , side]))]
    out
  }
  list(profA = one(1L), profB = one(2L))
}

# ---------------------------------------------------------------------------
# The spec object and the generic API
# ---------------------------------------------------------------------------

.rum_design_spec <- function(study = c("gs", "br", "sw_additive", "sw_explicit")) {
  study <- match.arg(study)
  base <- switch(study,
    gs = list(
      T = 13L, features = .RUM_GS_FEATURES, uses_resp = TRUE,
      draw = .rum_gs_draw, code = .rum_gs_code, key = .rum_gs_key,
      to_raw = .rum_gs_profiles_to_raw, to_profiles = .rum_gs_raw_to_profiles,
      law = list(
        source = "Graham and Svolik (2020) online appendix, 'Candidate choices and the randomization procedure'; F_design_specs.md section 1",
        tasks = "scenarios 1-13 in scenario-ID order (display order not in the data); side 1 = displayed left",
        democracy = list(levels = .RUM_GS_DEM, generic = 1:7, negative = 8:16,
          rule = "per sequence: 4 generic-vs-generic scenarios (two distinct generic levels, each uniform) and each of the 9 negative positions exactly once vs a uniform generic; permuted over IDs; negative side from a shuffled block of 8 left / 8 right over scenario IDs 1-16 (left w.p. 1/2 per scenario; the 9 negatives' left count is hypergeometric, variance 1.05, not binomial)"),
        party = list(pairs = c(DR = 4L, RD = 4L, DD = 3L, RR = 3L),
          rule = "14 (left, right) pairs over scenarios 1-13 and 16, permuted; 16 is opposite-party, so 1-13 hold 3 DD, 3 RR, DR = 3 or 4 (1/2 each)"),
        areas = list(pairs = paste0(.RUM_GS_AREA_P1, .RUM_GS_AREA_P2),
          rule = "16 slots, each (economic, social) pair 4 times, permuted; scenarios 1-13 take 13 slots; not in d"),
        positions = "p1, p2 iid uniform on 1-4 per candidate",
        sex = "12 women among the 32 candidates of scenarios 1-16 (permuted); the 26 of scenarios 1-13 are hypergeometric",
        race = stats::setNames(.RUM_GS_RACE_P, .RUM_GS_RACE),
        profession = stats::setNames(.RUM_GS_PRO_P, .RUM_GS_PRO),
        not_drawn = "age and experience (not in d), policy listing order, display order",
        respondent = "co-partisan x = 1(party == pid_lean); pid_lean D if pid7 < 0, R if > 0, I if 0 (always 0)")),
    br = list(
      T = 8L, features = .RUM_BR_RECIPE$features, uses_resp = FALSE,
      draw = .rum_br_draw, code = .rum_br_code, key = function(resp) "all",
      to_raw = .rum_br_profiles_to_raw, to_profiles = .rum_br_raw_to_profiles,
      law = list(
        source = "Ballard-Rosa, Martin and Scheve (2017) JOP 79(1) pp. 5-6; F_design_specs.md section 2; F_br_revenue_rule.R",
        tasks = "8 tasks in table order; side 1 = Plan A",
        menus = .RUM_BR_MENUS,
        rates = "each plan's six rates uniform and independent on the bracket menus, independent across plans and tasks",
        revenue = list(weights = .RUM_BR_REV_W, sigma = .RUM_BR_REV_SIGMA, cuts = .RUM_BR_REV_CUTS,
          rule = "taxrev = category of w'rates + N(0, sigma^2), drawn per plan; 1 = much more, 5 = much less"),
        respondent = "none")),
    sw_additive = , sw_explicit = list(
      T = 3L, uses_resp = FALSE,
      features = if (study == "sw_explicit") .RUM_SW_FEATURES_EXPLICIT else .RUM_SW_FEATURES,
      draw = .rum_sw_draw,
      code = if (study == "sw_explicit") function(raw, key = NULL) .rum_sw_code(raw, key, TRUE)
             else function(raw, key = NULL) .rum_sw_code(raw, key, FALSE),
      key = function(resp) "all",
      to_raw = .rum_sw_profiles_to_raw, to_profiles = .rum_sw_raw_to_profiles,
      law = list(
        source = "Saha and Weeks (2022) Political Behavior p. 7; F_design_specs.md section 3",
        tasks = "3 elections in order; side 1 = Candidate A",
        levels = .RUM_SW_LEVELS,
        rule = "each attribute uniform, independent across attributes, candidates and elections",
        respondent = "none")))
  base$study <- study
  base$p <- length(base$features)
  structure(base, class = "rum_design_spec")
}

.rum_with_seed <- function(seed, code) {
  if (is.null(seed)) return(code)
  withr::with_seed(seed, code, .rng_kind = "Mersenne-Twister",
                   .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
}

.rum_spec_arg <- function(spec) {
  if (is.character(spec)) spec <- .rum_design_spec(spec)
  if (!inherits(spec, "rum_design_spec")) stop("spec must come from .rum_design_spec().")
  spec
}

.rum_design_draw_raw <- function(spec, M, T = NULL, seed = NULL) {
  spec <- .rum_spec_arg(spec)
  M <- as.integer(M)
  if (length(M) != 1L || is.na(M) || M < 1L) stop("M must be a positive integer.")
  if (is.null(T)) T <- spec$T
  .rum_with_seed(seed, spec$draw(M, T))
}

# Code raw sequences for one respondent (resp: one row or a pid value) or, in GS,
# for one respondent per draw. Returns an M x T x p array.
.rum_design_code <- function(spec, raw, resp = NULL) {
  spec <- .rum_spec_arg(spec)
  spec$code(raw, if (spec$uses_resp) spec$key(resp) else NULL)
}

# M fresh sequences for respondent `resp`, coded into the fitted d columns.
# Returns an M x T x p array (draw, task, feature); d[m, , ] is draw m's T x p
# task matrix and matrix(d, M * T, p) %*% beta is the M x T matrix of d'beta.
# raw = TRUE attaches the raw sequences as attr(, "raw"); format = "list"
# returns a list of M matrices (T x p) instead.
.rum_draw_designs <- function(spec, resp = NULL, M, seed = NULL, T = NULL,
                              raw = FALSE, format = c("array", "list")) {
  spec <- .rum_spec_arg(spec)
  format <- match.arg(format)
  if (spec$uses_resp) {
    key <- spec$key(resp)
    if (length(key) != 1L) stop("resp must describe one respondent; use .rum_draw_designs_crn() for many.")
  }
  r <- .rum_design_draw_raw(spec, M, T, seed)
  d <- .rum_design_code(spec, r, resp)
  if (format == "list") {
    dd <- dim(d)
    d <- lapply(seq_len(dd[1L]), function(m)
      matrix(d[m, , ], dd[2L], dd[3L], dimnames = list(NULL, spec$features)))
  }
  if (raw) attr(d, "raw") <- r
  d
}

# Common random numbers: ONE set of M raw sequences, coded for every respondent in
# `resp` (a data frame with one row per respondent, or a pid7 / pid_lean vector).
# The coded design depends on the respondent only through a key (GS: pid_lean;
# BR, SW: none), so each distinct key is coded once.
# Returns list(designs = named list key -> M x T x p array, key = per-respondent
# index into designs, raw = the raw sequences, study, M, T). Use
# .rum_crn_designs(crn, i) for respondent i. For a respondent with T_i < T tasks,
# take the first T_i tasks: [, seq_len(T_i), , drop = FALSE] (the GS law is
# exchangeable over scenario IDs, BR and SW tasks are iid).
.rum_draw_designs_crn <- function(spec, resp = NULL, M, seed = NULL, T = NULL, n = NULL) {
  spec <- .rum_spec_arg(spec)
  r <- .rum_design_draw_raw(spec, M, T, seed)
  if (spec$uses_resp) {
    keys <- spec$key(resp)
  } else {
    if (is.null(n)) n <- if (is.null(resp)) 1L else if (is.data.frame(resp)) nrow(resp) else length(resp)
    keys <- rep("all", n)
  }
  u <- sort(unique(keys))
  designs <- lapply(u, function(k) spec$code(r, if (spec$uses_resp) k else NULL))
  names(designs) <- u
  list(designs = designs, key = match(keys, u), keys = u, raw = r,
       study = spec$study, M = dim(r[[1L]])[1L], T = dim(r[[1L]])[2L])
}

.rum_crn_designs <- function(crn, i) crn$designs[[crn$key[i]]]

# Code data-frame profiles (one task per row): profA is side 1 (GS displayed
# left, BR Plan A, SW Candidate A). Formats as in F's specs: GS party, p1_num,
# p2_num, dem_code, sex, race, pro; BR r1..r6 (or tax_rate_j_pct) and taxrev;
# SW gender, run, talent, agenda, child. resp: GS one row, or one per task.
# Returns an n x p matrix in fitted column order.
.rum_design_code_profiles <- function(spec, profA, profB, resp = NULL) {
  spec <- .rum_spec_arg(spec)
  if (nrow(profA) != nrow(profB)) stop("profA and profB must have the same rows.")
  r <- spec$to_raw(profA, profB)
  d <- .rum_design_code(spec, r, resp)
  out <- matrix(d, dim(d)[1L], dim(d)[3L])
  colnames(out) <- spec$features
  out
}

# Raw sequences as data-frame profiles (columns draw, task, then attributes).
.rum_design_profiles <- function(spec, raw) {
  spec <- .rum_spec_arg(spec)
  spec$to_profiles(raw)
}
