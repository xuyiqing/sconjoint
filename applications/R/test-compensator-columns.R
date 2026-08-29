#!/usr/bin/env Rscript
#
# Verification script for applications/R/compensator_columns.R.
#
#   Rscript applications/R/test-compensator-columns.R
#
# Prints PASS/FAIL per check and exits nonzero if any check fails. Runs
# entirely on SYNTHETIC fits: no real fit is required (and none is available on
# the machine where this was written).
#
# Cross-language oracle. The Python reference engine validated by the math
# audit lives at the path in `CC_PYTHON_REFERENCE` (default: the audit's
# scratchpad copy). When it, python3, numpy, and scipy are all present, checks
# A1/A2 drive both engines from the same synthetic configuration file and
# compare. When it is absent the live comparison reports SKIP and the same
# configurations are still checked against GOLDEN values recorded from that
# oracle (check A0), so the cross-language agreement never silently disappears.
# The reference file is read, never modified: only its function-definition
# prefix is exec'd, so the audit's own script body does not run.
#
# Checks
#   A0   golden agreement with the Python reference engine (16 configs x 5 cols)
#   A1   live agreement with the Python reference engine
#   A2   live agreement with the Python dense-breakpoint engine
#   B1   Any column = union of the three acceptance sets (audit counterexamples)
#   B2   the buggy literal breakpoint reuse is reproduced and is NOT what we ship
#   B3   Any equals the archived three-way OR, by 4e6-draw Monte Carlo
#   C    independent in-R breakpoint engine agrees with the half-line engine
#   D    convexity: <= 2 acceptance components, never a bounded one
#   E    domination theorems over random configs
#   E2   Co-party correctly has no domination gate
#   F1   invariance A -> -A
#   F2   invariance to permuting the governance selectors
#   F3   a single-coordinate sign flip is NOT an invariance and is detected
#   G    Sigma = 0 reduces to the plain indicator fractions
#   H1   capstone: Co-party engine vs the package's closed-form C_0 (Sigma route)
#   H1b  the same capstone against the |c'A| route (isolates engine exactness)
#   H2   capstone: b = 0 engine vs the S_0 closed form
#   H3   capstone at amounts a != 1
#   I1   gate sensitivity equals d(cell)/d(level shift) numerically
#   I2   gate sensitivity reproduces the audit's dC/dmu_p = 0.22112935
#   I3   the max-slope gate is one-sided; the per-respondent flag catches it
#   I3b  the I3 canary cannot receive a reportable status (fail-closed gate)
#   I3c  one clearing respondent cannot release a failing cell
#   C2a  composite envelope floor exceeds the marginal maximum
#   C2b  aligned loading: released under marginal max, withheld under composite
#   C5a  matched composite floor never exceeds the triangle fallback
#   C5b  matched aggregation is max-then-quantile, not quantile-then-max
#   C5c  the aligned-loading canary stays withheld under the matched floor
#   C5d  floor_rule stamps the method token in every cell
#   J1   floored cells inherit a SAME-SIDE None bound; Co-party does not
#   J1b  an opposition-side None bound is refused
#   J1c  an undeclared-side None bound is refused
#   J1d  data-frame none_bounds honours event_side
#   J1e  no floor supplied -> the engine point is never released
#   J2   unfloored cells release the point value
#   J3   cc_check_domination() on an assembled table
#   K1   zero-slope / identically-zero lines do not crash or produce NaN
#   K2   an identically-zero piece is flagged (the `>=` boundary carries mass)
#   L1-3 q > 1 seeded Monte Carlo fallback
#   M1-4 end-to-end on a fit-shaped object (raw-scale loadings, by-group table,
#        units assertion, q > 1 refusal)
#   N    selector and floor-reduction helpers

## ---------------------------------------------------------------------------
## Harness
## ---------------------------------------------------------------------------

.this_dir <- function() {
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1L]))))
  getwd()
}
ENGINE <- file.path(.this_dir(), "compensator_columns.R")
if (!file.exists(ENGINE)) stop("Cannot find compensator_columns.R next to this script.")
source(ENGINE)
## The matched composite floor delegates to share_bounds.R, which owns the
## calibration reshaping and the signed-loading contrast statistic. The
## runner sources both; so does this suite, so the matched path is
## exercised here rather than only in production.
SBSRC <- file.path(.this_dir(), "share_bounds.R")
if (!file.exists(SBSRC)) stop("Cannot find share_bounds.R next to this script.")
source(SBSRC)

N_FAIL <- 0L
N_PASS <- 0L
N_SKIP <- 0L
report <- function(id, name, ok, detail = "") {
  if (is.na(ok)) {
    N_SKIP <<- N_SKIP + 1L
    cat(sprintf("SKIP  %-4s %-52s %s\n", id, name, detail))
  } else if (isTRUE(ok)) {
    N_PASS <<- N_PASS + 1L
    cat(sprintf("PASS  %-4s %-52s %s\n", id, name, detail))
  } else {
    N_FAIL <<- N_FAIL + 1L
    cat(sprintf("FAIL  %-4s %-52s %s\n", id, name, detail))
  }
}
fmt <- function(x, d = 3) formatC(x, format = "e", digits = d)

## ---------------------------------------------------------------------------
## Synthetic coordinate layout
##
## p = 10: 1 action, 2 co-party, 3 econ policy, 4 social policy, 5-10 the six
## governance coordinates. A "config" is one respondent: mu and A rows.
## ---------------------------------------------------------------------------

P <- 10L
ATTR <- c("u_action", "diff_respParty", "diff_p1_num", "diff_p2_num",
          paste0("g_", c("committee", "officestructure", "procedure",
                         "progEval", "record", "schedule")))
SEL <- cc_selectors(ATTR, action = "u_action", coparty = "diff_respParty",
                    econ = "diff_p1_num", social = "diff_p2_num",
                    governance = ATTR[5:10])
SPECS <- cc_specs()

## config: list(p = c(m, a), b =, e =, s =, g = 6 x c(m, a))
cfg_to_rows <- function(cfg) {
  mu <- c(cfg$p[1L], cfg$b[1L], cfg$e[1L], cfg$s[1L],
          vapply(cfg$g, `[`, numeric(1L), 1L))
  a <- c(cfg$p[2L], cfg$b[2L], cfg$e[2L], cfg$s[2L],
         vapply(cfg$g, `[`, numeric(1L), 2L))
  list(mu = mu, a = a)
}
pack <- function(cfgs) {
  rs <- lapply(cfgs, cfg_to_rows)
  list(mu = do.call(rbind, lapply(rs, `[[`, "mu")),
       A  = do.call(rbind, lapply(rs, `[[`, "a")))
}

set.seed(20260827L)
rand_cfg <- function(scale = 1) {
  ln <- function() c(stats::rnorm(1L, 0, scale), stats::rnorm(1L, 0, scale))
  list(p = ln(), b = ln(), e = ln(), s = ln(),
       g = replicate(6L, ln(), simplify = FALSE))
}
gflat <- function(m) lapply(m, function(x) c(x, 0))

CFG <- list()
for (i in 1:4) CFG[[paste0("G", i, "-generic")]] <- rand_cfg()
c1 <- rand_cfg(); c1$e <- c(0.7, 0.0);  CFG[["Z-zero-slope-econ"]] <- c1
c1 <- rand_cfg(); c1$e <- c(0.0, 0.0);  CFG[["Z-identically-zero-econ"]] <- c1
c1 <- rand_cfg(); c1$e <- c(1.0, 2.0); c1$s <- c(-0.5, -1.0)
CFG[["Z-coincident-kinks"]] <- c1                      # both roots at u = -0.5
c1 <- rand_cfg(); c1$g <- gflat(c(-0.4, -0.9, -1.2, -0.3, -2.0, -0.6))
CFG[["Z-gov-benefit-identically-0"]] <- c1
c1 <- rand_cfg(); c1$p[2L] <- 0;        CFG[["Z-flat-penalty"]] <- c1
## The audit's MINIMAL any-column counterexample (party line 0.7u, penalty -1,
## policies identically zero, governance constant at -0.5).
CFG[["Z-any-minimal"]] <- list(p = c(-1, 0), b = c(0, 0.7), e = c(0, 0),
                               s = c(0, 0), g = gflat(rep(-0.5, 6)))
## Root exactly at the kink: h_pol = 4u + 3|u|.
CFG[["Z-collision-root-at-kink"]] <- list(p = c(0, 4), b = c(0, 0), e = c(0, 1),
                                          s = c(0, 0), g = gflat(rep(-1, 6)))
## Governance envelope attaining all six kinks (u = +-1, +-2, +-3).
CFG[["Z-six-gov-kinks"]] <- list(
  p = c(0, 0.05), b = c(0, 0), e = c(0, 0), s = c(0, 0),
  g = list(c(-1, 1), c(-3, 2), c(-6, 3), c(-1, -1), c(-3, -2), c(-6, -3)))
## Column floor gate passes on max slope while the mass sits on the flat piece.
CFG[["Z-floor-gate-gap"]] <- list(
  p = c(0.03, 0.001), b = c(0, 0), e = c(0, 0), s = c(0, 0),
  g = c(list(c(-8, 2)), gflat(rep(-9, 5))))
## Everything identically zero on the penalty line: exercises `>=` at the
## boundary (a flat line sitting exactly at 0 accepts).
CFG[["Z-flat-zero-penalty"]] <- list(p = c(0, 0), b = c(0, 0), e = c(0, 0),
                                     s = c(0, 0), g = gflat(rep(-1, 6)))
## The audit's two interior generics, verbatim. C11 is the GENERIC
## any-column counterexample (true 0.564263 vs literal reuse 0.545472);
## C12 carries the audit's differentiability probe (pol cell 0.383147,
## dC/dmu_p = 0.22112935).
CFG[["Z-interior-C11"]] <- list(
  p = c(-1.720572601381092, 0.33732633257503086),
  b = c(1.407272199556532, -1.454024302481208),
  e = c(-0.05213046206281766, -0.15801313848372694),
  s = c(-0.4402548685818507, 0.18373167731459789),
  g = list(c(-0.005860979638711163, 0.017860461073151337),
           c(-0.1880778681113973, 0.11369604074922915),
           c(-0.13482433737183025, -0.03572580212419017),
           c(-0.27706519804537827, -0.3040256900520488),
           c(0.33388296382500565, -0.1267761761346188),
           c(0.07292008908895048, -0.00844760869184308)))
CFG[["Z-interior-C12"]] <- list(
  p = c(-1.0691384615031754, 0.6300825914455861),
  b = c(-0.3018676045339098, -0.15144364817129202),
  e = c(0.005555389298619878, 0.29412708006300037),
  s = c(0.17012774365828456, 0.0956500669319982),
  g = list(c(-0.08453570900298625, -0.20729530800096982),
           c(0.1424294897602953, 0.14496707013313143),
           c(-0.021106259868501044, 0.08128257624052769),
           c(0.11721645808281075, 0.12467766789974725),
           c(0.13820752098697062, -0.06834264759916991),
           c(0.22724594739970763, -0.18698895874285623)))

PK <- pack(CFG)
ENG <- lapply(SPECS, function(sp) cc_cell_exact(PK$mu, PK$A, SEL, sp)$share)
names(ENG) <- SPECS

## ---------------------------------------------------------------------------
## A0. Golden agreement with the Python reference engine
##
## Recorded from `exact_mass()` in the audit's verification program on exactly
## the configurations above. Regenerate with CC_EMIT_GOLDEN=1.
## ---------------------------------------------------------------------------

GOLDEN <- c(
"G1-generic|none"=0.53233523127796689,
"G1-generic|party"=0.49100908358795325,
"G1-generic|pol"=0.9936303775905978,
"G1-generic|gov"=0.7221854724481499,
"G1-generic|any"=0.9936303775905978,
"G2-generic|none"=0.97592893784096046,
"G2-generic|party"=0.62731400318717823,
"G2-generic|pol"=1,
"G2-generic|gov"=0.99997401306954448,
"G2-generic|any"=1,
"G3-generic|none"=0.78104033775365089,
"G3-generic|party"=0.32524752898277154,
"G3-generic|pol"=1,
"G3-generic|gov"=1,
"G3-generic|any"=1,
"G4-generic|none"=0.21859688611133984,
"G4-generic|party"=0.34929682756979552,
"G4-generic|pol"=1,
"G4-generic|gov"=0.57868548058997038,
"G4-generic|any"=1,
"Z-zero-slope-econ|none"=0.63280844123576196,
"Z-zero-slope-econ|party"=0.43207075079892654,
"Z-zero-slope-econ|pol"=0.9670175727419642,
"Z-zero-slope-econ|gov"=1,
"Z-zero-slope-econ|any"=1,
"Z-identically-zero-econ|none"=2.2085627408651802e-75,
"Z-identically-zero-econ|party"=0.10633228834175718,
"Z-identically-zero-econ|pol"=0.99999999997722921,
"Z-identically-zero-econ|gov"=1,
"Z-identically-zero-econ|any"=1,
"Z-coincident-kinks|none"=0.82984259868056276,
"Z-coincident-kinks|party"=0.7604979437315641,
"Z-coincident-kinks|pol"=1,
"Z-coincident-kinks|gov"=1,
"Z-coincident-kinks|any"=1,
"Z-gov-benefit-identically-0|none"=0.0026175927169698188,
"Z-gov-benefit-identically-0|party"=0.41959514736429027,
"Z-gov-benefit-identically-0|pol"=0.99894803540808852,
"Z-gov-benefit-identically-0|gov"=0.0026175927169698188,
"Z-gov-benefit-identically-0|any"=0.99894803540808852,
"Z-flat-penalty|none"=0,
"Z-flat-penalty|party"=0.67138967984702591,
"Z-flat-penalty|pol"=1,
"Z-flat-penalty|gov"=0.49345687208859235,
"Z-flat-penalty|any"=1,
"Z-any-minimal|none"=0,
"Z-any-minimal|party"=0.076563725509834701,
"Z-any-minimal|pol"=0,
"Z-any-minimal|gov"=0,
"Z-any-minimal|any"=0.076563725509834701,
"Z-collision-root-at-kink|none"=0.5,
"Z-collision-root-at-kink|party"=0.5,
"Z-collision-root-at-kink|pol"=0.5,
"Z-collision-root-at-kink|gov"=0.5,
"Z-collision-root-at-kink|any"=0.5,
"Z-six-gov-kinks|none"=0.5,
"Z-six-gov-kinks|party"=0.5,
"Z-six-gov-kinks|pol"=0.5,
"Z-six-gov-kinks|gov"=0.64625493909194276,
"Z-six-gov-kinks|any"=0.64625493909194276,
"Z-floor-gate-gap|none"=1,
"Z-floor-gate-gap|party"=1,
"Z-floor-gate-gap|pol"=1,
"Z-floor-gate-gap|gov"=1,
"Z-floor-gate-gap|any"=1,
"Z-flat-zero-penalty|none"=1,
"Z-flat-zero-penalty|party"=1,
"Z-flat-zero-penalty|pol"=1,
"Z-flat-zero-penalty|gov"=1,
"Z-flat-zero-penalty|any"=1,
"Z-interior-C11|none"=1.6927354784535001e-07,
"Z-interior-C11|party"=0.38952408049961162,
"Z-interior-C11|pol"=0.38412937509080769,
"Z-interior-C11|gov"=1.1589826782731905e-05,
"Z-interior-C11|any"=0.56426278930837936,
"Z-interior-C12|none"=0.04486509025191554,
"Z-interior-C12|party"=0.002089101167995544,
"Z-interior-C12|pol"=0.38314672135840938,
"Z-interior-C12|gov"=0.1159113358587881,
"Z-interior-C12|any"=0.38314672135840938
)

eng_flat <- unlist(lapply(SPECS, function(sp)
  stats::setNames(ENG[[sp]], paste(names(CFG), sp, sep = "|"))))

if (length(GOLDEN) && all(names(eng_flat) %in% names(GOLDEN))) {
  d0 <- max(abs(eng_flat - GOLDEN[names(eng_flat)]))
  report("A0", "golden Python-oracle agreement (16 cfg x 5 col)", d0 <= 1e-12,
         sprintf("max|R-golden| = %s over %d cells", fmt(d0), length(eng_flat)))
} else {
  report("A0", "golden Python-oracle agreement", NA, "no golden table")
}

## ---------------------------------------------------------------------------
## A1/A2. Live cross-language comparison
## ---------------------------------------------------------------------------

PY_REF <- Sys.getenv(
  "CC_PYTHON_REFERENCE",
  unset = paste0("/private/tmp/claude-501/-Users-xyq-GitHub-workbench/",
                 "19c7c76b-8936-45cf-92a1-271a667832b3/scratchpad/",
                 "audit_compensator.py"))

run_oracle <- function() {
  py <- Sys.which("python3")
  if (!nzchar(py) || !file.exists(PY_REF)) return(NULL)
  td <- tempfile("cc-oracle-"); dir.create(td)
  cin <- file.path(td, "configs.csv")
  cout <- file.path(td, "oracle.csv")
  drv <- file.path(td, "oracle.py")
  num <- function(x) sprintf("%.17g", x)
  hdr <- c("name", "m_p", "a_p", "m_b", "a_b", "m_e", "a_e", "m_s", "a_s",
           paste0("m_g", 1:6), paste0("a_g", 1:6))
  body <- vapply(names(CFG), function(nm) {
    cf <- CFG[[nm]]
    paste(c(nm, num(cf$p[1L]), num(cf$p[2L]), num(cf$b[1L]), num(cf$b[2L]),
            num(cf$e[1L]), num(cf$e[2L]), num(cf$s[1L]), num(cf$s[2L]),
            vapply(cf$g, function(z) num(z[1L]), character(1L)),
            vapply(cf$g, function(z) num(z[2L]), character(1L))),
          collapse = ",")
  }, character(1L))
  writeLines(c(paste(hdr, collapse = ","), body), cin)
  writeLines(c(
    "import sys, csv",
    "ref, cin, cout = sys.argv[1], sys.argv[2], sys.argv[3]",
    "text = open(ref).read()",
    "MARK = \"specs = ['none', 'party', 'pol', 'gov', 'any']\"",
    "if MARK not in text:",
    "    raise SystemExit('reference marker not found')",
    "ns = {}",
    "exec(compile(text[:text.index(MARK) + len(MARK)], ref, 'exec'), ns)",
    "exact_mass = ns['exact_mass']",
    "dense_mass = ns['dense_mass']",
    "draft_engine = ns['draft_engine']",
    "with open(cin) as f:",
    "    rows = list(csv.DictReader(f))",
    "with open(cout, 'w', newline='') as f:",
    "    w = csv.writer(f)",
    "    w.writerow(['name', 'spec', 'exact', 'dense', 'draft'])",
    "    for r in rows:",
    "        cfg = {'p': (float(r['m_p']), float(r['a_p'])),",
    "               'b': (float(r['m_b']), float(r['a_b'])),",
    "               'e': (float(r['m_e']), float(r['a_e'])),",
    "               's': (float(r['m_s']), float(r['a_s'])),",
    "               'g': [(float(r['m_g%d' % i]), float(r['a_g%d' % i]))",
    "                     for i in range(1, 7)]}",
    "        for spec in ['none', 'party', 'pol', 'gov', 'any']:",
    "            dr = ''",
    "            if spec in ('pol', 'gov', 'any'):",
    "                dr = '%.17g' % float(draft_engine(spec, cfg))",
    "            w.writerow([r['name'], spec,",
    "                        '%.17g' % float(exact_mass(spec, cfg)),",
    "                        '%.17g' % float(dense_mass(spec, cfg)), dr])"), drv)
  st <- suppressWarnings(system2(py, c(shQuote(drv), shQuote(PY_REF),
                                       shQuote(cin), shQuote(cout)),
                                 stdout = TRUE, stderr = TRUE))
  if (!file.exists(cout)) {
    attr(st, "failed") <- TRUE
    return(st)
  }
  utils::read.csv(cout, stringsAsFactors = FALSE)
}

ORACLE <- tryCatch(run_oracle(), error = function(e) NULL)
if (is.data.frame(ORACLE)) {
  ORACLE$exact <- as.numeric(ORACLE$exact)
  ORACLE$dense <- as.numeric(ORACLE$dense)
  key <- paste(ORACLE$name, ORACLE$spec, sep = "|")
  ok_key <- key %in% names(eng_flat)
  if (nzchar(Sys.getenv("CC_EMIT_GOLDEN"))) {
    cat("GOLDEN <- c(\n")
    cat(paste0('"', key[ok_key], '"=',
               vapply(ORACLE$exact[ok_key], function(z) sprintf("%.17g", z),
                      character(1L))), sep = ",\n")
    cat(")\n")
  }
  d1 <- max(abs(eng_flat[key[ok_key]] - ORACLE$exact[ok_key]))
  d2 <- max(abs(eng_flat[key[ok_key]] - ORACLE$dense[ok_key]))
  report("A1", "live Python exact engine", d1 <= 1e-12,
         sprintf("max|R-python| = %s over %d cells", fmt(d1), sum(ok_key)))
  report("A2", "live Python dense-breakpoint engine", d2 <= 1e-9,
         sprintf("max|R-dense| = %s", fmt(d2)))
} else {
  msg <- if (is.null(ORACLE)) "python3 or reference file unavailable" else
    "oracle driver failed"
  report("A1", "live Python exact engine", NA, msg)
  report("A2", "live Python dense-breakpoint engine", NA, msg)
}

## ---------------------------------------------------------------------------
## B. Any column = union of acceptance sets, not the literal breakpoint reuse
##
## `bad_any_literal()` deliberately implements the recipe the audit REFUTED:
## reuse the b_pol and b_gov breakpoint lists on h_any and treat the function
## as linear in between. It exists only so the test can show the shipped engine
## does not agree with it, and reproduces the audit's counterexample values.
## ---------------------------------------------------------------------------

bad_any_literal <- function(cfg) {
  r <- cfg_to_rows(cfg); mu <- r$mu; a <- r$a
  pts <- numeric(0)
  for (j in c(3L, 4L)) if (a[j] != 0) pts <- c(pts, -mu[j] / a[j])
  env <- rbind(c(0, 0), cbind(mu[5:10], a[5:10]))
  for (i in seq_len(nrow(env))) for (j in seq_len(nrow(env))) {
    if (j > i && env[i, 2L] != env[j, 2L]) {
      x <- (env[j, 1L] - env[i, 1L]) / (env[i, 2L] - env[j, 2L])
      v <- env[i, 1L] + env[i, 2L] * x
      em <- max(env[, 1L] + env[, 2L] * x)
      if (abs(v - em) <= 1e-9 * max(1, abs(em))) pts <- c(pts, x)
    }
  }
  grid <- c(-Inf, sort(unique(pts)), Inf)
  mass <- 0
  for (k in seq_len(length(grid) - 1L)) {
    lo <- grid[k]; hi <- grid[k + 1L]
    if (!(hi > lo)) next
    if (!is.finite(lo) && !is.finite(hi)) { x1 <- -1; x2 <- 1 }
    else if (!is.finite(lo)) { x1 <- hi - 2; x2 <- hi - 1 }
    else if (!is.finite(hi)) { x1 <- lo + 1; x2 <- lo + 2 }
    else { w <- hi - lo; x1 <- lo + 0.25 * w; x2 <- lo + 0.75 * w }
    v1 <- cc_h_eval(mu, a, SEL, "any", x1); v2 <- cc_h_eval(mu, a, SEL, "any", x2)
    sl <- (v2 - v1) / (x2 - x1)
    if (sl == 0) {
      if (v1 >= 0) mass <- mass + stats::pnorm(hi) - stats::pnorm(lo)
    } else {
      rt <- x1 - v1 / sl
      if (sl > 0) { A_ <- max(lo, rt); B_ <- hi } else { A_ <- lo; B_ <- min(hi, rt) }
      if (B_ > A_) mass <- mass + stats::pnorm(B_) - stats::pnorm(A_)
    }
  }
  mass
}

any_true_min <- ENG$any[[which(names(CFG) == "Z-any-minimal")]]
any_true_gen <- ENG$any[[which(names(CFG) == "Z-interior-C11")]]
any_bad_min <- bad_any_literal(CFG[["Z-any-minimal"]])
any_bad_gen <- bad_any_literal(CFG[["Z-interior-C11"]])

b1 <- abs(any_true_min - 0.076564) < 5e-7 && abs(any_true_gen - 0.564263) < 5e-7
report("B1", "Any = union: audit true values reproduced", b1,
       sprintf("minimal %.6f (audit 0.076564); generic %.6f (audit 0.564263)",
               any_true_min, any_true_gen))
b2 <- abs(any_bad_min - 0.031645) < 5e-7 && abs(any_bad_gen - 0.545472) < 5e-7 &&
  abs(any_true_min - any_bad_min) > 0.04 && abs(any_true_gen - any_bad_gen) > 0.018
report("B2", "buggy literal reuse reproduced and rejected", b2,
       sprintf("literal %.6f / %.6f (audit 0.031645 / 0.545472); gaps %.6f / %.6f",
               any_bad_min, any_bad_gen,
               abs(any_true_min - any_bad_min), abs(any_true_gen - any_bad_gen)))

## Any as an explicit three-way OR of the acceptance events, checked by dense MC
## on the two counterexample configs (independent of both engines).
or_mc <- function(cfg, n = 4e6, seed = 11L) {
  set.seed(seed); r <- cfg_to_rows(cfg); u <- stats::rnorm(n)
  ok <- (cc_h_eval(r$mu, r$a, SEL, "party", u) >= 0) |
    (cc_h_eval(r$mu, r$a, SEL, "pol", u) >= 0) |
    (cc_h_eval(r$mu, r$a, SEL, "gov", u) >= 0)
  mean(ok)
}
mc_min <- or_mc(CFG[["Z-any-minimal"]]); mc_gen <- or_mc(CFG[["Z-interior-C11"]])
b3 <- abs(any_true_min - mc_min) < 1e-3 && abs(any_true_gen - mc_gen) < 1e-3
report("B3", "Any = archived three-way OR (4e6-draw MC)", b3,
       sprintf("|engine-MC| = %.2e / %.2e", abs(any_true_min - mc_min),
               abs(any_true_gen - mc_gen)))

## ---------------------------------------------------------------------------
## C/D. Independent in-R breakpoint engine + convexity structure
## ---------------------------------------------------------------------------

dc <- 0; ncomp <- 0L; bounded <- FALSE
for (nm in names(CFG)) {
  r <- cfg_to_rows(CFG[[nm]])
  for (sp in SPECS) {
    bp <- cc_cell_breakpoints(r$mu, r$a, SEL, sp)
    dc <- max(dc, abs(bp$share - ENG[[sp]][[which(names(CFG) == nm)]]))
    ncomp <- max(ncomp, bp$n_components)
    bounded <- bounded || bp$bounded_component
  }
}
report("C", "in-R breakpoint engine vs half-line engine", dc <= 1e-12,
       sprintf("max|half-line - breakpoint| = %s", fmt(dc)))
report("D", "convexity: <=2 components, none bounded", ncomp <= 2L && !bounded,
       sprintf("max components = %d; any bounded component = %s", ncomp, bounded))

## ---------------------------------------------------------------------------
## E. Domination theorems over many random configs
## ---------------------------------------------------------------------------

set.seed(4242L)
NR <- 800L
RC <- replicate(NR, rand_cfg(), simplify = FALSE)
PR <- pack(RC)
RS <- lapply(SPECS, function(sp) cc_cell_exact(PR$mu, PR$A, SEL, sp)$share)
names(RS) <- SPECS
pairs <- list(c("none", "pol"), c("none", "gov"), c("none", "any"),
              c("party", "any"), c("pol", "any"), c("gov", "any"))
viol <- 0L; worst <- 0
for (pp in pairs) {
  d <- RS[[pp[1L]]] - RS[[pp[2L]]]
  viol <- viol + sum(d > 1e-12)
  worst <- max(worst, max(d))
}
report("E", "domination (Full/Gov/Any >= None; Any >= each)", viol == 0L,
       sprintf("%d violations over %d configs x %d pairs; worst excess = %s",
               viol, NR, length(pairs), fmt(worst)))
## Co-party has no such gate; confirm the engine can put it below None.
report("E2", "Co-party correctly has NO domination gate",
       any(RS$party < RS$none - 0.05),
       sprintf("min(party - none) = %.6f over %d configs",
               min(RS$party - RS$none), NR))

## ---------------------------------------------------------------------------
## F. Free invariances
## ---------------------------------------------------------------------------

NEG <- lapply(SPECS, function(sp) cc_cell_exact(PR$mu, -PR$A, SEL, sp)$share)
names(NEG) <- SPECS
dsign <- max(vapply(SPECS, function(sp) max(abs(RS[[sp]] - NEG[[sp]])), numeric(1L)))
report("F1", "invariance A -> -A", dsign <= 1e-14,
       sprintf("max|C(A) - C(-A)| = %s over %d configs x 5 cols", fmt(dsign), NR))

perm <- c(4L, 1L, 6L, 2L, 5L, 3L)
SELP <- SEL; SELP$governance <- SEL$governance[perm]
dperm <- max(vapply(c("gov", "any"), function(sp)
  max(abs(RS[[sp]] - cc_cell_exact(PR$mu, PR$A, SELP, sp)$share)), numeric(1L)))
report("F2", "invariance to permuting governance selectors", dperm == 0,
       sprintf("max deviation = %s (exact 0 expected)", fmt(dperm)))

## Power check: flipping ONE loading entry is not an invariance.
Aflip <- PR$A; Aflip[, 5L] <- -Aflip[, 5L]
dflip <- max(abs(RS$gov - cc_cell_exact(PR$mu, Aflip, SEL, "gov")$share))
report("F3", "single-coordinate sign flip IS detected", dflip > 1e-6,
       sprintf("max change = %.6f (must be nonzero)", dflip))

## ---------------------------------------------------------------------------
## G. Sigma = 0 reduces to the plain indicator fractions
## ---------------------------------------------------------------------------

A0 <- matrix(0, nrow = NR, ncol = P)
bad0 <- 0L
for (sp in SPECS) {
  eng <- cc_cell_exact(PR$mu, A0, SEL, sp)$share
  ind <- vapply(seq_len(NR), function(i)
    as.numeric(cc_h_eval(PR$mu[i, ], rep(0, P), SEL, sp, 0) >= 0), numeric(1L))
  bad0 <- bad0 + sum(eng != ind)
}
report("G", "Sigma = 0 == plain indicator fractions", bad0 == 0L,
       sprintf("%d mismatches over %d configs x 5 cols (exact equality)",
               bad0, NR))

## ---------------------------------------------------------------------------
## H. Capstones against the closed forms
## ---------------------------------------------------------------------------

## `cc_closed_form_linear()` deliberately follows the package's audited route
## (`scmix_paper_signshare`: variance = crossprod(d, Sigma %*% d), Sigma = A A',
## ties = "include"), NOT the numerically stable |c'A|. That is the whole point
## of the memo's "order 1e-13, not bitwise" gate: when c'A nearly cancels, the
## double sum c'(A A')c loses relative precision that |c'A| does not. H1b
## isolates the two effects -- against |c'A| the engine is exact to ~1e-16, so
## the residual in H1 is the closed form's rounding, not an engine error.
c_party <- SEL$action + SEL$coparty
cf_party <- cc_closed_form_linear(PR$mu, PR$A, c_party)
dcap <- max(abs(RS$party - cf_party))
report("H1", "capstone: Co-party engine vs closed-form C_0", dcap <= 1e-12,
       sprintf("max|engine - Phi(c'mu/sqrt(c'Sigma c))| = %s (memo gate: order 1e-13)",
               fmt(dcap)))
z_stable <- as.numeric(PR$mu %*% c_party) / abs(as.numeric(PR$A %*% c_party))
cf_stable <- ifelse(is.finite(z_stable), stats::pnorm(z_stable),
                    as.numeric(as.numeric(PR$mu %*% c_party) >= 0))
dcap2 <- max(abs(RS$party - cf_stable))
report("H1b", "capstone against the |c'A| route (engine exactness)",
       dcap2 <= 1e-13, sprintf("max|engine - Phi(c'mu/|c'A|)| = %s", fmt(dcap2)))
cf_none <- cc_closed_form_linear(PR$mu, PR$A, SEL$action)
dnone <- max(abs(RS$none - cf_none))
report("H2", "capstone: b = 0 engine vs the S_0 closed form", dnone <= 1e-12,
       sprintf("max|engine - Phi(c_p'mu/s)| = %s", fmt(dnone)))
## And at a non-unit amount a, the manuscript's C_0(c_p, c_b; a).
for (amt in c(0.5, 2)) {
  e_a <- cc_cell_exact(PR$mu, PR$A, SEL, "party", coparty_amount = amt)$share
  d_a <- max(abs(e_a - cc_closed_form_linear(PR$mu, PR$A,
                                             SEL$action + amt * SEL$coparty)))
  report("H3", sprintf("capstone at amount a = %g", amt), d_a <= 1e-13,
         sprintf("max deviation = %s", fmt(d_a)))
}

## ---------------------------------------------------------------------------
## I. Gate metadata: per-respondent binding-line sensitivity
## ---------------------------------------------------------------------------

## The action selector is a unit coordinate not used by any benefit, so a shift
## of mu[action] is exactly a level shift of h. The reported sensitivity must
## equal d(share)/d(shift).
shift_share <- function(sp, t, mu, A) {
  m2 <- mu; m2[, 1L] <- m2[, 1L] + t
  cc_cell_exact(m2, A, SEL, sp)$share
}
worst_sens <- 0; n_used <- 0L
for (sp in c("none", "party", "pol", "gov", "any")) {
  res <- cc_cell_exact(PR$mu, PR$A, SEL, sp)
  h <- 1e-6
  fd <- (shift_share(sp, h, PR$mu, PR$A) - shift_share(sp, -h, PR$mu, PR$A)) / (2 * h)
  ## Restrict to respondents where the central difference is itself accurate:
  ## a binding slope below 1e-2 makes the cell a near-step in the shift, and a
  ## tied endpoint is a collision manifold where only one-sided derivatives
  ## exist (audit Finding 1).
  use <- res$n_endpoints > 0 & !res$whole & !res$tied & res$binding_min_slope > 1e-2
  if (any(use)) {
    worst_sens <- max(worst_sens, max(abs(res$sensitivity[use] - fd[use])))
    n_used <- n_used + sum(use)
  }
}
report("I1", "sensitivity == d(cell)/d(level shift), numerically",
       worst_sens < 1e-5,
       sprintf("max|analytic - central difference| = %s over %d respondents",
               fmt(worst_sens), n_used))

i_c12 <- which(names(CFG) == "Z-interior-C12")
sens_c12 <- cc_cell_exact(PK$mu, PK$A, SEL, "pol")$sensitivity[i_c12]
report("I2", "audit probe: dC/dmu_p = 0.22112935 on C12/pol",
       abs(sens_c12 - 0.22112935) < 5e-8,
       sprintf("engine sensitivity = %.8f (pol cell %.6f, audit 0.383147)",
               sens_c12, ENG$pol[[i_c12]]))

## The MAX-SLOPE gate is one-sided: the audit's Z-floor-gate-gap config
## passes it while a binding slope sits below the floor.
i_fg <- which(names(CFG) == "Z-floor-gate-gap")
fg <- cc_cell_exact(PK$mu, PK$A, SEL, "gov", floor = 0.01)
mu_fg <- PK$mu; mu_fg[i_fg, 1L] <- mu_fg[i_fg, 1L] - 0.06
fg_shift <- cc_cell_exact(mu_fg, PK$A, SEL, "gov", floor = 0.01)
report("I3", "max-slope gate is one-sided; per-respondent flag catches it",
       fg$max_abs_slope[i_fg] > 0.01 &&
         abs(fg$share[i_fg] - fg_shift$share[i_fg]) > 0.5,
       sprintf("max slope %.3f clears floor 0.01, yet share %.6f -> %.6f under a -0.06 mean shift",
               fg$max_abs_slope[i_fg], fg$share[i_fg], fg_shift$share[i_fg]))

## AUDIT C2 REGRESSION. The I3 canary must not be able to receive a
## reportable status. Put it in a cell of its own and check the fail-closed
## binding-slope gate withholds it, and that the old max-slope rule would
## have released it.
mu_i3 <- PK$mu[i_fg, , drop = FALSE]
A_i3 <- PK$A[i_fg, , drop = FALSE]
tab_i3 <- cc_columns(mu_i3, A_i3, list(u_action = "u_action"), SEL,
                     specs = "gov", floors = 0.01)
old_rule_would_release <- tab_i3$max_abs_slope >= tab_i3$floor
report("I3b", "I3 canary cannot receive a reportable status",
       isTRUE(tab_i3$floored) && is.na(tab_i3$released_value) &&
         grepl("^withheld", tab_i3$release_kind) &&
         isTRUE(old_rule_would_release),
       sprintf("binding slope %.4f < floor %.2f -> '%s'; old max-slope rule (%.3f) would have released",
               tab_i3$binding_slope_min, tab_i3$floor, tab_i3$release_kind,
               tab_i3$max_abs_slope))

## AUDIT C2 REGRESSION. One favourable respondent must not release a cell
## whose other respondents fail the gate.
i_ok <- which(names(CFG) == "G1-generic")
mu_mix <- PK$mu[c(i_ok, i_fg), , drop = FALSE]
A_mix <- PK$A[c(i_ok, i_fg), , drop = FALSE]
tab_mix <- cc_columns(mu_mix, A_mix, list(u_action = "u_action"), SEL,
                      specs = "gov", floors = 0.01)
report("I3c", "one clearing respondent cannot release a failing cell",
       isTRUE(tab_mix$floored) && is.na(tab_mix$released_value) &&
         tab_mix$n_gate_failures >= 1L,
       sprintf("%d of %d respondents fail the binding-slope gate; release '%s'",
               tab_mix$n_gate_failures, tab_mix$n_respondents,
               tab_mix$release_kind))

## AUDIT C2. cc_composite_floor(): an ALIGNED-LOADING example where the
## composite support line c_p + 3c_e + 3c_s has a far larger slope than any
## single coordinate. The marginal maximum understates the floor the
## calibration supports; the envelope does not.
reps_al <- matrix(0, nrow = 40L, ncol = P)
set.seed(4242L)
reps_al[, 1L] <- abs(stats::rnorm(40L, 0.10, 0.01))    # action
reps_al[, 3L] <- abs(stats::rnorm(40L, 0.10, 0.01))    # econ
reps_al[, 4L] <- abs(stats::rnorm(40L, 0.10, 0.01))    # social
marg_al <- cc_reduce_floor(apply(reps_al, 2L, stats::quantile, probs = 0.95,
                                 names = FALSE, type = 1L),
                           SEL, "pol", quiet = TRUE)
comp_al <- cc_composite_floor(reps_al, SEL, "pol", gamma = 0.05)
report("C2a", "composite envelope floor exceeds the marginal maximum",
       comp_al$column > 5 * marg_al,
       sprintf("marginal max %.4f vs composite envelope %.4f (ratio %.1f) for c_p + 3c_e + 3c_s",
               marg_al, comp_al$column, comp_al$column / marg_al))

## ---------------------------------------------------------------------------
## C5. Matched composite calibration (audit work package 5)
##
## The triangle envelope bounds ||A~' c|| from above with
## sum_j |c_j| ||A~_j.||. With the SIGNED A~ retained the exact statistic is
## computable, and it must never come out larger. Both are aggregated the
## same way --- fold-average within replication, maximum over the column's
## support lines within the replication, then the quantile across
## replications --- so the pointwise inequality survives aggregation.
## ---------------------------------------------------------------------------

## A synthetic calibration in sb_zero_floor(keep_loadings = TRUE) shape.
mk_signed_cal <- function(R = 24L, n_folds = 2L, gamma = 0.05, seed = 808L,
                          p = P, q = 1L) {
  set.seed(seed)
  n <- R * n_folds
  A_list <- lapply(seq_len(n), function(i)
    matrix(stats::rnorm(p * q, sd = 0.08), p, q,
           dimnames = list(ATTR, NULL)))
  draws <- t(vapply(A_list, function(A) sqrt(rowSums(A^2)), numeric(p)))
  colnames(draws) <- ATTR
  list(draws = draws, rep_id = rep(seq_len(R), each = n_folds),
       fold_of_draw = rep(seq_len(n_folds), times = R),
       R = R, folds_use = seq_len(n_folds), gamma = gamma,
       attr_names = ATTR, A_raw_draws = A_list, keep_loadings = TRUE)
}
cal5 <- mk_signed_cal()

matched_le_tri <- TRUE
ratios5 <- numeric(0)
for (sp in SPECS) {
  mm <- cc_composite_floor(cal5, SEL, sp, gamma = cal5$gamma,
                           method = "matched_composite")
  tt <- cc_composite_floor(cal5, SEL, sp, gamma = cal5$gamma,
                           method = "triangle_fallback")
  matched_le_tri <- matched_le_tri && (mm$column <= tt$column + 1e-12)
  ratios5 <- c(ratios5, mm$column / tt$column)
}
report("C5a", "matched composite floor never exceeds the triangle fallback",
       matched_le_tri,
       sprintf("matched/triangle ratio by column: %s",
               paste(sprintf("%.2f", ratios5), collapse = " ")))

## The aggregation order is load-bearing: maximising within a replication
## and then quantiling dominates quantiling each line and then maximising.
C5 <- .cc_support_contrasts(SEL, "any")
reps5 <- sb_matched_contrast_reps(cal5, C5)
q_then_max <- max(apply(reps5, 2L, stats::quantile, probs = 0.95,
                        names = FALSE, type = 1L))
max_then_q <- cc_composite_floor(cal5, SEL, "any", gamma = 0.05,
                                 method = "matched_composite")$column
report("C5b", "matched aggregation is max-then-quantile (the conservative order)",
       max_then_q >= q_then_max - 1e-12,
       sprintf("max-then-quantile %.4f >= quantile-then-max %.4f",
               max_then_q, q_then_max))

report("C5d", "cc_composite_floor stamps the method token",
       identical(cc_composite_floor(cal5, SEL, "pol", gamma = 0.05,
                                    method = "matched_composite")$floor_rule,
                 "matched_composite") &&
         identical(cc_composite_floor(cal5$draws, SEL, "pol",
                                      gamma = 0.05)$floor_rule,
                   "triangle_fallback"),
       "matched_composite / triangle_fallback")

report("C5e", "matched_composite is refused when no signed draws exist",
       grepl("needs a calibration carrying signed loadings",
             tryCatch({ cc_composite_floor(cal5$draws, SEL, "pol",
                                           gamma = 0.05,
                                           method = "matched_composite")
                        "" }, error = conditionMessage), fixed = TRUE),
       "errors rather than silently falling back")

## A respondent whose binding slope sits between the two floors: released
## under the marginal maximum, withheld under the composite envelope.
mu_al <- matrix(0, 1L, P); A_al <- matrix(0, 1L, P)
mu_al[1L, 1L] <- -0.30                       # action penalty
## Support slopes c_p +- 3c_e +- 3c_s = {0.51, 0.33, 0.27, 0.09}, all
## positive and none cancelling: the binding slope is 0.51, above the
## marginal-max floor and below the composite envelope floor.
A_al[1L, 1L] <- 0.30; A_al[1L, 3L] <- 0.04; A_al[1L, 4L] <- 0.03
tab_marg <- cc_columns(mu_al, A_al, list(u_action = "u_action"), SEL,
                       specs = "pol", floors = marg_al)
tab_comp <- cc_columns(mu_al, A_al, list(u_action = "u_action"), SEL,
                       specs = "pol", floors = comp_al$column)
## The same canary against a MATCHED floor built from signed draws whose
## aligned rows make the composite slope genuinely large: still withheld.
cal_al <- local({
  R <- 24L; nf <- 2L
  set.seed(5150L)
  A_list <- lapply(seq_len(R * nf), function(i) {
    A <- matrix(0, P, 1L, dimnames = list(ATTR, NULL))
    A[1L, 1L] <- abs(stats::rnorm(1L, 0.10, 0.01))
    A[3L, 1L] <- abs(stats::rnorm(1L, 0.10, 0.01))
    A[4L, 1L] <- abs(stats::rnorm(1L, 0.10, 0.01))
    A
  })
  draws <- t(vapply(A_list, function(A) sqrt(rowSums(A^2)), numeric(P)))
  colnames(draws) <- ATTR
  list(draws = draws, rep_id = rep(seq_len(R), each = nf),
       R = R, folds_use = seq_len(nf), gamma = 0.05, attr_names = ATTR,
       A_raw_draws = A_list, keep_loadings = TRUE)
})
mf_al <- cc_composite_floor(cal_al, SEL, "pol", gamma = 0.05,
                            method = "matched_composite")
tab_matched <- cc_columns(mu_al, A_al, list(u_action = "u_action"), SEL,
                          specs = "pol", floors = mf_al$column,
                          floor_rule = mf_al$floor_rule)
report("C5c", "the aligned-loading canary stays withheld under the matched floor",
       isTRUE(tab_matched$floored) && is.na(tab_matched$released_value) &&
         identical(tab_matched$floor_rule[[1L]], "matched_composite"),
       sprintf("binding slope %.3f < matched floor %.3f; floor_rule = %s",
               tab_matched$binding_slope_min, mf_al$column,
               tab_matched$floor_rule[[1L]]))

report("C2b", "aligned-loading cell: released under marginal max, withheld under composite",
       !isTRUE(tab_marg$floored) && isTRUE(tab_comp$floored) &&
         is.na(tab_comp$released_value),
       sprintf("binding slope %.3f; marginal floor %.4f releases, composite floor %.4f withholds",
               tab_comp$binding_slope_min, marg_al, comp_al$column))

## ---------------------------------------------------------------------------
## J. Floored cells inherit the None bound (Co-party does not)
## ---------------------------------------------------------------------------

mu_j <- PR$mu[1:50, , drop = FALSE]
A_j <- matrix(1e-6, nrow = 50L, ncol = P)
acts <- list(u_action = "u_action")
## SAME-SIDE INHERITANCE (audit C3). The bound must declare the event side
## it certifies; only an acceptance-side bound may be inherited here.
bound_acc <- structure(0.42, event_side = "acceptance")
tab_j <- suppressWarnings(
  cc_columns(mu_j, A_j, acts, SEL, group = NULL, floors = 1e-3,
             none_bounds = bound_acc))
inh <- tab_j[tab_j$spec %in% c("none", "pol", "gov", "any"), ]
cop <- tab_j[tab_j$spec == "party", ]
j_ok <- all(inh$floored) && all(inh$released_value == 0.42) &&
  all(inh$release_kind == "lower_bound") && !any(is.na(inh$released_value)) &&
  all(is.finite(inh$share)) &&
  isTRUE(cop$floored) && is.na(cop$released_value) &&
  grepl("no domination inheritance", cop$release_kind)
report("J1", "floored None/Full/Gov/Any inherit a SAME-SIDE certified bound",
       j_ok,
       sprintf("released %s; Co-party '%s'",
               paste(unique(inh$released_value), collapse = "/"),
               cop$release_kind))

## An OPPOSITION-side bound is the real Graham--Svolik case: refused.
tab_wrong <- suppressWarnings(
  cc_columns(mu_j, A_j, acts, SEL, group = NULL, floors = 1e-3,
             none_bounds = structure(0.42, event_side = "opposition")))
wrong <- tab_wrong[tab_wrong$spec %in% c("none", "pol", "gov", "any"), ]
report("J1b", "an opposition-side None bound is refused, not inherited",
       all(is.na(wrong$released_value)) &&
         all(grepl("opposition", wrong$release_kind)),
       sprintf("release_kind: '%s'", unique(wrong$release_kind)[1L]))

## A bound with no declared side is refused as unknown-side.
tab_unk <- suppressWarnings(
  cc_columns(mu_j, A_j, acts, SEL, group = NULL, floors = 1e-3,
             none_bounds = 0.42))
unk <- tab_unk[tab_unk$spec %in% c("none", "pol", "gov", "any"), ]
report("J1c", "an undeclared-side None bound is refused",
       all(is.na(unk$released_value)) &&
         all(grepl("no event_side", unk$release_kind)),
       sprintf("release_kind: '%s'", unique(unk$release_kind)[1L]))

## Data-frame form, mixed sides: only the acceptance-side row inherits.
nb_df <- data.frame(action = "u_action", group = "Overall", bound = 0.42,
                    event_side = "acceptance", stringsAsFactors = FALSE)
tab_df <- suppressWarnings(
  cc_columns(mu_j, A_j, acts, SEL, group = NULL, floors = 1e-3,
             none_bounds = nb_df))
nb_df$event_side <- "opposition"
tab_df2 <- suppressWarnings(
  cc_columns(mu_j, A_j, acts, SEL, group = NULL, floors = 1e-3,
             none_bounds = nb_df))
report("J1d", "data-frame none_bounds honours event_side",
       all(tab_df$released_value[tab_df$spec == "none"] == 0.42) &&
         all(is.na(tab_df2$released_value[tab_df2$spec == "none"])),
       "acceptance row inherits; the same row on the opposition side does not")

## A cell with no floor at all can never release the ENGINE POINT: the gate
## cannot be evaluated, so the point fails closed. A same-side certified
## bound may still be inherited, because that bound does not depend on the
## gate; with no bound the cell is withheld outright.
tab_nofloor <- cc_columns(mu_j, A_j, acts, SEL, group = NULL,
                          floors = NA_real_, none_bounds = bound_acc)
tab_nofloor_nb <- cc_columns(mu_j, A_j, acts, SEL, group = NULL,
                             floors = NA_real_, none_bounds = NULL)
report("J1e", "no floor supplied -> the engine point is never released",
       !any(tab_nofloor$release_kind == "point") &&
         !any(tab_nofloor_nb$release_kind == "point") &&
         all(is.na(tab_nofloor_nb$released_value)) &&
         any(grepl("no floor supplied", tab_nofloor_nb$release_kind)),
       sprintf("with a same-side bound: '%s'; without: '%s'",
               unique(tab_nofloor$release_kind)[1L],
               unique(tab_nofloor_nb$release_kind)[1L]))
tab_ok <- cc_columns(PR$mu[1:50, , drop = FALSE], PR$A[1:50, , drop = FALSE],
                     acts, SEL, floors = 1e-6, none_bounds = bound_acc)
report("J2", "unfloored cells release the point value",
       all(!tab_ok$floored) && all(tab_ok$release_kind == "point") &&
         max(abs(tab_ok$released_value - tab_ok$share)) == 0,
       sprintf("%d cells, all release_kind = point", nrow(tab_ok)))
report("J3", "cc_check_domination() finds no violations",
       nrow(cc_check_domination(tab_ok)) == 0L, "0 violations")

## ---------------------------------------------------------------------------
## K. Degenerate lines: no crash, no NaN
## ---------------------------------------------------------------------------

k_vals <- unlist(ENG)
k_ok <- all(is.finite(k_vals)) && all(k_vals >= 0 & k_vals <= 1)
i_z <- which(names(CFG) == "Z-identically-zero-econ")
report("K1", "zero-slope / identically-zero lines: no NaN, no 0/0", k_ok,
       sprintf("all %d cells finite in [0,1]; identically-zero-econ pol = %.6f",
               length(k_vals), ENG$pol[[i_z]]))

## The memo's boundary-mass condition: {h = 0} carries positive mass exactly
## when some support line is identically zero. Flag it; do not assume it away.
zl <- cc_cell_exact(PK$mu, PK$A, SEL, "none")$zero_line
i_fz <- which(names(CFG) == "Z-flat-zero-penalty")
k2 <- zl[i_fz] && sum(zl) == 1L && ENG$none[[i_fz]] == 1
report("K2", "identically-zero piece flagged (>= carries real mass)", k2,
       sprintf("flat-zero-penalty: zero_line = TRUE, None cell = %.1f under `>=`; %d of %d configs flagged",
               ENG$none[[i_fz]], sum(zl), length(zl)))

## ---------------------------------------------------------------------------
## L. q > 1 seeded Monte Carlo fallback (memo step 7)
## ---------------------------------------------------------------------------

mu_l <- PR$mu[1:20, , drop = FALSE]
a_l <- matrix(PR$A[1L, ], ncol = 1L)                 # common p x 1 loading
ex_l <- mean(cc_cell_exact(mu_l, a_l, SEL, "pol", loading_form = "common")$share)
mc_l <- cc_cell_mc(mu_l, a_l, SEL, "pol", M = 40000L, seed = 7L)
report("L1", "MC fallback matches the exact engine at q = 1",
       abs(mc_l$share - ex_l) < 4 * max(mc_l$mc_se, 1e-6),
       sprintf("exact %.6f vs MC %.6f (se %.2e, doubled-M gap %.2e)",
               ex_l, mc_l$share, mc_l$mc_se, mc_l$replicate_gap))
A2q <- cbind(PR$A[1L, ], PR$A[2L, ] * 0.5)           # genuine q = 2
mc_q2 <- cc_cell_mc(mu_l, A2q, SEL, "gov", M = 40000L, seed = 9L)
report("L2", "MC fallback runs at q = 2 with a stable replicate",
       is.finite(mc_q2$share) && mc_q2$replicate_gap < 6 * mc_q2$mc_se,
       sprintf("share %.6f, se %.2e, doubled-M gap %.2e, printed-digit stable = %s",
               mc_q2$share, mc_q2$mc_se, mc_q2$replicate_gap,
               mc_q2$printed_digit_stable))
mc_def <- cc_cell_mc(mu_l, a_l, SEL, "pol", seed = 7L)$M
report("L3", "default M meets the memo's SE gate (SE <= 0.001)",
       mc_def >= 250000L, sprintf("default M = %d", mc_def))

## ---------------------------------------------------------------------------
## M. End-to-end on a fit-shaped object
##
## Mirrors the assembled-fit fields the engine consumes: A_folds (standardized),
## sd_dx_folds, mu_hat (task-level, out-of-fold), respondent_id, fold_id, K,
## attr_names, deltaX. This is the exact shape a real
## `fit_primary_assembled.rds` presents, so the other machine can swap it in.
## ---------------------------------------------------------------------------

set.seed(99L)
n_resp <- 120L; tasks <- 4L; K <- 2L
rid <- rep(sprintf("R%03d", seq_len(n_resp)), each = tasks)
fold <- rep(rep(1:K, length.out = n_resp), each = tasks)
mu_resp <- matrix(stats::rnorm(n_resp * P, 0, 0.6), n_resp, P,
                  dimnames = list(NULL, ATTR))
mu_task <- mu_resp[rep(seq_len(n_resp), each = tasks), , drop = FALSE]
sd_dx <- lapply(seq_len(K), function(k) stats::runif(P, 0.8, 1.6))
A_std <- lapply(seq_len(K), function(k)
  matrix(stats::rnorm(P, 0, 0.5) * sd_dx[[k]], ncol = 1L))
dX <- matrix(sample(-3:3, n_resp * tasks * P, TRUE), ncol = P,
             dimnames = list(NULL, ATTR))
fit <- structure(list(
  mu_hat = mu_task, A_folds = A_std, sd_dx_folds = sd_dx,
  respondent_id = rid, fold_id = fold, K = K, q = 1L,
  attr_names = ATTR, deltaX = dX),
  class = c("scmix_nested_assembled", "list"))

inp <- cc_fit_inputs(fit)
A_expect <- t(vapply(inp$fold, function(k) as.numeric(A_std[[k]][, 1L] / sd_dx[[k]]),
                     numeric(P)))
m_ok <- identical(dim(inp$mu), c(n_resp, P)) &&
  max(abs(inp$mu - mu_resp)) == 0 &&
  max(abs(inp$A - A_expect)) == 0 && inp$q == 1L
report("M1", "cc_fit_inputs(): respondent collapse + raw-scale loadings", m_ok,
       sprintf("%d respondents, q = %d, K = %d, max loading deviation = %s",
               nrow(inp$mu), inp$q, inp$K, fmt(max(abs(inp$A - A_expect)))))

ideo <- sample(1:7, n_resp, TRUE)
tab <- cc_compensator_columns(
  fit, actions = list(banProtest = "u_action"), coparty = "diff_respParty",
  econ = "diff_p1_num", social = "diff_p2_num", governance = ATTR[5:10],
  ideology = ideo, floors = 1e-4, none_bounds = 0.3)
groups_ok <- setequal(unique(tab$group),
                      c("Overall", levels(cc_ideology_group(1:7))))
overall <- tab[tab$group == "Overall", ]
sub <- tab[tab$group != "Overall", ]
wtd <- vapply(SPECS, function(sp) {
  s <- sub[sub$spec == sp, ]
  sum(s$share * s$n_respondents) / sum(s$n_respondents)
}, numeric(1L))
m2_ok <- groups_ok && nrow(tab) == 4L * 5L &&
  max(abs(wtd - overall$share[match(SPECS, overall$spec)])) < 1e-12 &&
  nrow(cc_check_domination(tab)) == 0L &&
  all(is.finite(tab$sensitivity)) && all(!is.na(tab$max_abs_slope))
report("M2", "end-to-end table: groups, aggregation, gates", m2_ok,
       sprintf("%d cells; overall = respondent-weighted group mean (max dev %s)",
               nrow(tab), fmt(max(abs(wtd - overall$share[match(SPECS, overall$spec)])))))

m3 <- tryCatch({
  bad <- fit; bad$deltaX <- dX * 1.5
  cc_compensator_columns(bad, actions = list(a = "u_action"),
                         coparty = "diff_respParty", econ = "diff_p1_num",
                         social = "diff_p2_num", governance = ATTR[5:10])
  FALSE
}, error = function(e) grepl("raw ordinal units|ordinal scale", conditionMessage(e)))
report("M3", "raw-ordinal-units assertion fires on a rescaled deltaX", m3,
       "stop() on non-integer policy differences")

m4 <- tryCatch({
  q2 <- fit; q2$A_folds <- lapply(seq_len(K), function(k) cbind(A_std[[k]], A_std[[k]]))
  cc_compensator_columns(q2, actions = list(a = "u_action"),
                         coparty = "diff_respParty", econ = "diff_p1_num",
                         social = "diff_p2_num", governance = ATTR[5:10],
                         assert_units = FALSE)
  FALSE
}, error = function(e) grepl("q = 2", conditionMessage(e)))
report("M4", "q > 1 fit is refused by the exact path with the MC pointer", m4,
       "stop() naming cc_cell_mc() and M = 250000")

## ---------------------------------------------------------------------------
## N. Selector and floor-reduction helpers
## ---------------------------------------------------------------------------

sel_num <- cc_selectors(ATTR, action = 1L, coparty = 2L, econ = 3L, social = 4L,
                        governance = lapply(5:10, function(j) {
                          v <- numeric(P); v[j] <- 1; v }))
n1 <- max(abs(cc_cell_exact(PK$mu, PK$A, sel_num, "any")$share - ENG$any)) == 0
report("N1", "selectors by name, index, and explicit vector agree", n1,
       "identical Any column under all three selector forms")

## Per-coordinate floor -> one scalar: the conservative (largest) floor over
## the coordinates that enter any support line of the column.
fv <- stats::setNames(c(0.05, 0.9, 0.02, 0.03, rep(0.01, 6)), ATTR)
n2 <- cc_reduce_floor(fv, SEL, "pol") == 0.05 &&      # action, econ, social
  cc_reduce_floor(fv, SEL, "gov") == 0.05 &&          # action + governance
  cc_reduce_floor(fv, SEL, "any") == 0.9 &&           # also touches co-party
  cc_reduce_floor(0.07, SEL, "pol") == 0.07
report("N2", "cc_reduce_floor(): conservative per-column reduction", n2,
       sprintf("pol %.2f, gov %.2f, any %.2f (co-party floor 0.90 enters Any only)",
               cc_reduce_floor(fv, SEL, "pol"), cc_reduce_floor(fv, SEL, "gov"),
               cc_reduce_floor(fv, SEL, "any")))

## ---------------------------------------------------------------------------

cat(sprintf("\n%d passed, %d failed, %d skipped\n", N_PASS, N_FAIL, N_SKIP))
if (N_FAIL > 0L) quit(status = 1L) else quit(status = 0L)
