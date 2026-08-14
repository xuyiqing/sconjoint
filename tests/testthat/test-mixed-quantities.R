## Tests for the derived scmix estimands (estimand-memo P-list):
## P1 scmix_average, P2 scmix_mrs/scmix_wtp + the weak-denominator
## guard, P4 scmix_design_check, P7 the raw-share benchmark, P3 the
## zero-floor wiring in print/summary, P5 the population-claim note.

skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())

fixed <- .fit_mixed_fixture()
fit <- fixed$fit

test_that("scmix_average reproduces the corrected psi group means (P1)", {
  ts <- sconjoint:::.scmix_theta_psi(fit)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  g <- factor(ifelse(fit$Z[first, "z1"] > 0, "hi", "lo"))

  out <- scmix_average(fit, by = g)
  expect_s3_class(out, "scmix_quantity")
  expect_length(out$estimate, 2L * ncol(fit$deltaX))

  for (lev in levels(g)) {
    rows <- which(g == lev)
    man_est <- colMeans(ts$psi[rows, , drop = FALSE])
    man_se <- sqrt(apply(ts$psi[rows, , drop = FALSE], 2L, var) / length(rows))
    got <- out$estimate[paste(lev, fit$attr_names, sep = ": ")]
    expect_equal(unname(got), unname(man_est), tolerance = 1e-10)
    got_se <- out$se[paste(lev, fit$attr_names, sep = ": ")]
    expect_equal(unname(got_se), unname(man_se), tolerance = 1e-10)
  }

  ## moderator-name form splits at the median
  out2 <- scmix_average(fit, by = "z1")
  expect_length(out2$estimate, 2L * ncol(fit$deltaX))
  ## bad length errors
  expect_error(scmix_average(fit, by = rep(1, 7)), "length")
})

test_that("scmix_mrs matches the theta ratio with a joint-IF SE (P2)", {
  th <- scmix_theta(fit)
  out <- scmix_mrs(fit, "a1yes", "a2yes")
  expect_equal(unname(out$estimate),
               unname(th$estimate[1L] / th$estimate[2L]),
               tolerance = 1e-10)
  expect_true(is.finite(out$se) && out$se > 0)
  ## delta-method equivalence: sd(IF)/sqrt(N) == quadratic form in the
  ## joint covariance of the corrected psi columns
  ts <- sconjoint:::.scmix_theta_psi(fit)
  V <- var(ts$psi) / nrow(ts$psi)
  r <- unname(out$estimate); thk <- unname(th$estimate[2L])
  se_quad <- sqrt((V[1, 1] - 2 * r * V[1, 2] + r^2 * V[2, 2]) / thk^2)
  expect_equal(unname(out$se), se_quad, tolerance = 1e-6)
  expect_true(all(c("fieller_lo", "fieller_hi", "fieller_type") %in%
                    names(out$extra)))

  wtp <- scmix_wtp(fit, "a1yes", "a2yes")
  expect_equal(unname(wtp$estimate), -unname(out$estimate), tolerance = 1e-10)
  expect_equal(unname(wtp$se), unname(out$se), tolerance = 1e-10)
})

test_that("the weak-denominator guard fires below t = 4 (P2/P6)", {
  expect_warning(
    sconjoint:::.sc_ratio_denominator_guard("sc_mrs", 0.1, 0.05, "x"),
    "weak denominator")
  expect_silent(
    sconjoint:::.sc_ratio_denominator_guard("sc_mrs", 1.0, 0.05, "x"))
})

test_that("scmix_design_check reports spectrum and loading t-ratios (P4)", {
  chk <- scmix_design_check(fit)
  p <- ncol(fit$deltaX)
  expect_s3_class(chk, "scmix_design_check")
  expect_length(chk$spectrum, p * chk$q)
  expect_equal(chk$spectrum[1L], 1)
  expect_true(all(diff(chk$spectrum) <= 1e-12))
  expect_equal(nrow(chk$loadings), p * chk$q)
  expect_true(all(chk$loadings$se_std > 0))
  expect_setequal(c(chk$identified, chk$not_identified), fit$attr_names)
  expect_output(print(chk), "design-rank check")
})

test_that(".sc_raw_share averages within respondent first (P7)", {
  ## respondent 1: T = 4 matching tasks, all y = 1; respondent 2: T = 1
  ## matching task, y = 0. Task pooling gives 4/5; respondent-first 1/2.
  dX <- matrix(c(1, 1, 1, 1, 1), 5, 1)
  y <- c(1, 1, 1, 1, 0)
  rid <- c(1, 1, 1, 1, 2)
  out <- sconjoint:::.sc_raw_share(dX, y, rid, cv = 1)
  expect_equal(out$raw_share, 0.5)
  expect_equal(out$raw_n_tasks, 5L)
  expect_equal(out$raw_n_respondents, 2L)
  ## negated contrasts contribute 1 - y
  out2 <- sconjoint:::.sc_raw_share(-dX, y, rid, cv = 1)
  expect_equal(out2$raw_share, 0.5)
  ## off-design contrast returns NA
  out3 <- sconjoint:::.sc_raw_share(dX, y, rid, cv = 2)
  expect_true(is.na(out3$raw_share))
  expect_equal(out3$raw_n_tasks, 0L)
})

test_that("scmix_counterfactual attaches the raw-share benchmark (P7)", {
  vc <- scmix_counterfactual(fit, contrast = c(1, 0))
  expect_true(all(c("raw_share", "raw_share_se", "raw_n_tasks",
                    "raw_n_respondents") %in% names(vc$extra)))
  expect_false(is.na(vc$extra$raw_share))
  expect_gt(vc$extra$raw_n_tasks, 0L)
  ## the fixture design assigns the contrast; model and raw share agree
  ## within a few joint SEs on well-specified data
  gap <- abs(unname(vc$estimate) - vc$extra$raw_share)
  expect_lt(gap, 4 * sqrt(vc$se^2 + vc$extra$raw_share_se^2))
})

test_that("batch counterfactual contrasts equal single calls (P7/E6)", {
  Dm <- rbind(pro_a1 = c(1, 0), mixed = c(1, -1))
  batch <- scmix_counterfactual(fit, contrast = Dm, n_bins = 10L, M = 200L,
                                seed = 2L)
  expect_named(batch$estimate, c("pro_a1", "mixed"))
  expect_s3_class(batch$extra$raw, "data.frame")
  expect_equal(nrow(batch$extra$raw), 2L)
  for (j in 1:2) {
    single <- scmix_counterfactual(fit, contrast = Dm[j, ], n_bins = 10L,
                                   M = 200L, seed = 2L)
    expect_equal(unname(batch$estimate[j]), unname(single$estimate),
                 tolerance = 1e-12)
    expect_equal(unname(batch$se[j]), unname(single$se), tolerance = 1e-12)
    expect_equal(batch$extra$raw$raw_share[j], single$extra$raw_share,
                 tolerance = 1e-12)
    expect_equal(batch$extra$raw$raw_n_tasks[j], single$extra$raw_n_tasks)
  }
  ## single-contrast extra keeps the flat backward-compatible fields
  one <- scmix_counterfactual(fit, contrast = c(1, 0), n_bins = 10L,
                              M = 200L, seed = 2L)
  expect_named(one$estimate, "V(c)")
  expect_true(all(c("contrast", "raw_share", "raw_n_tasks") %in%
                    names(one$extra)))
})

test_that("print/summary report the zero-floor status (P3)", {
  expect_output(print(fit), "zero-floor calibration: not run")
  fit2 <- fit
  fit2$zero_floor <- list(ratio = 3.2)
  expect_output(print(fit2), "ratio = 3.20")
  expect_output(print(fit2), "above the small-T floor")
  fit2$zero_floor <- list(ratio = 1.4)
  expect_output(print(fit2), "not supported at this design")
  expect_output(summary(fit2), "not supported at this design")
})

test_that("the population-claim note prints once per session (P5)", {
  old <- sconjoint:::.sc_state$warned
  on.exit(assign("warned", old, envir = sconjoint:::.sc_state), add = TRUE)
  assign("warned", character(), envir = sconjoint:::.sc_state)
  expect_message(sconjoint:::.sc_population_claim_note("sc_polarization"),
                 "respondent-level")
  expect_silent(sconjoint:::.sc_population_claim_note("sc_polarization"))
})

test_that("every scmix entry point rejects a non-scmix fit", {
  not_a_fit <- list(theta = 1)
  expect_error(scmix_theta(not_a_fit), "scmix")
  expect_error(scmix_polarization(not_a_fit), "scmix")
  expect_error(scmix_counterfactual(not_a_fit, contrast = 1), "scmix")
  expect_error(scmix_calibrate_zero(not_a_fit), "scmix")
})
