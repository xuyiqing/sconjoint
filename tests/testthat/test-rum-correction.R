# Tests of the identification check (R/rum-correction-solver.R), the targeted updates with
# omega on the log scale and the joint simplex update (R/rum-bounded-joint.R), and the
# discrete-switch AMEs (R/rum-targets-switch.R). Inputs: the small simulated fixture of
# test-rum-inference.R (90 respondents, 3 features, 3 folds of untrained networks), B1's
# design generators, and simulated data. Single-threaded; nothing is written.

rinf <- function(nm) get(nm, envir = asNamespace("sconjoint"))
fx <- readRDS(test_path("fixtures", "rum-inference-fixture.rds"))
fold_final <- function(k) fx$fins[[k]]
CTOR <- list(mean = rinf(".rum_tgt_mean"), share = rinf(".rum_tgt_share"),
             choice = rinf(".rum_tgt_choice"), choice_pref = rinf(".rum_tgt_choice_pref"),
             choice_set = rinf(".rum_tgt_choice_set"),
             choice_set_pref = rinf(".rum_tgt_choice_set_pref"),
             lincomb = rinf(".rum_tgt_lincomb"))
spec_targets <- function(specs) lapply(specs, function(s) {
  a <- s$args; if (s$f == "lincomb") a$parts <- spec_targets(a$parts)
  do.call(CTOR[[s$f]], c(list(name = s$name), a))
})
TG <- spec_targets(fx$specs)
BND <- TG[vapply(TG, function(t) t$kind %in% rinf(".rum_bounded_kinds"), TRUE)]
# importance shares over single features: r_g = E_u[b_g^2 / sum_h b_h^2], b = mu + lambda u
UQ <- rinf(".rum_gh_rule")(21L)
imp_target <- function(name, g, w = NULL) list(name = name, kind = "importance", w = w,
  m = function(mu, th) {
    acc <- 0
    for (j in seq_along(UQ$z)) {
      B <- mu + matrix(th$loading * UQ$z[j], nrow(mu), ncol(mu), byrow = TRUE)
      acc <- acc + UQ$w[j] * B[, g]^2 / rowSums(B^2)
    }
    acc
  })
IMP <- lapply(1:3, function(g) imp_target(paste0("imp", g), g))

# ---- identification check ------------------------------------------------------------------

test_that("the check flags targets loading on a feature some respondents never face", {
  # respondents in group 1 never see feature 2: its design column is always 0
  p <- 3L; n <- 40L
  set.seed(1)
  mu <- matrix(rnorm(n * p, sd = .2), n, p); th <- list(kappa = .1, loading = c(.3, .2, .1), omega = .5)
  grp <- rep(1:2, each = n / 2)
  null <- list(N = list(matrix(c(0, 1, 0), 3, 1), matrix(0, 3, 0)), group = grp,
               keys = c("never sees f2", "all"), features = c("f1", "f2", "f3"),
               source = "test", tol_rel = 1e-8)
  tg <- list(rinf(".rum_tgt_mean")("mean f1", c(1, 0, 0)),
             rinf(".rum_tgt_mean")("mean f2", c(0, 1, 0)),
             rinf(".rum_tgt_share")("share f2 + f3", c(0, 1, 1)),
             rinf(".rum_tgt_mean")("mean f2, group 2", c(0, 1, 0), w = as.numeric(grp == 2)))
  chk <- rinf(".rum_identification_check")
  expect_error(chk(tg, mu, th, null), "mean f2: 20 of 40 respondents")
  r <- chk(tg, mu, th, null, on_fail = "none")
  expect_identical(r$table$n_not_identified, c(0L, 20L, 20L, 0L))
  expect_identical(r$floor_hits, 20L)
  expect_identical(names(r$suggest_w), c("mean f2", "share f2 + f3"))
  expect_identical(r$suggest_w[["mean f2"]], as.numeric(grp == 2))
  expect_match(r$table$null_span[2], "f2")
  # restricting to the suggestion passes
  tg2 <- tg[2]; tg2[[1]]$w <- r$suggest_w[["mean f2"]]
  expect_identical(chk(tg2, mu, th, null)$table$n_not_identified, 0L)
})

test_that("the null space from the information matrices matches the design support", {
  p <- 3L; set.seed(2)
  I <- array(0, c(4, 2 * p + 2, 2 * p + 2))
  for (i in 1:4) { X <- matrix(rnorm(50 * 8), 50, 8); if (i > 2) X[, 2] <- 0; I[i, , ] <- crossprod(X) / 50 }
  ns <- rinf(".rum_nullspace_info")(I, p)
  expect_identical(ns$group, c(1L, 1L, 2L, 2L))
  expect_identical(unname(ns$dims), c(0L, 1L))
  expect_equal(abs(drop(ns$N[[2]])), c(0, 1, 0), tolerance = 1e-10)
})

test_that("GS pure independents have a co-partisan null direction in the design law", {
  ns <- rinf(".rum_nullspace_design")("gs", resp = c(-3, 0, 2, 0), M = 200L, seed = 5L)
  expect_identical(unname(ns$dims[ns$keys == "I"]), 1L)
  expect_identical(unname(ns$dims[ns$keys != "I"]), c(0L, 0L))
  NI <- ns$N[[which(ns$keys == "I")]]
  expect_equal(abs(NI[1, 1]), 1, tolerance = 1e-12)                  # diff_respParty
  expect_identical(ns$group[c(2, 4)], rep(which(ns$keys == "I"), 2))
})

test_that("the checked one-step leaves identified targets' numbers unchanged and restricts the others", {
  n <- fx$data$n; grp <- fx$grp
  null <- list(N = list(matrix(c(0, 0, 1), 3, 1), matrix(0, 3, 0)),
               group = ifelse(grp == 1, 1L, 2L), keys = c("no f3", "all"),
               features = fx$data$feature_names, source = "test", tol_rel = 1e-8)
  tg <- list(rinf(".rum_tgt_mean")("mean f1", c(1, 0, 0)),
             rinf(".rum_tgt_mean")("mean f3", c(0, 0, 1)))
  expect_error(rinf(".rum_onestep_checked")(fx$data, fx$Z, fx$fold, fold_final, tg, null),
               "mean f3: 4[0-9] of 90 respondents|mean f3: [0-9]+ of 90 respondents")
  r <- rinf(".rum_onestep_checked")(fx$data, fx$Z, fx$fold, fold_final, tg, null,
                                    on_unidentified = "restrict")
  base <- rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, fold_final, tg[1])
  expect_identical(unname(r$est[1]), unname(base$est[1]))
  expect_identical(unname(r$IFP[, 1]), unname(base$IFP[, 1]))
  expect_identical(r$identification$n_dropped, c(0L, as.integer(sum(grp == 1))))
  expect_identical(r$restricted_w[["mean f3"]], as.numeric(grp != 1))
  expect_equal(unname(r$n_group[2]), sum(grp != 1))
})

test_that("the check catches a mean confounded with kappa (a feature constant in every task)", {
  # Codex review of eae4719, finding 2: with d = 1 for feature 1 in every task, s_mu1 = s_kappa.
  # I_mumu is positive definite, so the mean-block check passes, but mu1 + c, kappa - c leaves
  # the likelihood unchanged: a target loading on mu1 is not identified.
  set.seed(41); p <- 3L; B <- 400L; T <- 6L
  DL <- lapply(seq_len(T), function(t) cbind(1, matrix(sample(c(-1, 0, 1), B * 2, TRUE), B, 2)))
  th <- list(kappa = .2, loading = c(.3, .4, -.2), omega = .5)
  MU <- matrix(rnorm(B * p, sd = .3), B, p)
  u <- rnorm(B); v <- rnorm(B)
  Y <- sapply(seq_len(T), function(t) as.numeric(runif(B) < plogis(th$kappa +
    rowSums(DL[[t]] * (MU + outer(u, th$loading))) + sqrt(th$omega) * v)))
  S <- rinf(".rum_seq_scores")(MU, DL, Y, th, rum_quadrature(c(11L, 9L)))
  expect_lt(max(abs(S[, 1] - S[, p + 1])), 1e-12)            # s_mu1 = s_kappa
  I <- crossprod(S) / B
  tg <- list(rinf(".rum_tgt_mean")("mean f1", c(1, 0, 0)),
             rinf(".rum_tgt_mean")("mean f2", c(0, 1, 0)),
             rinf(".rum_tgt_share")("share f1 + f2", c(1, 1, 0)),
             rinf(".rum_tgt_mean")("mean f2 - f3", c(0, 1, -1)))
  chk <- rinf(".rum_identification_check")
  null <- rinf(".rum_nullspace_info")(array(rep(I, each = B), c(B, dim(I))), p)
  old <- chk(tg, MU, th, null, on_fail = "none")              # the mean-block check alone
  expect_identical(old$table$n_not_identified, c(0L, 0L, 0L, 0L))
  new <- chk(tg, MU, th, null, on_fail = "none", info = I)
  expect_identical(new$table$common_confounded, c(TRUE, FALSE, TRUE, FALSE))
  expect_gt(new$table$max_rel_common[1], .5)
  NJ <- new$common$null_J; expect_identical(ncol(NJ), 1L)
  expect_equal(abs(unname(NJ["kappa", 1])), 1, tolerance = 1e-6)      # the kappa direction
  expect_error(chk(tg, MU, th, null, info = I), "confounded")
  expect_silent(chk(tg[c(2, 4)], MU, th, null, info = I))
  # per-respondent information gives the same answer
  new2 <- chk(tg, MU, th, null, on_fail = "none", info = array(rep(I, each = B), c(B, dim(I))))
  expect_identical(new2$table$common_confounded, new$table$common_confounded)
  # without the confounding (feature 1 varies) nothing is flagged
  DL2 <- lapply(DL, function(d) { d[, 1] <- sample(c(-1, 0, 1), B, TRUE); d })
  I2 <- crossprod(rinf(".rum_seq_scores")(MU, DL2, Y, th, rum_quadrature(c(11L, 9L)))) / B
  expect_false(any(chk(tg, MU, th, null, on_fail = "none", info = I2)$table$common_confounded))
})

test_that("the checked one-step stops on a mean confounded with kappa", {
  n <- fx$data$n; set.seed(42); T <- 5L
  d <- lapply(seq_len(n), function(i) cbind(1, matrix(sample(c(-1, 0, 1), T * 2, TRUE), T, 2)))
  y <- lapply(seq_len(n), function(i) rbinom(T, 1, .5))
  dat <- rum_data(y, d, ids = fx$data$ids, feature_names = fx$data$feature_names,
                  metadata = list(orientation = "test"))
  null0 <- list(N = list(matrix(0, 3, 0)), group = rep(1L, n), keys = "all",
                features = fx$data$feature_names, source = "test", tol_rel = 1e-8)
  tg <- list(rinf(".rum_tgt_mean")("mean f1", c(1, 0, 0)), rinf(".rum_tgt_mean")("mean f2", c(0, 1, 0)))
  msg <- tryCatch(rinf(".rum_onestep_checked")(dat, fx$Z, fx$fold, fold_final, tg, null0,
                                               on_unidentified = "restrict"), error = conditionMessage)
  expect_match(msg, "confounded", fixed = TRUE)
  expect_match(msg, "mean f1 (relative", fixed = TRUE)
  expect_false(grepl("mean f2 (relative", msg, fixed = TRUE))
  r <- rinf(".rum_onestep_checked")(dat, fx$Z, fx$fold, fold_final, tg[2], null0)
  expect_true(is.finite(r$est[1]))
  # the check can be switched off (the old behavior: the floored inverse gives a number)
  r0 <- rinf(".rum_onestep_checked")(dat, fx$Z, fx$fold, fold_final, tg, null0, common = FALSE)
  expect_true(all(is.finite(r0$est)))
})

# ---- targeted updates -------------------------------------------------------------------------

test_that("the legacy path reproduces .rum_tmle_dnn exactly and records floor hits", {
  old <- rinf(".rum_tmle_dnn")(fx$data, fx$Z, fx$fold, fold_final, BND)
  leg <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, fold_final, BND, omega_path = "legacy",
                                on_fail = "last")
  expect_lt(max(abs(old$estimate - leg$estimate)), 1e-12)
  expect_lt(max(abs(old$se_population - leg$se_population)), 1e-12)
  expect_lt(max(abs(attr(old, "IFP") - attr(leg, "IFP"))), 1e-12)
  expect_identical(old$converged, leg$converged)
  expect_identical(as.integer(old$iterations), leg$iterations)
  expect_true(all(c("floor_hit_any", "omega_linear_min") %in% names(leg)))
})

test_that("forced non-convergence returns a failure status and no number", {
  r <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, fold_final, BND, max_iter = 0L)
  expect_true(all(r$status == "max_iter"))
  expect_true(all(is.na(r$estimate) & is.na(r$se_population) & is.na(r$lower_population)))
  expect_true(all(is.na(attr(r, "IFP"))))
  expect_identical(attr(r, "n_failed"), length(BND))
  j <- rinf(".rum_tmle_joint")(fx$data, fx$Z, fx$fold, fold_final, IMP, max_iter = 1L)
  expect_identical(j$status, "max_iter")
  expect_true(all(is.na(j$table$estimate)))
  bd <- rinf(".rum_bounded_dnn")(fx$data, fx$Z, fx$fold, fold_final, BND, max_iter = 0L,
                                 update = "log_omega")
  expect_true(all(is.na(bd$estimate)) && all(bd$status == "max_iter"))
  expect_identical(attr(bd, "n_failed"), length(BND))
  # Codex review of eae4719, finding 5: the DEFAULT bounded routine is the checked one, and
  # it returns no number on non-convergence; the legacy routine needs an explicit opt-in
  bd0 <- rinf(".rum_bounded_dnn")(fx$data, fx$Z, fx$fold, fold_final, BND, max_iter = 0L)
  expect_true(all(bd0$status == "max_iter"))
  expect_true(all(is.na(bd0$estimate) & is.na(bd0$se_population) & is.na(bd0$lower_population) &
                  is.na(bd0$upper_population) & is.na(bd0$lower_fixed)))
  expect_true(all(is.na(attr(bd0, "IFP"))))
  bl <- rinf(".rum_bounded_dnn")(fx$data, fx$Z, fx$fold, fold_final, BND, max_iter = 0L,
                                 update = "legacy")
  expect_true(all(is.finite(bl$estimate)))          # legacy keeps the last iterate, labeled
  expect_true(all(grepl("^max_iter", bl$status[!bl$tmle_converged])))
})

test_that("on a path that crosses the old floor, log-omega stays positive and converges", {
  # a share whose correction pulls omega down hard: kappa-heavy choice target in a fold
  # state with a tiny omega. Codex review of eae4719, finding 5: the test now requires that
  # the legacy path really crosses the floor and that the checked update converges.
  fins <- lapply(fx$fins, function(f) { f$omega <- 2e-3; f })
  ffl <- function(k) fins[[k]]
  tg <- list(rinf(".rum_tgt_choice_set")("choice", rbind(c(1, 0, 0)), coef = 1))
  tg[[1]]$kind <- "share"
  leg <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, ffl, tg, omega_path = "legacy",
                                on_fail = "last")
  new <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, ffl, tg)
  expect_true(leg$floor_hit_any)                      # the case the test is about
  expect_lt(leg$omega_linear_min, 1e-8)
  expect_identical(new$status, "converged")
  expect_gt(new$omega_min, 0)
  expect_false(new$floor_hit_any)
  expect_true(new$estimate >= 0 && new$estimate <= 1)
  expect_true(is.finite(new$se_population) && new$se_population > 0)
  # the default bounded routine is this checked update, and returns the same numbers
  bd <- rinf(".rum_bounded_dnn")(fx$data, fx$Z, fx$fold, ffl, tg)
  expect_identical(bd$estimate, new$estimate); expect_identical(bd$status, "converged")
})

test_that("a converged targeted update also solves the influence equation used for its SEs", {
  # Codex review of eae4719, finding 4: the solver zeroes the path score phi, the SEs use
  # phi_se; on the log path they differ by the factor omega_eps / omega on the omega part.
  # Here the correction pulls omega from 0.1 to about 0.03.
  fins <- lapply(fx$fins, function(f) { f$omega <- 0.1; f })
  ffl <- function(k) fins[[k]]
  tg <- list(rinf(".rum_tgt_choice_set")("choice", rbind(c(1, 0, 0)), coef = 1))
  tg[[1]]$kind <- "share"
  crit <- function(x) abs(sum(x)) / sqrt(sum(x^2))
  one <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, ffl, tg, omega_path = "log", max_rounds = 1L)
  # one round (the eae4719 rule): the path score is solved, the influence equation is not
  expect_lt(one$crit, 1e-3)
  expect_gt(one$if_crit, 1e-3)
  expect_identical(one$status, "influence_residual")
  expect_true(is.na(one$estimate))
  new <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, ffl, tg, omega_path = "log")
  expect_identical(new$status, "converged")
  expect_gt(new$rounds, 1L)
  expect_lt(new$if_crit, 1e-3)
  expect_lt(crit(attr(new, "PHI")[, 1]), 1e-3)        # the reported influence values' own residual
  # joint update: the same check
  fj <- lapply(fx$fins, function(f) { f$omega <- 5e-3; f })
  j <- rinf(".rum_tmle_joint")(fx$data, fx$Z, fx$fold, function(k) fj[[k]], IMP, omega_path = "log")
  if (j$converged) expect_lt(j$if_crit, 1e-3)
})

test_that("the path score is the derivative of the held-out log likelihood along the path taken", {
  model <- rinf(".rum_model_options")(NULL, rinf(".rum_info_quadrature")(), 1L, NULL, NULL, FALSE, FALSE)
  q <- rinf(".rum_score_quadrature")()
  FS <- rinf(".rum_tmle_folds")(fx$data, fx$Z, fx$fold, fold_final, q, "constant", 1e-2, 1e-6, model)
  dirs <- list(rinf(".rum_tmle_dirs")(FS, IMP[[1]], rep(1, fx$data$n)),
               rinf(".rum_tmle_dirs")(FS, IMP[[2]], rep(1, fx$data$n)))
  # make the omega directions large and negative so that the clamp binds at eps = 1 in some fold
  dirs[[1]] <- lapply(dirs[[1]], function(d) { d$g[length(d$g)] <- -0.6; d })
  n <- fx$data$n; p <- fx$data$p
  for (mode in c("log", "clamp")) for (e1 in c(0.3, 1)) {
    eps <- c(e1, -0.2); h <- 1e-6
    r <- rinf(".rum_path_all")(FS, dirs, eps, n, p, q, mode)
    num <- vapply(1:2, function(j) { e <- eps; e[j] <- e[j] + h; a <- rinf(".rum_path_all")(FS, dirs, e, n, p, q, mode)$ll
      e[j] <- eps[j] - h; b <- rinf(".rum_path_all")(FS, dirs, e, n, p, q, mode)$ll; (a - b) / (2 * h) }, 0)
    expect_lt(max(abs(colSums(r$phi) - num)), 1e-5 * max(1, abs(num)))
    if (mode == "log") expect_true(all(r$omega > 0))
    if (mode == "clamp" && e1 == 1) expect_true(r$floor_hit)
  }
  # the legacy floored path does NOT have this property where the floor binds
  r <- rinf(".rum_path_all")(FS, dirs, c(1, -0.2), n, p, q, "linear_floor")
  expect_true(r$floor_hit)
  e <- c(1 + 1e-6, -0.2); a <- rinf(".rum_path_all")(FS, dirs, e, n, p, q, "linear_floor")$ll
  e <- c(1 - 1e-6, -0.2); b <- rinf(".rum_path_all")(FS, dirs, e, n, p, q, "linear_floor")$ll
  expect_gt(abs(sum(r$phi[, 1]) - (a - b) / 2e-6), 1e-3)
})

test_that("at a boundary omega the log path breaks down and the default switches that fold to the clamp", {
  fins <- fx$fins; fins[[2]]$omega <- 1e-6                     # one fold at the trainer's lower bound
  ffb <- function(k) fins[[k]]
  lg <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, ffb, IMP, omega_path = "log")
  au <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, ffb, IMP)
  cl <- rinf(".rum_tmle_path")(fx$data, fx$Z, fx$fold, ffb, IMP, omega_path = "clamp")
  expect_true(all(au$folds_clamped == 1L))
  expect_true(all(is.finite(au$se_population[au$converged])))
  expect_true(all(au$omega_min >= 0))
  expect_true(all(lg$omega_min >= 0))     # the log path can underflow omega to 0 here
  expect_true(all(cl$folds_clamped == 3L))
  j <- rinf(".rum_tmle_joint")(fx$data, fx$Z, fx$fold, ffb, IMP)
  if (j$converged) expect_lt(abs(sum(j$table$estimate[1:3]) - 1), 1e-12)
})

test_that("the joint update keeps shares in [0, 1] adding to exactly 1, with joint covariance", {
  sep <- rinf(".rum_tmle_dnn")(fx$data, fx$Z, fx$fold, fold_final, IMP)
  j <- rinf(".rum_tmle_joint")(fx$data, fx$Z, fx$fold, fold_final, IMP)
  tb <- j$table
  expect_identical(j$status, "converged")
  est <- tb$estimate[1:3]
  expect_true(all(est >= 0 & est <= 1))
  expect_lt(abs(sum(est) - 1), 1e-12)
  expect_lt(abs(sum(tb$onestep[1:3]) - 1), 1e-8)       # plain corrections add to zero
  expect_lt(j$dir_sum_max, 1e-6)
  expect_lt(max(abs(rowSums(j$IFP))), 1e-6)            # stacked influence values add to zero
  expect_lt(abs(sum(j$V_population)), 1e-12)
  expect_equal(unname(sqrt(diag(j$V_population))), tb$se_population[1:3], tolerance = 1e-12)
  expect_gt(abs(sum(sep$estimate) - 1), 1e-3)          # the separate updates do not add to 1
  # subgroup weights
  jw <- rinf(".rum_tmle_joint")(fx$data, fx$Z, fx$fold, fold_final,
                                lapply(1:3, function(g) imp_target(paste0("w", g), g, w = fx$grp)))
  expect_lt(abs(sum(jw$table$estimate[1:3]) - 1), 1e-12)
  expect_error(rinf(".rum_tmle_joint")(fx$data, fx$Z, fx$fold, fold_final, IMP[1:2]),
               "do not sum to one")
})

test_that("where the bound does not bind, the targeted estimates match the one-step to first order", {
  # data simulated from the fitted state itself (the same state in every fold), so the
  # corrections are O(n^-1/2) and targeted minus one-step is O(1/n): small against the SE
  set.seed(11); n <- 12000L; p <- 3L; T <- 6L
  net <- fx$fins[[1]]$network; th <- fx$fins[[1]][c("kappa", "loading", "omega")]
  Z <- matrix(rnorm(n * 2), n, 2, dimnames = list(sprintf("r%05d", 1:n), colnames(fx$Z)))
  mu <- rum_dnn_gaussian_forward(net, Z)
  d <- lapply(seq_len(n), function(i) matrix(sample(c(-1, 0, 1), T * p, replace = TRUE), T, p))
  u <- rnorm(n); v <- rnorm(n)
  y <- lapply(seq_len(n), function(i) {
    eta <- th$kappa + drop(d[[i]] %*% (mu[i, ] + th$loading * u[i])) + sqrt(th$omega) * v[i]
    as.numeric(runif(T) < stats::plogis(eta)) })
  dat <- rum_data(y, d, ids = rownames(Z), feature_names = fx$data$feature_names,
                  metadata = list(orientation = "test"))
  fold <- rep(1:3, length.out = n); ffs <- function(k) fx$fins[[1]]
  tg <- c(list(rinf(".rum_tgt_share")("share_f1", c(1, 0, 0))), IMP)   # IMP last
  ji <- 2:4
  one <- rinf(".rum_onestep")(dat, Z, fold, ffs, tg)
  se <- apply(one$IFP, 2, stats::sd) / sqrt(n)
  tp <- rinf(".rum_tmle_path")(dat, Z, fold, ffs, tg)
  expect_true(all(tp$converged))
  expect_true(all(abs(tp$estimate - one$est) < 0.05 * se))
  expect_lt(max(abs(tp$onestep - one$est)), 1e-12)
  j <- rinf(".rum_tmle_joint")(dat, Z, fold, ffs, IMP)
  expect_true(all(abs(j$table$estimate[1:3] - one$est[ji]) < 0.05 * se[ji]))
  expect_lt(max(abs(j$table$se_population[1:3] / se[ji] - 1)), 0.05)
})

test_that("the joint solver normalizes directions and does not hide a residual in a dropped one", {
  # Codex review of eae4719, finding 3: score rows (1, 1e-10) and (-1, 1e-10). The second
  # direction's standardized residual is sqrt(2); the old truncated pseudoinverse dropped it
  # and reported criterion 0 ("converged").
  solve_ <- rinf(".rum_tmle_solve"); crit_ <- rinf(".rum_score_crit")
  phi <- rbind(c(1, 1e-10), c(-1, 1e-10))
  cc <- crit_(phi)
  expect_equal(cc$crit, sqrt(2), tolerance = 1e-12)
  expect_identical(cc$rank, 2L)
  r <- solve_(function(eps) list(phi = phi, ll = 0), 2L, max_iter = 3L)
  expect_false(identical(r$status, "converged"))
  expect_equal(r$crit, sqrt(2), tolerance = 1e-12)
  # scale invariance: rescaling a direction does not change the criterion
  set.seed(31); P <- matrix(rnorm(300), 100, 3) + .05
  expect_equal(crit_(P)$crit, crit_(sweep(P, 2, c(1, 1e-9, 1e6), "*"))$crit, tolerance = 1e-8)
  # J = 1: the old rule |sum phi| / sqrt(sum phi^2)
  expect_equal(crit_(P[, 1, drop = FALSE])$crit, abs(sum(P[, 1])) / sqrt(sum(P[, 1]^2)), tolerance = 1e-12)
  # exactly collinear directions: one is dropped, its residual is 0, the kept one decides
  x <- P[, 1] - mean(P[, 1]); Q <- cbind(x, 2 * x)
  cq <- crit_(Q); expect_identical(cq$rank, 1L); expect_lt(cq$resid_dropped, 1e-12)
  expect_identical(solve_(function(eps) list(phi = Q, ll = 0), 2L)$status, "converged")
  # nearly collinear directions whose difference carries a material residual: with a coarse
  # rank tolerance the difference is dropped, and the solver reports a rank failure instead
  # of converging on the kept direction
  z <- rnorm(100) + 1; wz <- 3e-3 * sqrt(sum(x^2)) * z / sqrt(sum(z^2))
  Q2 <- cbind(x - wz, x + wz)                      # sums -s and +s: the kept sum direction is solved
  c2 <- crit_(Q2, rank_tol = 1e-4)
  expect_identical(c2$rank, 1L); expect_lt(c2$crit, 1e-3); expect_gt(c2$resid_dropped, 1e-3)
  r2 <- solve_(function(eps) list(phi = Q2, ll = 0), 2L, rank_tol = 1e-4)
  expect_identical(r2$status, "rank_deficient")
  expect_false(identical(solve_(function(eps) list(phi = Q2, ll = 0), 2L, max_iter = 2L)$status, "converged"))
})

# ---- discrete-switch AMEs ------------------------------------------------------------------

# brute force: P(choose focal) by nested integrate() over u and v
p_focal <- function(D, s, mu, lam, kappa, omega, position) {
  f <- function(u, v) stats::plogis((if (position) s * (kappa + sqrt(omega) * v) else 0) +
                                    sum(D * mu) + sum(D * lam) * u)
  stats::integrate(function(v) vapply(v, function(vv) stats::integrate(function(u)
    f(u, vv) * stats::dnorm(u), -Inf, Inf, rel.tol = 1e-11)$value, 0) * stats::dnorm(v),
    -Inf, Inf, rel.tol = 1e-10)$value
}

test_that("the switch AME equals brute-force enumeration on a tiny design with an interaction", {
  # attributes a (2 levels), b (3 levels); features a2, b2, b3, a2:b3; uniform independent
  # design, one task: enumerate all 36 (A, B) pairs
  levs <- expand.grid(a = 1:2, b = 1:3)
  x <- function(a, b) c(a == 2, b == 2, b == 3, (a == 2) * (b == 3)) * 1
  pairs <- expand.grid(A = 1:6, B = 1:6)
  raw <- list(a = array(0L, c(36, 1, 2)), b = array(0L, c(36, 1, 2)))
  raw$a[, 1, 1] <- levs$a[pairs$A]; raw$b[, 1, 1] <- levs$b[pairs$A]
  raw$a[, 1, 2] <- levs$a[pairs$B]; raw$b[, 1, 2] <- levs$b[pairs$B]
  code <- function(r) {
    X1 <- t(mapply(x, r$a[, 1, 1], r$b[, 1, 1])); X2 <- t(mapply(x, r$a[, 1, 2], r$b[, 1, 2]))
    array(X1 - X2, c(dim(r$a)[1], 1, 4))
  }
  mu <- rbind(c(.4, -.3, .5, -.8), c(-.2, .6, .1, .9)); lam <- c(.3, -.2, .4, .5)
  th <- list(kappa = .25, loading = lam, omega = .6)
  for (position in c(TRUE, FALSE)) {
    rows <- rinf(".rum_switch_rows")(raw, code, "a", 2L, 1L)
    tg <- rinf(".rum_tgt_choice_rows")("ame a", rows, position = position, order = 41L)
    got <- tg$m(mu, th)
    bf <- vapply(1:2, function(i) {
      acc <- 0
      for (r in 1:36) for (s in 1:2) {
        A <- c(raw$a[r, 1, 1], raw$b[r, 1, 1]); B <- c(raw$a[r, 1, 2], raw$b[r, 1, 2])
        foc <- if (s == 1) A else B; opp <- if (s == 1) B else A
        D1 <- x(2, foc[2]) - x(opp[1], opp[2]); D0 <- x(1, foc[2]) - x(opp[1], opp[2])
        sg <- if (s == 1) 1 else -1
        acc <- acc + p_focal(D1, sg, mu[i, ], lam, th$kappa, th$omega, position) -
          p_focal(D0, sg, mu[i, ], lam, th$kappa, th$omega, position)
      }
      acc / 72 }, 0)
    expect_lt(max(abs(got - bf)), 1e-8)
    # exact derivatives against central differences
    g <- tg$grad(mu, th); tg2 <- tg; tg2$grad <- NULL
    nd <- rinf(".rum_target_grad")(tg2, mu, th, h = 1e-5)
    expect_lt(max(abs(g$gm - nd$gm)), 1e-8)
    expect_lt(max(abs(g$gt - nd$gt)), 1e-8)
  }
  # the conditional (difference-in-means) version equals the switch version here: the
  # attribute is independent of everything else in the task
  rc <- rinf(".rum_switch_rows")(raw, code, "a", 2L, 1L, context = "conditional")
  tc <- rinf(".rum_tgt_choice_rows")("dm a", rc, order = 41L)
  ts <- rinf(".rum_tgt_choice_rows")("ame a", rinf(".rum_switch_rows")(raw, code, "a", 2L, 1L),
                                     order = 41L)
  expect_lt(max(abs(tc$m(mu, th) - ts$m(mu, th))), 1e-12)
})

test_that("an attribute without interaction weight has the same AME with or without interaction columns", {
  raw <- rinf(".rum_design_draw_raw")("sw_additive", 300L, seed = 3L)
  set.seed(4); n <- 5L
  mu13 <- matrix(rnorm(n * 13, sd = .5), n, 13); lam13 <- rnorm(13, sd = .3)
  mu72 <- cbind(mu13, matrix(0, n, 59)); lam72 <- c(lam13, numeric(59))
  th13 <- list(kappa = .1, loading = lam13, omega = .4); th72 <- th13; th72$loading <- lam72
  for (a in list(list("gender", 2L, 1L), list("talent", 4L, 1L), list("child", 3L, 1L))) {
    t13 <- rinf(".rum_tgt_switch_ame")("x", "sw_additive", raw, a[[1]], a[[2]], a[[3]])
    t72 <- rinf(".rum_tgt_switch_ame")("x", "sw_explicit", raw, a[[1]], a[[2]], a[[3]])
    expect_lt(max(abs(t13$m(mu13, th13) - t72$m(mu72, th72))), 1e-12)
    expect_lt(max(abs(t13$grad(mu13, th13)$gm - t72$grad(mu72, th72)$gm[, 1:13])), 1e-12)
  }
  # a nonzero interaction weight on gender x run changes the gender AME (the coder updates it)
  k <- which(rinf(".RUM_SW_FEATURES_EXPLICIT") == "cand_genderMale:cand_runYes")
  mu72b <- mu72; mu72b[, k] <- 1
  t72 <- rinf(".rum_tgt_switch_ame")("x", "sw_explicit", raw, "gender", 2L, 1L)
  t13 <- rinf(".rum_tgt_switch_ame")("x", "sw_additive", raw, "gender", 2L, 1L)
  expect_gt(min(abs(t72$m(mu72b, th72) - t13$m(mu13, th13))), 1e-3)
  # and switching an attribute not in that interaction is unchanged only to the extent that
  # its contexts never meet it: the agenda AME moves too (the interaction enters every task)
  expect_true(all(is.finite(t72$m(mu72b, th72))))
})

test_that("on a large randomized simulation the true-law AME equals the design-based AMCE", {
  set.seed(21); n <- 30000L
  spec <- rinf(".rum_design_spec")("sw_explicit")
  raw <- rinf(".rum_design_draw_raw")(spec, n, seed = 22L)
  d <- spec$code(raw)                                          # n x 3 x 72
  theta <- c(-.3, .2, rnorm(11, sd = .3), rnorm(59, sd = .15))
  lam <- c(rnorm(13, sd = .4), numeric(59)); kappa <- .3; omega <- .5
  u <- rnorm(n); v <- rnorm(n)
  y <- matrix(0, n, 3)
  for (t in 1:3) {
    eta <- kappa + drop(d[, t, ] %*% theta) + drop(d[, t, ] %*% lam) * u + sqrt(omega) * v
    y[, t] <- as.numeric(runif(n) < stats::plogis(eta))
  }
  ctx <- rinf(".rum_design_draw_raw")(spec, 4000L, seed = 23L)
  mu <- matrix(theta, 1, 72); th <- list(kappa = kappa, loading = lam, omega = omega)
  for (a in list(list("gender", 2L, 1L), list("agenda", 3L, 1L), list("talent", 6L, 1L))) {
    dm <- rinf(".rum_dm_amce")(as.vector(raw[[a[[1]]]][, , 1]), as.vector(raw[[a[[1]]]][, , 2]),
                               as.vector(y), rep(seq_len(n), 3), a[[2]], a[[3]])
    for (cx in c("switch", "conditional")) {
      tg <- rinf(".rum_tgt_switch_ame")("x", spec, ctx, a[[1]], a[[2]], a[[3]], context = cx)
      truth <- tg$m(mu, th)
      expect_lt(abs(truth - dm[["estimate"]]), 4 * dm[["se"]])
    }
  }
  # without position effects the AME differs (kappa = .3 and omega = .5 enter the level)
  tg0 <- rinf(".rum_tgt_switch_ame")("x", spec, ctx, "gender", 2L, 1L, position = FALSE)
  tg1 <- rinf(".rum_tgt_switch_ame")("x", spec, ctx, "gender", 2L, 1L)
  expect_gt(abs(tg0$m(mu, th) - tg1$m(mu, th)), 1e-4)
})

test_that("GS co-partisan switches are defined for partisans only", {
  raw <- rinf(".rum_design_draw_raw")("gs", 20L, seed = 9L)
  keys <- c("D", "R", "I", "D")
  lev <- function(k) c(D = 1L, R = 2L, I = NA)[[k]]; ref <- function(k) c(D = 2L, R = 1L, I = NA)[[k]]
  tg <- rinf(".rum_tgt_switch_ame")("copartisan", "gs", raw, "party", lev, ref, keys = keys)
  expect_identical(tg$excluded, 3L)
  expect_identical(tg$w, c(1, 1, 0, 1))
  mu <- matrix(0, 4, 30); mu[, 1] <- 1
  th <- list(kappa = 0, loading = numeric(30), omega = .3)
  m <- tg$m(mu, th)
  expect_gt(min(m[-3]), 0); expect_identical(m[3], 0)
  # the rows recode the co-partisan column from the respondent's party
  ch <- rinf(".rum_identification_check")(list(tg), mu, th,
    rinf(".rum_nullspace_design")("gs", resp = keys, M = 100L), on_fail = "none")
  expect_identical(ch$table$n_not_identified, 0L)
})
