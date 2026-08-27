.paperps_fit <- function() {
  rid <- c("A", "A", "A", "B", "C", "C")
  mu_resp <- rbind(A = c(1, -1), B = c(3, 1), C = c(5, 2))
  mu_task <- mu_resp[match(rid, rownames(mu_resp)), , drop = FALSE]
  colnames(mu_task) <- c("x1", "x2")
  fit <- list(
    respondent_id = rid,
    mu_full = mu_task,
    mu_hat = mu_task,
    A_folds = list(matrix(c(0.5, 0.2), 2, 1),
                   matrix(c(0.5, 0.2), 2, 1)),
    attr_names = c("x1", "x2"),
    q = 1L
  )
  class(fit) <- c("scmix", "list")
  fit
}

test_that("paper quantities average respondents rather than task rows", {
  fit <- .paperps_fit()
  out <- sconjoint:::scmix_paper_theta(fit)
  expect_s3_class(out, "scmix_paper_quantity")
  expect_equal(unname(out$estimate), c(3, 2 / 3), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(unname(out$estimate), colMeans(fit$mu_hat))))
  expect_identical(out$respondent_weighting, "equal weight per respondent")
  expect_false(out$posterior_summaries_used)

  sub <- sconjoint:::scmix_paper_theta(fit, subgroup = c("g1", "g1", "g2"))
  expect_equal(unname(sub$details$subgroup["g1", ]), c(2, 0))
  expect_equal(unname(sub$details$subgroup["g2", ]), c(5, 2))
  expect_equal(unname(sub$details$subgroup_n), c(2, 1))
})

test_that("full-fit structural objects are preferred and recorded", {
  fit <- .paperps_fit()
  fit$full_fit <- list(
    mu_resp = matrix(c(10, 20, 30, 40, 50, 60), 3, 2,
                     dimnames = list(NULL, c("x1", "x2"))),
    Sigma = diag(c(1, 2)), kappa = 0.25
  )
  out <- sconjoint:::scmix_paper_theta(fit)
  expect_equal(unname(out$estimate), c(20, 50))
  expect_true(out$sources$full_fit)
  expect_match(out$sources$mu, "full_fit")
})

test_that("choice probabilities include kappa and the position-neutral form", {
  fit <- .paperps_fit()
  mu <- rbind(c(1, -1), c(3, 1), c(5, 2))
  Sigma <- matrix(0, 2, 2)
  d <- c(1, 0)
  ordinary <- sconjoint:::scmix_paper_choice(
    fit, d, kappa = 0.4, mu = mu, Sigma = Sigma, on_support = TRUE
  )
  expect_equal(ordinary$estimate, mean(plogis(0.4 + c(1, 3, 5))),
               tolerance = 1e-12)
  expect_identical(ordinary$details$support, "on randomized support")

  neutral <- sconjoint:::scmix_paper_choice(
    fit, d, position_neutral = TRUE, kappa = 0.4,
    mu = mu, Sigma = Sigma
  )
  expected <- mean((plogis(0.4 + c(1, 3, 5)) +
                    plogis(-0.4 + c(1, 3, 5))) / 2)
  expect_equal(neutral$estimate, expected, tolerance = 1e-12)
})

test_that("MRS uses the paper's minus sign and enforces its gate", {
  fit <- .paperps_fit()
  pass <- sconjoint:::scmix_paper_mrs(
    fit, c(1, 0), c(0, 1), denominator_margin = 0.5
  )
  expect_equal(pass$estimate, -4.5, tolerance = 1e-12)
  expect_true(pass$gate$pass)
  fail <- sconjoint:::scmix_paper_mrs(
    fit, c(1, 0), c(0, 1), denominator_margin = 1
  )
  expect_false(fail$gate$pass)
  expect_false(fail$gate$reported)
})

test_that("zero-variance threshold shares implement the tie conventions", {
  fit <- .paperps_fit()
  mu <- rbind(c(1, -1), c(3, 1), c(5, 2))
  Sigma <- matrix(0, 2, 2)
  strict <- sconjoint:::scmix_paper_signshare(
    fit, c(1, 1), ties = "exclude", variance_margin = 0.01,
    mu = mu, Sigma = Sigma
  )
  inclusive <- sconjoint:::scmix_paper_signshare(
    fit, c(1, 1), ties = "include", variance_margin = 0.01,
    mu = mu, Sigma = Sigma
  )
  expect_equal(strict$estimate, 2 / 3)
  expect_equal(inclusive$estimate, 1)
  expect_false(strict$gate$pass)

  comp <- sconjoint:::scmix_paper_compensating(
    fit, penalty = c(1, 0), benefit = c(0, 1), amount = 1,
    variance_margin = 0.01, mu = mu, Sigma = Sigma
  )
  expect_equal(comp$estimate, 1)
  expect_identical(comp$details$ties, "include")
})

test_that("heterogeneity decomposition uses population respondent moments", {
  fit <- .paperps_fit()
  mu <- rbind(c(1, -1), c(3, 1), c(5, 2))
  Sigma <- diag(c(0.25, 0.04))
  out <- sconjoint:::scmix_paper_heterogeneity(
    fit, direction = c(1, 0), total_margin = 0.1,
    mu = mu, Sigma = Sigma
  )
  centered <- sweep(mu, 2, colMeans(mu), "-")
  Omega_Z <- crossprod(centered) / 3
  expect_equal(unname(out$details$Omega_Z), Omega_Z, tolerance = 1e-12)
  expect_equal(unname(out$details$Omega_R), Sigma, tolerance = 1e-12)
  expect_equal(unname(out$details$Omega_T), Omega_Z + Sigma, tolerance = 1e-12)
  expect_equal(unname(out$estimate["H_R"]), 0.25)
  expect_true(out$gate$pass)
})

test_that("unsafe defaults are not silently supplied", {
  fit <- .paperps_fit()
  expect_error(
    sconjoint:::scmix_paper_choice(fit, c(1, 0)),
    "Supply"
  )
  ungated <- sconjoint:::scmix_paper_mrs(fit, c(1, 0), c(0, 1))
  expect_true(is.na(ungated$gate$pass))
  expect_false(ungated$gate$reported)
})
