test_that("numeric respondent ids preserve typed task and response order", {
  skip_if_not_installed("torch")

  respondent <- rep(c(1, 2, 10, 11), each = 2L)
  profile <- rep(1:2, 4L)
  first_choice <- c(0, 1, 0, 1)
  dat <- data.frame(
    resp_id = respondent,
    task_id = 1L,
    profile_id = profile,
    choice = as.numeric(c(rbind(first_choice, 1 - first_choice))),
    x = c(rbind(c(1, 2, 10, 11), 0)),
    z = rep(c(-1, -0.25, 0.25, 1), each = 2L)
  )
  dat <- dat[c(7, 2, 5, 4, 1, 8, 3, 6), , drop = FALSE]

  fit <- scmix(
    choice ~ x | z, dat,
    respondent = "resp_id", task = "task_id", profile = "profile_id",
    q = 0L, K = 2L, hidden = 2L, n_epochs = 1L,
    learning_rate = 0.01, weight_decay = 0, n_starts = 1L,
    opt_tol = 1e6, grad_tol = 1e6, seed = 20260824L,
    device = "cpu"
  )

  expect_equal(fit$respondent_id, c(1, 2, 10, 11))
  expect_equal(unname(fit$deltaX[, 1L]), c(1, 2, 10, 11))
  expect_equal(fit$y, first_choice)
})
