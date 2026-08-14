## Tests for scmix_posterior (estimand-memo P10): respondent-level
## posterior means and SDs as descriptive extractors.

skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())
skip_on_cran()

fixed <- .fit_mixed_fixture()
fit <- fixed$fit

## from-scratch quadrature posterior for one respondent
.hand_posterior <- function(fit, resp_id) {
  cf <- sconjoint:::.scmix_canon(fit)
  rows <- which(cf$respondent_id == resp_id)
  k <- cf$fold_id[rows[1L]]
  A <- cf$A_folds[[k]]
  gh <- cf$gh
  dxk <- cf$deltaX[rows, , drop = FALSE]
  mu_i <- cf$mu_hat[rows[1L], ]
  idx <- rowSums(dxk * matrix(mu_i, length(rows), length(mu_i),
                              byrow = TRUE)) + (dxk %*% A) %*% t(gh$U)
  yk <- cf$y[rows]
  lp <- ifelse(rep(yk, length(gh$w)) == 1,
               stats::plogis(idx, log.p = TRUE),
               stats::plogis(-idx, log.p = TRUE))
  dim(lp) <- dim(idx)
  lw <- colSums(lp) + log(gh$w)
  m <- max(lw)
  postw <- exp(lw - (m + log(sum(exp(lw - m)))))
  Eu <- as.numeric(postw %*% gh$U)
  Euu <- matrix(0, ncol(gh$U), ncol(gh$U))
  for (r in seq_len(ncol(gh$U))) {
    for (s in seq_len(ncol(gh$U))) {
      Euu[r, s] <- sum(postw * gh$U[, r] * gh$U[, s])
    }
  }
  Cov_u <- Euu - Eu %o% Eu
  list(mean = mu_i + as.numeric(A %*% Eu),
       sd = sqrt(pmax(diag(A %*% Cov_u %*% t(A)), 0)))
}

test_that("posterior means match the scores path and a hand quadrature (P10)", {
  post <- scmix_posterior(fit)
  sc <- sconjoint:::.scmix_scores(fit)
  expect_equal(unname(post$mean), unname(sc$post_mean), tolerance = 1e-12)

  ids <- unique(fit$respondent_id)[c(1L, 5L)]
  for (id in ids) {
    hand <- .hand_posterior(fit, id)
    i <- match(as.character(id), post$respondent)
    expect_equal(unname(post$mean[i, ]), unname(hand$mean),
                 tolerance = 1e-10)
  }
})

test_that("posterior SDs match a hand quadrature and are nonnegative (P10)", {
  post <- scmix_posterior(fit, what = c("mean", "sd"))
  expect_false(is.null(post$sd))
  expect_true(all(post$sd >= 0))
  ids <- unique(fit$respondent_id)[c(2L, 7L)]
  for (id in ids) {
    hand <- .hand_posterior(fit, id)
    i <- match(as.character(id), post$respondent)
    expect_equal(unname(post$sd[i, ]), unname(hand$sd), tolerance = 1e-10)
  }
})

test_that("shapes, order, data-frame form, and print (P10)", {
  post <- scmix_posterior(fit, what = c("mean", "sd"))
  N <- length(unique(fit$respondent_id))
  expect_equal(nrow(post$mean), N)
  expect_equal(colnames(post$mean), fit$attr_names)
  expect_equal(post$respondent,
               as.character(unique(fit$respondent_id)))

  df <- as.data.frame(post)
  expect_equal(nrow(df), N)
  expect_true(all(c("respondent", "T_i", "fold",
                    paste0("mean_", fit$attr_names),
                    paste0("sd_", fit$attr_names)) %in% names(df)))
  expect_equal(df[[paste0("mean_", fit$attr_names[1L])]],
               unname(post$mean[, 1L]))
  expect_output(print(post), "descriptive shrinkage")
})

test_that("posterior means are invariant to the stored loading orientation (P10)", {
  post <- scmix_posterior(fit)
  flipped <- fit
  flipped$A_folds <- lapply(flipped$A_folds, function(A) -A)
  post2 <- scmix_posterior(flipped)
  expect_equal(post$mean, post2$mean, tolerance = 1e-12)
})

test_that("the default scores path never computes the SD (P10)", {
  sc <- sconjoint:::.scmix_scores(fit)
  expect_null(sc$post_sd)
  expect_true(all(c("resp", "S", "S_A", "loglik", "post_mean", "T_i",
                    "fold_resp") %in% names(sc)))
  expect_error(scmix_posterior(list()), "scmix")
})
