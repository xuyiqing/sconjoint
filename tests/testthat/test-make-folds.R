test_that(".sc_make_folds assigns each respondent to one fold", {
  rid <- rep(1:20, each = 3)
  fold <- sconjoint:::.sc_make_folds(rid, K = 5, seed = 1)
  expect_length(fold, length(rid))
  # Every respondent has a single fold id across their rows
  per_resp <- tapply(fold, rid, function(x) length(unique(x)))
  expect_true(all(per_resp == 1L))
})

test_that(".sc_make_folds is deterministic given a seed", {
  rid <- rep(1:30, each = 2)
  a <- sconjoint:::.sc_make_folds(rid, K = 5, seed = 42)
  b <- sconjoint:::.sc_make_folds(rid, K = 5, seed = 42)
  expect_identical(a, b)
})

test_that(".sc_make_folds gives balanced fold sizes at the respondent level", {
  rid <- rep(1:25, each = 2)
  fold <- sconjoint:::.sc_make_folds(rid, K = 5, seed = 7)
  # Each respondent appears twice; check unique-respondent fold sizes.
  resp_fold <- fold[!duplicated(rid)]
  sizes <- as.integer(table(resp_fold))
  expect_true(max(sizes) - min(sizes) <= 1L)
})

test_that(".sc_make_folds rejects bad K", {
  rid <- rep(1:5, each = 2)
  expect_error(sconjoint:::.sc_make_folds(rid, K = 10, seed = 1), "fewer than K")
  expect_error(sconjoint:::.sc_make_folds(rid, K = 1, seed = 1), "integer")
})
