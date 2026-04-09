test_that(".sc_make_seed_streams is deterministic and distinct", {
  s1 <- sconjoint:::.sc_make_seed_streams(42L, 4L)
  s2 <- sconjoint:::.sc_make_seed_streams(42L, 4L)
  expect_identical(s1, s2)
  expect_length(s1, 4L)

  ## Distinct streams
  expect_false(identical(s1[[1]], s1[[2]]))
  expect_false(identical(s1[[2]], s1[[3]]))

  ## Does not pollute the caller's RNG state
  set.seed(99)
  before <- .Random.seed
  invisible(sconjoint:::.sc_make_seed_streams(7L, 3L))
  after <- .Random.seed
  expect_identical(before, after)
})

test_that(".sc_with_torch_seed restores R RNG state", {
  set.seed(99)
  before <- .Random.seed
  sconjoint:::.sc_with_torch_seed(11L, {
    ## Perturb RNG inside
    runif(5)
  })
  after <- .Random.seed
  expect_identical(before, after)
})
