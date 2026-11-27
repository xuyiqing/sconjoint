# sc_example_data(): structure of the paper's inputs built from the bundled data.
# Exact equality with the paper's own input files is checked by gate 3
# (statsclaw-workspace/sconjoint/runs/2026-11-25-paper-v4/gates/gate3-example-data.R).

expect_example_shape <- function(ex, app, n, tasks, p, nz) {
  expect_s3_class(ex, "sc_data")
  expect_named(ex, c("data", "Z", "groups", "fold", "validation", "design"))
  expect_identical(ex$design, app)
  expect_s3_class(ex$data, "rum_data")
  expect_identical(ex$data$n, n)
  expect_identical(length(ex$data$y), tasks)
  expect_identical(dim(ex$data$d), c(tasks, p))
  expect_true(all(ex$data$y %in% c(0, 1)))
  ids <- ex$data$ids
  expect_identical(dim(ex$Z), c(n, nz))
  expect_identical(rownames(ex$Z), ids)
  expect_false(anyNA(ex$Z))
  expect_identical(length(ex$fold), n)
  expect_identical(sort(unique(ex$fold)), 1:5)
  expect_length(ex$validation, 5L)
  for (k in 1:5) {
    v <- ex$validation[[k]]
    expect_true(is.character(v))
    expect_true(all(v %in% ids))
    expect_false(anyDuplicated(v) > 0)
    expect_true(all(ex$fold[match(v, ids)] != k))   # validation respondents are training respondents
  }
  expect_identical(nrow(ex$groups), n)
  expect_true(all(ex$groups$party %in% c("Dem", "Ind", "Rep")))
}

test_that("sc_example_data('gs') returns the left-first candidate inputs", {
  ex <- sc_example_data("gs")
  expect_example_shape(ex, "gs", 1605L, 20657L, 30L, 16L)
  expect_identical(tabulate(ex$fold), rep(321L, 5))
  expect_identical(lengths(ex$validation, use.names = FALSE), rep(192L, 5))
  expect_identical(table(ex$groups$party)[["Ind"]], 228L)
  expect_identical(sum(ex$groups$PL), 1377)
  expect_true(all(ex$groups$ideology %in% c("lib", "mod", "con")))
  expect_match(ex$data$metadata$orientation, "left")

  # Orientation: a matchup whose candidate 1 was on the right is flipped.
  gs <- sconjoint::gs2020
  p1 <- gs[gs$profile == 1L, ]
  flip <- p1$on_left == 0L
  d0 <- as.matrix(p1[, grep("^diff_", names(p1))])
  expect_identical(ex$data$y[flip], 1 - p1$choice[flip])
  expect_identical(ex$data$y[!flip], as.numeric(p1$choice[!flip]))
  expect_equal(ex$data$d[flip, ], -unname(d0[flip, ]))
  expect_equal(ex$data$d[!flip, ], unname(d0[!flip, ]))
})

test_that("sc_example_data('br') returns the valid-income tax-plan inputs", {
  ex <- sc_example_data("br")
  expect_example_shape(ex, "br", 1991L, 15928L, 16L, 16L)
  expect_identical(tabulate(ex$fold), c(399L, 398L, 396L, 400L, 398L))
  expect_named(ex$groups, c("pid7", "party"))
  expect_identical(sum(ex$groups$pid7 == 4), 389L)        # the paper's independents
  expect_identical(table(ex$groups$party)[["Ind"]], 463L)  # includes "not sure" (pid7 = 8)
  expect_identical(colnames(ex$Z), c("age", "female", paste0("educ_", 2:6), "race_white",
                                     "income", paste0("pid7_", 2:8)))
  expect_false(any(c("206770863", "207186054") %in% ex$data$ids))
  expect_identical(ex$data$feature_names[c(1, 13)], c("tax1_linear", "revenue_helmert1"))
  # Each respondent's education and party dummies sum to at most one.
  expect_true(all(rowSums(ex$Z[, paste0("educ_", 2:6)]) <= 1))
  expect_true(all(rowSums(ex$Z[, paste0("pid7_", 2:8)]) <= 1))
})

test_that("sc_example_data rejects an unknown application", {
  expect_error(sc_example_data("sw"))
})
