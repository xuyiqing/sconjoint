## Generate simdata: synthetic conjoint with known ground truth
## Used in the Sanity Check chapter of the tutorial.

set.seed(42)

M  <- 1000L  # respondents
TT <- 6L     # tasks per respondent

## Respondent-level moderators
z1 <- rnorm(M)
z2 <- rnorm(M)

## True beta(Z) for each respondent
## beta1(Z) = 0.5 + 0.3*z1  (heterogeneous, mostly positive)
## beta2(Z) = -0.8 + 0.2*z2 (heterogeneous, mostly negative)
## beta3(Z) = 0.3            (homogeneous, always positive)
beta_true <- cbind(
  x1 = 0.5 + 0.3 * z1,
  x2 = -0.8 + 0.2 * z2,
  x3 = rep(0.3, M)
)

## Build long-format data frame
rows <- vector("list", M * TT * 2L)
idx <- 0L
for (i in seq_len(M)) {
  for (tt in seq_len(TT)) {
    xA <- rbinom(3, 1, 0.5)
    xB <- rbinom(3, 1, 0.5)
    deltaX <- xA - xB
    V <- sum(deltaX * beta_true[i, ])
    choiceA <- rbinom(1, 1, plogis(V))
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      respondent = i, task = tt, profile = 1L, choice = choiceA,
      x1 = xA[1], x2 = xA[2], x3 = xA[3], z1 = z1[i], z2 = z2[i]
    )
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      respondent = i, task = tt, profile = 2L, choice = 1L - choiceA,
      x1 = xB[1], x2 = xB[2], x3 = xB[3], z1 = z1[i], z2 = z2[i]
    )
  }
}
simdata <- do.call(rbind, rows)
rownames(simdata) <- NULL

## Store true parameters as attributes
attr(simdata, "beta_true") <- beta_true
attr(simdata, "dgp") <- list(
  M = M, TT = TT,
  beta1 = "0.5 + 0.3 * z1",
  beta2 = "-0.8 + 0.2 * z2",
  beta3 = "0.3"
)

usethis::use_data(simdata, overwrite = TRUE)
