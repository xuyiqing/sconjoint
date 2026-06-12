## 01_prep_sw.R -- Build paired-choice data from the shipped sw2022 dataset.
##
## Feasibility experiment: low-rank interaction extension (exp/lowrank-interaction).
## Mirrors the production SW coding (code/31_data_preparation_saha_weeks.R)
## minimally: dummy-code the 5 attributes (reference level dropped), deltaX =
## X_A - X_B, Z = (resp_female, std age, pid dummies).
##
## Run from the worktree root:  Rscript inst/experiments/lowrank/01_prep_sw.R
## Output: inst/experiments/lowrank/prep_sw.rds  (gitignored cache)

out_dir <- "inst/experiments/lowrank"
stopifnot(dir.exists(out_dir))

load("data/sw2022.rda")
d <- sw2022
cat("Rows:", nrow(d), " respondents:", length(unique(d$respondent)), "\n")

## --- Dummy-code attributes (reference coding, drop intercept) ---------------
ATTRS <- c("agenda", "talent", "children", "cand_gender", "prior_office")
fml <- as.formula(paste("~", paste(ATTRS, collapse = " + ")))
mm  <- model.matrix(fml, data = d)
grp <- attr(mm, "assign")[-1]          # attribute id for each dummy column
X   <- mm[, -1, drop = FALSE]
colnames(X) <- make.names(colnames(X))
p <- ncol(X)
cat("Dummy columns p =", p, "\n")      # expect 2+6+3+1+1 = 13

## --- Pair profiles within task ----------------------------------------------
ia <- which(d$profile == 1L)
ib <- which(d$profile == 2L)
a <- d[ia, ]; b <- d[ib, ]
oa <- order(a$respondent, a$task); ob <- order(b$respondent, b$task)
a <- a[oa, ]; b <- b[ob, ]
stopifnot(all(a$respondent == b$respondent), all(a$task == b$task))
XA <- X[ia, , drop = FALSE][oa, , drop = FALSE]
XB <- X[ib, , drop = FALSE][ob, , drop = FALSE]
stopifnot(all(a$choice + b$choice == 1L))   # forced choice
y  <- a$choice
dX <- XA - XB
cat("Tasks:", nrow(dX), " mean(y) =", round(mean(y), 4), "\n")

## --- Respondent moderators Z --------------------------------------------------
Z <- cbind(
  resp_female = a$resp_female,
  age         = as.numeric(scale(a$age)),
  pid_rep     = as.numeric(a$pid == "Republican (GOP)"),
  pid_ind     = as.numeric(a$pid == "Independent")
)
respondent <- a$respondent

## --- Interaction features -----------------------------------------------------
## (1) Difference-of-quadratics (structurally coherent, derived from profile
##     utility):  feat_{kl} = XA_k XA_l - XB_k XB_l  for k < l.
##     With dummy coding:
##       * diagonal terms X_k^2 = X_k -> collinear with main effects, dropped;
##       * within-attribute pairs X_k X_l = 0 (mutually exclusive levels) ->
##         structurally zero, dropped.
##     Identified columns = cross-attribute pairs only.
pairs_all   <- t(combn(p, 2))
cross_attr  <- grp[pairs_all[, 1]] != grp[pairs_all[, 2]]
pairs_cross <- pairs_all[cross_attr, , drop = FALSE]
F_dq <- matrix(0, nrow(dX), nrow(pairs_cross))
cn <- character(nrow(pairs_cross))
for (j in seq_len(nrow(pairs_cross))) {
  k <- pairs_cross[j, 1]; l <- pairs_cross[j, 2]
  F_dq[, j] <- XA[, k] * XA[, l] - XB[, k] * XB[, l]
  cn[j] <- paste0(colnames(X)[k], ":", colnames(X)[l])
}
colnames(F_dq) <- cn
cat("Diff-of-quadratics features:", ncol(F_dq),
    "(of", nrow(pairs_all), "raw pairs;",
    sum(!cross_attr), "within-attribute pairs structurally zero;",
    p, "diagonal terms collinear with main effects)\n")
zero_dq <- which(apply(F_dq, 2, function(v) all(v == 0)))
if (length(zero_dq)) { F_dq <- F_dq[, -zero_dq, drop = FALSE] }
cat("After dropping empirically-zero columns:", ncol(F_dq), "\n")

## (2) Paper-as-written quadratic in the DIFFERENCE:  ||V' dX||^2 expands into
##     dX_k dX_l terms for all k <= l (diagonal dX_k^2 = |dX_k| is NOT collinear
##     with dX_k; within-attribute products are NOT structurally zero here).
F_dx <- matrix(0, nrow(dX), p + nrow(pairs_all))
cn2 <- character(ncol(F_dx))
for (k in seq_len(p)) { F_dx[, k] <- dX[, k]^2; cn2[k] <- paste0("d2.", colnames(X)[k]) }
for (j in seq_len(nrow(pairs_all))) {
  k <- pairs_all[j, 1]; l <- pairs_all[j, 2]
  F_dx[, p + j] <- dX[, k] * dX[, l]
  cn2[p + j] <- paste0("dd.", colnames(X)[k], ":", colnames(X)[l])
}
colnames(F_dx) <- cn2
zero_dx <- which(apply(F_dx, 2, function(v) all(v == 0)))
if (length(zero_dx)) { F_dx <- F_dx[, -zero_dx, drop = FALSE] }
cat("deltaX-quadratic (as-written) features:", ncol(F_dx), "\n")

prep <- list(y = y, dX = dX, XA = XA, XB = XB, Z = Z,
             respondent = respondent, grp = grp,
             F_dq = F_dq, F_dx = F_dx,
             n_within_zero = sum(!cross_attr))
saveRDS(prep, file.path(out_dir, "prep_sw.rds"))
cat("Saved", file.path(out_dir, "prep_sw.rds"), "\n")
