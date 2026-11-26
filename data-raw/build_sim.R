###############################################################################
# data-raw/build_sim.R
#
# Build the simulated candidate-choice conjoint `simconjoint` and its truth
# `simconjoint_truth` (data/simconjoint.rda, data/simconjoint_truth.rda).
#
# The data follow the package's own model exactly (R/rum-dnn-gaussian.R,
# R/sc-posterior.R conventions): respondent i chooses the first profile of
# task t with probability G(kappa + a_i + d_it' beta_i), G the logistic
# function, d_it the coded first profile minus the coded second,
#   beta_i = mu(Z_i) + Lambda u_i,  u_i ~ N(0, I_2),
#   a_i = sqrt(omega) v_i,          v_i ~ N(0, 1).
# mu(.) is a smooth nonlinear function of three covariates (below), q = 2 taste
# factors, kappa = 0.2, omega = 0.3. Tasks follow sc_design(): each attribute
# uniform on its levels, independently across attributes, the two profiles and
# the ten tasks.
#
# True population quantities. The covariates are discrete (age in whole years
# 18-80, a 7-point ideology scale, a 0/1 indicator), so a population average
# is an exact finite sum over the 63 x 7 x 2 = 882 covariate cells with their
# probabilities. Inside a cell:
#   * mean tastes: exact (c' mu(z));
#   * shares above zero: exact (Phi(c' mu(z) / ||Lambda' c||));
#   * the contest probability and the AME: one-dimensional logistic-normal
#     integrals by 100-node Gauss-Hermite (error below 1e-12); the AME
#     enumerates every context of the design exactly (the switched profile's
#     other attributes, the opponent profile, the screen side) with their
#     design probabilities;
#   * importance shares: a two-dimensional integral over u by a 256 x 256
#     Gauss-Hermite product rule; the change from 192 x 192 is recorded as the
#     accuracy (about 5e-7; the ratio converges slowly, 64 x 64 is off by 1e-4).
#
# Run from the package root:  Rscript data-raw/build_sim.R
###############################################################################

options(warn = 1)
suppressMessages(devtools::load_all(".", quiet = TRUE))   # sc_design(), sc_code()

seed <- 20261125L
n <- 800L
n_tasks <- 10L

# ---- design -------------------------------------------------------------------------------------------
design <- sc_design(list(
  party = c("Democrat", "Republican"),
  experience = c("none", "some", "long"),
  position = c("liberal", "moderate", "conservative"),
  gender = c("male", "female")), tasks = n_tasks)
features <- design$features      # party_Republican, experience_some, experience_long,
p <- length(features)            # position_moderate, position_conservative, gender_female

# ---- covariate law ------------------------------------------------------------------------------------
ages <- 18:80                                    # uniform over whole years
ideo_levels <- -3:3                              # 1 very liberal ... 7 very conservative, centred
ideo_probs <- c(0.09, 0.14, 0.15, 0.20, 0.15, 0.15, 0.12)
female_prob <- 0.5

# ---- true model ---------------------------------------------------------------------------------------
# Mean tastes. a = (age - 49) / 18 (about -1.7 to 1.7), s = ideology / 3 (-1 to 1).
#   party_Republican        2 tanh(3 s) + 0.1 a              partisanship follows ideology, saturating
#   experience_some         0.4 + 0.15 a
#   experience_long         0.6 + 0.35 tanh(1.5 a)           older respondents value experience more
#   position_moderate       g(s) (1 + 2 s)                   quadratic loss around the ideal point s,
#   position_conservative   g(s) 4 s                         weight g(s) = 0.25 + 0.25 s^2 (extremes care more)
#   gender_female           0.15 + 0.35 female - 0.15 a
mean_taste <- function(age, ideology, female) {
  a <- (age - 49) / 18; s <- ideology / 3; g <- 0.25 + 0.25 * s^2
  out <- cbind(party_Republican = 2 * tanh(3 * s) + 0.1 * a,
               experience_some = 0.4 + 0.15 * a,
               experience_long = 0.6 + 0.35 * tanh(1.5 * a),
               position_moderate = g * (1 + 2 * s),
               position_conservative = g * 4 * s,
               gender_female = 0.15 + 0.35 * female - 0.15 * a)
  out
}
environment(mean_taste) <- baseenv()
loading <- cbind(factor1 = c(0.5, 0, 0, 0.3, 0.6, 0),       # ideological intensity: party and policy
                 factor2 = c(0, 0.4, 0.8, 0, -0.2, 0.6))    # experience and gender
rownames(loading) <- features
kappa <- 0.2
omega <- 0.3

# ---- simulate -----------------------------------------------------------------------------------------
set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
age <- sample(ages, n, replace = TRUE)
ideology <- sample(ideo_levels, n, replace = TRUE, prob = ideo_probs)
female <- rbinom(n, 1, female_prob)
mu <- mean_taste(age, ideology, female)
u <- matrix(rnorm(2L * n), n, 2L, dimnames = list(NULL, c("u1", "u2")))
v <- rnorm(n)
beta <- mu + u %*% t(loading)
side <- sqrt(omega) * v                           # a_i, the respondent's persistent pull to profile 1
ids <- seq_len(n)
rownames(mu) <- rownames(beta) <- ids

draw_profiles <- function() {
  out <- lapply(design$attributes, function(l) factor(sample(l, n * n_tasks, replace = TRUE), levels = l))
  as.data.frame(out)
}
P1 <- draw_profiles(); P2 <- draw_profiles()
X <- sc_code(design, P1, P2)
resp <- rep(ids, each = n_tasks)
eta <- kappa + side[resp] + rowSums(X * beta[resp, , drop = FALSE])
y <- rbinom(n * n_tasks, 1, plogis(eta))

task_rows <- data.frame(id = resp, task = rep(seq_len(n_tasks), n))
long <- rbind(cbind(task_rows, profile = 1L, P1, chosen = y),
              cbind(task_rows, profile = 2L, P2, chosen = 1L - y))
long <- long[order(long$id, long$task, long$profile), ]
long$age <- age[long$id]; long$ideology <- ideology[long$id]; long$female <- female[long$id]
rownames(long) <- NULL
simconjoint <- long

# ---- true population quantities ----------------------------------------------------------------------
cells <- expand.grid(age = ages, ideology = ideo_levels, female = 0:1)
cells$w <- (1 / length(ages)) * ideo_probs[match(cells$ideology, ideo_levels)] *
  ifelse(cells$female == 1, female_prob, 1 - female_prob)
stopifnot(abs(sum(cells$w) - 1) < 1e-12)
MU <- mean_taste(cells$age, cells$ideology, cells$female)
W <- cells$w

gh <- function(k) {                               # Gauss-Hermite for N(0, 1) (Golub-Welsch)
  b <- sqrt(seq_len(k - 1L)); J <- matrix(0, k, k); J[cbind(1:(k - 1), 2:k)] <- b; J[cbind(2:k, 1:(k - 1))] <- b
  e <- eigen(J, symmetric = TRUE); o <- order(e$values)
  list(x = e$values[o], w = (e$vectors[1L, o])^2)
}
G1 <- gh(100L)
# E[G(m + s e)], e ~ N(0, 1): elementwise over m (matrix) with s (one per column, or scalar)
logit_normal <- function(m, s) {
  s <- matrix(s, nrow(m), ncol(m), byrow = TRUE); out <- 0
  for (k in seq_along(G1$x)) out <- out + G1$w[k] * plogis(m + s * G1$x[k])
  out
}
vec <- function(...) { x <- numeric(p); names(x) <- features; a <- list(...); x[names(a)] <- unlist(a); x }

# Mean tastes and shares
c_party <- vec(party_Republican = 1)
c_long <- vec(experience_long = 1)
c_cons <- vec(position_conservative = 1)
c_female <- vec(gender_female = 1)
c_comp <- vec(party_Republican = 1, experience_long = -1)
mean_of <- function(cc) sum(W * drop(MU %*% cc))
share_of <- function(cc) sum(W * pnorm(drop(MU %*% cc) / sqrt(sum(drop(t(loading) %*% cc)^2))))

# Contest (position effects set to zero): A = Republican, long experience, moderate, female;
# B = Democrat, some experience, liberal, male.
contest_A <- data.frame(party = "Republican", experience = "long", position = "moderate", gender = "female")
contest_B <- data.frame(party = "Democrat", experience = "some", position = "liberal", gender = "male")
d_contest <- drop(sc_code(design, contest_A, contest_B))
contest_value <- sum(W * logit_normal(matrix(drop(MU %*% d_contest)), sqrt(sum(drop(t(loading) %*% d_contest)^2))))

# AME of experience long vs none, with position effects kept (sc_ame_switch's definition): the switched
# profile sits on either screen side with probability 1/2, its other attributes and the opponent profile
# follow the design.
profiles <- expand.grid(design$attributes, stringsAsFactors = FALSE)
# x(profile): the coded profile minus the all-reference profile (row 1), which treatment dummies code as 0
xprof <- sc_code(design, profiles, profiles[rep(1L, nrow(profiles)), ])
prob_prof <- Reduce(`*`, lapply(names(design$attributes), function(a)
  design$probs[[a]][match(profiles[[a]], design$attributes[[a]])]))
ame_truth <- function(attr, level, ref) {
  others <- unique(profiles[setdiff(names(profiles), attr)])
  po <- Reduce(`*`, lapply(setdiff(names(design$attributes), attr), function(a)
    design$probs[[a]][match(others[[a]], design$attributes[[a]])]))
  val <- function(lv) {
    f <- others; f[[attr]] <- lv; f <- f[names(design$attributes)]
    xf <- sc_code(design, f, profiles[rep(1L, nrow(f)), ])          # x(focal)
    # contexts: (focal others) x (opponent)
    K <- expand.grid(f = seq_len(nrow(f)), o = seq_len(nrow(profiles)))
    D <- xf[K$f, , drop = FALSE] - xprof[K$o, , drop = FALSE]
    wk <- po[K$f] * prob_prof[K$o]
    s <- sqrt(rowSums((D %*% loading)^2) + omega)
    m <- MU %*% t(D)                                               # cells x contexts
    pr <- 0.5 * logit_normal(m + kappa, s) + 0.5 * logit_normal(m - kappa, s)
    sum(W * drop(pr %*% wk))
  }
  val(level) - val(ref)
}
ame_value <- ame_truth("experience", "long", "none")

# Importance shares: groups = attributes, level menus and weights = the design's (uniform).
Qg <- lapply(names(design$attributes), function(a) {
  A <- design$coding[[a]]; pi <- design$probs[[a]]; idx <- match(colnames(A), features)
  m <- colSums(pi * A); Q <- matrix(0, p, p); Q[idx, idx] <- crossprod(A * sqrt(pi)) - tcrossprod(m); Q
})
names(Qg) <- names(design$attributes)
importance_truth <- function(k) {
  g <- gh(k); U <- as.matrix(expand.grid(g$x, g$x)); wu <- as.vector(outer(g$w, g$w))
  out <- matrix(0, nrow(MU), length(Qg))
  for (j in seq_len(nrow(U))) {
    B <- MU + matrix(drop(loading %*% U[j, ]), nrow(MU), p, byrow = TRUE)
    v <- vapply(Qg, function(Q) rowSums((B %*% Q) * B), numeric(nrow(MU)))
    out <- out + wu[j] * v / rowSums(v)
  }
  colSums(W * out)
}
imp256 <- importance_truth(256L); imp192 <- importance_truth(192L)
imp_acc <- max(abs(imp256 - imp192))

truth_table <- data.frame(
  name = c("mean: Republican", "mean: long experience", "mean: conservative position", "mean: female candidate",
           "share: prefer Republican", "share: prefer long experience", "share: prefer female candidate",
           "share: Republican without experience over Democrat with long",
           "contest: A beats B", "AME: long vs no experience",
           paste("importance:", names(Qg))),
  kind = c(rep("mean", 4), rep("share", 4), "choice", "ame", rep("importance", length(Qg))),
  value = c(mean_of(c_party), mean_of(c_long), mean_of(c_cons), mean_of(c_female),
            share_of(c_party), share_of(c_long), share_of(c_female), share_of(c_comp),
            contest_value, ame_value, imp256),
  method = c(rep("exact sum over the 882 covariate cells", 8),
             "exact cell sum; 100-node Gauss-Hermite in each cell",
             "exact cell sum and exact enumeration of design contexts; 100-node Gauss-Hermite",
             rep("exact cell sum; 256 x 256 Gauss-Hermite over the two taste factors", length(Qg))),
  accuracy = c(rep(1e-12, 10), rep(signif(imp_acc, 2), length(Qg))),
  stringsAsFactors = FALSE)

simconjoint_truth <- list(
  design = design,
  parameters = list(kappa = kappa, omega = omega, loading = loading, q = 2L),
  mean_taste = mean_taste,
  covariate_law = list(age = ages, ideology = ideo_levels, ideology_probs = ideo_probs, female_prob = female_prob),
  respondents = data.frame(id = ids, age = age, ideology = ideology, female = female),
  mu = mu, beta = beta, u = u, side = side,
  population = truth_table,
  contrasts = list(party = c_party, long = c_long, conservative = c_cons, female = c_female, compensating = c_comp,
                   contest = d_contest),
  seed = seed)

print(truth_table[, c("name", "value", "accuracy")], digits = 4)
cat(sprintf("share of y = 1: %.3f; importance accuracy (192 vs 256 nodes): %.2g\n", mean(y), imp_acc))
save(simconjoint, file = "data/simconjoint.rda", compress = "xz")
save(simconjoint_truth, file = "data/simconjoint_truth.rda", compress = "xz")
cat("sizes (bytes):", file.size("data/simconjoint.rda"), file.size("data/simconjoint_truth.rda"), "\n")
