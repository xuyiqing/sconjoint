## Helper: build a tiny long-format conjoint data frame for scfit() tests.
## Each (respondent, task) has 2 profiles (pos = 1, 2).  The first
## profile carries the non-trivial attribute vector dX_i; the second
## profile is all zeros, so DeltaX = dX_i.  The response y is 1 on the
## first profile when the choice lands on it, 0 otherwise, and the
## complement on the second profile.
.make_toy_long <- function(M = 60L, T_i = 3L, p = 3L, p_Z = 2L, seed = 1L) {
  set.seed(seed)
  Z_mat <- matrix(stats::rnorm(M * p_Z), M, p_Z)
  beta_true <- cbind(
    0.5 + 0.3 * Z_mat[, 1],
    -0.4 + 0.5 * Z_mat[, 2],
    matrix(0.2, M, p - 2L)
  )
  rid <- rep(seq_len(M), each = T_i)
  tid <- rep(rep(seq_len(T_i), M), 1)
  dX  <- matrix(sample(c(-1, 0, 1), M * T_i * p, replace = TRUE),
                M * T_i, p)
  logit <- rowSums(dX * beta_true[rid, ])
  y1 <- stats::rbinom(M * T_i, 1, stats::plogis(logit))

  n_rows <- 2L * M * T_i
  out <- data.frame(
    rid = rep(rid, each = 2L),
    tid = rep(tid, each = 2L),
    pos = rep(c(1L, 2L), M * T_i)
  )
  attrs <- matrix(0, n_rows, p)
  for (j in seq_len(p)) {
    vals <- numeric(n_rows)
    vals[seq(1L, n_rows, by = 2L)] <- dX[, j]
    attrs[, j] <- vals
  }
  attr_df <- as.data.frame(attrs)
  names(attr_df) <- paste0("a", seq_len(p))
  zvals <- Z_mat[rid, , drop = FALSE]
  zdf <- as.data.frame(zvals[rep(seq_len(nrow(zvals)), each = 2L), , drop = FALSE])
  names(zdf) <- paste0("z", seq_len(p_Z))
  y_col <- numeric(n_rows)
  y_col[seq(1L, n_rows, by = 2L)] <- y1
  y_col[seq(2L, n_rows, by = 2L)] <- 1 - y1

  out <- cbind(out, attr_df, zdf, y = y_col)
  list(data = out, beta_true = beta_true,
       attr_names = paste0("a", seq_len(p)),
       z_names = paste0("z", seq_len(p_Z)))
}
