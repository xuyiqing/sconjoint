## 02_fit_ladder.R -- Model ladder for the low-rank interaction feasibility test.
##
## Respondent-level 5-fold CV on the shipped sw2022 paired-choice data.
## Models:
##   (a)  glm_main        : main-effects logistic regression on deltaX (no intercept)
##   (b1) ridge_dq        : [deltaX unpenalized] + diff-of-quadratics features, ridge
##   (b2) lasso_dq        : same features, lasso
##   (b3) ridge_dx        : [deltaX unpenalized] + paper-as-written deltaX-quadratic
##   (b4) lasso_dx        : same, lasso
##   (a') torch_fz        : package-pattern DNN f(Z) main effects only (r = 0)
##   (c)  torch_fz_lr{r}  : f(Z) + population low-rank V (p x r), r in {2,3},
##                          two weight-decay levels for V
##
## Run from worktree root: Rscript inst/experiments/lowrank/02_fit_ladder.R
## Output: inst/experiments/lowrank/results_ladder.rds + printed table.

suppressPackageStartupMessages({
  library(glmnet)
  library(pROC)
  library(torch)
})

set.seed(20260612)
exp_dir <- "inst/experiments/lowrank"
prep <- readRDS(file.path(exp_dir, "prep_sw.rds"))
y  <- prep$y;  dX <- prep$dX; XA <- prep$XA; XB <- prep$XB
Z  <- prep$Z;  resp <- prep$respondent
F_dq <- prep$F_dq; F_dx <- prep$F_dx
p <- ncol(dX); n <- length(y)

## --- Respondent-level 5-fold assignment --------------------------------------
uresp <- unique(resp)
fold_of_resp <- sample(rep(1:5, length.out = length(uresp)))
names(fold_of_resp) <- uresp
fold <- fold_of_resp[resp]
cat("Folds (tasks):", paste(table(fold), collapse = " "), "\n")

## --- Metrics ------------------------------------------------------------------
metrics <- function(y, pr) {
  pr <- pmin(pmax(pr, 1e-12), 1 - 1e-12)
  c(logloss = -mean(y * log(pr) + (1 - y) * log(1 - pr)),
    acc     = mean((pr > 0.5) == (y == 1)),
    auc     = as.numeric(pROC::auc(y, pr, quiet = TRUE, direction = "<")))
}

## --- glmnet fitter (main effects unpenalized, interactions penalized) ---------
fit_glmnet_int <- function(tr, te, Fmat, alpha) {
  Xtr <- cbind(dX[tr, ], Fmat[tr, , drop = FALSE])
  Xte <- cbind(dX[te, ], Fmat[te, , drop = FALSE])
  pf  <- c(rep(0, p), rep(1, ncol(Fmat)))
  ## inner CV folds grouped by respondent
  utr <- unique(resp[tr])
  inner_of <- sample(rep(1:5, length.out = length(utr))); names(inner_of) <- utr
  cv <- cv.glmnet(Xtr, y[tr], family = "binomial", alpha = alpha,
                  penalty.factor = pf, intercept = FALSE,
                  foldid = inner_of[resp[tr]], standardize = TRUE)
  pr <- as.numeric(predict(cv, Xte, s = "lambda.min", type = "response"))
  nz <- sum(coef(cv, s = "lambda.min")[-1][-(1:p)] != 0)
  list(pr = pr, nz_int = nz, lambda = cv$lambda.min)
}

## --- Torch fitter: f(Z) main effects + optional population low-rank V --------
fit_torch <- function(tr, te, r = 0L, lam_V = 1e-2, hidden = c(32L, 32L, 16L),
                      epochs = 1000L, lr = 5e-3, wd = 1e-4, seed = 1L,
                      val_frac = 0.15, patience = 8L, eval_every = 10L) {
  torch_manual_seed(seed)
  p_Z <- ncol(Z)
  ## respondent-level validation split within the training fold (early stopping)
  utr <- unique(resp[tr])
  vresp <- sample(utr, ceiling(val_frac * length(utr)))
  va <- tr[resp[tr] %in% vresp]
  tr <- tr[!(resp[tr] %in% vresp)]
  net <- nn_module(
    "LowRankConjoint",
    initialize = function() {
      layers <- list(); in_dim <- p_Z
      for (i in seq_along(hidden)) {
        layers[[paste0("hidden_", i)]] <- nn_linear(in_dim, hidden[i])
        in_dim <- hidden[i]
      }
      self$hidden <- nn_module_list(layers)
      self$param_layer <- nn_linear(in_dim, p)
      self$r <- r
      if (r > 0L) self$V <- nn_parameter(torch_randn(p, r) * 0.05)
    },
    get_beta = function(z) {
      h <- z
      for (i in seq_along(self$hidden)) h <- nnf_relu(self$hidden[[i]](h))
      self$param_layer(h)
    },
    forward = function(dx, z, xa, xb) {
      idx <- torch_sum(dx * self$get_beta(z), dim = 2L)
      if (self$r > 0L) {
        qa <- torch_sum(torch_mm(xa, self$V)^2, dim = 2L)
        qb <- torch_sum(torch_mm(xb, self$V)^2, dim = 2L)
        idx <- idx + qa - qb
      }
      idx
    }
  )()
  tt <- function(m) torch_tensor(as.matrix(m), dtype = torch_float())
  dx_tr <- tt(dX[tr, ]); z_tr <- tt(Z[tr, ]); xa_tr <- tt(XA[tr, ]); xb_tr <- tt(XB[tr, ])
  y_tr  <- torch_tensor(as.numeric(y[tr]), dtype = torch_float())
  dx_va <- tt(dX[va, ]); z_va <- tt(Z[va, ]); xa_va <- tt(XA[va, ]); xb_va <- tt(XB[va, ])
  y_va  <- torch_tensor(as.numeric(y[va]), dtype = torch_float())
  opt <- optim_adam(net$parameters, lr = lr, weight_decay = wd)
  lossfn <- nn_bce_with_logits_loss()
  best_val <- Inf; best_state <- NULL; bad <- 0L
  for (e in seq_len(epochs)) {
    net$train()
    opt$zero_grad()
    idx  <- net(dx_tr, z_tr, xa_tr, xb_tr)
    loss <- lossfn(idx, y_tr)
    if (r > 0L) loss <- loss + lam_V * torch_sum(net$V^2)
    loss$backward(); opt$step()
    if (e %% eval_every == 0L) {
      net$eval()
      vl <- with_no_grad(as.numeric(lossfn(net(dx_va, z_va, xa_va, xb_va), y_va)))
      if (vl < best_val - 1e-5) {
        best_val <- vl; bad <- 0L
        best_state <- lapply(net$state_dict(), function(t) t$clone())
      } else {
        bad <- bad + 1L
        if (bad >= patience) break
      }
    }
  }
  if (!is.null(best_state)) net$load_state_dict(best_state)
  net$eval()
  pr <- with_no_grad(
    as.numeric(torch_sigmoid(net(tt(dX[te, ]), tt(Z[te, ]), tt(XA[te, ]), tt(XB[te, ]))))
  )
  list(pr = pr)
}

## --- Run the ladder over folds -------------------------------------------------
models <- list(
  glm_main = function(tr, te) {
    df_tr <- data.frame(y = y[tr], dX[tr, ]); df_te <- data.frame(dX[te, ])
    fit <- glm(y ~ . - 1, family = binomial(), data = df_tr)
    list(pr = predict(fit, df_te, type = "response"))
  },
  ridge_dq = function(tr, te) fit_glmnet_int(tr, te, F_dq, alpha = 0),
  lasso_dq = function(tr, te) fit_glmnet_int(tr, te, F_dq, alpha = 1),
  ridge_dx = function(tr, te) fit_glmnet_int(tr, te, F_dx, alpha = 0),
  lasso_dx = function(tr, te) fit_glmnet_int(tr, te, F_dx, alpha = 1),
  torch_fz       = function(tr, te) fit_torch(tr, te, r = 0L),
  torch_fz_lr2_d3 = function(tr, te) fit_torch(tr, te, r = 2L, lam_V = 1e-3),
  torch_fz_lr2_d2 = function(tr, te) fit_torch(tr, te, r = 2L, lam_V = 1e-2),
  torch_fz_lr3_d3 = function(tr, te) fit_torch(tr, te, r = 3L, lam_V = 1e-3),
  torch_fz_lr3_d2 = function(tr, te) fit_torch(tr, te, r = 3L, lam_V = 1e-2)
)

res <- list()
for (m in names(models)) {
  fold_metrics <- matrix(NA_real_, 5, 3, dimnames = list(NULL, c("logloss", "acc", "auc")))
  times <- numeric(5); extras <- list()
  for (f in 1:5) {
    tr <- which(fold != f); te <- which(fold == f)
    t0 <- Sys.time()
    out <- models[[m]](tr, te)
    times[f] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    fold_metrics[f, ] <- metrics(y[te], out$pr)
    if (!is.null(out$nz_int)) extras[[f]] <- out$nz_int
  }
  res[[m]] <- list(mean = colMeans(fold_metrics), sd = apply(fold_metrics, 2, sd),
                   per_fold = fold_metrics, time_mean = mean(times),
                   nz_int = if (length(extras)) unlist(extras) else NULL)
  cat(sprintf("%-16s logloss %.4f (sd %.4f)  acc %.4f  auc %.4f  [%.1fs/fit]%s\n",
              m, res[[m]]$mean["logloss"], res[[m]]$sd["logloss"],
              res[[m]]$mean["acc"], res[[m]]$mean["auc"], res[[m]]$time_mean,
              if (!is.null(res[[m]]$nz_int))
                paste0("  nz_int: ", paste(res[[m]]$nz_int, collapse = "/")) else ""))
}

saveRDS(res, file.path(exp_dir, "results_ladder.rds"))
cat("Saved", file.path(exp_dir, "results_ladder.rds"), "\n")
