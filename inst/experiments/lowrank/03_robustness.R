## 03_robustness.R -- Is the low-rank edge real or fold/seed noise?
##
## (1) Paired per-fold differences (same folds): torch_fz_lr2_d2 vs glm_main
##     and vs torch_fz.
## (2) Seed sensitivity: refit torch_fz and torch_fz_lr2_d2 with torch seeds
##     2 and 3 (same CV folds), report mean metrics.
##
## Run AFTER 02_fit_ladder.R from the worktree root.

suppressPackageStartupMessages({ library(glmnet); library(pROC); library(torch) })
exp_dir <- "inst/experiments/lowrank"
res <- readRDS(file.path(exp_dir, "results_ladder.rds"))

cat("== Paired per-fold differences (negative logloss diff = low-rank better) ==\n")
for (ref in c("glm_main", "torch_fz")) {
  dll <- res$torch_fz_lr2_d2$per_fold[, "logloss"] - res[[ref]]$per_fold[, "logloss"]
  dau <- res$torch_fz_lr2_d2$per_fold[, "auc"]     - res[[ref]]$per_fold[, "auc"]
  cat(sprintf("lr2_d2 - %-9s  dlogloss: %s | mean %+.4f (t=%.2f)\n", ref,
              paste(sprintf("%+.4f", dll), collapse = " "),
              mean(dll), mean(dll) / (sd(dll) / sqrt(5))))
  cat(sprintf("                   dauc:     %s | mean %+.4f (t=%.2f)\n",
              paste(sprintf("%+.4f", dau), collapse = " "),
              mean(dau), mean(dau) / (sd(dau) / sqrt(5))))
}

## --- Seed sensitivity ----------------------------------------------------------
prep <- readRDS(file.path(exp_dir, "prep_sw.rds"))
y  <- prep$y;  dX <- prep$dX; XA <- prep$XA; XB <- prep$XB
Z  <- prep$Z;  resp <- prep$respondent
p <- ncol(dX)

set.seed(20260612)                       # reproduce identical fold assignment
uresp <- unique(resp)
fold_of_resp <- sample(rep(1:5, length.out = length(uresp)))
names(fold_of_resp) <- uresp
fold <- fold_of_resp[resp]

metrics <- function(y, pr) {
  pr <- pmin(pmax(pr, 1e-12), 1 - 1e-12)
  c(logloss = -mean(y * log(pr) + (1 - y) * log(1 - pr)),
    acc     = mean((pr > 0.5) == (y == 1)),
    auc     = as.numeric(pROC::auc(y, pr, quiet = TRUE, direction = "<")))
}

## same fitter as 02 (kept in sync by sourcing the function definition would be
## nicer; duplicated here to keep scripts standalone)
fit_torch <- function(tr, te, r = 0L, lam_V = 1e-2, hidden = c(32L, 32L, 16L),
                      epochs = 1000L, lr = 5e-3, wd = 1e-4, seed = 1L,
                      val_frac = 0.15, patience = 8L, eval_every = 10L) {
  torch_manual_seed(seed)
  p_Z <- ncol(Z)
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

cat("\n== Seed sensitivity (CV-mean metrics) ==\n")
seed_res <- list()
for (sd_i in 1:3) {
  for (cfg in list(list(name = "torch_fz", r = 0L, lam = 1e-2),
                   list(name = "torch_fz_lr2_d2", r = 2L, lam = 1e-2))) {
    fm <- matrix(NA_real_, 5, 3, dimnames = list(NULL, c("logloss", "acc", "auc")))
    for (f in 1:5) {
      tr <- which(fold != f); te <- which(fold == f)
      out <- fit_torch(tr, te, r = cfg$r, lam_V = cfg$lam, seed = sd_i)
      fm[f, ] <- metrics(y[te], out$pr)
    }
    key <- paste0(cfg$name, "_seed", sd_i)
    seed_res[[key]] <- colMeans(fm)
    cat(sprintf("%-26s logloss %.4f  acc %.4f  auc %.4f\n",
                key, mean(fm[, 1]), mean(fm[, 2]), mean(fm[, 3])))
  }
}
saveRDS(seed_res, file.path(exp_dir, "results_seeds.rds"))
cat("Saved", file.path(exp_dir, "results_seeds.rds"), "\n")
