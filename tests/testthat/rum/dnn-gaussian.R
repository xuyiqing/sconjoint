# Ported from conjointRUM 0.1.3 tests/dnn-gaussian.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Deterministic Gaussian sequence / ReLU-chain checks. No responses are generated,
# no optimizer is called, and no application data are read.
argv <- commandArgs(FALSE)
arg <- grep("^--file=", argv, value = TRUE)
script <- if (length(arg)) sub("^--file=", "", arg[1L]) else ""
roots <- c(if (nzchar(script)) dirname(dirname(normalizePath(script))),
           getwd(), dirname(getwd()))
found <- vapply(roots, function(x) file.exists(file.path(x, "R", "rum-dnn-gaussian.R")), logical(1))
root <- if (any(found)) roots[which(found)[1L]] else NA_character_
if (!is.na(root) && dir.exists(file.path(root, ".local-library")))
  .libPaths(c(file.path(root, ".local-library"), .libPaths()))
if (!requireNamespace("sconjoint", quietly = TRUE)) stop("Install sconjoint before this test")
ns <- asNamespace("sconjoint")
api <- new.env(parent = ns)
# RUM_DNN_INSTALLED=1 audits the installed API without overlaying new R source.
if (!is.na(root) && Sys.getenv("RUM_DNN_INSTALLED", "0") != "1")
  sys.source(file.path(root, "R", "rum-dnn-gaussian.R"), envir = api)
get_api <- function(name) get(name, envir = api, inherits = TRUE)
make_data <- get_api("rum_data"); make_par <- get_api("rum_parameters")
make_quad <- get_api("rum_quadrature"); native_eval <- get_api("rum_loglik")
subset_data <- get_api("rum_subset")
evaluate <- get_api("rum_dnn_gaussian_evaluate")
network <- get_api("rum_dnn_gaussian")
forward <- get_api("rum_dnn_gaussian_forward")
network_loss <- get_api("rum_dnn_gaussian_loss")

checks <- list()
check <- function(name, actual, expected, tol = 2e-10) {
  shape <- identical(dim(actual), dim(expected)) && length(actual) == length(expected)
  error <- if (shape && all(is.finite(actual)) && all(is.finite(expected)))
    max(c(0, abs(actual - expected))) else Inf
  checks[[length(checks) + 1L]] <<- data.frame(check = name,
    max_abs_error = error, tolerance = tol, passed = error <= tol,
    stringsAsFactors = FALSE)
}
truth <- function(name, condition) check(name, as.numeric(isTRUE(condition)), 1, 0)
reject <- function(name, expr) truth(name, tryCatch({force(expr); FALSE}, error = function(e) TRUE))
softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
logsumexp <- function(x) { m <- max(x); m + log(sum(exp(x - m))) }

# Independent literal node loop, including the zero-variance heat derivative.
reference <- function(data, mu, kappa, loading, omega, quadrature) {
  n <- data$n; p <- data$p
  ll <- numeric(n); gm <- matrix(0, n, p); gc <- matrix(0, n, p + 2L)
  for (i in seq_len(n)) {
    ix <- data$offsets[i] + seq_len(data$T[i])
    d <- data$d[ix, , drop = FALSE]; y <- data$y[ix]
    node_ll <- numeric(length(quadrature$weight))
    derivative <- matrix(0, length(node_ll), 2L*p + 2L)
    for (q in seq_along(node_ll)) {
      eta <- kappa + drop(d %*% mu[i, ]) +
        drop(d %*% loading) * quadrature$u[q] + sqrt(omega) * quadrature$v[q]
      prob <- plogis(eta); residual <- y - prob
      node_ll[q] <- log(quadrature$weight[q]) + sum(y*eta - softplus(eta))
      mean_derivative <- drop(crossprod(d, residual))
      variance_derivative <- if (omega > 0)
        sum(residual)*quadrature$v[q]/(2*sqrt(omega)) else
        .5*(sum(residual)^2 - sum(prob*(1-prob)))
      derivative[q, ] <- c(mean_derivative, sum(residual),
        mean_derivative*quadrature$u[q], variance_derivative)
    }
    ll[i] <- logsumexp(node_ll)
    weighted <- colSums(derivative * exp(node_ll - ll[i]))
    gm[i, ] <- weighted[seq_len(p)]
    gc[i, ] <- weighted[p + seq_len(p + 2L)]
  }
  list(loglik = ll, objective = mean(ll), output_score = gm, common_score = gc)
}
literal_forward <- function(net, z) {
  a <- z
  for (j in seq_along(net$weights)) {
    a <- sweep(a %*% net$weights[[j]], 2L, net$biases[[j]], "+")
    if (j < length(net$weights)) a <- pmax(a, 0)
  }
  a
}
ref_objective <- function(data, mu, kappa, loading, omega, quadrature)
  reference(data, mu, kappa, loading, omega, quadrature)$objective

d <- list(matrix(c(.8, -.5, 1.1, -.3, .7, .4), 3L, 2L),
          matrix(c(-.7, .9), 1L, 2L),
          matrix(c(.2, -1, .5, .9, .8, .3, -.4, -.6), 4L, 2L),
          matrix(c(-.4, .6, 1, -.2), 2L, 2L))
y <- list(c(1, 0, 1), 0, c(1, 1, 0, 1), c(0, 0))
n <- length(d); p <- 2L
mu <- matrix(c(.3, -.25, .1, .5, -.2, .4, .15, -.35), n, p)
# Separate output coordinates for every respondent give an independent native
# bridge without assuming mu lies in a particular covariate mean family.
bs <- lapply(seq_len(n), function(i) {
  b <- matrix(0, p, n*p); b[, (i-1L)*p + seq_len(p)] <- diag(p); b
})
data <- make_data(y, d, bs, ids = c("r3", "r1", "r4", "r2"),
  feature_names = c("first", "second"),
  metadata = list(orientation = "Synthetic fixture: y=1 selects recorded first profile"))
kappa <- .17; loading <- c(.65, -.4); omega <- .36
quad <- make_quad(c(9L, 11L))
par <- make_par(data, kappa, as.vector(t(mu)), loading, omega)
raw <- reference(data, mu, kappa, loading, omega, quad)
old <- native_eval(data, par, quad)
check("literal reference vs native log likelihood", raw$loglik, old$loglik)
old_mu <- t(vapply(seq_len(n), function(i)
  old$score[i, 1L + (i-1L)*p + seq_len(p)], numeric(p)))
old_common <- old$score[, c(1L, 1L+n*p+seq_len(p), length(par)), drop = FALSE]
check("literal output scores vs native selected mean coordinates", raw$output_score, old_mu)
check("literal common scores vs native full common block", raw$common_score, old_common)

new <- evaluate(data, mu, kappa, loading, omega, quad)
check("DNN output evaluator vs literal per-respondent likelihood", new$loglik, raw$loglik)
check("DNN output evaluator vs native likelihood", new$loglik, old$loglik)
check("output score analytic bridge", new$output_score, raw$output_score)
check("common score analytic bridge", new$common_score, raw$common_score)
check("equal-respondent objective with unequal T", new$objective, mean(raw$loglik))
check("loss sign", new$loss, -new$objective)
truth("fixture distinguishes per-task normalization", abs(new$objective - sum(raw$loglik)/sum(data$T)) > .1)
truth("fixture distinguishes average within-person task loss", abs(new$objective - mean(raw$loglik/data$T)) > .1)
check("objective output gradient averages respondents", new$objective_gradient$mu, raw$output_score/n)
check("objective kappa gradient", new$objective_gradient$kappa, mean(raw$common_score[, 1]))
check("objective loading gradient", new$objective_gradient$loading, colMeans(raw$common_score[, 1L+seq_len(p), drop=FALSE]))
check("objective variance gradient", new$objective_gradient$omega, mean(raw$common_score[, p+2L]))
for (nm in c("mu", "kappa", "loading", "omega"))
  check(paste("loss gradient sign", nm), new$loss_gradient[[nm]], -new$objective_gradient[[nm]])

step <- 1e-6
fd_mu <- matrix(0, n, p)
for (j in seq_along(mu)) {
  plus <- minus <- mu; plus[j] <- plus[j]+step; minus[j] <- minus[j]-step
  fd_mu[j] <- (ref_objective(data, plus, kappa, loading, omega, quad)-
              ref_objective(data, minus, kappa, loading, omega, quad))/(2*step)
}
check("all output coordinates independently finite-differenced", new$objective_gradient$mu, fd_mu, 2e-8)
common <- c(kappa, loading, omega)
fd_common <- vapply(seq_along(common), function(j) {
  plus <- minus <- common; plus[j] <- plus[j]+step; minus[j] <- minus[j]-step
  (ref_objective(data, mu, plus[1], plus[1L+seq_len(p)], tail(plus,1), quad)-
   ref_objective(data, mu, minus[1], minus[1L+seq_len(p)], tail(minus,1), quad))/(2*step)
}, numeric(1))
check("kappa/loading/variance independently finite-differenced",
      c(new$objective_gradient$kappa, new$objective_gradient$loading, new$objective_gradient$omega), fd_common, 2e-8)

# A deliberately wrong taskwise mixture must differ on this exact fixture.
wrong_ll <- vapply(seq_len(n), function(i) {
  ix <- data$offsets[i] + seq_len(data$T[i])
  sum(vapply(ix, function(t) {
    eta <- kappa + sum(data$d[t, ]*mu[i, ]) +
      sum(data$d[t, ]*loading)*quad$u + sqrt(omega)*quad$v
    logsumexp(log(quad$weight)+data$y[t]*eta-softplus(eta))
  }, numeric(1)))
}, numeric(1))
truth("fixture detects fresh effects at every task", max(abs(new$loglik-wrong_ll)) > .01)

part <- list(c(1L,3L,4L), 2L)
pieces <- lapply(part, function(ix) evaluate(subset_data(data, ix), mu[ix,,drop=FALSE], kappa, loading, omega, quad))
check("unequal-batch objective aggregation", sum(vapply(seq_along(part), function(j)
  length(part[[j]])*pieces[[j]]$objective/n, numeric(1))), new$objective)
for (j in seq_along(part)) {
  check(paste("batch likelihood",j), pieces[[j]]$loglik, new$loglik[part[[j]]])
  check(paste("batch raw output scores",j), pieces[[j]]$output_score, new$output_score[part[[j]],,drop=FALSE])
}
order <- c(4L,2L,1L,3L)
reordered <- evaluate(subset_data(data, order), mu[order,,drop=FALSE], kappa, loading, omega, quad)
check("respondent-order invariance", reordered$loglik, new$loglik[order])

swapped <- make_data(lapply(y,function(x)1-x), lapply(d,function(x)-x), bs,
  metadata=list(orientation="Synthetic fixture: labels jointly swapped"))
swap <- evaluate(swapped, mu, -kappa, loading, omega, quad)
check("complete label swap likelihood", swap$loglik, new$loglik)
check("complete label swap mean score", swap$output_score, new$output_score)
check("complete label swap common score transport", swap$common_score,
      sweep(new$common_score,2L,c(-1,rep(1,p+1L)),"*"))
flip <- evaluate(data, mu, kappa, -loading, omega, quad)
check("latent policy sign invariance", flip$loglik, new$loglik)
check("latent policy sign score transport", flip$common_score,
      sweep(new$common_score,2L,c(1,rep(-1,p),1),"*"))

zero <- evaluate(data, mu, kappa, rep(0,p), 0, quad)
ordinary_ll <- vapply(seq_len(n),function(i){ix<-data$offsets[i]+seq_len(data$T[i]);
  eta<-kappa+drop(data$d[ix,,drop=FALSE]%*%mu[i,]);sum(data$y[ix]*eta-softplus(eta))},numeric(1))
check("zero persistent effects reduce to ordinary sequence logit", zero$loglik, ordinary_ll)
check("zero loading score is zero", zero$common_score[,1L+seq_len(p),drop=FALSE], matrix(0,n,p))
boundary <- evaluate(data, mu, kappa, loading, 0, quad)
boundary_ref <- reference(data,mu,kappa,loading,0,quad)
truth("zero variance explicitly flags right derivative", identical(boundary$gradient_convention, "omega_right_derivative"))
truth("interior evaluation flag", identical(new$gradient_convention, "interior"))
truth("likelihood evaluation does not claim inference", identical(new$inference_status, "unavailable"))
check("omega boundary heat score", boundary$common_score[,p+2L], boundary_ref$common_score[,p+2L])
hh <- 2e-5
fd_right <- (-3*boundary_ref$objective +
  4*ref_objective(data,mu,kappa,loading,hh,quad)-
  ref_objective(data,mu,kappa,loading,2*hh,quad))/(2*hh)
check("omega boundary second-order right difference", boundary$objective_gradient$omega, fd_right, 2e-7)
large_mu <- mu*350
large <- evaluate(data,large_mu,-85,c(2,-1),.7,quad)
large_ref <- reference(data,large_mu,-85,c(2,-1),.7,quad)
check("extreme utility stable log likelihood", large$loglik, large_ref$loglik, 2e-9)
truth("extreme utility scores remain finite", all(is.finite(large$output_score))&&all(is.finite(large$common_score)))

# A small deterministic network has active and deliberately inactive units.
# Independent central differences cover every weight/bias, not just one layer.
z <- matrix(c(-.6,.1,.8,1.1, .2,-.3,.4,-.1, -.4,.5,.3,.6), n, 3L)
net <- network(3L,p,hidden=c(4L,3L),seed=17L)
net$weights <- list(matrix(c(.2,-.1,.15, .1,.1,-.1, -.15,.2,.1, .05,-.1,.2),3L,4L),
                    matrix(c(.2,-.1,.3,.1, -.15,.05,.1,.2, .1,.2,-.1,.05),4L,3L),
                    matrix(c(.3,-.2,.1, -.15,.25,.2),3L,2L))
net$biases <- list(c(.8,-1.7,.6,.9),c(.5,.7,-1.8),c(.1,-.2))
stage <- z; margin <- Inf
for(j in seq_len(length(net$weights)-1L)) {
  stage <- sweep(stage%*%net$weights[[j]],2L,net$biases[[j]],"+")
  margin <- min(margin,abs(stage)); stage<-pmax(stage,0)
}
truth("network fixture stays away from every ReLU kink", margin > .1)
check("network forward independent affine/ReLU calculation", forward(net,z), literal_forward(net,z))
nn <- network_loss(net,data,z,kappa,loading,omega,quad)
check("network objective independent sequence reference", nn$objective,
  ref_objective(data,literal_forward(net,z),kappa,loading,omega,quad))
for (block in c("weights","biases")) for (layer in seq_along(net[[block]])) {
  expected <- net[[block]][[layer]]*0
  for (j in seq_along(expected)) {
    plus <- minus <- net
    plus[[block]][[layer]][j] <- plus[[block]][[layer]][j]+step
    minus[[block]][[layer]][j] <- minus[[block]][[layer]][j]-step
    expected[j] <- (ref_objective(data,literal_forward(plus,z),kappa,loading,omega,quad)-
                   ref_objective(data,literal_forward(minus,z),kappa,loading,omega,quad))/(2*step)
  }
  check(paste("full network finite difference",block,layer), nn$objective_gradient[[block]][[layer]], expected, 3e-8)
  check(paste("network loss-gradient sign",block,layer), nn$loss_gradient[[block]][[layer]], -nn$objective_gradient[[block]][[layer]])
}
truth("active network derivatives nonzero", max(abs(unlist(nn$objective_gradient$weights))) > .01)
check("inactive first-layer unit has zero weights gradient", nn$objective_gradient$weights[[1]][,2], rep(0,3))
check("inactive second-layer unit has zero bias gradient", nn$objective_gradient$biases[[2]][3], 0)
for(nm in c("kappa","loading","omega")) {
  evaluated <- evaluate(data,literal_forward(net,z),kappa,loading,omega,quad)
  check(paste("network common gradient",nm), nn$objective_gradient[[nm]], evaluated$objective_gradient[[nm]])
}
nn_parts <- lapply(part,function(ix)network_loss(net,subset_data(data,ix),z[ix,,drop=FALSE],kappa,loading,omega,quad))
for(block in c("weights","biases")) for(layer in seq_along(net[[block]])) {
  combined <- net[[block]][[layer]]*0
  for(j in seq_along(part)) combined <- combined + length(part[[j]])/n*nn_parts[[j]]$objective_gradient[[block]][[layer]]
  check(paste("network unequal-batch gradient",block,layer), combined, nn$objective_gradient[[block]][[layer]])
}

# The declared application architecture, private initialization, and affine
# special case are tested without training either network.
default_net <- network()
truth("declared default architecture", identical(default_net$hidden, c(32L,32L,16L)) &&
      default_net$p_z == 16L && default_net$p == 16L)
check("declared default network parameter count", default_net$parameter_count, 2400)
truth("network construction does not claim inference", identical(default_net$inference_status, "unavailable"))
rng_existed <- exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
if (rng_existed) rng_before <- get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
again <- network(3L,p,hidden=c(4L,3L),seed=17L)
truth("constructor preserves caller RNG", if (rng_existed)
  identical(rng_before,get(".Random.seed",envir=.GlobalEnv,inherits=FALSE)) else
  !exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
check("constructor seed reproducibility", unlist(again[c("weights","biases")]),
      unlist(network(3L,p,hidden=c(4L,3L),seed=17L)[c("weights","biases")]))
affine <- network(3L,p,hidden=integer(0),seed=9L)
affine$weights <- list(matrix(c(.1,-.2,.3,-.4,.5,.2),3L,p))
affine$biases <- list(c(.2,-.1))
affine_mu <- literal_forward(affine,z)
affine_result <- network_loss(affine,data,z,kappa,loading,omega,quad)
affine_ref <- reference(data,affine_mu,kappa,loading,omega,quad)
check("affine-network special case likelihood",affine_result$loglik,affine_ref$loglik)
check("affine-network special case weight gradient",affine_result$objective_gradient$weights[[1]],
      crossprod(z,affine_ref$output_score/n))
check("affine-network special case bias gradient",affine_result$objective_gradient$biases[[1]],
      colMeans(affine_ref$output_score))

reject("negative variance rejected",evaluate(data,mu,kappa,loading,-.01,quad))
reject("nonfinite mean output rejected",evaluate(data,mu*Inf,kappa,loading,omega,quad))
reject("wrong output row count rejected",evaluate(data,mu[-1,,drop=FALSE],kappa,loading,omega,quad))
reject("nonGaussian rule rejected by Gaussian API",evaluate(data,mu,kappa,loading,omega,make_quad(9L,"three_point")))
reject("wrong network input columns rejected",forward(net,z[,-1,drop=FALSE]))
reject("nonfinite network input rejected",forward(net,z*Inf))
bad_mu_names <- mu; rownames(bad_mu_names) <- rev(data$ids)
bad_z_names <- z; rownames(bad_z_names) <- rev(data$ids)
reject("named mean row mismatch rejected",evaluate(data,bad_mu_names,kappa,loading,omega,quad))
reject("named network input row mismatch rejected",network_loss(net,data,bad_z_names,kappa,loading,omega,quad))
named_mu <- mu; rownames(named_mu) <- data$ids
named_z <- z; rownames(named_z) <- data$ids
check("correct named output rows preserved",evaluate(data,named_mu,kappa,loading,omega,quad)$loglik,new$loglik)
check("correct named network rows preserved",network_loss(net,data,named_z,kappa,loading,omega,quad)$loglik,nn$loglik)

# Finite activations do not guarantee finite chain-rule products. This fixture
# reaches the backpropagation guard while its independent sequence LL is finite.
overflow_net <- network(1L,1L,hidden=1L,seed=1L)
overflow_net$weights <- list(matrix(0,1L,1L),matrix(1,1L,1L))
overflow_net$biases <- list(1,0)
overflow_z <- matrix(1e308,1L,1L)
overflow_data <- make_data(list(1),list(matrix(10,1L,1L)),
  list(matrix(1,1L,1L)), metadata=list(orientation="Synthetic fixed response"))
overflow_mu <- forward(overflow_net,overflow_z)
check("overflow fixture has finite forward output",overflow_mu,matrix(1,1L,1L))
check("overflow fixture has finite sequence likelihood",
      evaluate(overflow_data,overflow_mu,-10,0,0,quad)$loglik,log(.5))
overflow_message <- tryCatch({network_loss(overflow_net,overflow_data,overflow_z,-10,0,0,quad); ""},
                             error=function(e)conditionMessage(e))
truth("backpropagation overflow rejected explicitly",
      grepl("Non-finite network gradient",overflow_message,fixed=TRUE))
invalid_hidden <- overflow_net; invalid_hidden$hidden <- .Machine$integer.max + 1
reject("mutated out-of-range hidden width rejected",forward(invalid_hidden,overflow_z))

result <- do.call(rbind,checks)
print(result,row.names=FALSE)
stopifnot(all(result$passed))
cat(nrow(result),"deterministic Gaussian DNN likelihood/gradient checks passed; no fits or response simulations.\n")
