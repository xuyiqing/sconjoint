# Ported from conjointRUM 0.1.3 tests/dnn-train.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Tiny deterministic optimizer unit tests, not scientific simulation fits.
# Every response is fixed below. No application data or generated responses.
argv <- commandArgs(FALSE)
arg <- grep("^--file=", argv, value=TRUE)
script <- if(length(arg))sub("^--file=","",arg[1L]) else ""
roots <- c(if(nzchar(script))dirname(dirname(normalizePath(script))),
           getwd(),dirname(getwd()))
found <- vapply(roots,function(x)file.exists(file.path(x,"R","rum-dnn-train.R")),logical(1))
root <- if(any(found))roots[which(found)[1L]] else NA_character_
if(!is.na(root) && dir.exists(file.path(root,".local-library")))
  .libPaths(c(file.path(root,".local-library"),.libPaths()))
if(!requireNamespace("sconjoint",quietly=TRUE))stop("Install sconjoint before these unit tests")
ns <- asNamespace("sconjoint")
api <- new.env(parent=ns)
if(!is.na(root) && Sys.getenv("RUM_DNN_INSTALLED","0")!="1") {
  sys.source(file.path(root,"R","rum-dnn-gaussian.R"),api)
  sys.source(file.path(root,"R","rum-dnn-train.R"),api)
}
get_api <- function(name)get(name,envir=api,inherits=TRUE)
make_data <- get_api("rum_data")
make_quad <- get_api("rum_quadrature")
subset_data <- get_api("rum_subset")
make_network <- get_api("rum_dnn_gaussian")
eval_outputs <- get_api("rum_dnn_gaussian_evaluate")
loss <- get_api("rum_dnn_gaussian_loss")

checks <- list()
check <- function(name,actual,expected,tol=2e-10) {
  shape <- identical(dim(actual),dim(expected)) && length(actual)==length(expected)
  error <- if(shape && all(is.finite(actual)) && all(is.finite(expected)))
    max(c(0,abs(actual-expected))) else Inf
  checks[[length(checks)+1L]] <<- data.frame(check=name,max_abs_error=error,
    tolerance=tol,passed=error<=tol,stringsAsFactors=FALSE)
}
truth <- function(name,condition)check(name,as.numeric(isTRUE(condition)),1,0)
reject <- function(name,expr)truth(name,tryCatch({force(expr);FALSE},error=function(e)TRUE))

# Two policy features, one covariate, unequal task counts, six named respondents.
d <- list(matrix(c(.3,-.4),1,2),matrix(c(.7,-.2,.4,.8),2,2),
          matrix(c(-.8,.3,.6,.2,-.7,.5),3,2),matrix(c(.9,-.5,-.2,.4),2,2),
          matrix(c(-.6,.7),1,2),matrix(c(.1,.5,-.9,.8,-.3,.4),3,2))
y <- list(1,c(0,1),c(1,0,1),c(0,0),1,c(0,1,1))
data <- make_data(y,d,rep(list(diag(2)),6),ids=paste0("unit",seq_len(6)),
  metadata=list(orientation="Fixed synthetic optimizer unit fixture"))
Z <- matrix(c(-.8,-.3,.1,.4,.7,1),6,1)
rownames(Z) <- data$ids
quad <- make_quad(3L)

# This literal forward/objective is independent of the trainer's flattening,
# backpropagation, minibatch scaling, and regularization implementation.
literal_forward <- function(net,z) {
  a <- z
  for(j in seq_along(net$weights)) {
    a <- sweep(a%*%net$weights[[j]],2L,net$biases[[j]],"+")
    if(j<length(net$weights))a <- pmax(a,0)
  }
  a
}
softplus <- function(x)pmax(x,0)+log1p(exp(-abs(x)))
logsumexp <- function(x){m<-max(x);m+log(sum(exp(x-m)))}
literal_loglik <- function(data,mu,kappa,loading,omega,quadrature) {
  vapply(seq_len(data$n),function(i) {
    ix <- data$offsets[i]+seq_len(data$T[i])
    terms <- vapply(seq_along(quadrature$weight),function(q) {
      eta <- kappa+drop(data$d[ix,,drop=FALSE]%*%mu[i,])+ 
        drop(data$d[ix,,drop=FALSE]%*%loading)*quadrature$u[q]+sqrt(omega)*quadrature$v[q]
      log(quadrature$weight[q])+sum(data$y[ix]*eta-softplus(eta))
    },numeric(1))
    logsumexp(terms)
  },numeric(1))
}

literal_pack <- function(state)unname(c(unlist(state$network$weights),
  unlist(state$network$biases),state$kappa,state$loading,log(state$omega)))
literal_unpack <- function(x,template) {
  state <- template; pos <- 0L
  for(block in c("weights","biases"))for(j in seq_along(state$network[[block]])) {
    count <- length(state$network[[block]][[j]])
    state$network[[block]][[j]][] <- x[pos+seq_len(count)];pos<-pos+count
  }
  state$kappa <- x[pos+1L]
  state$loading <- x[pos+1L+seq_along(state$loading)]
  state$omega <- exp(tail(x,1L))
  state
}
literal_objective <- function(x,template,data,z,quadrature,network_l2) {
  s <- literal_unpack(x,template)
  parameters <- unname(c(unlist(s$network$weights),unlist(s$network$biases)))
  -mean(literal_loglik(data,literal_forward(s$network,z),s$kappa,s$loading,
                       s$omega,quadrature)) + network_l2*sum(parameters^2)/2
}
central_gradient <- function(fn,x,h=1e-6)vapply(seq_along(x),function(j) {
  up<-down<-x;up[j]<-up[j]+h;down[j]<-down[j]-h
  (fn(up)-fn(down))/(2*h)
},numeric(1))
literal_adam <- function(x,gradient,m,v,t,learning_rate,beta1,beta2,epsilon,
                         lower=rep(-Inf,length(x)),upper=rep(Inf,length(x))) {
  m <- beta1*m+(1-beta1)*gradient
  v <- beta2*v+(1-beta2)*gradient^2
  x <- pmax(lower,pmin(upper,x-learning_rate*(m/(1-beta1^t))/
    (sqrt(v/(1-beta2^t))+epsilon)))
  list(x=x,m=m,v=v)
}

# The starting outputs remain away from ReLU kinks, making the chain-rule
# finite-difference comparison nonvacuous and free of subgradient ambiguity.
unit_network <- make_network(1L,2L,hidden=2L,seed=8L)
unit_network$weights <- list(matrix(c(.18,-.12),1,2),
                             matrix(c(.25,-.17,.13,.21),2,2))
unit_network$biases <- list(c(.7,.9),c(.15,-.2))
unit_start <- list(network=unit_network,kappa=.12,loading=c(.35,-.22),omega=.3)
theta0 <- literal_pack(unit_start)

train <- get_api("rum_dnn_gaussian_train")
pack <- get_api(".rum_dnn_train_pack")
unpack <- get_api(".rum_dnn_train_unpack")
pack_gradient <- get_api(".rum_dnn_train_pack_gradient")
penalize <- get_api(".rum_dnn_train_penalize")
adam <- get_api(".rum_dnn_adam_step")
prepare <- get_api(".rum_dnn_train_prepare")
evaluate_prepared <- get_api(".rum_dnn_train_eval")
control <- list(max_epochs=2L,validation_every=1L,patience=10L,
  learning_rate=.02,network_l2=.03,max_seconds=20,start_seconds=3,
  refit_seconds=3,evaluation_reserve=2,train_order=3L,check_orders=c(3L,5L),
  block_size=2L,loglik_tolerance=1,score_tolerance=1,network_gradient_tolerance=1)
network_count <- length(c(unlist(unit_network$weights),unlist(unit_network$biases)))
check("parameter packing independent coordinate order",pack(unit_start),theta0)
check("parameter unpacking round trip",literal_pack(unpack(theta0,unit_start)),theta0)
ev <- loss(unit_network,data,Z,unit_start$kappa,unit_start$loading,unit_start$omega,quad)
raw_gradient <- pack_gradient(ev$loss_gradient,unit_start)
pp <- penalize(ev$loss,raw_gradient,theta0,network_count,control$network_l2)
fn <- function(x)literal_objective(x,unit_start,data,Z,quad,control$network_l2)
check("penalized objective independent GH loss",pp$objective,fn(theta0))
check("all penalized network/common gradients including logomega",pp$gradient,
      central_gradient(fn,theta0),2e-8)
check("network-only penalty derivative",pp$gradient[seq_len(network_count)]-raw_gradient[seq_len(network_count)],
      control$network_l2*theta0[seq_len(network_count)])
common_idx <- (network_count+1L):length(theta0)
check("common parameters receive no penalty",pp$gradient[common_idx],raw_gradient[common_idx])
check("variance reparameterization chain factor",tail(raw_gradient,1),ev$loss_gradient$omega*unit_start$omega)
truth("gradient fixture has nonzero network and common signals",
      max(abs(pp$gradient[seq_len(network_count)]))>.01 && max(abs(pp$gradient[common_idx]))>.01)
reference_update <- literal_adam(theta0,pp$gradient,theta0*0,theta0*0,1L,.02,.9,.999,1e-8)
actual_update <- adam(theta0,pp$gradient,theta0*0,theta0*0,1L,.02,.9,.999,1e-8,
                      rep(-Inf,length(theta0)),rep(Inf,length(theta0)))
for(nm in c("m","v"))check(paste("Adam first step",nm),actual_update[[nm]],reference_update[[nm]])
check("Adam descent sign and bias correction",actual_update$par,reference_update$x)
truth("small Adam step reduces penalized objective",fn(actual_update$par)<fn(theta0))
second_gradient <- central_gradient(fn,reference_update$x)
ref2 <- literal_adam(reference_update$x,second_gradient,reference_update$m,reference_update$v,
                     2L,.02,.9,.999,1e-8)
got2 <- adam(reference_update$x,second_gradient,reference_update$m,reference_update$v,
             2L,.02,.9,.999,1e-8,rep(-Inf,length(theta0)),rep(Inf,length(theta0)))
check("Adam second-step history and bias correction",got2$par,ref2$x)
bounded <- adam(c(0,0),c(1,-1),c(0,0),c(0,0),1L,.2,.9,.999,1e-8,c(-.05,-.05),c(.05,.05))
check("Adam projects declared bounds",bounded$par,c(-.05,.05))
truth("Adam projection is recorded",all(bounded$projected))

# Chunking is a memory choice, not stochastic minibatching or task weighting.
blocked <- evaluate_prepared(unit_start,prepare(data,Z,2L),3L,Inf,retain=TRUE)
whole <- evaluate_prepared(unit_start,prepare(data,Z,100L),3L,Inf,retain=TRUE)
for(nm in c("loss","gradient","loglik","output_score","common_score"))
  check(paste("full respondent gradient invariant to chunk size",nm),blocked[[nm]],whole[[nm]])
literal_ll <- literal_loglik(data,literal_forward(unit_network,Z),unit_start$kappa,
                              unit_start$loading,unit_start$omega,quad)
check("unequal T objective remains mean sequence loss",blocked$loss,-mean(literal_ll))
truth("unequal T fixture distinguishes within-person normalization",
      abs(blocked$loss+mean(literal_ll/data$T))>.1)

validation_ids <- data$ids[c(2L,5L)]
inner <- which(!data$ids%in%validation_ids)
start2 <- unit_start;start2$network$biases[[2]]<-c(-.1,.25);start2$omega<-.45
starts <- list(unit_start,start2)
fit <- train(data,Z,validation_ids,hidden=2L,seeds=c(8L,19L),control=control,starts=starts)
truth("tiny two-start procedure completed",isTRUE(fit$completed) && !is.null(fit$final))
truth("all declared starts preserved",length(fit$starts)==2L && all(vapply(fit$starts,
      function(x)x$completed_epochs==2L,logical(1))))
truth("respondent split is explicit and disjoint",identical(fit$training_ids,data$ids[inner]) &&
      identical(fit$validation_ids,validation_ids) && !length(intersect(fit$training_ids,fit$validation_ids)))
truth("trainer never claims convergence or inference",identical(fit$convergence_claim,FALSE) &&
      identical(fit$inference_status,"unavailable"))
for(s in seq_along(starts)) {
  stage <- fit$starts[[s]]
  check(paste("original state retained",s),pack(stage$initial_state),literal_pack(starts[[s]]))
  x <- literal_pack(starts[[s]]);m<-v<-x*0
  for(epoch in 1:2) {
    objective <- function(b)literal_objective(b,starts[[s]],subset_data(data,inner),Z[inner,,drop=FALSE],quad,.03)
    trace <- stage$trace[stage$trace$epoch==epoch,,drop=FALSE]
    check(paste("trace pre-update objective",s,epoch),trace$penalized_objective_before_update,objective(x),2e-8)
    g <- central_gradient(objective,x)
    update <- literal_adam(x,g,m,v,epoch,.02,.9,.999,1e-8)
    x<-update$x;m<-update$m;v<-update$v
    cp <- stage$checkpoints[[epoch+1L]]
    check(paste("independent Adam trajectory",s,epoch),pack(cp$state),x,3e-7)
    vv <- which(data$ids%in%validation_ids)
    st <- literal_unpack(x,starts[[s]])
    vl <- -mean(literal_loglik(subset_data(data,vv),literal_forward(st$network,Z[vv,,drop=FALSE]),
                               st$kappa,st$loading,st$omega,quad))
    check(paste("validation uses unpenalized heldout sequence loss",s,epoch),cp$validation_loss,vl,2e-8)
  }
  cp_losses <- vapply(stage$checkpoints,`[[`,numeric(1),"validation_loss")
  truth(paste("best checkpoint preserved",s),stage$best_checkpoint$epoch==
      stage$checkpoints[[which.min(cp_losses)]]$epoch)
}
val_losses <- vapply(fit$starts,function(x)x$best_checkpoint$validation_loss,numeric(1))
truth("winner uses best unpenalized validation score",fit$selection$start_index==which.min(val_losses))
winner <- fit$selection$start_index
check("full refit resets original chosen state",pack(fit$refit$initial_state),pack(starts[[winner]]))
check("full refit resets first Adam moment",fit$refit$initial_adam$m,theta0*0)
check("full refit resets second Adam moment",fit$refit$initial_adam$v,theta0*0)
truth("full refit runs exactly selected epoch count",fit$refit$completed_epochs==fit$selection$epoch)
if(fit$selection$epoch>0L) {
  x<-literal_pack(starts[[winner]]);m<-v<-x*0
  for(epoch in seq_len(fit$selection$epoch)) {
    f <- function(b)literal_objective(b,starts[[winner]],data,Z,quad,.03)
    update<-literal_adam(x,central_gradient(f,x),m,v,epoch,.02,.9,.999,1e-8)
    x<-update$x;m<-update$m;v<-update$v
    if(epoch==1L)check("refit first step uses full-bank gradient and reset moments",
                      pack(fit$refit$checkpoints[[1L]]$state),x,3e-7)
  }
  check("full refit matches independent complete trajectory",pack(fit$final),x,5e-7)
} else check("zero-epoch refit preserves original state",pack(fit$final),pack(starts[[winner]]))

# Validation outcomes may select a different checkpoint; they must not enter
# the fixed two-epoch inner training trajectories themselves.
changed <- data
for(i in which(data$ids%in%validation_ids)) {
  ii<-data$offsets[i]+seq_len(data$T[i]);changed$y[ii]<-1-changed$y[ii]
}
altered <- train(changed,Z,validation_ids,hidden=2L,seeds=c(8L,19L),control=control,starts=starts)
for(s in seq_along(starts)) {
  check(paste("validation outcomes cannot change inner training state",s),
        pack(altered$starts[[s]]$last_state),pack(fit$starts[[s]]$last_state))
  check(paste("validation outcomes cannot change inner training loss trace",s),
        altered$starts[[s]]$trace$training_loss_before_update,
        fit$starts[[s]]$trace$training_loss_before_update)
}
truth("validation isolation test changes heldout loss",
      abs(altered$starts[[1]]$checkpoints[[1]]$validation_loss-
          fit$starts[[1]]$checkpoints[[1]]$validation_loss)>1e-4)

# Equal initial states give the declared first-start tie, with no seed-based
# winner shopping. min_delta forces the zero-epoch rule to be exercised.
tie_control <- control;tie_control$min_delta<-1e6
tie <- train(data,Z,validation_ids,hidden=2L,seeds=c(8L,19L),control=tie_control,
             starts=list(unit_start,unit_start))
truth("exact across-start tie chooses first declared start",tie$selection$start_index==1L)
truth("zero epoch selected and explicitly labeled",isTRUE(tie$selection$zero_epoch) &&
      identical(tie$status,"completed_zero_epoch_selection"))
check("zero epoch returns original state without warm continuation",pack(tie$final),theta0)
truth("zero epoch has no refit Adam steps",tie$refit$completed_epochs==0L && !nrow(tie$refit$trace))

# Private seed handling is tested when the trainer constructs its own starts.
rng_existed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
if(rng_existed)rng_before<-get(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
seed_fit <- train(data,Z,validation_ids,hidden=2L,seeds=71L,control=tie_control)
truth("training preserves caller RNG",if(rng_existed)
      identical(rng_before,get(".Random.seed",envir=.GlobalEnv,inherits=FALSE)) else
      !exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
seed_again <- train(data,Z,validation_ids,hidden=2L,seeds=71L,control=tie_control)
check("seeded procedure state reproducibility",pack(seed_fit$final),pack(seed_again$final))
check("seeded training trace reproducibility",seed_fit$starts[[1]]$trace$training_loss_before_update,
      seed_again$starts[[1]]$trace$training_loss_before_update)

# Budget tests use zero stage allowance, not nondeterministic sleeping. They
# must preserve diagnostics without substituting an inner fit for final refit.
no_start_control<-control;no_start_control$start_seconds<-0
no_start<-train(data,Z,validation_ids,hidden=2L,seeds=8L,control=no_start_control,starts=list(unit_start))
truth("zero start allowance yields incomplete no-checkpoint result",
      !no_start$completed && is.null(no_start$final) && identical(no_start$status,"incomplete_no_valid_checkpoint"))
truth("zero start allowance is recorded as budget stop",no_start$starts[[1]]$budget_limited &&
      identical(no_start$starts[[1]]$stop_reason,"per_start_cap"))
no_refit_control<-control;no_refit_control$refit_seconds<-0
no_refit<-train(data,Z,validation_ids,hidden=2L,seeds=8L,control=no_refit_control,starts=list(unit_start))
truth("incomplete final refit never returns inner checkpoint",
      !no_refit$completed && is.null(no_refit$final) && identical(no_refit$status,"incomplete_refit"))
truth("final refit budget stop explicitly recorded",identical(no_refit$refit$stop_reason,"refit_time_cap"))
truth("partial evidence retained after refit timeout",length(no_refit$starts)==1L &&
      !is.null(no_refit$starts[[1]]$best_checkpoint) && nrow(no_refit$stage_cost)>=2L)

# Deterministic clock injection exercises a final total-cap crossing without
# depending on a machine's speed or sleeping. Only copies of closures are
# rebound: the installed namespace and package sources remain unchanged.
timed_api<-new.env(parent=api)
helper_names<-unique(c(ls(api,all.names=TRUE),ls(ns,all.names=TRUE)))
helper_names<-helper_names[grepl("^\\.rum_dnn_train|^\\.rum_dnn_adam_step$|^rum_dnn_gaussian_train$",helper_names)]
for(nm in helper_names) {
  value<-get_api(nm)
  if(is.function(value))environment(value)<-timed_api
  assign(nm,value,envir=timed_api)
}
clock_state<-new.env(parent=emptyenv());clock_state$expired<-FALSE
real_proc_time<-base::proc.time
timed_api$proc.time<-function() {
  value<-real_proc_time()
  if(clock_state$expired)value["elapsed"]<-value["elapsed"]+100
  value
}
timed_evaluate<-get(".rum_dnn_train_eval",timed_api)
timed_api$.rum_dnn_train_eval<-function(state,prepared,order,deadline,retain=FALSE,need_gradient=TRUE) {
  result<-timed_evaluate(state,prepared,order,deadline,retain,need_gradient)
  if(retain && order==5L)clock_state$expired<-TRUE
  result
}
total_cap<-timed_api$rum_dnn_gaussian_train(data,Z,validation_ids,hidden=2L,seeds=8L,
              control=control,starts=list(unit_start))
truth("total-cap crossing cannot retain completed flag or final estimate",
      !total_cap$completed && is.null(total_cap$final) &&
      identical(total_cap$status,"incomplete_total_time_cap"))
truth("total-cap result retains separately labeled refit evidence",
      !is.null(total_cap$refit$last_state) && total_cap$budget_overrun_seconds>0)

reject("unnamed Z rejected for trainer",train(data,unname(Z),validation_ids,hidden=2L,seeds=8L,
       control=control,starts=list(unit_start)))
reject("all respondents cannot be validation",train(data,Z,data$ids,hidden=2L,seeds=8L,
       control=control,starts=list(unit_start)))
reject("unknown validation respondent rejected",train(data,Z,"outsider",hidden=2L,seeds=8L,
       control=control,starts=list(unit_start)))
reject("duplicate start seeds rejected",train(data,Z,validation_ids,hidden=2L,seeds=c(8L,8L),
       control=control,starts=starts))
bad_state<-unit_start;bad_state$omega<-0
reject("zero omega rejected by declared interior logvariance optimizer",train(data,Z,validation_ids,
       hidden=2L,seeds=8L,control=control,starts=list(bad_state)))
bad_control<-control;bad_control$typo<-1
reject("unknown trainer control rejected",train(data,Z,validation_ids,hidden=2L,seeds=8L,
       control=bad_control,starts=list(unit_start)))
bad_state<-unit_start;bad_state$network$parameter_count<-network_count+1L
reject("stale network count cannot alter penalty or common bounds",train(data,Z,validation_ids,
       hidden=2L,seeds=8L,control=control,starts=list(bad_state)))

# Zero penalty must not turn a finite unpenalized gradient into NaN merely
# because squaring an otherwise finite parameter would overflow.
unpenalized<-penalize(1,rep(0,3),c(1e308,2,3),1L,0)
check("zero penalty bypasses irrelevant squared-norm overflow",unpenalized$objective,1)
check("zero penalty leaves gradient finite",unpenalized$gradient,rep(0,3))

result<-do.call(rbind,checks)
print(result,row.names=FALSE)
stopifnot(all(result$passed))
cat(nrow(result),"deterministic Gaussian trainer unit checks passed; no application or scientific smoke fits.\n")
