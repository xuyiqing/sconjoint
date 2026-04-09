## Parallel-safe RNG utilities for sconjoint (M2).
##
## These helpers lay the foundation for the M3 cross-fitting driver.
## The M3 guarantee is that `scfit(..., seed = 42)` produces bit-
## identical coefficients on 1 core and on N cores.  That requires
## two things:
##
##   1. A deterministic per-worker RNG stream, derived from a master
##      seed via `parallel::nextRNGStream()` (L'Ecuyer-CMRG).
##   2. A deterministic per-worker torch seed that does NOT depend
##      on scheduling order.
##
## `.sc_make_seed_streams()` provides (1); `.sc_with_torch_seed()`
## provides the save-and-restore primitive needed inside workers.

#' Build a deterministic list of L'Ecuyer-CMRG RNG streams
#'
#' Given a master `seed` and the desired number of `n_streams`,
#' return a list of integer vectors, each a valid `.Random.seed`
#' state for the "L'Ecuyer-CMRG" RNG kind.  The `i`-th stream is
#' the result of `i` applications of `parallel::nextRNGStream()` to
#' the master stream.
#'
#' The function is pure: it saves and restores `RNGkind()` and
#' `.Random.seed` so that the caller's RNG state is unaffected.
#'
#' @param seed Integer master seed.
#' @param n_streams Integer, number of streams to generate.
#' @return A list of length `n_streams`, each element a valid
#'   `.Random.seed` for kind "L'Ecuyer-CMRG".
#' @keywords internal
#' @noRd
.sc_make_seed_streams <- function(seed, n_streams) {
  if (!is.numeric(seed) || length(seed) != 1L) {
    stop(".sc_make_seed_streams(): `seed` must be a single number.")
  }
  if (!is.numeric(n_streams) || length(n_streams) != 1L || n_streams < 1) {
    stop(".sc_make_seed_streams(): `n_streams` must be a positive integer.")
  }
  n_streams <- as.integer(n_streams)

  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    suppressWarnings(RNGkind(kind = old_kind[1L], normal.kind = old_kind[2L],
                             sample.kind = old_kind[3L]))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)

  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  current <- get(".Random.seed", envir = globalenv(), inherits = FALSE)

  streams <- vector("list", n_streams)
  for (i in seq_len(n_streams)) {
    current <- parallel::nextRNGStream(current)
    streams[[i]] <- current
  }
  streams
}

#' Evaluate an expression under a temporary torch seed
#'
#' Saves the current torch RNG state, sets it from `seed`, evaluates
#' `expr`, then restores the previous torch state (if it could be
#' captured) and any R-level seed state changes.  If torch is not
#' available the expression is evaluated under a plain `set.seed()`.
#'
#' @param seed Integer, the seed to set.
#' @param expr An expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.sc_with_torch_seed <- function(seed, expr) {
  expr <- substitute(expr)
  env  <- parent.frame()

  had_r_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_r_seed) {
    old_r_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }

  old_torch_state <- NULL
  torch_ok <- requireNamespace("torch", quietly = TRUE)
  if (torch_ok) {
    old_torch_state <- tryCatch(torch::torch_get_rng_state(),
                                error = function(e) NULL)
  }

  on.exit({
    if (had_r_seed) {
      assign(".Random.seed", old_r_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    if (torch_ok && !is.null(old_torch_state)) {
      tryCatch(torch::torch_set_rng_state(old_torch_state),
               error = function(e) NULL)
    }
  }, add = TRUE)

  set.seed(seed)
  if (torch_ok) {
    torch::torch_manual_seed(seed)
  }
  eval(expr, envir = env)
}
