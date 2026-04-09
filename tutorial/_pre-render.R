## Pre-render hook for the sconjoint Quarto book.
## Enforces the 8-core worker cap before any chapter chunk runs.
Sys.setenv(OMP_NUM_THREADS = "8")
Sys.setenv(OPENBLAS_NUM_THREADS = "8")
Sys.setenv(MKL_NUM_THREADS = "8")
options(Ncpus = 8L, mc.cores = 8L)
if (requireNamespace("future", quietly = TRUE)) {
  future::plan(future::multisession, workers = 8L)
}
invisible(NULL)
