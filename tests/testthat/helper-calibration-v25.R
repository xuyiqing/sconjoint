.v25_test_dispatch <- function(job_fun = NULL) {
  force(job_fun)
  function(jobs, production_job_fun) {
    selected <- if (is.null(job_fun)) production_job_fun else job_fun
    sconjoint:::.scmix_v25_run_worker_schedule(
      jobs, 12L, selected, parallel = FALSE)
  }
}

.v25_local_serial_dispatch <- function(job_fun = NULL,
                                       .env = parent.frame()) {
  testthat::local_mocked_bindings(
    .scmix_v25_fixed_psock_dispatch = .v25_test_dispatch(job_fun),
    .package = "sconjoint", .env = .env)
}

.v25_test_run_dir <- function(root, plan) {
  file.path(normalizePath(root, mustWork = TRUE),
            plan$canonical$run_relative)
}
