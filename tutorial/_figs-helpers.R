# Helpers for the application chapters. The figures themselves come from the package's plot
# functions (sc_plot_estimates(), sc_plot_laws(), ...; see the chapter on plots and tables).

# Colours: the house blue, and the paper's green and red for second and third groups.
blue <- "#004a8c"; green <- "#27805A"; red <- "#BD4C39"
cols <- c(blue, green, red)

# Estimate a list of targets one at a time in parallel processes and stack the rows. Each element
# of `targets` is one target or one target set (an importance set is updated jointly). Forked
# processes are not available on Windows, where this runs one target after another.
estimate_each <- function(object, targets, cores = min(8L, parallel::detectCores())) {
  if (.Platform$OS.type == "windows") cores <- 1L
  out <- parallel::mclapply(targets, function(t) sc_estimate(object, t),
                            mc.cores = cores, mc.preschedule = FALSE)
  bad <- vapply(out, inherits, logical(1), "try-error")
  if (any(bad)) stop(out[[which(bad)[1]]], call. = FALSE)
  do.call(rbind, out)
}

# Rows of an estimate table in a given order of names.
pick <- function(est, names) {
  i <- match(names, est$name)
  stopifnot(!anyNA(i))
  est[i, , drop = FALSE]
}
