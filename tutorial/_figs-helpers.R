# Helpers for the application chapters. The figures themselves come from the package's plot
# functions (sc_plot_estimates(), sc_plot_laws(), ...; see the chapter on plots and tables).

# Colours: the house blue, and the paper's green and red for second and third groups.
blue <- "#004a8c"; green <- "#27805A"; red <- "#BD4C39"
cols <- c(blue, green, red)

# Processes for sc_estimate(cores = ): independent targets (each share's bounded update, each
# importance set) run in parallel, with the same results as one process. On Windows sc_estimate()
# runs them one after another.
cores <- min(8L, parallel::detectCores())

# Rows of an estimate table in a given order of names.
pick <- function(est, names) {
  i <- match(names, est$name)
  stopifnot(!anyNA(i))
  est[i, , drop = FALSE]
}
