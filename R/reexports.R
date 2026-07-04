#' Re-exported functions from other packages
#'
#' The [ggplot2::autoplot()] generic is re-exported so that `autoplot()`
#' methods for sconjoint objects (see [autoplot.sc_fit()]) can be called
#' without attaching ggplot2.
#'
#' @return The re-exported [ggplot2::autoplot()] generic. When called on
#'   an `sc_fit` object it dispatches to [autoplot.sc_fit()] and returns
#'   a ggplot object.
#' @name sconjoint-reexports
#' @keywords internal
NULL

#' @importFrom ggplot2 autoplot
#' @name autoplot
#' @rdname sconjoint-reexports
#' @export
ggplot2::autoplot
