# Ballard-Rosa, Martin and Scheve (2017) application helpers: revenue-following contrasts, plan contests
# and endpoint AME rows (paper Appendix "BR targets").
#
# Ported from the paper's rebuild (replication/2026-11_v4/rebuild_results.R:105-161, br_ame_helpers.R:1-55)
# with the package's own recipe and revenue rule (.rum_br_recipe, .rum_br_revenue_probs in rum-design.R),
# which hold the same menus, feature maps, weights, noise and cutpoints as the external specification.R
# and F_br_revenue_rule.R the scripts sourced (checked equal in the paper-v4 gates).
#
# Coded features of one BR plan: the six tax rates (percent; bracket 1 = lowest income) through the
# linear and quadratic menu codes, and the displayed revenue category (taxrev: 1 = much more revenue ...
# 5 = much less) through four Helmert columns: 16 features, in the order of .RUM_BR_RECIPE$features.

.sc_br_rate_names <- paste0("tax_rate_", 1:6, "_pct")

# Coded features (rows x 16) of plans given as a rates matrix (rows x 6) and categories (recycled).
.sc_br_features <- function(rates, taxrev) {
  rates <- matrix(as.numeric(rates), ncol = 6L); rec <- .RUM_BR_RECIPE
  taxrev <- rep_len(taxrev, nrow(rates))
  ans <- lapply(1:6, function(j) {
    index <- match(rates[, j], .RUM_BR_MENUS[[j]])
    if (anyNA(index)) stop(sprintf("a tax rate of bracket %d is not on its menu (%s)", j,
                                   paste(.RUM_BR_MENUS[[j]], collapse = ", ")), call. = FALSE)
    rec$tax_maps[[j]][index, , drop = FALSE]
  })
  index <- match(as.numeric(taxrev), 1:5)
  if (anyNA(index)) stop("taxrev must be a revenue category 1..5", call. = FALSE)
  out <- do.call(cbind, c(ans, list(rec$revenue_map[index, , drop = FALSE])))
  dimnames(out) <- NULL
  out
}
# The six rates of a profile table (data frame with tax_rate_1_pct ... tax_rate_6_pct) as a matrix.
.sc_br_rates <- function(x) {
  if (is.matrix(x) && ncol(x) == 6L && is.null(colnames(x))) return(x)
  miss <- setdiff(.sc_br_rate_names, colnames(x))
  if (length(miss)) stop("BR profiles need the columns ", paste(.sc_br_rate_names, collapse = ", "), call. = FALSE)
  as.matrix(x[, .sc_br_rate_names])
}
# Check a BR design (list(A, B): the recorded profiles of every task, same rows).
.sc_br_design <- function(profiles) {
  if (!is.list(profiles) || is.null(profiles$A) || is.null(profiles$B) || NROW(profiles$A) != NROW(profiles$B))
    stop("profiles must be list(A = , B = ): the two recorded profiles of every task, with the six rate columns and taxrev",
         call. = FALSE)
  profiles
}

#' Revenue-following tax-rate contrast (Ballard-Rosa et al. 2017)
#'
#' Builds the coefficient vector of a tax-rate comparison in the Ballard-Rosa,
#' Martin and Scheve (2017) application. Raising one bracket's rate also changes
#' the revenue category the experiment displays. The contrast therefore has two
#' parts: the change in the rate codes, with the other five rates at 15 percent
#' and revenue "about the same", and the average change in the revenue codes
#' that the raise causes over the rate contexts respondents actually saw.
#'
#' @param bracket Bracket whose rate changes (1 = lowest income, 6 = highest).
#' @param high,low The two rates compared (percent; on the bracket's menu).
#'   Default: the top and bottom of the menu.
#' @param profiles The recorded design: `list(A = , B = )`, two data frames
#'   with the columns `tax_rate_1_pct` ... `tax_rate_6_pct` (the two plans of
#'   every task). Only the rates are used.
#' @param follow_revenue If `TRUE` (default), add the revenue part. If `FALSE`,
#'   the displayed revenue is held fixed.
#' @param per_point If `TRUE`, divide by `high - low` (utility per percentage
#'   point, the paper's bracket contrasts).
#' @return A numeric vector with one coefficient per coded feature (16), named
#'   by feature; pass it to [sc_mean()], [sc_share()] or [sc_fitted_law()].
#' @examples
#' A <- data.frame(tax_rate_1_pct = 5, tax_rate_2_pct = 15, tax_rate_3_pct = 25,
#'                 tax_rate_4_pct = 25, tax_rate_5_pct = 35, tax_rate_6_pct = 45)
#' B <- data.frame(tax_rate_1_pct = 0, tax_rate_2_pct = 5, tax_rate_3_pct = 15,
#'                 tax_rate_4_pct = 25, tax_rate_5_pct = 35, tax_rate_6_pct = 55)
#' round(sc_br_revenue_contrast(6, profiles = list(A = A, B = B)), 3)
#' @export
sc_br_revenue_contrast <- function(bracket, high = NULL, low = NULL, profiles, follow_revenue = TRUE,
                                   per_point = FALSE) {
  j <- as.integer(bracket); stopifnot(length(j) == 1L, j %in% 1:6)
  menu <- .RUM_BR_MENUS[[j]]
  if (is.null(high)) high <- max(menu)
  if (is.null(low)) low <- min(menu)
  a <- b <- rep(15, 6); a[j] <- high; b[j] <- low
  cc <- drop(.sc_br_features(a, 3) - .sc_br_features(b, 3))
  if (follow_revenue) {
    profiles <- .sc_br_design(profiles)
    context <- rbind(.sc_br_rates(profiles$A), .sc_br_rates(profiles$B))
    hi_rates <- lo_rates <- context
    hi_rates[, j] <- high; lo_rates[, j] <- low
    cc[13:16] <- cc[13:16] + colMeans(
      (.rum_br_revenue_probs(hi_rates) - .rum_br_revenue_probs(lo_rates)) %*% .RUM_BR_RECIPE$revenue_map)
  }
  if (per_point) cc <- cc / (high - low)
  stats::setNames(cc, .RUM_BR_RECIPE$features)
}

# Rows of a plan contest: every pair of revenue categories with probability > 0 under the revenue rule,
# weighted by the product of the two plans' category probabilities.
.sc_br_plan_rows <- function(plan, opponent) {
  rates <- rbind(as.numeric(plan), as.numeric(opponent))
  if (ncol(rates) != 6L) stop("a plan is six tax rates (percent, bracket 1 = lowest income)", call. = FALSE)
  probs <- .rum_br_revenue_probs(rates)
  mix <- expand.grid(a = 1:5, b = 1:5)
  coef <- probs[1L, mix$a] * probs[2L, mix$b]
  keep <- coef > 0
  mix <- mix[keep, , drop = FALSE]; coef <- coef[keep]
  p <- length(.RUM_BR_RECIPE$features)
  D <- t(vapply(seq_len(nrow(mix)), function(i)
    drop(.sc_br_features(rates[1L, ], mix$a[i]) - .sc_br_features(rates[2L, ], mix$b[i])), numeric(p)))
  colnames(D) <- .RUM_BR_RECIPE$features           # sc_contest() checks them against the data's features
  list(D = D, coef = unname(coef))
}

# Endpoint AME rows of one bracket (br_ame_reference): for both endpoints and both sides of every recorded
# pair, the focal plan's rate is set to the endpoint and its revenue category integrated with the rule
# (categories with probability <= probability_tol dropped); the opponent's recorded features stay fixed.
.sc_br_ame_rows <- function(bracket, profiles, probability_tol = 1e-14) {
  j <- as.integer(bracket); profiles <- .sc_br_design(profiles)
  menu <- .RUM_BR_MENUS[[j]]; lo <- min(menu); hi <- max(menu)
  side_tab <- list(profiles$A, profiles$B)
  if (is.null(side_tab[[1]]$taxrev) || is.null(side_tab[[2]]$taxrev))
    stop("profiles need the recorded revenue category (column taxrev)", call. = FALSE)
  opponent <- lapply(rev(side_tab), function(x) .sc_br_features(.sc_br_rates(x), x$taxrev))
  blocks <- list(); omitted <- 0
  for (value in c(hi, lo)) for (side in 1:2) {
    x <- .sc_br_rates(side_tab[[side]]); x[, j] <- value
    probs <- .rum_br_revenue_probs(x)
    sgn <- if (value == hi) 1 else -1
    for (category in 1:5) {
      keep <- probs[, category] > probability_tol
      omitted <- omitted + sum(probs[!keep, category]) / (2 * nrow(x))
      if (!any(keep)) next
      blocks[[length(blocks) + 1L]] <- list(
        D = .sc_br_features(x[keep, , drop = FALSE], category) - opponent[[side]][keep, , drop = FALSE],
        coef = sgn * probs[keep, category] / (2 * nrow(x)),
        side = rep(if (side == 1L) 1 else -1, sum(keep)))
    }
  }
  D <- do.call(rbind, lapply(blocks, `[[`, "D")); colnames(D) <- .RUM_BR_RECIPE$features
  list(D = D,
       coef = unlist(lapply(blocks, `[[`, "coef"), use.names = FALSE),
       side = unlist(lapply(blocks, `[[`, "side"), use.names = FALSE),
       low = lo, high = hi, omitted = omitted)
}
