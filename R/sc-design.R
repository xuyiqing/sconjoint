# sc_design(): a general conjoint design law (the most common one), so that sc_states(), sc_ame_switch() and
# sc_importance() work on experiments other than the paper's two. Each attribute's level is drawn
# independently (uniform by default), independently across attributes, the two profiles of a task and
# the tasks; every respondent answers the same number of tasks; a profile is coded by one numeric row per
# level (default: treatment dummies against the first level) and a task by the first profile's coded row
# minus the second's.
#
# The object is plain data (class "sc_design": levels, probabilities, tasks, coding matrices, feature
# names), so it saves with sc_save() and prints. .sc_design_spec() turns it into the internal design spec
# (class "rum_design_spec", R/rum-design.R) whose draw() and code() the conditional information and the
# switch AMEs use; .rum_spec_arg() calls it, so a design object goes wherever a study string went.

#' A conjoint design with independently drawn attributes
#'
#' Describes how an experiment drew its profiles, for [sc_data()],
#' [sc_states()], [sc_ame_switch()] and [sc_importance()]. This is the most
#' common conjoint design: each task shows two profiles; each attribute's level
#' is drawn on its own (uniform over the levels unless `probs` says otherwise),
#' independently across attributes, the two profiles and the tasks; and every
#' respondent answers `tasks` tasks.
#'
#' The design also fixes how a profile becomes numbers. By default each
#' attribute with `L` levels gets `L - 1` treatment dummies against its first
#' level, named `<attribute>_<level>` (characters other than letters, digits,
#' `.` and `_` in the level become `_`). A task is coded as the first
#' profile's row minus the second's, and `y = 1` means the first profile was
#' chosen. [sc_code()] codes recorded profiles this way, and [sc_data()]
#' checks that the data's feature names are the design's, in the design's
#' order.
#'
#' Designs with restrictions (levels that cannot appear together), blocked or
#' balanced draws, or codings that depend on the respondent need their own
#' generator; [sc_states()] then supports only the built-in designs `"gs"`
#' and `"br"`.
#'
#' @param attributes Named list, one element per attribute: the levels
#'   (character), the first being the reference level.
#' @param tasks Number of tasks per respondent.
#' @param probs Optional named list of level probabilities for some
#'   attributes, one nonnegative number per level (normalized to sum to one).
#'   Attributes not named are uniform.
#' @param coding Optional named list of codings for some attributes: a numeric
#'   matrix with one row per level (in the order of `attributes`) and named
#'   columns (the feature names). For example `matrix(0:2, dimnames =
#'   list(NULL, "experience"))` codes a three-level attribute as one linear
#'   score. Attributes not named get treatment dummies.
#' @return An object of class `sc_design`: a list with `attributes`, `probs`,
#'   `tasks`, `coding` (one matrix per attribute, rows named by level) and
#'   `features` (the coded feature names in order).
#' @seealso [sc_code()], [simconjoint].
#' @examples
#' des <- sc_design(list(party = c("Democrat", "Republican"),
#'                       experience = c("none", "some", "long")),
#'                  tasks = 8, probs = list(experience = c(0.5, 0.25, 0.25)))
#' des
#' des$features
#' @export
sc_design <- function(attributes, tasks, probs = NULL, coding = NULL) {
  if (!is.list(attributes) || !length(attributes) || is.null(names(attributes)) ||
      any(!nzchar(names(attributes))) || anyDuplicated(names(attributes)))
    stop("attributes must be a named list (distinct names), one element per attribute", call. = FALSE)
  an <- names(attributes)
  attributes <- lapply(an, function(a) {
    lv <- as.character(attributes[[a]])
    if (length(lv) < 2L || anyNA(lv) || any(!nzchar(lv)) || anyDuplicated(lv))
      stop(sprintf("attribute '%s' needs at least two distinct, nonempty levels", a), call. = FALSE)
    lv
  })
  names(attributes) <- an
  tasks <- as.integer(tasks)
  if (length(tasks) != 1L || is.na(tasks) || tasks < 1L) stop("tasks must be a positive integer", call. = FALSE)
  .named_sub <- function(x, what) {
    if (is.null(x)) return(list())
    if (!is.list(x) || is.null(names(x)) || any(!names(x) %in% an))
      stop(what, " must be a named list whose names are attributes", call. = FALSE)
    x
  }
  probs <- .named_sub(probs, "probs"); coding <- .named_sub(coding, "coding")
  P <- lapply(an, function(a) {
    L <- length(attributes[[a]])
    if (is.null(probs[[a]])) return(rep(1 / L, L))
    pr <- as.numeric(probs[[a]])
    if (length(pr) != L || anyNA(pr) || any(!is.finite(pr)) || any(pr < 0) || !(sum(pr) > 0))
      stop(sprintf("probs of '%s' must be %d nonnegative numbers (one per level) with a positive sum", a, L), call. = FALSE)
    pr / sum(pr)
  })
  names(P) <- an
  C <- lapply(an, function(a) {
    lv <- attributes[[a]]
    if (is.null(coding[[a]])) {
      m <- diag(length(lv))[, -1L, drop = FALSE]
      colnames(m) <- paste0(a, "_", gsub("[^A-Za-z0-9._]", "_", lv[-1L]))
    } else {
      m <- as.matrix(coding[[a]])
      if (!is.numeric(m) || nrow(m) != length(lv) || !ncol(m) || any(!is.finite(m)) || is.null(colnames(m)))
        stop(sprintf("the coding of '%s' must be a finite numeric matrix with %d rows (one per level) and named columns",
                     a, length(lv)), call. = FALSE)
    }
    rownames(m) <- lv
    m
  })
  names(C) <- an
  features <- unlist(lapply(C, colnames), use.names = FALSE)
  if (anyDuplicated(features) || any(!nzchar(features)))
    stop("the coded feature names must be distinct and nonempty: ", paste(unique(features[duplicated(features)]), collapse = ", "),
         call. = FALSE)
  structure(list(attributes = attributes, probs = P, tasks = tasks, coding = C, features = features),
            class = "sc_design")
}

#' @export
print.sc_design <- function(x, ...) {
  cat(sprintf("sconjoint design: %d attributes, two profiles per task, %d tasks per respondent, %d coded features\n",
              length(x$attributes), x$tasks, length(x$features)))
  for (a in names(x$attributes)) {
    pr <- x$probs[[a]]; uni <- isTRUE(all.equal(pr, rep(1 / length(pr), length(pr))))
    cat(sprintf("  %s: %s%s\n", a, paste(x$attributes[[a]], collapse = ", "),
                if (uni) "" else paste0(" (probabilities ", paste(format(round(pr, 3)), collapse = ", "), ")")))
  }
  cat("  features:", paste(utils::head(x$features, 8L), collapse = ", "), if (length(x$features) > 8L) ", ..." else "", "\n")
  invisible(x)
}

#' Code recorded profiles with a design
#'
#' Turns the two profiles of each task into the coded difference that
#' [sc_data()] takes as `X`: the first profile's coded row minus the
#' second's, with the design's feature names.
#'
#' @param design A design from [sc_design()].
#' @param A,B Data frames with one row per task and one column per attribute
#'   of the design (named as in the design; other columns are ignored): the
#'   first and the second profile. Levels are matched as character strings.
#' @return A numeric matrix, one row per task and one column per coded feature.
#' @examples
#' des <- sc_design(list(party = c("D", "R"), experience = c("none", "some", "long")), tasks = 2)
#' A <- data.frame(party = c("R", "D"), experience = c("long", "none"))
#' B <- data.frame(party = c("D", "D"), experience = c("none", "some"))
#' sc_code(des, A, B)
#' @export
sc_code <- function(design, A, B) {
  if (!inherits(design, "sc_design")) stop("design must come from sc_design()", call. = FALSE)
  A <- as.data.frame(A); B <- as.data.frame(B)
  if (nrow(A) != nrow(B)) stop("A and B must have one row per task (the same number of rows)", call. = FALSE)
  spec <- .sc_design_spec(design)
  out <- .rum_design_code_profiles(spec, A, B)
  out
}

# The internal design spec of an sc_design (same API as .rum_design_spec(); class rum_design_spec).
.sc_design_spec <- function(design) {
  at <- design$attributes; pr <- design$probs; cd <- design$coding; an <- names(at); S <- design$tasks
  uni <- vapply(an, function(a) isTRUE(all.equal(pr[[a]], rep(1 / length(at[[a]]), length(at[[a]])))), TRUE)
  draw <- function(M, T = S) {
    n2 <- M * S * 2L
    raw <- lapply(an, function(a) {
      L <- length(at[[a]])
      array(if (uni[[a]]) sample.int(L, n2, replace = TRUE) else sample.int(L, n2, replace = TRUE, prob = pr[[a]]),
            c(M, S, 2L))
    })
    names(raw) <- an
    .rum_raw_head(raw, T)
  }
  x_side <- function(raw, side)
    do.call(cbind, lapply(an, function(a) cd[[a]][as.vector(raw[[a]][, , side]), , drop = FALSE]))
  code <- function(raw, key = NULL) {
    M <- dim(raw[[1L]])[1L]; T <- dim(raw[[1L]])[2L]
    d <- x_side(raw, 1L) - x_side(raw, 2L)
    array(d, c(M, T, length(design$features)), dimnames = list(NULL, NULL, design$features))
  }
  to_raw <- function(profA, profB) {
    n <- nrow(profA)
    miss <- setdiff(an, intersect(names(profA), names(profB)))
    if (length(miss)) stop("the profiles need a column for every attribute; missing: ", paste(miss, collapse = ", "), call. = FALSE)
    out <- lapply(an, function(a) {
      f <- function(p) { ix <- match(as.character(p[[a]]), at[[a]])
        if (anyNA(ix)) stop(sprintf("unknown level(s) of '%s': %s", a,
                                    paste(utils::head(unique(as.character(p[[a]])[is.na(ix)]), 5L), collapse = ", ")),
                            call. = FALSE)
        ix }
      .rum_side_array(f(profA), f(profB), n, 1L)
    })
    names(out) <- an
    out
  }
  to_profiles <- function(raw) {
    M <- dim(raw[[1L]])[1L]; T <- dim(raw[[1L]])[2L]
    one <- function(side) {
      out <- data.frame(draw = rep(seq_len(M), each = T), task = rep(seq_len(T), M))
      for (a in an) out[[a]] <- at[[a]][as.vector(t(raw[[a]][, , side]))]
      out
    }
    list(profA = one(1L), profB = one(2L))
  }
  structure(list(
    study = paste0("sc_design:", digest::digest(unclass(design)[c("attributes", "probs", "tasks", "coding")], algo = "xxhash64")),
    T = S, p = length(design$features), features = design$features, uses_resp = FALSE,
    draw = draw, code = code, key = function(resp) "all", to_raw = to_raw, to_profiles = to_profiles,
    law = list(source = "sc_design()", tasks = sprintf("%d tasks; side 1 = the first profile", S),
               levels = at, probs = pr,
               rule = "each attribute drawn independently with the given level probabilities, independently across attributes, profiles and tasks",
               respondent = "none")),
    class = "rum_design_spec")
}

# Feature names a design codes ("gs", "br" or an sc_design).
.sc_design_features <- function(design) {
  if (inherits(design, "sc_design")) design$features else .rum_design_spec(design)$features
}
# A short label of a design for messages and printing.
.sc_design_label <- function(design) {
  if (inherits(design, "sc_design")) sprintf("sc_design (%d attributes, %d tasks)", length(design$attributes), design$tasks)
  else design
}
# The design argument of sc_data(): NULL, "gs", "br" or an sc_design.
.sc_design_arg <- function(design) {
  if (is.null(design) || inherits(design, "sc_design")) return(design)
  if (is.character(design) && length(design) == 1L) return(match.arg(design, c("gs", "br")))
  stop("design must be \"gs\", \"br\" or a design from sc_design()", call. = FALSE)
}
