## Pre-render check for the sconjoint book. It runs in its own R process before
## any chapter, so it only checks; settings that chapters need are made inside
## each chapter.
if (!requireNamespace("sconjoint", quietly = TRUE) ||
    utils::packageVersion("sconjoint") < "0.4.1" ||
    !"sc_estimate" %in% getNamespaceExports("sconjoint")) {
  stop("The book needs sconjoint 0.4.1 or later (with sc_estimate()). ",
       "Install it from the package repository first.", call. = FALSE)
}
## The installation, application, inference and respondents chapters read the
## saved fits from getOption("sconjoint.fits", "fits").
## The option can only be set inside the chapter processes (for example in
## .Rprofile), so this check looks at the default folder and only warns.
for (f in c("gs_q3.rds", "br_q4.rds")) {
  if (!file.exists(file.path("fits", f))) {
    message("Note: fits/", f, " not found. The installation, application, inference ",
            "and respondents chapters need the saved fits; set options(sconjoint.fits = \"<folder>\") in .Rprofile if they live elsewhere.")
  }
}
invisible(NULL)
