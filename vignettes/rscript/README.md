# vignettes/rscript/

This directory holds plain-R mirror scripts of the R code blocks in
the Quarto book chapters under `tutorial/`. Each `.R` file must
correspond to exactly one `.qmd` chapter and contain the same code
blocks in the same order, so users and contributors can run the
tutorial code without needing Quarto.

**Build exclusion**: this directory is listed in `.Rbuildignore` and
is NOT part of the CRAN package source. It is committed to the
repository for user convenience and maintained by hand (and by
`scriber` during documentation milestones).

During milestone M1 the directory is empty.
