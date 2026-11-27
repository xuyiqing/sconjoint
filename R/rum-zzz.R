# Package load hook. .rum_omega_small (R/rum-inference-targets.R) takes its value from the C++
# kernel's single definition, RUM_OMEGA_SMALL (src/rum_core.cpp), so the pure-R scores, the
# conditional information and the kernel use one threshold (Codex review of 829393d, finding 1).
.onLoad <- function(libname, pkgname) {
  assign(".rum_omega_small", rum_omega_small_cpp(), envir = parent.env(environment()))
}
