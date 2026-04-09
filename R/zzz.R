.onAttach <- function(libname, pkgname) {
  # sconjoint will depend on 'torch' once the DNN backend lands in M2.
  # During M1 (skeleton), torch is in Suggests and is not required.
  # Emit a friendly note if torch is not installed so users know what
  # will be needed going forward.
  if (!requireNamespace("torch", quietly = TRUE)) {
    packageStartupMessage(
      "sconjoint: the 'torch' package is not installed. ",
      "It will become a hard dependency when the deep-learning ",
      "backend lands in a future release. Install with ",
      "install.packages('torch') and then torch::install_torch()."
    )
  }
}
