## Declare `self` as a non-global so R CMD check does not flag it.
## `self` is injected by torch::nn_module into the `initialize`,
## `forward`, and method closures at instantiation time.
utils::globalVariables("self")
