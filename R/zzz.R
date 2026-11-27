## Package-local mutable state, for once-per-session notes.
.sc_state <- new.env(parent = emptyenv())
.sc_state$warned <- character()
