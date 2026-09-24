# The nine test scripts of conjointRUM 0.1.3 (Avidit Acharya), ported with the RUM code.
# Each script checks its own results with stop()/stopifnot() and prints a final
# "... passed" line. The scripts compare the native code with embedded reference
# implementations (compiled here with Rcpp::sourceCpp), so they need a compiler.
rum_scripts <- sort(list.files(test_path("rum"), pattern = "[.]R$", full.names = TRUE))

test_that("all nine conjointRUM 0.1.3 test scripts are present", {
  expect_length(rum_scripts, 9L)
})

for (rum_script in rum_scripts) {
  test_that(paste("conjointRUM script", basename(rum_script)), {
    skip_on_cran()
    env <- new.env(parent = asNamespace("sconjoint"))
    out <- utils::capture.output(sys.source(rum_script, envir = env))
    expect_match(utils::tail(out, 1L), "passed")
  })
}
