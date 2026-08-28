#!/usr/bin/env Rscript

## Fail-closed validator for the isolated Saha--Weeks sensitivity artifacts.

options(stringsAsFactors = FALSE)

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_profile <- function(args) {
  profile <- "smoke"
  for (arg in args) {
    if (!grepl("^--profile=", arg)) stop("Only --profile=<name> is accepted.",
                                         call. = FALSE)
    profile <- sub("^--profile=", "", arg)
  }
  profile
}

.manual_medians <- function(Z, rid, training_respondents) {
  take <- as.character(rid) %in% as.character(training_respondents)
  index <- which(take)
  first <- index[!duplicated(as.character(rid[index]))]
  apply(Z[first, , drop = FALSE], 2L, stats::median, na.rm = TRUE)
}

.main <- function() {
  profile <- .parse_profile(commandArgs(trailingOnly = TRUE))
  root <- normalizePath(file.path(dirname(.script_file()), "../../.."),
                        mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  out <- file.path(app, "results", "mixed_logit", profile,
                   "sensitivity_analysis")
  paths <- c(
    prepared = file.path(app, "results", "prep_analysis_data.rds"),
    manifest = file.path(out, "sensitivity_manifest.rds"),
    z19 = file.path(out, "fit_z19_sensitivity.rds"),
    interaction = file.path(out, "fit_male_run_interaction.rds"),
    process = file.path(out, "task_process_diagnostics.rds"),
    completion = file.path(out, "completion_sample_sensitivity.rds"),
    structural = file.path(out, "structural_sensitivity.rds")
  )
  if (any(!file.exists(paths))) {
    stop("Missing sensitivity artifact(s): ",
         paste(names(paths)[!file.exists(paths)], collapse = ", "),
         call. = FALSE)
  }
  p <- readRDS(paths[["prepared"]]); m <- readRDS(paths[["manifest"]])
  z <- readRDS(paths[["z19"]]); i <- readRDS(paths[["interaction"]])
  r <- readRDS(paths[["process"]]); c0 <- readRDS(paths[["completion"]])
  s <- readRDS(paths[["structural"]])

  checks <- list()
  add <- function(check, pass, detail) {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = check, pass = isTRUE(pass), detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  }
  input_paths <- unlist(m$input_paths)
  current_md5 <- unname(tools::md5sum(input_paths))
  add("primary input hashes unchanged",
      identical(current_md5, as.character(m$input_md5)),
      "current hashes equal the sensitivity manifest input hashes")
  add("no primary artifact write declared", identical(m$primary_artifacts_modified,
                                                       FALSE),
      "manifest records isolated outputs only")
  manifested_paths <- file.path(out, names(m$artifacts))
  manifested_exists <- file.exists(manifested_paths)
  manifested_hash <- rep(NA_character_, length(manifested_paths))
  manifested_hash[manifested_exists] <-
    unname(tools::md5sum(manifested_paths[manifested_exists]))
  manifested_match <- manifested_exists &
    manifested_hash == as.character(m$artifacts)
  add("sensitivity component manifest hashes match",
      length(manifested_paths) > 0L && all(manifested_match),
      paste(sum(manifested_match), "of", length(manifested_match),
            "manifested artifacts verified"))

  fold_ok <- vapply(seq_along(z$fit$folds), function(k) {
    f <- z$fit$folds[[k]]
    heldout <- unique(as.character(p$respondent_id[
      z$fit$assembled$fold_id == k]))
    no_overlap <- !any(f$training_respondents %in% heldout)
    expected <- .manual_medians(p$Z_sensitivity19_raw, p$respondent_id,
                                f$training_respondents)
    observed <- f$preprocessing$imputation$median
    no_overlap && isTRUE(all.equal(as.numeric(expected), as.numeric(observed),
                                   tolerance = 0))
  }, logical(1L))
  add("19-Z imputation is outer-training-only", all(fold_ok),
      paste(sum(fold_ok), "of", length(fold_ok), "folds verified exactly"))
  add("19-Z post-conjoint fields remain sensitivity only",
      !isTRUE(z$fit$maintained_model) &&
        identical(z$formal_inference_available, FALSE),
      "artifact withholds formal inference and primary-model status")

  audit <- i$design_audit
  theoretical <- audit[audit$support == "theoretical full-profile", ]
  add("Male-by-run augmented affine rank",
      nrow(theoretical) == 1L &&
        theoretical$affine_rank == theoretical$affine_required,
      if (nrow(theoretical)) paste(theoretical$affine_rank, "of",
                                  theoretical$affine_required) else "missing")
  add("Male-by-run augmented covariance rank",
      nrow(theoretical) == 1L &&
        theoretical$covariance_vech_rank ==
        theoretical$covariance_vech_required,
      if (nrow(theoretical)) paste(theoretical$covariance_vech_rank, "of",
                                  theoretical$covariance_vech_required) else
        "missing")
  add("Male-by-run fielded protocol remains unverified",
      nrow(theoretical) == 1L && !isTRUE(theoretical$protocol_verified),
      "algebra does not replace missing protocol probabilities")

  pred <- r$heldout_predictions
  fold_by_id <- split(pred$task$fold, pred$task$respondent_id)
  add("task diagnostics preserve respondent folds",
      all(vapply(fold_by_id, function(x) length(unique(x)) == 1L,
                 logical(1L))),
      paste(length(fold_by_id), "respondents each occur in one fold"))
  add("task-order diagnostics cover all primary tasks",
      nrow(pred$task) == nrow(p$deltaX) &&
        setequal(unique(pred$task$task_order), c(1, 2, 3)),
      paste(nrow(pred$task), "task predictions"))
  add("serial alternative remains visibly not run",
      identical(r$serial_shock_alternative_refit, "not_run"),
      "diagnostic residuals are not relabeled as a serial-shock refit")

  add("completion expanded sample reproduced",
      identical(as.integer(c0$sample$respondents), c(1191L, 1249L)) &&
        identical(as.integer(c0$sample$tasks), c(3573L, 3740L)),
      paste(paste(c0$sample$respondents, c0$sample$tasks, sep = "/"),
            collapse = ", "))
  add("completion source remains read-only",
      identical(c0$source_policy, "read-only") &&
        identical(unname(tools::md5sum(c0$source_path)), c0$source_md5),
      "raw source hash is unchanged")
  add("completion sensitivity withholds formal inference",
      identical(c0$formal_inference_available, FALSE),
      "descriptive sample perturbation only")

  add("structural battery fails closed",
      !isTRUE(s$complete) && !isTRUE(s$substantive_pass),
      "unimplemented alternatives and absent approved margins prevent passage")
  add("maintained assumptions not marked verified",
      identical(m$maintained_assumptions_verified, FALSE) &&
        identical(m$formal_inference_available, FALSE),
      "manifest explicitly withholds verification and formal inference")

  table <- do.call(rbind, checks)
  table_path <- file.path(out, "tables", "sensitivity_validation.csv")
  utils::write.csv(table, table_path, row.names = FALSE, na = "")
  validation <- list(
    schema_version = "sw2022-sensitivity-validation-v1",
    profile = profile, passed = all(table$pass), checks = table,
    paths = paths,
    manifest_md5 = unname(tools::md5sum(paths[["manifest"]])),
    validated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  saveRDS(validation, file.path(out, "sensitivity_validation.rds"),
          version = 3, compress = "xz")
  if (!validation$passed) {
    stop("Sensitivity artifact validation failed: ",
         paste(table$check[!table$pass], collapse = "; "), call. = FALSE)
  }
  cat(sprintf("Saha--Weeks %s sensitivity validation passed (%d checks).\n",
              profile, nrow(table)))
  invisible(validation)
}

if (sys.nframe() == 0L) .main()
