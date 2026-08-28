#!/usr/bin/env Rscript

## Saha--Weeks (2022): freeze a future Section 5.1 results bundle.
##
## This is an export step, not manuscript drafting.  It collects the prepared
## sample definition, structural plug-ins, inference status, and every
## assessment table needed to write Section 5.1 later.  It preserves explicit
## `not_run`, `maintained`, and `protocol_unavailable` states.

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(arg)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}

.parse_args <- function(x) {
  out <- list()
  for (arg in x) {
    if (!startsWith(arg, "--") || !grepl("=", arg, fixed = TRUE)) {
      stop("Arguments must have the form --name=value: ", arg,
           call. = FALSE)
    }
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    out[[gsub("-", "_", bits[[1L]], fixed = TRUE)]] <-
      paste(bits[-1L], collapse = "=")
  }
  out
}

.write_csv <- function(x, path) {
  if (is.null(x)) return(invisible(FALSE))
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(TRUE)
}

.copy_tables <- function(source, destination, prefix) {
  if (!dir.exists(source)) return(data.frame())
  files <- list.files(source, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) return(data.frame())
  targets <- file.path(destination, paste0(prefix, basename(files)))
  copied <- file.copy(files, targets, overwrite = TRUE, copy.mode = TRUE)
  data.frame(
    role = prefix, source = files, bundle_path = targets, copied = copied,
    stringsAsFactors = FALSE
  )
}

.clear_sensitivity_export_tables <- function(table_dir) {
  if (!dir.exists(table_dir)) return(invisible(character()))
  files <- list.files(table_dir, full.names = TRUE)
  base <- basename(files)
  stale <- files[grepl(
    paste0(
      "^assessment__(sensitivity(__|_bridge|_misspecification)|",
      "party_gender(__|_bridge_)).*\\.csv$"
    ),
    base
  )]
  if (length(stale) && !all(file.remove(stale))) {
    stop("Could not clear stale exported sensitivity bridge tables.",
         call. = FALSE)
  }
  invisible(stale)
}

.read_csv_optional <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}

.plot_calibration <- function(path, output) {
  d <- .read_csv_optional(path)
  if (is.null(d) || !nrow(d) ||
      !all(c("type", "observed", "predicted") %in% names(d))) return(FALSE)
  d <- d[d$type == "probability_bin" & is.finite(d$observed) &
           is.finite(d$predicted), , drop = FALSE]
  if (!nrow(d)) return(FALSE)
  grDevices::png(output, width = 1800, height = 1500, res = 220)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4.5, 4.5, 2.2, 1))
  graphics::plot(
    d$predicted, d$observed, xlim = c(0, 1), ylim = c(0, 1),
    xlab = "Mean held-out predicted probability",
    ylab = "Mean observed choice rate", pch = 19, col = "#2C7FB8",
    main = "Saha--Weeks held-out marginal calibration"
  )
  graphics::abline(0, 1, lty = 2, col = "grey45")
  if ("se_gap" %in% names(d)) {
    ok <- is.finite(d$se_gap)
    graphics::segments(
      d$predicted[ok], d$observed[ok] - 1.96 * d$se_gap[ok],
      d$predicted[ok], d$observed[ok] + 1.96 * d$se_gap[ok],
      col = "#2C7FB8"
    )
  }
  TRUE
}

.plot_amce <- function(path, output) {
  d <- .read_csv_optional(path)
  if (is.null(d) || !nrow(d) ||
      !all(c("contrast", "estimate", "conf_low", "conf_high") %in% names(d))) {
    return(FALSE)
  }
  d <- d[is.finite(d$estimate) & is.finite(d$conf_low) &
           is.finite(d$conf_high), , drop = FALSE]
  if (!nrow(d)) return(FALSE)
  d <- d[order(d$estimate), , drop = FALSE]
  h <- max(1700, 125 * nrow(d))
  grDevices::png(output, width = 2200, height = h, res = 220)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4.5, 13, 2.2, 1))
  lim <- range(c(d$conf_low, d$conf_high, 0), finite = TRUE)
  graphics::plot(
    d$estimate, seq_len(nrow(d)), xlim = lim,
    ylim = c(0.5, nrow(d) + 0.5), yaxt = "n", ylab = "",
    xlab = "Marginal change in choice probability", pch = 19,
    col = "#7A0177", main = "Respondent-clustered AMCE-style benchmarks"
  )
  graphics::axis(2, at = seq_len(nrow(d)), labels = d$contrast,
                 las = 1, cex.axis = 0.75)
  graphics::segments(d$conf_low, seq_len(nrow(d)), d$conf_high,
                     seq_len(nrow(d)), col = "#7A0177", lwd = 1.4)
  graphics::abline(v = 0, lty = 2, col = "grey45")
  TRUE
}

.plot_plugin <- function(path, output) {
  d <- .read_csv_optional(path)
  if (is.null(d) || !nrow(d) ||
      !all(c("quantity", "component", "estimate") %in% names(d))) return(FALSE)
  keep <- is.finite(d$estimate) &
    !grepl("Omega_|conditional", d$component) &
    !grepl("preference heterogeneity", d$quantity, fixed = TRUE)
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(FALSE)
  d <- utils::head(d, 40L)
  label <- paste(d$quantity, d$component, sep = ": ")
  ord <- order(d$estimate)
  d <- d[ord, , drop = FALSE]
  label <- label[ord]
  h <- max(1800, 95 * nrow(d))
  grDevices::png(output, width = 2300, height = h, res = 220)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4.5, 14, 2.2, 1))
  graphics::dotchart(
    d$estimate, labels = label, pch = 19, color = "#238B45",
    xlab = "Full-sample structural plug-in estimate",
    main = "Saha--Weeks structural plug-ins (no interval implied)"
  )
  graphics::abline(v = 0, lty = 2, col = "grey45")
  TRUE
}

.relative <- function(path, root) {
  root <- paste0(normalizePath(root, mustWork = TRUE), "/")
  normalized <- normalizePath(path, mustWork = FALSE)
  if (startsWith(normalized, root)) substring(normalized, nchar(root) + 1L) else
    normalized
}

.git_revision <- function(root) {
  out <- tryCatch(system2("git", c("-C", root, "rev-parse", "HEAD"),
                          stdout = TRUE, stderr = FALSE),
                  error = function(e) character())
  if (length(out)) out[[1L]] else NA_character_
}

.verify_sensitivity_bridge <- function(assessment, sensitivity_dir, profile) {
  bridge <- assessment$sensitivity_bridge
  if (!dir.exists(sensitivity_dir)) {
    return(list(
      available = FALSE, validated = FALSE,
      status = data.frame(
        check = "validated sensitivity bridge", status = "not_run",
        detail = "sensitivity_analysis directory is absent",
        stringsAsFactors = FALSE
      )
    ))
  }
  valid_schema <- inherits(bridge, "sw2022_sensitivity_bridge") &&
    isTRUE(bridge$validated) && identical(bridge$profile, profile) &&
    identical(bridge$formal_inference_available, FALSE) &&
    identical(bridge$maintained_assumptions_verified, FALSE)
  if (!valid_schema) {
    stop(
      "A sensitivity_analysis directory exists, but the assessment does not ",
      "contain its validated fail-closed bridge. Rerun 05_assessment.R.",
      call. = FALSE
    )
  }
  source_exists <- file.exists(bridge$source_paths)
  source_md5 <- rep(NA_character_, length(bridge$source_paths))
  source_md5[source_exists] <-
    unname(tools::md5sum(bridge$source_paths[source_exists]))
  source_match <- source_exists & source_md5 == as.character(bridge$source_md5)

  artifact <- bridge$artifact_audit
  artifact_exists <- file.exists(artifact$path)
  artifact_md5 <- rep(NA_character_, nrow(artifact))
  artifact_md5[artifact_exists] <-
    unname(tools::md5sum(artifact$path[artifact_exists]))
  artifact_match <- artifact_exists &
    artifact_md5 == as.character(artifact$expected_md5)

  copies <- assessment$sensitivity_table_copies
  copy_ok <- is.data.frame(copies) && nrow(copies) > 0L &&
    all(file.exists(copies$source)) && all(file.exists(copies$target)) &&
    all(unname(tools::md5sum(copies$source)) == copies$source_md5) &&
    all(unname(tools::md5sum(copies$target)) == copies$target_md5) &&
    all(copies$source_md5 == copies$target_md5)
  misspecification <- bridge$misspecification
  misspecification_dir <- file.path(sensitivity_dir, "misspecification")
  misspecification_present <- dir.exists(misspecification_dir)
  misspecification_schema <- if (misspecification_present) {
    inherits(misspecification, "sw2022_misspecification_bridge") &&
      isTRUE(misspecification$validated) &&
      identical(misspecification$profile, profile) &&
      identical(misspecification$formal_inference_available, FALSE) &&
      identical(misspecification$maintained_assumptions_verified, FALSE)
  } else !isTRUE(misspecification$validated)
  misspecification_source_match <- TRUE
  misspecification_artifact_match <- TRUE
  misspecification_input_match <- TRUE
  if (misspecification_present && misspecification_schema) {
    m_source_exists <- file.exists(misspecification$source_paths)
    m_source_md5 <- rep(NA_character_, length(m_source_exists))
    m_source_md5[m_source_exists] <- unname(tools::md5sum(
      misspecification$source_paths[m_source_exists]))
    misspecification_source_match <- all(
      m_source_exists &
        m_source_md5 == as.character(misspecification$source_md5)
    )
    m_artifact <- misspecification$artifact_audit
    m_artifact_exists <- file.exists(m_artifact$path)
    m_artifact_md5 <- rep(NA_character_, nrow(m_artifact))
    m_artifact_md5[m_artifact_exists] <- unname(tools::md5sum(
      m_artifact$path[m_artifact_exists]))
    misspecification_artifact_match <- all(
      m_artifact_exists &
        m_artifact_md5 == as.character(m_artifact$expected_md5)
    )
    m_input <- misspecification$input_audit
    m_input_exists <- file.exists(m_input$path)
    m_input_md5 <- rep(NA_character_, nrow(m_input))
    m_input_md5[m_input_exists] <- unname(tools::md5sum(
      m_input$path[m_input_exists]))
    misspecification_input_match <- all(
      m_input_exists & m_input_md5 == as.character(m_input$expected_md5)
    )
  }
  if (!all(source_match) || !all(artifact_match) || !copy_ok ||
      !misspecification_schema || !misspecification_source_match ||
      !misspecification_artifact_match || !misspecification_input_match) {
    stop(
      "Sensitivity artifacts or their assessment-table copies changed after ",
      "validation. Rerun misspecification 08/09 (when present), parent 07/08, ",
      "and then 05; stale results are not exported.",
      call. = FALSE
    )
  }
  status <- data.frame(
    check = c("bridge schema/profile", "source artifact hashes",
              "manifested artifact hashes", "assessment table copies",
              "formal inference/assumption verification withheld"),
    status = "pass",
    detail = c(
      paste("validated", profile, "bridge"),
      paste(sum(source_match), "source hashes matched"),
      paste(sum(artifact_match), "manifested hashes matched"),
      paste(nrow(copies), "byte-identical table copies"),
      "bridge flags remain FALSE"
    ), stringsAsFactors = FALSE
  )
  if (misspecification_present) {
    status <- rbind(status, data.frame(
      check = c(
        "misspecification simulation bridge",
        "misspecification source/artifact/input hashes",
        "simulation versus empirical alternatives"
      ),
      status = c("pass", "pass", "run_simulated_data_diagnostic"),
      detail = c(
        paste("independent", profile, "validator retained"),
        paste(
          nrow(misspecification$artifact_audit), "nested artifacts and",
          nrow(misspecification$input_audit), "input/config hashes matched"
        ),
        misspecification$validation$distinction
      ), stringsAsFactors = FALSE
    ))
  } else {
    status <- rbind(status, data.frame(
      check = "misspecification simulation bridge", status = "not_run",
      detail = paste(
        "No validated simulated-data battery is present; empirical",
        "alternative-family refits remain not_run."
      ), stringsAsFactors = FALSE
    ))
  }
  list(
    available = TRUE, validated = TRUE,
    misspecification_validated = misspecification_present,
    status = status
  )
}

.verify_party_gender_mean_bridge <- function(assessment, directory, profile) {
  bridge <- assessment$party_gender_mean_bridge
  if (!dir.exists(directory)) {
    if (isTRUE(bridge$validated)) {
      stop(
        "The assessment records a validated party-gender diagnostic, but its ",
        "source directory is now absent. Rerun 05_assessment.R.",
        call. = FALSE
      )
    }
    return(list(
      available = FALSE, validated = FALSE,
      formal_inference_available = FALSE,
      maintained_model = FALSE, outcome_blind = FALSE,
      status = data.frame(
        check = "party-by-candidate-gender mean diagnostic",
        status = "not_run",
        detail = paste(
          "No validated post-hoc diagnostic is present; no diagnostic,",
          "formal-inference, or model-selection claim is exported."
        ), stringsAsFactors = FALSE
      )
    ))
  }

  result_schema <- "sw2022-party-gender-mean-diagnostic-v1"
  valid_schema <- inherits(bridge, "sw2022_party_gender_mean_bridge") &&
    isTRUE(bridge$validated) && identical(bridge$profile, profile) &&
    identical(bridge$formal_inference_available, FALSE) &&
    identical(bridge$maintained_model, FALSE) &&
    identical(bridge$outcome_blind, FALSE) &&
    identical(bridge$primary_artifacts_modified, FALSE) &&
    identical(bridge$result$schema_version, result_schema) &&
    identical(bridge$result$profile, profile) &&
    identical(bridge$result$formal_inference_available, FALSE) &&
    identical(bridge$result$maintained_model, FALSE) &&
    identical(bridge$result$diagnostic_selection_outcome_blind, FALSE) &&
    identical(bridge$result$primary_artifacts_modified, FALSE) &&
    identical(bridge$result$posterior_summaries_used, FALSE) &&
    isTRUE(bridge$result$fold_construction_verified) &&
    isTRUE(bridge$result$inherited_primary_outer_folds) &&
    identical(bridge$manifest$schema_version,
              paste0(result_schema, "-manifest")) &&
    identical(bridge$manifest$profile, profile) &&
    isTRUE(bridge$manifest$descriptive_use_gate) &&
    isTRUE(bridge$manifest$primary_artifacts_unchanged) &&
    identical(bridge$manifest$formal_inference_available, FALSE) &&
    identical(bridge$manifest$maintained_model, FALSE) &&
    identical(bridge$manifest$outcome_blind, FALSE) &&
    identical(bridge$manifest$primary_artifacts_modified, FALSE)
  if (!valid_schema) {
    stop(
      "A party-gender diagnostic directory exists, but the assessment lacks ",
      "a validated fail-closed post-hoc bridge. Rerun 05_assessment.R.",
      call. = FALSE
    )
  }

  source_exists <- file.exists(bridge$source_paths)
  source_observed <- rep(NA_character_, length(bridge$source_paths))
  source_observed[source_exists] <- unname(tools::md5sum(
    bridge$source_paths[source_exists]
  ))
  source_match <- source_exists &
    source_observed == as.character(bridge$source_md5)

  artifact <- bridge$artifact_audit
  artifact_schema <- is.data.frame(artifact) && nrow(artifact) > 0L &&
    all(c("path", "expected_md5") %in% names(artifact))
  input <- bridge$input_audit
  input_schema <- is.data.frame(input) && nrow(input) > 0L &&
    all(c("path", "expected_md5") %in% names(input))
  if (!artifact_schema || !input_schema) {
    stop(
      "The party-gender bridge hash audits are malformed. Rerun ",
      "05_assessment.R.", call. = FALSE
    )
  }
  artifact_exists <- file.exists(artifact$path)
  artifact_observed <- rep(NA_character_, nrow(artifact))
  artifact_observed[artifact_exists] <- unname(tools::md5sum(
    artifact$path[artifact_exists]
  ))
  artifact_match <- artifact_exists &
    artifact_observed == as.character(artifact$expected_md5)

  input_exists <- file.exists(input$path)
  input_observed <- rep(NA_character_, nrow(input))
  input_observed[input_exists] <- unname(tools::md5sum(
    input$path[input_exists]
  ))
  input_match <- input_exists &
    input_observed == as.character(input$expected_md5)

  copies <- assessment$party_gender_mean_table_copies
  copy_ok <- is.data.frame(copies) && nrow(copies) > 0L &&
    all(file.exists(copies$source)) && all(file.exists(copies$target)) &&
    all(unname(tools::md5sum(copies$source)) == copies$source_md5) &&
    all(unname(tools::md5sum(copies$target)) == copies$target_md5) &&
    all(copies$source_md5 == copies$target_md5)

  current_result <- if (all(source_match)) {
    readRDS(bridge$source_paths[["result"]])
  } else NULL
  current_manifest <- if (all(source_match)) {
    readRDS(bridge$source_paths[["manifest"]])
  } else NULL
  current_fail_closed <- is.list(current_result) && is.list(current_manifest) &&
    identical(current_result$schema_version, result_schema) &&
    identical(current_result$profile, profile) &&
    identical(current_result$formal_inference_available, FALSE) &&
    identical(current_result$maintained_model, FALSE) &&
    identical(current_result$diagnostic_selection_outcome_blind, FALSE) &&
    identical(current_result$primary_artifacts_modified, FALSE) &&
    identical(current_manifest$schema_version,
              paste0(result_schema, "-manifest")) &&
    isTRUE(current_manifest$descriptive_use_gate) &&
    identical(current_manifest$formal_inference_available, FALSE) &&
    identical(current_manifest$maintained_model, FALSE) &&
    identical(current_manifest$outcome_blind, FALSE) &&
    identical(current_manifest$primary_artifacts_modified, FALSE)
  if (!all(source_match) || !all(artifact_match) || !all(input_match) ||
      !copy_ok || !current_fail_closed) {
    stop(
      "Party-gender diagnostic sources, inputs, artifacts, or assessment ",
      "copies changed after validation. Rerun its smoke/production runner and ",
      "05_assessment.R; stale or relabeled results are not exported.",
      call. = FALSE
    )
  }

  status <- data.frame(
    check = c(
      "party-gender bridge schema/profile",
      "party-gender source hashes",
      "party-gender manifested artifact hashes",
      "party-gender input/source-code hashes",
      "party-gender assessment table copies",
      "post-hoc/outcome-blind disclosure",
      "formal inference/maintained model"
    ),
    status = c(rep("pass", 5L), "failed_by_design_disclosed", "withheld"),
    detail = c(
      paste("validated", profile, "post-hoc bridge"),
      paste(sum(source_match), "source hashes matched"),
      paste(sum(artifact_match), "manifested hashes matched"),
      paste(sum(input_match), "input/source-code hashes matched"),
      paste(nrow(copies), "byte-identical table copies"),
      paste(
        "The two-slope extension was chosen after observing the primary",
        "mismatch; no outcome-blind model-assessment claim is made."
      ),
      paste(
        "Diagnostic only: no formal inference, maintained-model status,",
        "model-selection claim, or materiality pass."
      )
    ), stringsAsFactors = FALSE
  )
  list(
    available = TRUE, validated = TRUE, status = status,
    formal_inference_available = FALSE,
    maintained_model = FALSE, outcome_blind = FALSE,
    disclaimer = bridge$disclaimer
  )
}

.verify_assessment_inputs <- function(assessment) {
  manifest <- assessment$input_manifest
  if (!is.data.frame(manifest) ||
      !all(c("artifact", "path", "exists", "md5") %in% names(manifest))) {
    stop("Assessment input provenance is missing or malformed.", call. = FALSE)
  }
  expected <- which(manifest$exists %in% TRUE)
  if (length(expected)) {
    paths <- as.character(manifest$path[expected])
    current_exists <- file.exists(paths)
    current_md5 <- rep(NA_character_, length(paths))
    current_md5[current_exists] <-
      unname(as.character(tools::md5sum(paths[current_exists])))
    matches <- current_exists &
      current_md5 == as.character(manifest$md5[expected])
    if (!all(matches)) {
      stop(
        "An input artifact changed after 05_assessment.R: ",
        paste(manifest$artifact[expected][!matches], collapse = ", "),
        ". Rerun 05_assessment.R before export.", call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- .parse_args(args)
  script <- .script_file()
  project <- normalizePath(file.path(dirname(script), "../../.."),
                           mustWork = TRUE)
  app <- file.path(project, "applications", "sw2022")
  profile <- opt$profile %||% Sys.getenv("SCONJOINT_SW_PROFILE", "production")
  assessment_dir <- opt$assessment_dir %||%
    file.path(app, "results", "assessment", profile)
  assessment_path <- file.path(assessment_dir, "objects", "assessment_bundle.rds")
  prep_path <- file.path(app, "results", "prep_analysis_data.rds")
  if (!file.exists(assessment_path)) {
    stop("Assessment bundle not found: ", assessment_path,
         ". Run 05_assessment.R first.", call. = FALSE)
  }
  if (!file.exists(prep_path)) stop("Prepared analysis artifact is missing.",
                                    call. = FALSE)

  output <- opt$out_dir %||%
    file.path(app, "results", "section5_1_bundle", profile)
  tables <- file.path(output, "tables")
  figures <- file.path(output, "figures")
  manifests <- file.path(output, "manifests")
  invisible(lapply(c(output, tables, figures, manifests), dir.create,
                   recursive = TRUE, showWarnings = FALSE))
  .clear_sensitivity_export_tables(tables)

  assessment <- readRDS(assessment_path)
  prepared <- readRDS(prep_path)
  if (!inherits(assessment, "sw2022_application_assessment")) {
    stop("The assessment artifact has an unexpected schema.", call. = FALSE)
  }
  .verify_assessment_inputs(assessment)
  sensitivity_dir <- file.path(app, "results", "mixed_logit", profile,
                               "sensitivity_analysis")
  sensitivity_export <- .verify_sensitivity_bridge(
    assessment, sensitivity_dir, profile)
  .write_csv(sensitivity_export$status,
             file.path(manifests, "sensitivity_bridge_export_status.csv"))
  party_gender_dir <- file.path(
    app, "results", "party_gender_mean_sensitivity", profile
  )
  party_gender_export <- .verify_party_gender_mean_bridge(
    assessment, party_gender_dir, profile
  )
  .write_csv(
    party_gender_export$status,
    file.path(manifests, "party_gender_bridge_export_status.csv")
  )
  prediction_provenance <- assessment$prediction_assessment_provenance %||%
    list(candidate_grid_outcome_blind = NA,
         interpretation = "Predictive-assessment provenance was not recorded.")

  rank_paths <- c(
    common = file.path(app, "results", "rank_assessment", profile,
                       "common_outer_rank_assessment.rds"),
    q2 = file.path(app, "results", "rank_assessment", profile,
                   "q2_numerical_orientation_assessment.rds")
  )
  rank_expected <- c(
    common = !is.null(assessment$common_outer_rank_assessment),
    q2 = !is.null(assessment$q2_numerical_orientation_assessment)
  )
  if (!identical(unname(file.exists(rank_paths)), unname(rank_expected))) {
    stop(
      "Rank-assessment availability changed after 05_assessment.R. ",
      "Rerun 05_assessment.R before export.", call. = FALSE
    )
  }

  copied <- rbind(
    .copy_tables(file.path(assessment_dir, "tables"), tables, "assessment__"),
    .copy_tables(file.path(app, "tables"), tables, "preparation__"),
    .copy_tables(file.path(app, "results", "rank_assessment", profile,
                           "tables"), tables, "rank__"),
    .copy_tables(file.path(app, "results", "mixed_logit", profile),
                 tables, "fit__")
  )
  .write_csv(copied, file.path(manifests, "table_copy_manifest.csv"))

  figure_status <- data.frame(
    figure = c("heldout_calibration.png", "amce_benchmarks.png",
               "structural_plugin_points.png"),
    created = c(
      .plot_calibration(
        file.path(assessment_dir, "tables", "calibration_marginal.csv"),
        file.path(figures, "heldout_calibration.png")
      ),
      .plot_amce(
        file.path(assessment_dir, "tables", "design_amce_lpm.csv"),
        file.path(figures, "amce_benchmarks.png")
      ),
      .plot_plugin(
        file.path(assessment_dir, "tables", "headline_plugin_quantities.csv"),
        file.path(figures, "structural_plugin_points.png")
      )
    ),
    interpretation = c(
      if (identical(prediction_provenance$candidate_grid_outcome_blind, FALSE)) {
        paste(
          "Cross-fitted respondent-sequence diagnostic under a grid adapted",
          "after a same-sample pilot; marginal bins do not replace joint checks."
        )
      } else {
        "Held-out respondent-sequence fit; marginal bins do not replace joint checks."
      },
      "Different estimand and scale from mixed-logit preference coefficients.",
      "Point estimates only; the plot does not imply regular intervals."
    ), stringsAsFactors = FALSE
  )
  .write_csv(figure_status, file.path(manifests, "figure_manifest.csv"))

  inventory <- data.frame(
    manuscript_need = c(
      "sample and estimand", "profile coding", "primary structural plug-ins",
      "respondent-level inference", "held-out sequence fit",
      "marginal and joint calibration", "design-based empirical layer",
      "identification/design audit", "rank sensitivity", "numerical stability",
      "optimization", "information/profile likelihood",
      "completion/attrition", "shape/covariance/scale sensitivity",
      "gender-by-ambition sensitivity", "reporting gates"
    ),
    bundle_source = c(
      "preparation__prep_sample_flow.csv and section5_1_data.rds",
      "preparation__prep_coordinate_dictionary.csv",
      "assessment__headline_plugin_quantities.csv and assessment__structural_plugin_quantities.csv",
      "assessment__inference_target_diagnostics.csv, inference_transform_diagnostics.csv, and inference_summary.csv",
      "assessment__heldout_sequence_score.csv",
      paste(
        "assessment__calibration_marginal.csv,",
        "assessment__calibration_joint.csv,",
        "assessment__calibration_full_response_pattern.csv,",
        "assessment__calibration_prespecified_task_pairs.csv, and",
        "assessment__calibration_exact_repeated_contrast.csv"
      ),
      "assessment__design_amce_lpm.csv and amce_structural_parallel_check.csv; exact HT benchmark unavailable",
      "preparation__design_rank_summary.csv and preparation__design_summary.csv",
      "rank__common_outer_sequence_scores.csv, rank__common_outer_paired_score_differences.csv, and rank__rank_qoi_stability.csv when run",
      "assessment__integration_primary_grid.csv/refinement files plus rank__q2_integration_summary.csv and rank__q2_rotation_stability.csv",
      "assessment__optimization_summary.csv and optimization_starts.csv",
      "assessment__information_eigenvalues.csv and profile_likelihood_status.csv",
      paste(
        "preparation__completion_*, assessment__completion_status.csv,",
        "assessment__completion_early_response_assignment_by_task.csv, and",
        "assessment__sensitivity__completion_* when validated"
      ),
      paste(
        "assessment__assessment_component_status.csv and",
        "assessment__sensitivity__structural_sensitivity_protocol.csv;",
        "when validated, assessment__sensitivity__misspecification__",
        "qoi_bias_stability.csv, dgp_calibration.csv, truth_resolution.csv,",
        "coverage_status.csv, and structural_component_status.csv"
      ),
      paste(
        "assessment__sensitivity__male_run_conditional_effects.csv,",
        "male_run_choice_probabilities.csv, male_run_design_audit.csv, and",
        "male_run_heldout_score_difference.csv"
      ),
      "assessment__quantity_reporting_gates.csv and manuscript_claims_ledger.csv"
    ),
    ready_rule = c(
      rep("use only if the corresponding component status is executed", 6L),
      "AMCE may be reported only as a distinct marginal estimand; exact HT remains unavailable",
      "state that theorem applicability is conditional on advertised design support",
      "primary q=1 remains fixed; alternatives are not a selection exercise",
      "do not treat a finite grid as exact; require fresh-refit refinement for a stability claim",
      "do not claim a known global approximate-maximization gap",
      "profiles require nuisance reoptimization; slices are not profiles",
      "diagnostics cannot establish noninformative completion",
      paste(
        "design-specific simulated-data diagnostics are not empirical",
        "alternative-family refits; coverage/materiality/formal inference",
        "remain withheld and unrun refits stay not_run"
      ),
      paste(
        "validated descriptive sensitivity only; formal inference and",
        "document-verified fielded support for the augmented basis are withheld"
      ),
      "apply every quantity-specific support/domain/information/numerical gate"
    ), stringsAsFactors = FALSE
  )
  inventory <- rbind(
    inventory,
    data.frame(
      manuscript_need = c(
        "post-conjoint moderator sensitivity",
        "task-order/serial/position sensitivity",
        "conditional-randomization completion/assignment test"
      ),
      bundle_source = c(
        paste(
          "assessment__sensitivity__z19_theta.csv, z19_choices.csv, and",
          "z19_heldout_score_difference.csv"
        ),
        paste(
          "assessment__sensitivity__task_order_calibration.csv,",
          "serial_residual_diagnostics.csv, adjacent_transition_calibration.csv,",
          "position_diagnostics.csv, and position_profile_swap.csv"
        ),
        paste(
          "assessment__conditional_randomization_test_status.csv and",
          "assessment__design_assessment_status.csv"
        )
      ),
      ready_rule = c(
        paste(
          "descriptive sensitivity only; post-conjoint fields remain excluded",
          "from the primary model and formal inference is withheld"
        ),
        paste(
          "diagnostics do not substitute for the still-not-run task-process",
          "or serial-shock alternative likelihoods"
        ),
        paste(
          "withhold the test and any p-value unless the fielded randomizer,",
          "assignment restrictions, and exact exposure probabilities are recovered"
        )
      ), stringsAsFactors = FALSE
    )
  )
  inventory <- rbind(
    inventory,
    data.frame(
      manuscript_need = "party-by-candidate-gender mean diagnostic",
      bundle_source = paste(
        "assessment__party_gender__sequence_score_summary.csv,",
        "assessment__party_gender__sequence_score_paired_differences.csv,",
        "assessment__party_gender__party_calibration.csv,",
        "assessment__party_gender__party_amce_projection.csv, and",
        "assessment__party_gender__party_gender_structural.csv"
      ),
      ready_rule = paste(
        "post-hoc respondent-cross-fitted diagnostic only; do not report",
        "formal inference, an outcome-blind selection claim, maintained-model",
        "status, a primary-model replacement, or a materiality pass"
      ), stringsAsFactors = FALSE
    )
  )
  .write_csv(inventory, file.path(output, "section5_1_inventory.csv"))

  inference <- assessment$inference %||% list(status = "not_run")
  component_status <- assessment$component_status
  blocking <- if (is.data.frame(component_status)) {
    component_status[grepl("not_run|maintained|withheld|unavailable|failed|incomplete",
                           component_status$status), , drop = FALSE]
  } else data.frame()
  notes <- c(
    "# Saha--Weeks Section 5.1 evidence bundle",
    "",
    "This directory is an analysis handoff, not drafted manuscript prose.",
    "Every table must be read together with the component and claims ledgers.",
    "",
    "## Frozen analysis scope",
    "",
    paste0("- Profile: `", profile, "`."),
    paste0("- Estimand: ", assessment$estimand),
    paste0("- Sample: N = ", prepared$sample$N, " respondents and ",
           prepared$sample$task_rows, " task observations (three per included respondent)."),
    "- Outcome is candidate-A choice and each profile contrast is X(A)-X(B).",
    "- Primary residual factor rank is q=1; q=0 and q=2 are sensitivities, not a rank-selection rule.",
    "- Structural quantities integrate the fitted normal distribution and respondent empirical Z distribution; no respondent posterior modes are used.",
    "",
    "## Inference and assessment state",
    "",
    paste0("- Inference artifact status: `", inference$status %||% "not_run", "`."),
    paste0("- Formal inference available: `",
           isTRUE(inference$inference_available), "`."),
    paste0("- Predictive-assessment provenance: ",
           prediction_provenance$interpretation),
    paste0("- Validated sensitivity bridge available: `",
           isTRUE(sensitivity_export$validated), "`."),
    paste0("- Validated post-hoc party-gender diagnostic available: `",
           isTRUE(party_gender_export$validated), "`."),
    if (isTRUE(party_gender_export$validated)) paste(
      "- The party-by-candidate-gender mean comparison reuses respondent",
      "outer folds for frozen q=1 pooled and two-slope refits, but was chosen",
      "after the primary mismatch was observed. It is diagnostic only: no",
      "formal inference, outcome-blind model-selection claim, maintained-model",
      "status, primary-model replacement, or materiality pass is supplied."
    ) else paste(
      "- The post-hoc party-by-candidate-gender mean diagnostic was not",
      "ingested; no corresponding diagnostic or inferential claim is made."
    ),
    if (isTRUE(sensitivity_export$validated)) paste(
      "- Implemented descriptive sensitivities: 15-Z versus post-conjoint",
      "19-Z, Male-by-prior-run, A/B position swap, and 1,191-versus-1,249",
      "completion/sample refits. Formal inference and assumption verification",
      "remain withheld."
    ) else "- Application sensitivity refits were not ingested.",
    if (isTRUE(sensitivity_export$validated)) paste(
      "- Held-out task-order, transition, and residual-serial diagnostics were",
      "run; task-varying and explicit serial-shock alternative likelihoods",
      "remain `not_run`."
    ) else "- Task-process and serial-shock alternatives remain `not_run`.",
    if (isTRUE(sensitivity_export$misspecification_validated)) paste(
      "- Design-specific simulated-data diagnostics were run for skewed,",
      "bimodal, heavy-tailed, party-varying covariance, serial-shock, and",
      "random-scale DGPs. These are not empirical alternative-family refits;",
      "the corresponding empirical alternatives remain `not_run`."
    ) else paste(
      "- Skewed, bimodal, heavy-tailed, covariance-by-Z, serial-shock, and",
      "random-scale simulated-data diagnostics and empirical alternatives",
      "remain `not_run`."
    ),
    "- No application materiality threshold was preregistered; the structural-sensitivity protocol remains fail-closed and incomplete.",
    "- Any interval marked available is conditional on the documented high-level rate, approximation, information, rank, optimization, and numerical conditions.",
    "- Normality, common covariance, independent logit shocks, and noninformative completion are maintained conditions; diagnostics do not verify them.",
    paste(
      "- Joint predictive diagnostics include all eight complete three-task",
      "response patterns and the prespecified task pairs 1--2, 2--3, and",
      "1--3, integrated over one common respondent factor draw."
    ),
    paste(
      "- The exact repeated-contrast calibration has only one supporting",
      "respondent; it is retained as a sparse descriptive check and is not",
      "coarsened or promoted into a general fit test."
    ),
    "- Exact ordered-contrast Horvitz--Thompson benchmarking is unavailable because the fielded protocol probabilities/randomizer were not recovered. The LPM/AMCE table is a distinct marginal estimand.",
    paste(
      "- A conditional-randomization completion/assignment test is also",
      "withheld: no empirical or illustrative-uniform permutation law is",
      "substituted for the missing fielded protocol."
    ),
    "- Contest probabilities are conditional on the advertised unrestricted support, which is not document-certified from the fielded protocol; any off-support use must be labeled structural extrapolation.",
    "- Majority language is withheld unless a linked regular confidence interval excludes one-half and the residual-variance gate passes.",
    "",
    "## Files to start from",
    "",
    "- `section5_1_inventory.csv`: mapping from manuscript claims to artifacts.",
    "- `tables/assessment__headline_plugin_quantities.csv`: reporting-basis and subgroup headline plug-ins.",
    "- `tables/assessment__structural_plugin_quantities.csv`: classed structural plug-ins, including integrated shares and contest probabilities.",
    "- `tables/assessment__inference_target_diagnostics.csv`: one-step results with diagnostic versus formal uncertainty clearly separated.",
    "- `tables/assessment__quantity_reporting_gates.csv`: quantity-specific gate ledger.",
    "- `tables/assessment__manuscript_claims_ledger.csv`: claims that are available or withheld.",
    "- `tables/assessment__assessment_component_status.csv`: executed/unrun/maintained status.",
    "- `tables/assessment__calibration_full_response_pattern.csv` and `assessment__calibration_prespecified_task_pairs.csv`: shared-factor joint calibration checks.",
    "- `tables/assessment__completion_early_response_assignment_by_task.csv`: raw-universe descriptive completion/exclusion comparisons.",
    "- `tables/assessment__conditional_randomization_test_status.csv`: explicit fail-closed protocol gate.",
    "- `tables/assessment__sensitivity__application_sensitivity_status.csv`: implemented diagnostics versus unrun alternative models.",
    "- `tables/assessment__sensitivity_bridge_artifact_hashes.csv`: upstream sensitivity provenance and hash gate.",
    if (isTRUE(party_gender_export$validated)) paste(
      "- `tables/assessment__party_gender__*.csv` and",
      "`tables/assessment__party_gender_bridge_status.csv`: validated post-hoc",
      "party-gender diagnostic evidence, with formal inference and",
      "outcome-blind/maintained-model claims explicitly withheld."
    ) else "- Post-hoc party-gender diagnostic artifacts were not ingested.",
    if (isTRUE(sensitivity_export$misspecification_validated)) paste(
      "- `tables/assessment__sensitivity_misspecification_bridge_status.csv`",
      "and `assessment__sensitivity__misspecification__*.csv`: independently",
      "validated design-specific simulation diagnostics, not empirical refits."
    ) else "- Design-specific misspecification simulation artifacts were not ingested.",
    "- `section5_1_data.rds`: machine-readable bundle used to regenerate these summaries.",
    "",
    paste0("Components requiring caution or further work: ", nrow(blocking), ".")
  )
  writeLines(notes, file.path(output, "README_FOR_SECTION5_1.md"),
             useBytes = TRUE)

  data_bundle <- list(
    schema_version = "sw2022-section5.1-bundle-v1",
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    profile = profile,
    git_revision = .git_revision(project),
    sample = prepared$sample,
    estimand = prepared$estimand,
    provenance = prepared$provenance,
    coordinate_dictionary = prepared$coordinate_dictionary,
    attribute_blocks = prepared$attribute_blocks,
    assessment = assessment,
    sensitivity_bridge = assessment$sensitivity_bridge,
    sensitivity_export_verification = sensitivity_export,
    party_gender_mean_bridge = assessment$party_gender_mean_bridge,
    party_gender_export_verification = party_gender_export,
    inventory = inventory,
    blocking_or_maintained_components = blocking,
    posterior_summaries_used = FALSE,
    source_project_modified = FALSE
  )
  party_gender_export_final <- .verify_party_gender_mean_bridge(
    assessment, party_gender_dir, profile
  )
  if (!identical(isTRUE(party_gender_export_final$validated),
                 isTRUE(party_gender_export$validated)) ||
      !identical(isTRUE(party_gender_export_final$available),
                 isTRUE(party_gender_export$available))) {
    stop(
      "Party-gender diagnostic availability changed during export. Discard ",
      "this bundle and rerun after its producer finishes.", call. = FALSE
    )
  }
  saveRDS(data_bundle, file.path(output, "section5_1_data.rds"), version = 3)
  capture.output(utils::sessionInfo(),
                 file = file.path(manifests, "sessionInfo.txt"))

  output_files <- list.files(output, recursive = TRUE, full.names = TRUE)
  output_files <- output_files[file.info(output_files)$isdir %in% FALSE]
  output_files <- setdiff(
    output_files, file.path(manifests, "bundle_artifact_manifest.csv")
  )
  manifest <- data.frame(
    path = vapply(output_files, .relative, character(1L), root = project),
    bytes = file.info(output_files)$size,
    md5 = unname(tools::md5sum(output_files)),
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    git_revision = .git_revision(project), stringsAsFactors = FALSE
  )
  ## Exclude the manifest itself from its checksum list to avoid a recursive
  ## self-hash. It is written after all other bundle artifacts.
  .write_csv(manifest, file.path(manifests, "bundle_artifact_manifest.csv"))

  cat(sprintf(
    "Exported Saha--Weeks Section 5.1 evidence bundle (%s): %d files.\n",
    profile, nrow(manifest)
  ))
  invisible(data_bundle)
}

if (sys.nframe() == 0L) .main()
