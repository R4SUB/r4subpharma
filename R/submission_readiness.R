#' Score Submission Readiness for a 'pharmaverse' Deliverable
#'
#' A single entry point that harvests evidence from metadata and one or more
#' ADaM datasets and, when 'r4subscore' is available, computes the Submission
#' Confidence Index (SCI) in one call. It is the fastest way to go from a
#' 'pharmaverse' pipeline to a readiness score.
#'
#' @param data A named list of ADaM data.frames (names are dataset identifiers,
#'   e.g. `"ADSL"`), or a single data.frame when the metadata covers one
#'   dataset.
#' @param metadata A `Metacore` object or a data.frame accepted by
#'   [as_variable_metadata()].
#' @param ctx An [r4subcore::r4sub_run_context] providing run and study
#'   identifiers.
#' @param config An optional `sci_config` (see
#'   [r4subscore::sci_config_default()]). When `NULL`, the r4subscore default is
#'   used.
#'
#' @return An object of class `"submission_readiness"`: a list with `evidence`
#'   (the combined evidence data.frame), `pillar_scores`, and `sci` (an
#'   `sci_result`, or `NULL` if 'r4subscore' is not installed).
#'
#' @seealso [metacore_to_evidence()], [adam_to_evidence()]
#'
#' @examples
#' meta <- data.frame(
#'   dataset  = "ADSL",
#'   variable = c("USUBJID", "AGE"),
#'   label    = c("Unique Subject Identifier", "Age"),
#'   type     = c("text", "integer"),
#'   stringsAsFactors = FALSE
#' )
#' adsl <- data.frame(USUBJID = "01-001", AGE = 54, stringsAsFactors = FALSE)
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
#' res <- suppressMessages(submission_readiness(list(ADSL = adsl), meta, ctx))
#' nrow(res$evidence)
#'
#' @importFrom cli cli_abort cli_alert_info cli_alert_warning
#' @export
submission_readiness <- function(data, metadata, ctx, config = NULL) {
  if (!inherits(ctx, "r4sub_run_context")) {
    cli::cli_abort("{.arg ctx} must be an {.cls r4sub_run_context}.")
  }
  meta <- as_variable_metadata(metadata)
  data <- as_dataset_list(data, meta)

  frames <- c(
    list(metacore_to_evidence(meta, ctx)),
    lapply(names(data), function(nm) {
      adam_to_evidence(data[[nm]], meta, ctx, dataset_name = nm)
    })
  )
  evidence <- do.call(r4subcore::bind_evidence, frames)

  if (!requireNamespace("r4subscore", quietly = TRUE)) {
    cli::cli_alert_warning(
      "Install {.pkg r4subscore} to compute the Submission Confidence Index."
    )
    return(new_submission_readiness(evidence, NULL, NULL))
  }

  score_args <- if (is.null(config)) list() else list(config = config)
  pillar_scores <- do.call(r4subscore::compute_pillar_scores, c(list(evidence), score_args))
  sci <- do.call(r4subscore::compute_sci, c(list(pillar_scores), score_args))

  cli::cli_alert_info("Submission Confidence Index: {.val {sci$SCI}} ({sci$band})")
  new_submission_readiness(evidence, pillar_scores, sci)
}

# Accept either a named list of datasets or a single data.frame.
as_dataset_list <- function(data, meta) {
  if (is.data.frame(data)) {
    datasets <- unique(meta$dataset)
    if (length(datasets) != 1L) {
      cli::cli_abort(c(
        "A bare data.frame needs metadata for exactly one dataset.",
        "i" = "Metadata covers: {.val {datasets}}. Pass a named list instead."
      ))
    }
    data <- list(data)
    names(data) <- datasets
  }
  if (!is.list(data) || is.null(names(data)) || any(!nzchar(names(data)))) {
    cli::cli_abort("{.arg data} must be a named list of data.frames.")
  }
  data
}

new_submission_readiness <- function(evidence, pillar_scores, sci) {
  structure(
    list(evidence = evidence, pillar_scores = pillar_scores, sci = sci),
    class = "submission_readiness"
  )
}

#' @export
print.submission_readiness <- function(x, ...) {
  cat("<submission_readiness>\n")
  cat("  evidence rows:", nrow(x$evidence), "\n")
  if (is.null(x$sci)) {
    cat("  SCI:           not computed (r4subscore not installed)\n")
  } else {
    cat("  SCI:          ", x$sci$SCI, "\n")
    cat("  band:         ", x$sci$band, "\n")
  }
  invisible(x)
}
