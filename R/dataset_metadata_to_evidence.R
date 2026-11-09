#' Convert Dataset-Level Metadata to Submission Readiness Evidence
#'
#' Turns dataset-level metadata - either a `metacore` object or a plain
#' data.frame - into standardized R4SUB evidence rows describing how completely
#' each *dataset* (as opposed to each variable) documents itself. Where
#' [metacore_to_evidence()] reports variable-level documentation presence, this
#' adapter reports the dataset-level counterpart drawn from a `metacore`
#' object's `ds_spec` and `ds_vars`.
#'
#' One indicator is evaluated per dataset, reusing the dataset-level identifier
#' emitted by `r4subcore::define_xml_to_evidence()` so that metadata- and
#' Define-XML-sourced evidence group together in scoring and explainability:
#' \describe{
#'   \item{Q-DEFINE-001}{Dataset is documented: it carries a label, a structure
#'     description, and at least one key variable.}
#' }
#'
#' This is documentation-*presence* evidence only. The adapter records whether
#' each facet is present; it does not judge whether the documented label,
#' structure, or keys are correct or submission-conformant.
#'
#' @param metadata A `Metacore` object or a data.frame accepted by
#'   [as_dataset_metadata()].
#' @param ctx An [r4subcore::r4sub_run_context] providing run and study
#'   identifiers.
#' @param source_name Character. Label recorded as the evidence source.
#'   Default `"metacore"`.
#' @param source_version Character or `NULL`. Optional version label for the
#'   metadata source.
#'
#' @return A data.frame conforming to the R4SUB evidence schema, one row per
#'   dataset.
#'
#' @seealso [metacore_to_evidence()], [adam_to_evidence()],
#'   [submission_readiness()]
#'
#' @examplesIf requireNamespace("metacore", quietly = TRUE)
#' # The CDISC pilot ADaM metacore specification shipped with metacore.
#' load(metacore::metacore_example("pilot_ADaM.rda"))
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("CDISCPILOT01", "DEV"))
#' ev  <- suppressMessages(dataset_metadata_to_evidence(metacore, ctx))
#' table(ev$result)
#'
#' @importFrom cli cli_alert_info cli_abort
#' @export
dataset_metadata_to_evidence <- function(metadata,
                                         ctx,
                                         source_name = "metacore",
                                         source_version = NULL) {
  if (!inherits(ctx, "r4sub_run_context")) {
    cli::cli_abort("{.arg ctx} must be an {.cls r4sub_run_context}.")
  }
  assert_scalar_string(source_name, "source_name")
  assert_scalar_string(source_version, "source_version", allow_null = TRUE)
  meta <- as_dataset_metadata(metadata)
  if (nrow(meta) == 0L) {
    cli::cli_abort("{.arg metadata} describes no datasets.")
  }

  rows <- dataset_documentation_rows(meta, source_name, source_version)

  cli::cli_alert_info(
    "dataset_metadata_to_evidence: {nrow(rows)} row{?s} from {nrow(meta)} dataset{?s}"
  )
  r4subcore::as_evidence(rows, ctx = ctx)
}

# Q-DEFINE-001: dataset documented (label + structure + key variables present).
dataset_documentation_rows <- function(meta, source_name, source_version) {
  has_label     <- !is.na(meta$label)
  has_structure <- !is.na(meta$structure)
  has_keys      <- !is.na(meta$n_keys) & meta$n_keys > 0L

  n_present  <- has_label + has_structure + has_keys
  documented <- n_present == 3L

  result   <- ifelse(documented, "pass", ifelse(n_present == 0L, "fail", "warn"))
  severity <- ifelse(documented, "info", ifelse(n_present == 0L, "high", "medium"))
  message  <- mapply(
    dataset_documentation_message,
    dataset = meta$dataset,
    has_label = has_label, has_structure = has_structure, has_keys = has_keys,
    USE.NAMES = FALSE
  )

  data.frame(
    asset_type       = "define",
    asset_id         = meta$dataset,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-DEFINE-001",
    indicator_name   = "Dataset Documented in Metadata",
    indicator_domain = "quality",
    severity         = severity,
    result           = result,
    metric_value     = n_present / 3,
    metric_unit      = "score",
    message          = message,
    location         = meta$dataset,
    evidence_payload = mapply(
      payload_json,
      dataset = meta$dataset,
      has_label = tolower(has_label), has_structure = tolower(has_structure),
      has_keys = tolower(has_keys), n_keys = meta$n_keys,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Human-readable message for one dataset's documentation presence.
dataset_documentation_message <- function(dataset, has_label, has_structure, has_keys) {
  if (has_label && has_structure && has_keys) {
    return(sprintf("Dataset %s is documented (label + structure + key variables)",
                   dataset))
  }
  missing <- c(
    if (!has_label) "label",
    if (!has_structure) "structure",
    if (!has_keys) "key variables"
  )
  sprintf("Dataset %s is missing: %s", dataset, paste(missing, collapse = ", "))
}
