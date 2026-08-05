#' Convert Metadata to Submission Readiness Evidence
#'
#' Turns dataset metadata - either a `metacore` object or a
#' plain data.frame - into standardized R4SUB evidence rows describing how
#' completely each variable is documented. This lets metadata assembled for a
#' 'pharmaverse' pipeline feed the same evidence table and Submission Confidence
#' Index as any other source.
#'
#' Two indicators are evaluated, reusing the identifiers emitted by
#' `r4subcore::define_xml_to_evidence()` so that metadata- and Define-XML-sourced
#' evidence group together in scoring and explainability:
#' \describe{
#'   \item{Q-DEFINE-002}{Variable is documented (has a label and a data type).}
#'   \item{Q-DEFINE-003}{Derivation text is present for derived variables.}
#' }
#'
#' @param metadata A `Metacore` object or a data.frame accepted by
#'   [as_variable_metadata()].
#' @param ctx An [r4subcore::r4sub_run_context] providing run and study
#'   identifiers.
#' @param source_name Character. Label recorded as the evidence source.
#'   Default `"metacore"`.
#' @param source_version Character or `NULL`. Optional version label for the
#'   metadata source.
#'
#' @return A data.frame conforming to the R4SUB evidence schema.
#'
#' @seealso [adam_to_evidence()], [submission_readiness()]
#'
#' @examplesIf requireNamespace("r4subdata", quietly = TRUE)
#' # The example ADaM metadata shipped in r4subdata.
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
#' ev  <- suppressMessages(metacore_to_evidence(r4subdata::adam_metadata, ctx))
#' table(ev$indicator_id, ev$result)
#'
#' @importFrom cli cli_alert_info cli_abort
#' @export
metacore_to_evidence <- function(metadata,
                                 ctx,
                                 source_name = "metacore",
                                 source_version = NULL) {
  if (!inherits(ctx, "r4sub_run_context")) {
    cli::cli_abort("{.arg ctx} must be an {.cls r4sub_run_context}.")
  }
  assert_scalar_string(source_name, "source_name")
  assert_scalar_string(source_version, "source_version", allow_null = TRUE)
  meta <- as_variable_metadata(metadata)
  if (nrow(meta) == 0L) {
    cli::cli_abort("{.arg metadata} describes no variables.")
  }

  rows <- rbind(
    documentation_rows(meta, source_name, source_version),
    derivation_rows(meta, source_name, source_version)
  )

  cli::cli_alert_info(
    "metacore_to_evidence: {nrow(rows)} row{?s} from {nrow(meta)} variable{?s}"
  )
  r4subcore::as_evidence(rows, ctx = ctx)
}

# Q-DEFINE-002: variable documented (label + data type present).
documentation_rows <- function(meta, source_name, source_version) {
  has_label <- !is.na(meta$label)
  has_type  <- !is.na(meta$type)
  documented <- has_label & has_type

  result <- ifelse(documented, "pass", ifelse(has_label | has_type, "warn", "fail"))
  severity <- ifelse(documented, "info", ifelse(has_label | has_type, "medium", "high"))
  message <- ifelse(
    documented,
    sprintf("Variable %s in %s is documented (label + type)", meta$variable, meta$dataset),
    ifelse(
      !has_label & !has_type,
      sprintf("Variable %s in %s is missing both label and type", meta$variable, meta$dataset),
      ifelse(
        !has_label,
        sprintf("Variable %s in %s is missing a label", meta$variable, meta$dataset),
        sprintf("Variable %s in %s is missing a data type", meta$variable, meta$dataset)
      )
    )
  )

  data.frame(
    asset_type       = "define",
    asset_id         = meta$dataset,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-DEFINE-002",
    indicator_name   = "Variable Documented in Metadata",
    indicator_domain = "quality",
    severity         = severity,
    result           = result,
    metric_value     = 0.5 * has_label + 0.5 * has_type,
    metric_unit      = "score",
    message          = message,
    location         = paste0(meta$dataset, ":", meta$variable),
    evidence_payload = mapply(
      payload_json,
      variable = meta$variable, dataset = meta$dataset,
      has_label = tolower(has_label), has_type = tolower(has_type),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Q-DEFINE-003: derivation text present for derived variables only.
derivation_rows <- function(meta, source_name, source_version) {
  derived <- meta[which(meta$is_derived), , drop = FALSE]
  if (nrow(derived) == 0L) {
    return(pre_evidence_template())
  }

  has_deriv <- !is.na(derived$derivation)
  result   <- ifelse(has_deriv, "pass", "fail")
  severity <- ifelse(has_deriv, "info", "high")
  message  <- ifelse(
    has_deriv,
    sprintf("Derivation documented for %s in %s", derived$variable, derived$dataset),
    sprintf("Derived variable %s in %s has no derivation text", derived$variable, derived$dataset)
  )

  data.frame(
    asset_type       = "define",
    asset_id         = derived$dataset,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-DEFINE-003",
    indicator_name   = "Derivation Present in Metadata",
    indicator_domain = "quality",
    severity         = severity,
    result           = result,
    metric_value     = as.double(has_deriv),
    metric_unit      = "score",
    message          = message,
    location         = paste0(derived$dataset, ":", derived$variable),
    evidence_payload = mapply(
      payload_json,
      variable = derived$variable, dataset = derived$dataset,
      has_derivation = tolower(has_deriv),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}
