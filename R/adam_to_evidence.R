#' Convert an ADaM Dataset to Submission Readiness Evidence
#'
#' Compares a built ADaM dataset - such as one produced by an 'admiral' pipeline
#' - against its metadata and emits R4SUB evidence rows about conformance. The
#' checks are computed directly from the dataset and metadata, so they work
#' whether or not the dataset was passed through 'xportr' or 'metatools'.
#'
#' Four indicators are evaluated:
#' \describe{
#'   \item{T-ADAM-001}{Each variable described in the metadata is present in the
#'     dataset (traceability).}
#'   \item{T-ADAM-002}{Each dataset column is described in the metadata; columns
#'     that are not are flagged (traceability).}
#'   \item{Q-ADAM-001}{Present variables have the data type family the metadata
#'     specifies (quality).}
#'   \item{Q-ADAM-002}{Present variables carry a non-empty label attribute
#'     (usability).}
#' }
#'
#' @param data A data.frame: one ADaM dataset.
#' @param metadata A `Metacore` object or a data.frame accepted by
#'   [as_variable_metadata()].
#' @param ctx An [r4subcore::r4sub_run_context] providing run and study
#'   identifiers.
#' @param dataset_name Character or `NULL`. Which dataset in `metadata` to check
#'   `data` against. May be omitted when the metadata covers a single dataset.
#' @param source_name Character. Label recorded as the evidence source.
#'   Default `"adam"`.
#' @param source_version Character or `NULL`. Optional version label.
#'
#' @return A data.frame conforming to the R4SUB evidence schema.
#'
#' @seealso [metacore_to_evidence()], [submission_readiness()]
#'
#' @examples
#' meta <- data.frame(
#'   dataset  = "ADSL",
#'   variable = c("USUBJID", "AGE", "SEX"),
#'   label    = c("Unique Subject Identifier", "Age", "Sex"),
#'   type     = c("text", "integer", "text"),
#'   stringsAsFactors = FALSE
#' )
#' adsl <- data.frame(
#'   USUBJID = c("01-001", "01-002"),
#'   AGE     = c(54, 61),
#'   stringsAsFactors = FALSE
#' )
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
#' ev <- suppressMessages(adam_to_evidence(adsl, meta, ctx, dataset_name = "ADSL"))
#' nrow(ev)
#'
#' @importFrom cli cli_abort cli_alert_info
#' @export
adam_to_evidence <- function(data,
                             metadata,
                             ctx,
                             dataset_name = NULL,
                             source_name = "adam",
                             source_version = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame.")
  }
  if (!inherits(ctx, "r4sub_run_context")) {
    cli::cli_abort("{.arg ctx} must be an {.cls r4sub_run_context}.")
  }

  meta <- as_variable_metadata(metadata)
  dataset_name <- resolve_dataset_name(meta, dataset_name)
  meta <- meta[meta$dataset == dataset_name, , drop = FALSE]
  if (nrow(meta) == 0L) {
    cli::cli_abort("No metadata found for dataset {.val {dataset_name}}.")
  }

  rows <- rbind(
    presence_rows(data, meta, dataset_name, source_name, source_version),
    unexpected_rows(data, meta, dataset_name, source_name, source_version),
    type_rows(data, meta, dataset_name, source_name, source_version),
    label_rows(data, meta, dataset_name, source_name, source_version)
  )

  cli::cli_alert_info(
    "adam_to_evidence: {nrow(rows)} row{?s} for dataset {.val {dataset_name}}"
  )
  r4subcore::as_evidence(rows, ctx = ctx)
}

# Choose the dataset to check against, requiring an explicit name only when the
# metadata is ambiguous.
resolve_dataset_name <- function(meta, dataset_name) {
  if (!is.null(dataset_name)) {
    return(dataset_name)
  }
  datasets <- unique(meta$dataset)
  if (length(datasets) == 1L) {
    return(datasets)
  }
  cli::cli_abort(c(
    "{.arg dataset_name} is required when metadata covers several datasets.",
    "i" = "Metadata covers: {.val {datasets}}."
  ))
}

# T-ADAM-001: expected variable present in the dataset.
presence_rows <- function(data, meta, dataset_name, source_name, source_version) {
  present <- meta$variable %in% names(data)

  data.frame(
    asset_type       = "dataset",
    asset_id         = dataset_name,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "T-ADAM-001",
    indicator_name   = "Expected Variable Present",
    indicator_domain = "trace",
    severity         = ifelse(present, "info", "high"),
    result           = ifelse(present, "pass", "fail"),
    metric_value     = as.double(present),
    metric_unit      = "score",
    message          = ifelse(
      present,
      sprintf("Variable %s is present in %s", meta$variable, dataset_name),
      sprintf("Variable %s is described in metadata but missing from %s",
              meta$variable, dataset_name)
    ),
    location         = paste0(dataset_name, ":", meta$variable),
    evidence_payload = mapply(
      payload_json,
      variable = meta$variable, dataset = dataset_name, present = tolower(present),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# T-ADAM-002: dataset column not described in the metadata.
unexpected_rows <- function(data, meta, dataset_name, source_name, source_version) {
  extra <- setdiff(names(data), meta$variable)
  if (length(extra) == 0L) {
    return(pre_evidence_template())
  }

  data.frame(
    asset_type       = "dataset",
    asset_id         = dataset_name,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "T-ADAM-002",
    indicator_name   = "Unexpected Variable in Dataset",
    indicator_domain = "trace",
    severity         = "low",
    result           = "warn",
    metric_value     = 0,
    metric_unit      = "score",
    message          = sprintf("Variable %s is in %s but not described in metadata",
                               extra, dataset_name),
    location         = paste0(dataset_name, ":", extra),
    evidence_payload = mapply(
      payload_json,
      variable = extra, dataset = dataset_name,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Q-ADAM-001: present variable's type family matches the metadata.
type_rows <- function(data, meta, dataset_name, source_name, source_version) {
  keep <- (meta$variable %in% names(data)) & !is.na(normalize_type(meta$type))
  meta <- meta[keep, , drop = FALSE]
  if (nrow(meta) == 0L) {
    return(pre_evidence_template())
  }

  expected <- normalize_type(meta$type)
  actual   <- vapply(meta$variable, function(v) observed_type(data[[v]]), character(1))
  match_ok <- !is.na(actual) & actual == expected

  data.frame(
    asset_type       = "dataset",
    asset_id         = dataset_name,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-ADAM-001",
    indicator_name   = "Variable Type Matches Metadata",
    indicator_domain = "quality",
    severity         = ifelse(match_ok, "info", "medium"),
    result           = ifelse(match_ok, "pass", "warn"),
    metric_value     = as.double(match_ok),
    metric_unit      = "score",
    message          = ifelse(
      match_ok,
      sprintf("Variable %s in %s is %s as expected", meta$variable, dataset_name, expected),
      sprintf("Variable %s in %s is %s but metadata expects %s",
              meta$variable, dataset_name, actual, expected)
    ),
    location         = paste0(dataset_name, ":", meta$variable),
    evidence_payload = mapply(
      payload_json,
      variable = meta$variable, dataset = dataset_name,
      expected = expected, actual = actual,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Q-ADAM-002: present variable carries a non-empty label attribute.
label_rows <- function(data, meta, dataset_name, source_name, source_version) {
  meta <- meta[meta$variable %in% names(data), , drop = FALSE]
  if (nrow(meta) == 0L) {
    return(pre_evidence_template())
  }

  labelled <- vapply(meta$variable, function(v) has_text(column_label(data[[v]])), logical(1))

  data.frame(
    asset_type       = "dataset",
    asset_id         = dataset_name,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-ADAM-002",
    indicator_name   = "Variable Has Label",
    indicator_domain = "usability",
    severity         = ifelse(labelled, "info", "medium"),
    result           = ifelse(labelled, "pass", "fail"),
    metric_value     = as.double(labelled),
    metric_unit      = "score",
    message          = ifelse(
      labelled,
      sprintf("Variable %s in %s carries a label", meta$variable, dataset_name),
      sprintf("Variable %s in %s has no label attribute", meta$variable, dataset_name)
    ),
    location         = paste0(dataset_name, ":", meta$variable),
    evidence_payload = mapply(
      payload_json,
      variable = meta$variable, dataset = dataset_name, labelled = tolower(labelled),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}
