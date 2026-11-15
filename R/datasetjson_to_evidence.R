#' Convert Dataset-JSON File Metadata to Submission Readiness Evidence
#'
#' Turns the metadata of a CDISC Dataset-JSON data-exchange file - either a
#' parsed Dataset-JSON object or a plain data.frame of its column metadata -
#' into standardized R4SUB evidence rows describing whether that file declares
#' the structural keys the Dataset-JSON standard defines. Where
#' [metacore_to_evidence()] and [dataset_metadata_to_evidence()] report
#' documentation presence drawn from a `metacore` specification, this adapter
#' reports the structural-presence counterpart drawn from the Dataset-JSON
#' *exchange file* itself, so a pipeline that ships Dataset-JSON can feed the
#' same evidence table and Submission Confidence Index.
#'
#' Two indicators are evaluated, under identifiers in a dedicated `Q-DJSON`
#' family that does not collide with the `Q-DEFINE-001` through `Q-DEFINE-005`,
#' `Q-ADAM-*`, or `T-ADAM-*` indicators already in use, so Dataset-JSON evidence
#' stays separately identifiable in scoring and explainability:
#' \describe{
#'   \item{Q-DJSON-001}{The Dataset-JSON file documents its dataset-level
#'     metadata: a dataset name, a dataset label, and a record count. Emitted
#'     once per dataset.}
#'   \item{Q-DJSON-002}{Each column in the Dataset-JSON file declares the
#'     required column metadata keys: a name, a label, and a data type. Emitted
#'     once per column.}
#' }
#'
#' This is structural-*presence* evidence only. The adapter records whether each
#' Dataset-JSON metadata key is populated, exactly as the file reports it. It
#' does not judge whether the declared data types are correct, whether the
#' record count matches the number of rows, or whether the values conform to any
#' controlled terminology.
#'
#' @param metadata A parsed Dataset-JSON object or a data.frame accepted by
#'   [as_datasetjson_metadata()].
#' @param ctx An [r4subcore::r4sub_run_context] providing run and study
#'   identifiers.
#' @param dataset_name Character or `NULL`. Used as the asset identifier for a
#'   dataset whose name the file does not declare. Ignored when the metadata
#'   declares a dataset name.
#' @param source_name Character. Label recorded as the evidence source.
#'   Default `"datasetjson"`.
#' @param source_version Character or `NULL`. Optional version label for the
#'   metadata source.
#'
#' @return A data.frame conforming to the R4SUB evidence schema: one
#'   `Q-DJSON-001` row per dataset and one `Q-DJSON-002` row per column.
#'
#' @seealso [as_datasetjson_metadata()], [dataset_metadata_to_evidence()],
#'   [metacore_to_evidence()], [submission_readiness()]
#'
#' @examples
#' # A synthetic Dataset-JSON column-metadata frame (no external package needed).
#' cols <- data.frame(
#'   dataset   = "ADSL",
#'   dataset_label = "Subject-Level Analysis Dataset",
#'   records   = 306L,
#'   name      = c("STUDYID", "USUBJID", "AGE"),
#'   label     = c("Study Identifier", "Unique Subject Identifier", ""),
#'   dataType  = c("string", "string", "integer"),
#'   stringsAsFactors = FALSE
#' )
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("CDISCPILOT01", "DEV"))
#' ev  <- suppressMessages(datasetjson_to_evidence(cols, ctx))
#' table(ev$indicator_id, ev$result)
#'
#' @importFrom cli cli_alert_info cli_abort
#' @export
datasetjson_to_evidence <- function(metadata,
                                    ctx,
                                    dataset_name = NULL,
                                    source_name = "datasetjson",
                                    source_version = NULL) {
  if (!inherits(ctx, "r4sub_run_context")) {
    cli::cli_abort("{.arg ctx} must be an {.cls r4sub_run_context}.")
  }
  assert_scalar_string(dataset_name, "dataset_name", allow_null = TRUE)
  assert_scalar_string(source_name, "source_name")
  assert_scalar_string(source_version, "source_version", allow_null = TRUE)

  meta <- as_datasetjson_metadata(metadata)
  if (nrow(meta) == 0L) {
    cli::cli_abort("{.arg metadata} describes no Dataset-JSON columns.")
  }

  # Every evidence row needs a non-missing asset id. When the file does not
  # declare a dataset name, fall back to the caller's dataset_name (or a
  # placeholder); the "name present" facet still reflects the metadata itself.
  asset <- meta$dataset
  asset[is.na(asset)] <- dataset_name %||% "(unnamed)"
  meta$asset_id <- asset

  rows <- rbind(
    datasetjson_file_rows(meta, source_name, source_version),
    datasetjson_column_rows(meta, source_name, source_version)
  )

  cli::cli_alert_info(
    "datasetjson_to_evidence: {nrow(rows)} row{?s} from {nrow(meta)} column{?s}"
  )
  r4subcore::as_evidence(rows, ctx = ctx)
}

# Q-DJSON-001: Dataset-JSON file documents its dataset-level metadata
# (dataset name + label + record count present). One row per dataset.
datasetjson_file_rows <- function(meta, source_name, source_version) {
  first <- !duplicated(meta$asset_id)
  ds        <- meta$asset_id[first]
  has_name  <- !is.na(meta$dataset[first])
  has_label <- !is.na(meta$dataset_label[first])
  has_recs  <- !is.na(meta$records[first])
  records   <- meta$records[first]

  n_present  <- has_name + has_label + has_recs
  documented <- n_present == 3L

  result   <- ifelse(documented, "pass", ifelse(n_present == 0L, "fail", "warn"))
  severity <- ifelse(documented, "info", ifelse(n_present == 0L, "high", "medium"))
  message  <- mapply(
    datasetjson_file_message,
    dataset = ds, has_name = has_name, has_label = has_label, has_recs = has_recs,
    USE.NAMES = FALSE
  )

  data.frame(
    asset_type       = "dataset",
    asset_id         = ds,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-DJSON-001",
    indicator_name   = "Dataset-JSON File Metadata Present",
    indicator_domain = "quality",
    severity         = severity,
    result           = result,
    metric_value     = n_present / 3,
    metric_unit      = "score",
    message          = message,
    location         = ds,
    evidence_payload = mapply(
      payload_json,
      dataset = ds, has_name = tolower(has_name), has_label = tolower(has_label),
      has_records = tolower(has_recs),
      records = ifelse(is.na(records), "", as.character(records)),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Q-DJSON-002: each Dataset-JSON column declares the required metadata keys
# (name + label + data type present). One row per column.
datasetjson_column_rows <- function(meta, source_name, source_version) {
  has_name  <- !is.na(meta$name)
  has_label <- !is.na(meta$label)
  has_type  <- !is.na(meta$data_type)

  n_present  <- has_name + has_label + has_type
  documented <- n_present == 3L

  result   <- ifelse(documented, "pass", ifelse(n_present == 0L, "fail", "warn"))
  severity <- ifelse(documented, "info", ifelse(n_present == 0L, "high", "medium"))

  # A column may legitimately lack a declared name; label it positionally so the
  # location is never "<dataset>:NA".
  col_label <- meta$name
  missing_nm <- is.na(col_label)
  col_label[missing_nm] <- paste0("col", seq_len(nrow(meta)))[missing_nm]

  message <- mapply(
    datasetjson_column_message,
    dataset = meta$asset_id, column = col_label,
    has_name = has_name, has_label = has_label, has_type = has_type,
    USE.NAMES = FALSE
  )

  data.frame(
    asset_type       = "dataset",
    asset_id         = meta$asset_id,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-DJSON-002",
    indicator_name   = "Dataset-JSON Column Metadata Present",
    indicator_domain = "quality",
    severity         = severity,
    result           = result,
    metric_value     = n_present / 3,
    metric_unit      = "score",
    message          = message,
    location         = paste0(meta$asset_id, ":", col_label),
    evidence_payload = mapply(
      payload_json,
      dataset = meta$asset_id, column = col_label,
      has_name = tolower(has_name), has_label = tolower(has_label),
      has_data_type = tolower(has_type), data_type = meta$data_type,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Human-readable message for one dataset's file-level metadata presence.
datasetjson_file_message <- function(dataset, has_name, has_label, has_recs) {
  if (has_name && has_label && has_recs) {
    return(sprintf(
      "Dataset-JSON file for %s documents its dataset-level metadata (name + label + record count)",
      dataset
    ))
  }
  missing <- c(
    if (!has_name) "dataset name",
    if (!has_label) "dataset label",
    if (!has_recs) "record count"
  )
  sprintf("Dataset-JSON file for %s is missing: %s", dataset,
          paste(missing, collapse = ", "))
}

# Human-readable message for one column's metadata presence.
datasetjson_column_message <- function(dataset, column, has_name, has_label, has_type) {
  if (has_name && has_label && has_type) {
    return(sprintf("Column %s in Dataset-JSON %s declares name, label, and data type",
                   column, dataset))
  }
  missing <- c(
    if (!has_name) "name",
    if (!has_label) "label",
    if (!has_type) "data type"
  )
  sprintf("Column %s in Dataset-JSON %s is missing: %s", column, dataset,
          paste(missing, collapse = ", "))
}
