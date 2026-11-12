#' Convert Codelist Metadata to Submission Readiness Evidence
#'
#' Turns the controlled-terminology side of dataset metadata - either a
#' `metacore` object or a plain data.frame - into standardized R4SUB evidence
#' rows describing whether each variable that *references* a codelist actually
#' has that codelist documented. Where [metacore_to_evidence()] reports
#' variable-level documentation presence and [dataset_metadata_to_evidence()]
#' the dataset-level counterpart, this adapter reports the controlled-terminology
#' counterpart drawn from a `metacore` object's `value_spec` (which variables
#' point at which codelists) and `codelist` (how those codelists are defined).
#'
#' One indicator is evaluated per variable that references a codelist, under the
#' codelist-documentation identifier `Q-DEFINE-005`. This is a new identifier,
#' distinct from the dataset- and variable-level `Q-DEFINE-001` through
#' `Q-DEFINE-004` already in use, so the controlled-terminology check stays
#' separately identifiable in scoring and explainability:
#' \describe{
#'   \item{Q-DEFINE-005}{The codelist a variable references is documented: it
#'     resolves to a codelist that either enumerates at least one term or is a
#'     reference to an external dictionary (e.g. MedDRA, WHODrug).}
#' }
#'
#' This is documentation-*presence* evidence only. The adapter records whether
#' the referenced controlled terminology is documented at all; it does not judge
#' whether the terms are the correct CDISC Controlled Terminology values, nor
#' whether the dataset's data conforms to them. Variables that reference no
#' codelist produce no rows.
#'
#' @param metadata A `Metacore` object or a data.frame accepted by
#'   [as_codelist_metadata()].
#' @param ctx An [r4subcore::r4sub_run_context] providing run and study
#'   identifiers.
#' @param source_name Character. Label recorded as the evidence source.
#'   Default `"metacore"`.
#' @param source_version Character or `NULL`. Optional version label for the
#'   metadata source.
#'
#' @return A data.frame conforming to the R4SUB evidence schema, one row per
#'   variable that references a codelist.
#'
#' @seealso [metacore_to_evidence()], [dataset_metadata_to_evidence()],
#'   [as_codelist_metadata()], [submission_readiness()]
#'
#' @examplesIf requireNamespace("metacore", quietly = TRUE)
#' # The CDISC pilot ADaM metacore specification shipped with metacore.
#' load(metacore::metacore_example("pilot_ADaM.rda"))
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("CDISCPILOT01", "DEV"))
#' ev  <- suppressMessages(codelist_to_evidence(metacore, ctx))
#' table(ev$result)
#'
#' @importFrom cli cli_alert_info cli_abort
#' @export
codelist_to_evidence <- function(metadata,
                                 ctx,
                                 source_name = "metacore",
                                 source_version = NULL) {
  if (!inherits(ctx, "r4sub_run_context")) {
    cli::cli_abort("{.arg ctx} must be an {.cls r4sub_run_context}.")
  }
  assert_scalar_string(source_name, "source_name")
  assert_scalar_string(source_version, "source_version", allow_null = TRUE)
  meta <- as_codelist_metadata(metadata)
  if (nrow(meta) == 0L) {
    cli::cli_abort(
      "{.arg metadata} describes no variables that reference a codelist."
    )
  }

  rows <- codelist_documentation_rows(meta, source_name, source_version)

  cli::cli_alert_info(
    "codelist_to_evidence: {nrow(rows)} row{?s} from {nrow(meta)} codelist reference{?s}"
  )
  r4subcore::as_evidence(rows, ctx = ctx)
}

# Q-DEFINE-005: the codelist a variable references is documented (resolves to a
# codelist that either enumerates terms or is an external-dictionary reference).
codelist_documentation_rows <- function(meta, source_name, source_version) {
  external <- meta$external
  external[is.na(external)] <- FALSE
  has_terms <- !is.na(meta$n_terms) & meta$n_terms > 0L

  documented <- external | has_terms            # referenced CT is documented
  resolved   <- external | !is.na(meta$n_terms) # codelist entry exists at all

  result   <- ifelse(documented, "pass", ifelse(resolved, "warn", "fail"))
  severity <- ifelse(documented, "info", ifelse(resolved, "medium", "high"))
  metric   <- ifelse(documented, 1, ifelse(resolved, 0.5, 0))
  message  <- mapply(
    codelist_documentation_message,
    dataset = meta$dataset, variable = meta$variable, code_id = meta$code_id,
    external = external, has_terms = has_terms, resolved = resolved,
    n_terms = meta$n_terms,
    USE.NAMES = FALSE
  )

  data.frame(
    asset_type       = "define",
    asset_id         = meta$dataset,
    source_name      = source_name,
    source_version   = source_version %||% NA_character_,
    indicator_id     = "Q-DEFINE-005",
    indicator_name   = "Codelist Documented in Metadata",
    indicator_domain = "quality",
    severity         = severity,
    result           = result,
    metric_value     = metric,
    metric_unit      = "score",
    message          = message,
    location         = paste0(meta$dataset, ":", meta$variable),
    evidence_payload = mapply(
      payload_json,
      variable = meta$variable, dataset = meta$dataset,
      code_id = meta$code_id, external = tolower(external),
      n_terms = ifelse(is.na(meta$n_terms), "", as.character(meta$n_terms)),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Human-readable message for one variable's codelist-documentation presence.
codelist_documentation_message <- function(dataset, variable, code_id,
                                           external, has_terms, resolved,
                                           n_terms) {
  loc <- sprintf("Variable %s in %s references codelist %s", variable, dataset, code_id)
  if (external) {
    return(sprintf("%s, documented as an external dictionary", loc))
  }
  if (has_terms) {
    return(sprintf("%s, documented with %d term%s", loc, as.integer(n_terms),
                   if (as.integer(n_terms) == 1L) "" else "s"))
  }
  if (resolved) {
    return(sprintf("%s, which documents no terms", loc))
  }
  sprintf("%s, which is not documented", loc)
}
