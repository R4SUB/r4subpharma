#' Compose All 'pharmaverse' Adapters into One Evidence Table
#'
#' A convenience wrapper that runs the individual r4subpharma adapters and
#' returns their combined output in a single call. It composes
#' [metacore_to_evidence()] (variable-level documentation),
#' [dataset_metadata_to_evidence()] (dataset-level documentation), and - when
#' an ADaM dataset is supplied - [adam_to_evidence()] (dataset conformance),
#' then row-binds the results into one R4SUB evidence table.
#'
#' This is pure composition: it introduces no new indicators or semantics of
#' its own. The result is exactly the row-bind of the adapters' individual
#' outputs, so every indicator, severity, result, and message is identical to
#' calling the adapters separately and binding them yourself.
#'
#' @param metadata A `Metacore` object or a data.frame. It is forwarded
#'   unchanged to [metacore_to_evidence()] and [dataset_metadata_to_evidence()]
#'   (and, when `data` is supplied, to [adam_to_evidence()]).
#' @param ctx An [r4subcore::r4sub_run_context] providing run and study
#'   identifiers.
#' @param data An optional ADaM data.frame. When supplied, [adam_to_evidence()]
#'   is run as well and its rows are included.
#' @param dataset_name Character or `NULL`. Passed to [adam_to_evidence()] to
#'   select which dataset in `metadata` to check `data` against. Ignored when
#'   `data` is `NULL`.
#' @param source_name Character. Source label for the metadata-derived evidence
#'   (both [metacore_to_evidence()] and [dataset_metadata_to_evidence()]).
#'   Default `"metacore"`.
#' @param source_version Character or `NULL`. Optional version label for the
#'   metadata source.
#' @param adam_source_name Character. Source label for the ADaM-derived
#'   evidence. Default `"adam"`. Ignored when `data` is `NULL`.
#' @param adam_source_version Character or `NULL`. Optional version label for
#'   the ADaM source. Ignored when `data` is `NULL`.
#'
#' @return A data.frame conforming to the R4SUB evidence schema: the row-bind of
#'   the composed adapters' outputs.
#'
#' @seealso [metacore_to_evidence()], [dataset_metadata_to_evidence()],
#'   [adam_to_evidence()], [submission_readiness()]
#'
#' @examplesIf requireNamespace("metacore", quietly = TRUE)
#' # The CDISC pilot ADaM metacore specification shipped with metacore.
#' load(metacore::metacore_example("pilot_ADaM.rda"))
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("CDISCPILOT01", "DEV"))
#' ev  <- suppressMessages(pharma_evidence(metacore, ctx))
#' table(ev$indicator_id)
#'
#' @importFrom cli cli_alert_info cli_abort
#' @export
pharma_evidence <- function(metadata,
                            ctx,
                            data = NULL,
                            dataset_name = NULL,
                            source_name = "metacore",
                            source_version = NULL,
                            adam_source_name = "adam",
                            adam_source_version = NULL) {
  if (!inherits(ctx, "r4sub_run_context")) {
    cli::cli_abort("{.arg ctx} must be an {.cls r4sub_run_context}.")
  }

  frames <- list(
    metacore_to_evidence(
      metadata, ctx,
      source_name = source_name, source_version = source_version
    ),
    dataset_metadata_to_evidence(
      metadata, ctx,
      source_name = source_name, source_version = source_version
    )
  )

  if (!is.null(data)) {
    frames <- c(frames, list(
      adam_to_evidence(
        data, metadata, ctx,
        dataset_name = dataset_name,
        source_name = adam_source_name, source_version = adam_source_version
      )
    ))
  }

  combined <- do.call(rbind, frames)
  rownames(combined) <- NULL

  cli::cli_alert_info(
    "pharma_evidence: combined {length(frames)} adapter{?s} into {nrow(combined)} row{?s}"
  )
  r4subcore::as_evidence(combined, ctx = ctx)
}
