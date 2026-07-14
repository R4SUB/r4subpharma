# Variable-metadata seam -------------------------------------------------------
#
# Both adapters operate on a single, simple contract: a "variable metadata"
# table with one row per dataset variable and the columns
#   dataset, variable, label, type, origin, derivation, is_derived
# `as_variable_metadata()` turns the two accepted inputs - a 'metacore' object
# or a plain data.frame - into that contract. Keeping this seam explicit means
# the evidence logic can be exercised with an ordinary data.frame and does not
# depend on 'metacore' being installed.

metadata_columns <- function() {
  c("dataset", "variable", "label", "type", "origin", "derivation", "is_derived")
}

#' Coerce Metadata to the Variable-Metadata Contract
#'
#' Normalizes the accepted metadata inputs into a single tidy table with one row
#' per dataset variable. A `metacore` object is unpacked
#' into its dataset, variable, and derivation components; a data.frame is checked
#' for the required `dataset` and `variable` columns and completed with any
#' missing optional columns.
#'
#' @param metadata Either a `Metacore` object or a data.frame with at least the
#'   columns `dataset` and `variable`. Optional columns `label`, `type`,
#'   `origin`, `derivation`, and `is_derived` are used when present.
#'
#' @return A [tibble][tibble::tibble] with columns `dataset`, `variable`,
#'   `label`, `type`, `origin`, `derivation`, and `is_derived`.
#'
#' @examples
#' meta <- data.frame(
#'   dataset  = "ADSL",
#'   variable = c("USUBJID", "AGE"),
#'   label    = c("Unique Subject Identifier", "Age"),
#'   type     = c("text", "integer"),
#'   stringsAsFactors = FALSE
#' )
#' as_variable_metadata(meta)
#'
#' @importFrom cli cli_abort
#' @export
as_variable_metadata <- function(metadata) {
  if (inherits(metadata, "Metacore")) {
    return(metacore_variable_metadata(metadata))
  }
  if (is.data.frame(metadata)) {
    return(coerce_metadata_df(metadata))
  }
  cli::cli_abort(c(
    "{.arg metadata} must be a {.cls Metacore} object or a data.frame.",
    "x" = "Got {.cls {class(metadata)[1]}}."
  ))
}

# Complete a user-supplied metadata data.frame.
coerce_metadata_df <- function(x) {
  missing_req <- setdiff(c("dataset", "variable"), names(x))
  if (length(missing_req) > 0L) {
    cli::cli_abort(
      "Metadata data.frame is missing required column{?s}: {.field {missing_req}}"
    )
  }

  get_col <- function(name) {
    if (name %in% names(x)) blank_to_na(x[[name]]) else NA_character_
  }

  dataset    <- as.character(x$dataset)
  variable   <- as.character(x$variable)
  label      <- get_col("label")
  type       <- get_col("type")
  origin     <- get_col("origin")
  derivation <- get_col("derivation")

  if ("is_derived" %in% names(x)) {
    is_derived <- as.logical(x$is_derived)
  } else {
    is_derived <- derived_flag(origin, derivation)
  }

  tibble::tibble(
    dataset, variable, label, type, origin, derivation, is_derived
  )
}

# Infer whether a variable is derived from its origin and derivation text.
derived_flag <- function(origin, derivation) {
  from_deriv  <- !is.na(derivation) & nzchar(derivation)
  from_origin <- !is.na(origin) &
    grepl("deriv|assign|algorithm", tolower(origin))
  from_deriv | from_origin
}

# Unpack a 'metacore' object into the variable-metadata contract. Reached only
# when a Metacore object is supplied, so 'metacore' is necessarily attached.
metacore_variable_metadata <- function(mc) {
  pick <- function(df, name) if (name %in% names(df)) df[[name]] else NA

  ds_vars  <- as.data.frame(mc$ds_vars)
  var_spec <- as.data.frame(mc$var_spec)
  val_spec <- as.data.frame(mc$value_spec)
  derivs   <- as.data.frame(mc$derivations)

  dataset  <- as.character(pick(ds_vars, "dataset"))
  variable <- as.character(pick(ds_vars, "variable"))

  # Variable-level attributes (label, type) live in var_spec, keyed by variable.
  vs_var <- as.character(pick(var_spec, "variable"))
  label  <- blank_to_na(pick(var_spec, "label")[match(variable, vs_var)])
  type   <- blank_to_na(pick(var_spec, "type")[match(variable, vs_var)])

  # Origin and derivation live in value_spec, keyed by dataset + variable. Where
  # a variable has several value-level rows we take the first.
  vsp_key <- paste(pick(val_spec, "dataset"), pick(val_spec, "variable"))
  idx     <- match(paste(dataset, variable), vsp_key)
  origin  <- blank_to_na(pick(val_spec, "origin")[idx])
  der_id  <- pick(val_spec, "derivation_id")[idx]

  der_lookup <- pick(derivs, "derivation_id")
  derivation <- blank_to_na(pick(derivs, "derivation")[match(der_id, der_lookup)])

  tibble::tibble(
    dataset,
    variable,
    label,
    type,
    origin,
    derivation,
    is_derived = derived_flag(origin, derivation)
  )
}
