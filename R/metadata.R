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
#' @examplesIf requireNamespace("r4subdata", quietly = TRUE)
#' # The example ADaM metadata shipped in r4subdata.
#' meta <- as_variable_metadata(r4subdata::adam_metadata)
#' head(meta)
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
#
# metacore spec tables commonly key a variable as "DATASET.VARIABLE" (e.g.
# "ADSL.AGE") while ds_vars uses the plain name. We match on the qualified key
# first and fall back to the plain variable name, so both conventions resolve.
metacore_variable_metadata <- function(mc) {
  pick <- function(df, name) if (name %in% names(df)) df[[name]] else NA
  strip_ds <- function(x) sub("^[^.]+\\.", "", as.character(x))

  ds_vars  <- as.data.frame(mc$ds_vars)
  var_spec <- as.data.frame(mc$var_spec)
  val_spec <- as.data.frame(mc$value_spec)
  derivs   <- as.data.frame(mc$derivations)

  dataset  <- as.character(pick(ds_vars, "dataset"))
  variable <- as.character(pick(ds_vars, "variable"))

  # Variable-level attributes (label, type) live in var_spec.
  vs_var_raw   <- as.character(pick(var_spec, "variable"))
  vs_var_plain <- strip_ds(vs_var_raw)
  vs_idx <- match(paste0(dataset, ".", variable), vs_var_raw)
  vs_miss <- is.na(vs_idx)
  vs_idx[vs_miss] <- match(variable[vs_miss], vs_var_plain)
  label  <- blank_to_na(pick(var_spec, "label")[vs_idx])
  type   <- blank_to_na(pick(var_spec, "type")[vs_idx])

  # Origin and derivation live in value_spec, keyed by dataset + variable. Where
  # a variable has several value-level rows we take the first.
  vsp_var_plain <- strip_ds(pick(val_spec, "variable"))
  vsp_ds <- as.character(pick(val_spec, "dataset"))
  if (all(is.na(vsp_ds))) {
    # No dataset column: recover it from the qualified "DATASET.VARIABLE" key.
    vsp_ds <- sub("\\..*$", "", as.character(pick(val_spec, "variable")))
  }
  vsp_key <- paste(vsp_ds, vsp_var_plain)
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

# Dataset-metadata seam --------------------------------------------------------
#
# `dataset_metadata_to_evidence()` works at the dataset level rather than the
# variable level, so it needs its own small contract: a "dataset metadata" table
# with one row per dataset and the columns
#   dataset, label, structure, n_keys
# `as_dataset_metadata()` turns a 'metacore' object (its `ds_spec` plus the key
# variables recorded in `ds_vars`) or a plain data.frame into that contract.
# Keeping this seam explicit means the dataset-level evidence logic can be
# exercised with an ordinary data.frame and does not depend on 'metacore'.

dataset_metadata_columns <- function() {
  c("dataset", "label", "structure", "n_keys")
}

#' Coerce Metadata to the Dataset-Metadata Contract
#'
#' Normalizes the accepted metadata inputs into a tidy table with one row per
#' dataset, describing how completely each dataset itself is documented. A
#' `metacore` object contributes its `ds_spec` (dataset label and structure)
#' and the count of key variables recorded in `ds_vars` (those with a
#' `key_seq`); a data.frame is checked for the required `dataset` column and
#' completed with any missing optional columns.
#'
#' @param metadata Either a `Metacore` object or a data.frame with at least the
#'   column `dataset`. Optional columns `label`, `structure`, and `n_keys` are
#'   used when present. A logical `has_keys` column is accepted in place of
#'   `n_keys`.
#'
#' @return A [tibble][tibble::tibble] with columns `dataset`, `label`,
#'   `structure`, and `n_keys`.
#'
#' @seealso [dataset_metadata_to_evidence()], [as_variable_metadata()]
#'
#' @examplesIf requireNamespace("metacore", quietly = TRUE)
#' # The CDISC pilot ADaM metacore specification shipped with metacore.
#' load(metacore::metacore_example("pilot_ADaM.rda"))
#' head(as_dataset_metadata(metacore))
#'
#' @importFrom cli cli_abort
#' @export
as_dataset_metadata <- function(metadata) {
  if (inherits(metadata, "Metacore")) {
    return(metacore_dataset_metadata(metadata))
  }
  if (is.data.frame(metadata)) {
    return(coerce_dataset_metadata_df(metadata))
  }
  cli::cli_abort(c(
    "{.arg metadata} must be a {.cls Metacore} object or a data.frame.",
    "x" = "Got {.cls {class(metadata)[1]}}."
  ))
}

# Complete a user-supplied dataset-metadata data.frame.
coerce_dataset_metadata_df <- function(x) {
  if (!"dataset" %in% names(x)) {
    cli::cli_abort(
      "Dataset-metadata data.frame is missing required column: {.field dataset}"
    )
  }

  get_col <- function(name) {
    if (name %in% names(x)) blank_to_na(x[[name]]) else NA_character_
  }

  dataset   <- as.character(x$dataset)
  label     <- get_col("label")
  structure <- get_col("structure")

  if ("n_keys" %in% names(x)) {
    n_keys <- as.integer(x$n_keys)
    n_keys[is.na(n_keys)] <- 0L
  } else if ("has_keys" %in% names(x)) {
    n_keys <- as.integer(as.logical(x$has_keys))
    n_keys[is.na(n_keys)] <- 0L
  } else {
    n_keys <- rep(0L, length(dataset))
  }

  tibble::tibble(dataset, label, structure, n_keys)
}

# Unpack a 'metacore' object's dataset-level metadata. Reached only when a
# Metacore object is supplied, so 'metacore' is necessarily attached. Key
# variables are those carrying a key_seq in ds_vars.
metacore_dataset_metadata <- function(mc) {
  pick <- function(df, name) if (name %in% names(df)) df[[name]] else NA

  ds_spec <- as.data.frame(mc$ds_spec)
  ds_vars <- as.data.frame(mc$ds_vars)

  dataset   <- as.character(pick(ds_spec, "dataset"))
  label     <- blank_to_na(pick(ds_spec, "label"))
  structure <- blank_to_na(pick(ds_spec, "structure"))

  dv_dataset <- as.character(pick(ds_vars, "dataset"))
  dv_key     <- pick(ds_vars, "key_seq")
  is_key     <- !is.na(dv_key)
  key_counts <- tapply(is_key, dv_dataset, sum)
  n_keys     <- as.integer(key_counts[dataset])
  n_keys[is.na(n_keys)] <- 0L

  tibble::tibble(dataset, label, structure, n_keys)
}
