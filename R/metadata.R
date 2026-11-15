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

# Codelist-metadata seam -------------------------------------------------------
#
# `codelist_to_evidence()` works at the controlled-terminology level: one row
# per dataset variable that *references* a codelist, with the columns
#   dataset, variable, code_id, n_terms, external
# `as_codelist_metadata()` turns a 'metacore' object (its `value_spec`, which
# links a variable to a codelist via `code_id`, joined to its `codelist` table)
# or a plain data.frame into that contract. Keeping this seam explicit means the
# codelist evidence logic can be exercised with an ordinary data.frame and does
# not depend on 'metacore'.
#
#   n_terms  Integer count of enumerated terms the referenced codelist
#            documents. NA when the reference is to an external dictionary
#            (counted through `external`) or cannot be resolved to a codelist.
#   external TRUE when the referenced codelist is an external-dictionary
#            reference (e.g. MedDRA, WHODrug) rather than an enumerated list.

codelist_metadata_columns <- function() {
  c("dataset", "variable", "code_id", "n_terms", "external")
}

#' Coerce Metadata to the Codelist-Metadata Contract
#'
#' Normalizes the accepted metadata inputs into a tidy table with one row per
#' dataset variable that references a codelist, describing whether that
#' controlled terminology is documented. A `metacore` object contributes its
#' `value_spec` (which variables reference which codelists, via `code_id`)
#' joined to its `codelist` table (how each codelist is defined); a data.frame
#' is checked for the required `dataset`, `variable`, and `code_id` columns and
#' completed with any missing optional columns. Rows whose `code_id` is missing
#' or blank - variables that reference no codelist - are dropped.
#'
#' @param metadata Either a `Metacore` object or a data.frame with at least the
#'   columns `dataset`, `variable`, and `code_id`. Optional columns `n_terms`
#'   (integer count of enumerated terms) and `external` (logical; `TRUE` for an
#'   external-dictionary reference) are used when present. A logical `has_terms`
#'   column is accepted in place of `n_terms`, and a `codelist_type` column
#'   equal to `"external_library"` is accepted in place of `external`.
#'
#' @return A [tibble][tibble::tibble] with columns `dataset`, `variable`,
#'   `code_id`, `n_terms`, and `external`.
#'
#' @seealso [codelist_to_evidence()], [as_variable_metadata()],
#'   [as_dataset_metadata()]
#'
#' @examplesIf requireNamespace("metacore", quietly = TRUE)
#' # The CDISC pilot ADaM metacore specification shipped with metacore.
#' load(metacore::metacore_example("pilot_ADaM.rda"))
#' head(as_codelist_metadata(metacore))
#'
#' @importFrom cli cli_abort
#' @export
as_codelist_metadata <- function(metadata) {
  if (inherits(metadata, "Metacore")) {
    return(metacore_codelist_metadata(metadata))
  }
  if (is.data.frame(metadata)) {
    return(coerce_codelist_metadata_df(metadata))
  }
  cli::cli_abort(c(
    "{.arg metadata} must be a {.cls Metacore} object or a data.frame.",
    "x" = "Got {.cls {class(metadata)[1]}}."
  ))
}

# Complete a user-supplied codelist-metadata data.frame.
coerce_codelist_metadata_df <- function(x) {
  missing_req <- setdiff(c("dataset", "variable", "code_id"), names(x))
  if (length(missing_req) > 0L) {
    cli::cli_abort(
      "Codelist-metadata data.frame is missing required column{?s}: {.field {missing_req}}"
    )
  }

  dataset  <- as.character(x$dataset)
  variable <- as.character(x$variable)
  code_id  <- blank_to_na(x$code_id)

  if ("n_terms" %in% names(x)) {
    n_terms <- as.integer(x$n_terms)
  } else if ("has_terms" %in% names(x)) {
    n_terms <- as.integer(as.logical(x$has_terms))
  } else {
    n_terms <- rep(NA_integer_, length(dataset))
  }

  if ("external" %in% names(x)) {
    external <- as.logical(x$external)
  } else if ("codelist_type" %in% names(x)) {
    external <- tolower(as.character(x$codelist_type)) == "external_library"
  } else {
    external <- rep(FALSE, length(dataset))
  }
  external[is.na(external)] <- FALSE

  out <- tibble::tibble(dataset, variable, code_id, n_terms, external)
  out[!is.na(out$code_id), , drop = FALSE]
}

# Unpack a 'metacore' object's codelist references. Reached only when a Metacore
# object is supplied, so 'metacore' is necessarily attached. Variables and their
# codelist references live in value_spec; how each codelist is defined lives in
# codelist. As in metacore_variable_metadata(), the value_spec dataset column may
# be absent and is then recovered from the qualified "DATASET.VARIABLE" key.
metacore_codelist_metadata <- function(mc) {
  pick <- function(df, name) if (name %in% names(df)) df[[name]] else NA
  strip_ds <- function(x) sub("^[^.]+\\.", "", as.character(x))

  val_spec <- as.data.frame(mc$value_spec)
  codelist <- as.data.frame(mc$codelist)

  vs_var_raw <- as.character(pick(val_spec, "variable"))
  variable   <- strip_ds(vs_var_raw)
  dataset    <- as.character(pick(val_spec, "dataset"))
  if (all(is.na(dataset))) {
    dataset <- sub("\\..*$", "", vs_var_raw)
  }
  code_id <- blank_to_na(pick(val_spec, "code_id"))

  # One row per variable that references a codelist.
  keep <- !is.na(code_id)
  dataset  <- dataset[keep]
  variable <- variable[keep]
  code_id  <- code_id[keep]
  if (length(code_id) == 0L) {
    return(tibble::tibble(
      dataset = character(0), variable = character(0), code_id = character(0),
      n_terms = integer(0), external = logical(0)
    ))
  }

  # Resolve each referenced codelist: external-dictionary references carry no
  # enumerated terms (n_terms = NA, external = TRUE); enumerated codelists carry
  # a term count; an unresolved reference stays NA / FALSE (a documentation gap).
  cl_id       <- as.character(pick(codelist, "code_id"))
  cl_type     <- as.character(pick(codelist, "type"))
  cl_codes    <- pick(codelist, "codes")
  cl_external <- cl_type == "external_library"
  cl_nterms   <- vapply(seq_along(cl_id), function(i) {
    if (isTRUE(cl_external[i])) return(NA_integer_)
    codes_i <- if (is.list(cl_codes)) cl_codes[[i]] else NA
    if (is.data.frame(codes_i)) nrow(codes_i) else length(codes_i)
  }, integer(1))

  idx      <- match(code_id, cl_id)
  external <- cl_external[idx]
  external[is.na(external)] <- FALSE
  n_terms  <- cl_nterms[idx]

  out <- tibble::tibble(dataset, variable, code_id, n_terms, external)
  unique(out)
}

# Dataset-JSON metadata seam ---------------------------------------------------
#
# `datasetjson_to_evidence()` works at the level of a Dataset-JSON data-exchange
# file: it records whether that file's metadata declares the structural keys the
# CDISC Dataset-JSON standard defines. Its contract is a "dataset-json metadata"
# table with one row per column and the columns
#   dataset, dataset_label, records, name, label, data_type, length
# where the dataset-level fields (dataset, dataset_label, records) are repeated
# on each column row. `as_datasetjson_metadata()` turns a plain data.frame of
# column metadata, or a parsed Dataset-JSON object (a list carrying a `columns`
# element, as produced by `jsonlite::fromJSON()` or `datasetjson::read_dataset_json()`
# on a Dataset-JSON v1.1 file), into that contract. Keeping this seam explicit
# means the evidence logic can be exercised with an ordinary data.frame and does
# not depend on the 'datasetjson' package being installed.

datasetjson_metadata_columns <- function() {
  c("dataset", "dataset_label", "records", "name", "label", "data_type", "length")
}

# Return column `x[[name]]` for the first of `candidates` present, or NULL.
dj_col <- function(x, candidates) {
  hit <- candidates[candidates %in% names(x)]
  if (length(hit) == 0L) NULL else x[[hit[1]]]
}

#' Coerce Metadata to the Dataset-JSON Metadata Contract
#'
#' Normalizes the accepted inputs into a tidy table with one row per column of a
#' Dataset-JSON data-exchange file, describing whether that file's metadata
#' declares the structural keys the CDISC Dataset-JSON standard defines. A
#' parsed Dataset-JSON object - a list with a `columns` element, such as
#' `jsonlite::fromJSON()` or `datasetjson::read_dataset_json()` produce from a
#' Dataset-JSON v1.1 file - contributes its column array (`name`, `label`,
#' `dataType`, ...) together with the file's dataset-level `name`, `label`, and
#' `records`; a data.frame is checked for the required `name` column and
#' completed with any missing optional columns.
#'
#' @param metadata Either a data.frame of Dataset-JSON column metadata (one row
#'   per column, with at least a `name` column; optional `label`, `data_type`
#'   (alias `dataType`/`type`), and `length`, plus optional dataset-level
#'   columns `dataset`, `dataset_label`, and `records`), or a parsed Dataset-JSON
#'   object: a list with a `columns` element and optional top-level `name`,
#'   `label`, and `records`.
#'
#' @return A [tibble][tibble::tibble] with columns `dataset`, `dataset_label`,
#'   `records`, `name`, `label`, `data_type`, and `length`.
#'
#' @seealso [datasetjson_to_evidence()], [as_variable_metadata()]
#'
#' @examples
#' cols <- data.frame(
#'   name      = c("STUDYID", "USUBJID", "AGE"),
#'   label     = c("Study Identifier", "Unique Subject Identifier", ""),
#'   dataType  = c("string", "string", "integer"),
#'   stringsAsFactors = FALSE
#' )
#' as_datasetjson_metadata(cols)
#'
#' @importFrom cli cli_abort
#' @export
as_datasetjson_metadata <- function(metadata) {
  if (is.data.frame(metadata)) {
    return(coerce_datasetjson_metadata_df(metadata))
  }
  if (is.list(metadata)) {
    return(datasetjson_object_metadata(metadata))
  }
  cli::cli_abort(c(
    "{.arg metadata} must be a data.frame of Dataset-JSON column metadata or a parsed Dataset-JSON object (a list with a {.field columns} element).",
    "x" = "Got {.cls {class(metadata)[1]}}."
  ))
}

# Complete a user-supplied Dataset-JSON column-metadata data.frame.
coerce_datasetjson_metadata_df <- function(x) {
  name <- dj_col(x, c("name", "variable"))
  if (is.null(name)) {
    cli::cli_abort(
      "Dataset-JSON column-metadata data.frame is missing required column: {.field name}"
    )
  }
  name <- blank_to_na(name)
  n <- length(name)

  opt_chr <- function(cands) {
    v <- dj_col(x, cands)
    if (is.null(v)) rep(NA_character_, n) else blank_to_na(v)
  }
  opt_int <- function(cands) {
    v <- dj_col(x, cands)
    if (is.null(v)) rep(NA_integer_, n) else suppressWarnings(as.integer(v))
  }

  tibble::tibble(
    dataset       = opt_chr(c("dataset", "itemGroupName")),
    dataset_label = opt_chr(c("dataset_label", "datasetLabel")),
    records       = opt_int(c("records", "nrow")),
    name          = name,
    label         = opt_chr("label"),
    data_type     = opt_chr(c("data_type", "dataType", "type")),
    length        = opt_int(c("length", "len"))
  )
}

# Unpack a parsed Dataset-JSON object (a list with a `columns` element, as
# jsonlite::fromJSON() produces from a Dataset-JSON v1.1 file). Dataset-level
# name/label/records live at the top level; column attributes live in `columns`.
datasetjson_object_metadata <- function(obj) {
  cols <- obj$columns
  if (is.null(cols)) {
    cli::cli_abort(c(
      "{.arg metadata} looks like a parsed Dataset-JSON object but has no {.field columns} element.",
      "i" = "Expected a Dataset-JSON v1.1 structure with a {.field columns} array."
    ))
  }
  cols <- as.data.frame(cols, stringsAsFactors = FALSE)
  scalar_or_na <- function(v) if (is.null(v) || length(v) == 0L) NA else v[[1]]
  cols$dataset       <- scalar_or_na(obj$name)
  cols$dataset_label <- scalar_or_na(obj$label)
  cols$records       <- scalar_or_na(obj$records)
  coerce_datasetjson_metadata_df(cols)
}
