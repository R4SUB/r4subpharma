# Internal helpers -------------------------------------------------------------

# Null-coalescing helper.
`%||%` <- function(x, y) if (is.null(x)) y else x

# TRUE for scalar character values that carry actual content.
has_text <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

# Coerce empty strings to NA so downstream "documented?" checks behave.
blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

# Normalize a metadata data type to the two families the evidence checks care
# about: "character" or "numeric". Anything ambiguous (dates, times, unknown)
# returns NA so the type check is skipped rather than raising a false mismatch.
normalize_type <- function(x) {
  lower <- tolower(trimws(as.character(x)))
  vapply(lower, function(t) {
    if (is.na(t) || !nzchar(t)) {
      NA_character_
    } else if (t %in% c("text", "char", "character", "string")) {
      "character"
    } else if (t %in% c("integer", "float", "numeric", "num", "double", "dec")) {
      "numeric"
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
}

# The observed family of an R vector, in the same vocabulary as normalize_type().
observed_type <- function(x) {
  if (is.numeric(x)) {
    "numeric"
  } else if (is.character(x) || is.factor(x)) {
    "character"
  } else {
    NA_character_
  }
}

# Read the "label" attribute a dataset carries on a column, tolerating the
# absence of one.
column_label <- function(x) {
  lbl <- attr(x, "label", exact = TRUE)
  if (is.null(lbl)) NA_character_ else as.character(lbl)[1]
}

# A zero-row data.frame carrying exactly the columns the adapters assemble
# before handing off to r4subcore::as_evidence(). Used as the neutral element
# when a particular check produces no rows (e.g. no derived variables).
pre_evidence_template <- function() {
  data.frame(
    asset_type = character(0), asset_id = character(0),
    source_name = character(0), source_version = character(0),
    indicator_id = character(0), indicator_name = character(0),
    indicator_domain = character(0), severity = character(0),
    result = character(0), metric_value = double(0),
    metric_unit = character(0), message = character(0),
    location = character(0), evidence_payload = character(0),
    stringsAsFactors = FALSE
  )
}

# Small JSON object builder for the evidence payload. Values are coerced to
# strings and quoted; this keeps the dependency surface minimal and the payload
# human-readable for drilldown.
payload_json <- function(...) {
  parts <- list(...)
  if (length(parts) == 0L) {
    return("{}")
  }
  keys <- names(parts)
  entries <- vapply(seq_along(parts), function(i) {
    val <- parts[[i]]
    val <- if (length(val) == 0L || is.na(val)) "" else as.character(val)[1]
    val <- gsub('"', "'", val, fixed = TRUE)
    sprintf('"%s":"%s"', keys[i], val)
  }, character(1))
  paste0("{", paste(entries, collapse = ","), "}")
}
