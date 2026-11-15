make_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
}

# A fully documented single-dataset Dataset-JSON column-metadata frame.
good_cols <- function() {
  data.frame(
    dataset       = "ADSL",
    dataset_label = "Subject-Level Analysis Dataset",
    records       = 306L,
    name          = c("STUDYID", "USUBJID", "AGE"),
    label         = c("Study Identifier", "Unique Subject Identifier", "Age"),
    dataType      = c("string", "string", "integer"),
    stringsAsFactors = FALSE
  )
}

test_that("one Q-DJSON-001 row per dataset and one Q-DJSON-002 row per column", {
  ev <- suppressMessages(datasetjson_to_evidence(good_cols(), make_ctx()))

  expect_silent(r4subcore::validate_evidence(ev))
  expect_equal(sum(ev$indicator_id == "Q-DJSON-001"), 1L)
  expect_equal(sum(ev$indicator_id == "Q-DJSON-002"), 3L)
  expect_true(all(ev$indicator_domain == "quality"))
  expect_true(all(ev$asset_type == "dataset"))
  expect_true(all(ev$source_name == "datasetjson"))
})

test_that("a fully documented file and columns all pass", {
  ev <- suppressMessages(datasetjson_to_evidence(good_cols(), make_ctx()))
  expect_true(all(ev$result == "pass"))
  expect_true(all(ev$severity == "info"))
  expect_true(all(ev$metric_value == 1))

  file_row <- ev[ev$indicator_id == "Q-DJSON-001", ]
  expect_equal(file_row$location, "ADSL")
  expect_match(file_row$message, "documents its dataset-level metadata")

  col_rows <- ev[ev$indicator_id == "Q-DJSON-002", ]
  expect_setequal(col_rows$location, c("ADSL:STUDYID", "ADSL:USUBJID", "ADSL:AGE"))
})

test_that("a missing dataset label warns; missing both label and records keeps warn", {
  cols <- good_cols()
  cols$dataset_label <- ""           # drop the label only
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx()))
  file_row <- ev[ev$indicator_id == "Q-DJSON-001", ]
  expect_equal(file_row$result, "warn")
  expect_equal(file_row$severity, "medium")
  expect_equal(file_row$metric_value, 2 / 3)
  expect_match(file_row$message, "dataset label")
})

test_that("a file declaring no dataset-level metadata fails", {
  cols <- data.frame(
    name     = c("STUDYID", "AGE"),
    label    = c("Study Identifier", "Age"),
    dataType = c("string", "integer"),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx(),
                                                 dataset_name = "ADSL"))
  file_row <- ev[ev$indicator_id == "Q-DJSON-001", ]
  expect_equal(file_row$result, "fail")
  expect_equal(file_row$severity, "high")
  expect_equal(file_row$metric_value, 0)
  # dataset_name supplies the asset id even though the file declares no name.
  expect_equal(file_row$asset_id, "ADSL")
  expect_match(file_row$message, "dataset name")
})

test_that("records = 0 counts as present, not a gap", {
  cols <- good_cols()
  cols$records <- 0L
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx()))
  file_row <- ev[ev$indicator_id == "Q-DJSON-001", ]
  expect_equal(file_row$result, "pass")
  expect_match(file_row$evidence_payload, "\"records\":\"0\"")
})

test_that("a column missing a label warns; missing label and type fails", {
  cols <- data.frame(
    dataset       = "ADSL",
    dataset_label = "Subject-Level Analysis Dataset",
    records       = 10L,
    name          = c("GOOD", "NOLABEL", "BARE"),
    label         = c("Documented", "", ""),
    dataType      = c("string", "integer", ""),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx()))

  good    <- ev[ev$location == "ADSL:GOOD", ]
  nolabel <- ev[ev$location == "ADSL:NOLABEL", ]
  bare    <- ev[ev$location == "ADSL:BARE", ]

  expect_equal(good$result, "pass")
  expect_equal(good$metric_value, 1)

  expect_equal(nolabel$result, "warn")
  expect_equal(nolabel$severity, "medium")
  expect_equal(nolabel$metric_value, 2 / 3)
  expect_match(nolabel$message, "label")

  # Only the name survives: a partial declaration, so a warn (not a fail).
  expect_equal(bare$result, "warn")
  expect_equal(bare$severity, "medium")
  expect_equal(bare$metric_value, 1 / 3)
  expect_match(bare$message, "label, data type")
})

test_that("a column declaring no key at all fails", {
  cols <- data.frame(
    dataset       = "ADSL",
    dataset_label = "SL",
    records       = 1L,
    name          = c("GOOD", ""),       # second column: no name, label, or type
    label         = c("Documented", ""),
    dataType      = c("string", ""),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx()))
  col_rows <- ev[ev$indicator_id == "Q-DJSON-002", ]
  empty <- col_rows[grepl(":col2$", col_rows$location), ]
  expect_equal(empty$result, "fail")
  expect_equal(empty$severity, "high")
  expect_equal(empty$metric_value, 0)
})

test_that("dataType/type and datasetLabel/nrow aliases are accepted", {
  cols <- data.frame(
    dataset     = "ADAE",
    datasetLabel = "Adverse Events Analysis Dataset",
    nrow        = 42L,
    name        = c("AETERM", "AESEV"),
    label       = c("Reported Term", "Severity"),
    type        = c("string", "string"),     # 'type' alias for data_type
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx()))
  expect_true(all(ev$result == "pass"))
  expect_equal(ev$asset_id[1], "ADAE")
})

test_that("several datasets in one frame yield one Q-DJSON-001 row each", {
  cols <- data.frame(
    dataset       = c("ADSL", "ADSL", "ADAE"),
    dataset_label = c("SL", "SL", "AE"),
    records       = c(5L, 5L, 7L),
    name          = c("STUDYID", "AGE", "AETERM"),
    label         = c("Study", "Age", "Term"),
    dataType      = c("string", "integer", "string"),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx()))
  file_rows <- ev[ev$indicator_id == "Q-DJSON-001", ]
  expect_equal(nrow(file_rows), 2L)
  expect_setequal(file_rows$asset_id, c("ADSL", "ADAE"))
  expect_equal(sum(ev$indicator_id == "Q-DJSON-002"), 3L)
})

test_that("a parsed Dataset-JSON object (list with columns) is unpacked", {
  # Shape produced by jsonlite::fromJSON() on a Dataset-JSON v1.1 file.
  obj <- list(
    datasetJSONVersion = "1.1.0",
    name    = "ADSL",
    label   = "Subject-Level Analysis Dataset",
    records = 306L,
    columns = data.frame(
      itemOID  = c("IT.ADSL.STUDYID", "IT.ADSL.AGE"),
      name     = c("STUDYID", "AGE"),
      label    = c("Study Identifier", "Age"),
      dataType = c("string", "integer"),
      length   = c(12L, 8L),
      stringsAsFactors = FALSE
    )
  )

  dj <- as_datasetjson_metadata(obj)
  expect_setequal(names(dj), datasetjson_metadata_columns())
  expect_equal(nrow(dj), 2L)
  expect_true(all(dj$dataset == "ADSL"))
  expect_equal(dj$records, c(306L, 306L))

  ev <- suppressMessages(datasetjson_to_evidence(obj, make_ctx()))
  expect_silent(r4subcore::validate_evidence(ev))
  expect_true(all(ev$result == "pass"))
  file_row <- ev[ev$indicator_id == "Q-DJSON-001", ]
  expect_equal(file_row$asset_id, "ADSL")
})

test_that("a list without a columns element is rejected", {
  expect_error(
    as_datasetjson_metadata(list(name = "ADSL", records = 5L)),
    "columns"
  )
})

test_that("a data.frame missing the name column is rejected", {
  expect_error(
    as_datasetjson_metadata(data.frame(label = "x", dataType = "string")),
    "name"
  )
})

test_that("a zero-row metadata frame is rejected", {
  empty <- data.frame(name = character(0), label = character(0),
                      dataType = character(0), stringsAsFactors = FALSE)
  expect_equal(nrow(as_datasetjson_metadata(empty)), 0L)
  expect_error(
    suppressMessages(datasetjson_to_evidence(empty, make_ctx())),
    "no Dataset-JSON columns"
  )
})

test_that("a bad context is rejected", {
  expect_error(
    suppressMessages(datasetjson_to_evidence(good_cols(), ctx = "nope")),
    "r4sub_run_context"
  )
})

test_that("a column with no declared name is labelled positionally", {
  cols <- data.frame(
    dataset       = "ADSL",
    dataset_label = "SL",
    records       = 3L,
    name          = c("STUDYID", ""),      # second column has no name
    label         = c("Study", "Mystery"),
    dataType      = c("string", "string"),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(datasetjson_to_evidence(cols, make_ctx()))
  col_rows <- ev[ev$indicator_id == "Q-DJSON-002", ]
  expect_true(any(grepl(":col2$", col_rows$location)))
  # The unnamed column is a structural gap: name missing -> not a full pass.
  unnamed <- col_rows[grepl(":col2$", col_rows$location), ]
  expect_equal(unnamed$result, "warn")
  expect_match(unnamed$message, "name")
})

test_that("evidence validates and locations are well-formed for the object path", {
  ev <- suppressMessages(datasetjson_to_evidence(good_cols(), make_ctx(),
                                                 source_version = "1.1.0"))
  expect_silent(r4subcore::validate_evidence(ev))
  expect_true(all(ev$source_version == "1.1.0"))
  col_rows <- ev[ev$indicator_id == "Q-DJSON-002", ]
  expect_true(all(grepl("^[^:]+:[^:]+$", col_rows$location)))
})
