make_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
}

test_that("dataset_metadata_to_evidence emits one Q-DEFINE-001 row per dataset", {
  meta <- data.frame(
    dataset   = c("ADSL", "ADAE"),
    label     = c("Subject-Level Analysis Dataset", "Adverse Events Analysis"),
    structure = c("one record per subject", "one record per subject per event"),
    n_keys    = c(1L, 4L),
    stringsAsFactors = FALSE
  )

  ev <- suppressMessages(dataset_metadata_to_evidence(meta, make_ctx()))

  expect_true(all(ev$indicator_id == "Q-DEFINE-001"))
  expect_equal(nrow(ev), 2L)
  expect_true(all(ev$indicator_domain == "quality"))
  expect_true(all(ev$asset_type == "define"))
  expect_true(all(ev$result == "pass"))
  expect_equal(sort(ev$asset_id), c("ADAE", "ADSL"))
})

test_that("a fully documented dataset passes and a bare dataset fails", {
  meta <- data.frame(
    dataset   = c("ADSL", "ADXX"),
    label     = c("Subject-Level Analysis Dataset", NA),
    structure = c("one record per subject", NA),
    n_keys    = c(2L, 0L),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(dataset_metadata_to_evidence(meta, make_ctx()))

  pass <- ev[ev$location == "ADSL", ]
  fail <- ev[ev$location == "ADXX", ]
  expect_equal(pass$result, "pass")
  expect_equal(pass$severity, "info")
  expect_equal(pass$metric_value, 1)
  expect_equal(fail$result, "fail")
  expect_equal(fail$severity, "high")
  expect_equal(fail$metric_value, 0)
})

test_that("a partially documented dataset warns and names what is missing", {
  meta <- data.frame(
    dataset   = "ADSL",
    label     = "Subject-Level Analysis Dataset",
    structure = NA,
    n_keys    = 0L,
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(dataset_metadata_to_evidence(meta, make_ctx()))

  expect_equal(ev$result, "warn")
  expect_equal(ev$severity, "medium")
  expect_equal(ev$metric_value, 1 / 3)
  expect_match(ev$message, "structure")
  expect_match(ev$message, "key variables")
})

test_that("has_keys is accepted in place of n_keys", {
  meta <- data.frame(
    dataset   = "ADSL",
    label     = "Subject-Level Analysis Dataset",
    structure = "one record per subject",
    has_keys  = TRUE,
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(dataset_metadata_to_evidence(meta, make_ctx()))
  expect_equal(ev$result, "pass")
})

test_that("empty dataset metadata is rejected", {
  meta <- data.frame(dataset = character(0))
  expect_error(
    suppressMessages(dataset_metadata_to_evidence(meta, make_ctx())),
    "no datasets"
  )
})

test_that("missing required dataset column is rejected", {
  expect_error(
    as_dataset_metadata(data.frame(label = "x")),
    "dataset"
  )
})

test_that("as_dataset_metadata unpacks a metacore object with key counts", {
  skip_if_not_installed("metacore")
  load(metacore::metacore_example("pilot_ADaM.rda"))
  meta <- as_dataset_metadata(metacore)

  expect_true(all(c("dataset", "label", "structure", "n_keys") %in% names(meta)))
  expect_true(all(c("ADSL", "ADAE") %in% meta$dataset))
  # ADSL has exactly one key variable (USUBJID) in the pilot spec.
  expect_equal(meta$n_keys[meta$dataset == "ADSL"], 1L)
  expect_true(all(meta$n_keys >= 1L))
})

test_that("dataset_metadata_to_evidence runs on a real metacore object", {
  skip_if_not_installed("metacore")
  load(metacore::metacore_example("pilot_ADaM.rda"))
  ev <- suppressMessages(dataset_metadata_to_evidence(metacore, make_ctx()))

  expect_true(all(ev$indicator_id == "Q-DEFINE-001"))
  expect_equal(nrow(ev), nrow(as.data.frame(metacore$ds_spec)))
  expect_true(all(ev$result %in% c("pass", "warn", "fail")))
})
