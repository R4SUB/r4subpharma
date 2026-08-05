# Tests over the real pharma metadata/data, plus previously-untested edge paths.

rp_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("CDISCPILOT01", "DEV"))
}

test_that("metacore_to_evidence runs on r4subdata metadata", {
  skip_if_not_installed("r4subdata")
  ev <- suppressMessages(metacore_to_evidence(r4subdata::adam_metadata, rp_ctx()))
  expect_silent(r4subcore::validate_evidence(ev))
  expect_true(all(ev$indicator_id %in% c("Q-DEFINE-002", "Q-DEFINE-003")))
})

test_that("adam_to_evidence checks a real ADSL and captures payload content", {
  skip_if_not_installed("r4subdata")
  skip_if_not_installed("pharmaverseadam")
  ev <- suppressMessages(adam_to_evidence(
    pharmaverseadam::adsl, r4subdata::adam_metadata, rp_ctx(),
    dataset_name = "ADSL"
  ))
  expect_silent(r4subcore::validate_evidence(ev))
  expect_true(any(nzchar(ev$evidence_payload) & ev$evidence_payload != "{}"))
})

test_that("a non-scalar source_name is rejected", {
  meta <- data.frame(
    dataset = "ADSL", variable = "AGE", label = "Age", type = "integer",
    stringsAsFactors = FALSE
  )
  expect_error(
    adam_to_evidence(data.frame(AGE = 1), meta, rp_ctx(),
                     source_name = c("a", "b")),
    "single string"
  )
})

test_that("a date-typed column is skipped by the type check, not flagged", {
  meta <- data.frame(
    dataset = "ADSL", variable = c("AGE", "TRTSDT"),
    label = c("Age", "Start"), type = c("integer", "integer"),
    stringsAsFactors = FALSE
  )
  data <- data.frame(AGE = 54L, TRTSDT = as.Date("2020-01-01"))
  ev <- suppressMessages(adam_to_evidence(data, meta, rp_ctx(),
                                          dataset_name = "ADSL"))
  type_rows <- ev[ev$indicator_id == "Q-ADAM-001", ]
  # AGE is compared; TRTSDT (a Date) is skipped rather than reported as "NA".
  expect_true("ADSL:AGE" %in% type_rows$location)
  expect_false("ADSL:TRTSDT" %in% type_rows$location)
})

test_that("submission_readiness rejects an unnamed data list", {
  meta <- data.frame(
    dataset = c("ADSL", "ADAE"), variable = c("AGE", "AESEV"),
    label = c("Age", "Severity"), type = c("integer", "text"),
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressMessages(submission_readiness(list(data.frame(AGE = 1)), meta, rp_ctx())),
    "named list"
  )
})

test_that("print.submission_readiness handles a NULL sci", {
  sr <- structure(
    list(evidence = data.frame(x = 1), pillar_scores = NULL, sci = NULL),
    class = "submission_readiness"
  )
  expect_output(print(sr), "not computed")
})
