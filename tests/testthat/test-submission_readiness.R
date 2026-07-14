make_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
}

demo_meta <- function() {
  data.frame(
    dataset  = "ADSL",
    variable = c("USUBJID", "AGE", "SEX"),
    label    = c("Unique Subject Identifier", "Age", "Sex"),
    type     = c("text", "integer", "text"),
    origin   = c("Predecessor", "Derived", "Predecessor"),
    derivation = c(NA, "Age at informed consent", NA),
    stringsAsFactors = FALSE
  )
}

test_that("submission_readiness combines evidence from metadata and datasets", {
  adsl <- data.frame(
    USUBJID = "01-001", AGE = 54, SEX = "M",
    stringsAsFactors = FALSE
  )
  res <- suppressMessages(
    submission_readiness(list(ADSL = adsl), demo_meta(), make_ctx())
  )

  expect_s3_class(res, "submission_readiness")
  expect_true(nrow(res$evidence) > 0L)
  expect_true(all(c("Q-DEFINE-002", "T-ADAM-001") %in% res$evidence$indicator_id))
})

test_that("submission_readiness accepts a bare data.frame for single-dataset metadata", {
  adsl <- data.frame(USUBJID = "01-001", AGE = 54, SEX = "F",
                     stringsAsFactors = FALSE)
  res <- suppressMessages(submission_readiness(adsl, demo_meta(), make_ctx()))
  expect_s3_class(res, "submission_readiness")
})

test_that("submission_readiness computes an SCI when r4subscore is available", {
  skip_if_not_installed("r4subscore")

  adsl <- data.frame(USUBJID = "01-001", AGE = 54, SEX = "M",
                     stringsAsFactors = FALSE)
  res <- suppressMessages(
    submission_readiness(list(ADSL = adsl), demo_meta(), make_ctx())
  )

  expect_false(is.null(res$sci))
  expect_true(is.numeric(res$sci$SCI))
  expect_gte(res$sci$SCI, 0)
  expect_lte(res$sci$SCI, 100)
  expect_output(print(res), "submission_readiness")
})
