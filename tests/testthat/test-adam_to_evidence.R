make_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
}

adsl_meta <- function() {
  data.frame(
    dataset  = "ADSL",
    variable = c("USUBJID", "AGE", "SEX"),
    label    = c("Unique Subject Identifier", "Age", "Sex"),
    type     = c("text", "integer", "text"),
    stringsAsFactors = FALSE
  )
}

test_that("adam_to_evidence checks presence, extras, type and label", {
  adsl <- data.frame(
    USUBJID = c("01-001", "01-002"),
    AGE     = c(54, 61),
    STUDYID = c("S1", "S1"),
    stringsAsFactors = FALSE
  )

  ev <- suppressMessages(
    adam_to_evidence(adsl, adsl_meta(), make_ctx(), dataset_name = "ADSL")
  )

  presence <- ev[ev$indicator_id == "T-ADAM-001", ]
  expect_equal(presence$result[presence$location == "ADSL:USUBJID"], "pass")
  expect_equal(presence$result[presence$location == "ADSL:SEX"], "fail")

  extra <- ev[ev$indicator_id == "T-ADAM-002", ]
  expect_equal(extra$location, "ADSL:STUDYID")
  expect_equal(extra$result, "warn")

  # type checked only for present variables with a known type
  types <- ev[ev$indicator_id == "Q-ADAM-001", ]
  expect_setequal(types$location, c("ADSL:USUBJID", "ADSL:AGE"))
  expect_true(all(types$result == "pass"))

  expect_true(all(c("trace", "quality", "usability") %in% ev$indicator_domain))
})

test_that("a type mismatch produces a warn row", {
  adsl <- data.frame(USUBJID = "01-001", AGE = "54", stringsAsFactors = FALSE)
  ev <- suppressMessages(
    adam_to_evidence(adsl, adsl_meta(), make_ctx(), dataset_name = "ADSL")
  )
  age_type <- ev[ev$indicator_id == "Q-ADAM-001" & ev$location == "ADSL:AGE", ]
  expect_equal(age_type$result, "warn")
})

test_that("a present label passes Q-ADAM-002", {
  adsl <- data.frame(USUBJID = "01-001", stringsAsFactors = FALSE)
  attr(adsl$USUBJID, "label") <- "Unique Subject Identifier"

  ev <- suppressMessages(
    adam_to_evidence(adsl, adsl_meta(), make_ctx(), dataset_name = "ADSL")
  )
  lbl <- ev[ev$indicator_id == "Q-ADAM-002" & ev$location == "ADSL:USUBJID", ]
  expect_equal(lbl$result, "pass")
})

test_that("dataset_name is inferred for single-dataset metadata", {
  adsl <- data.frame(USUBJID = "01-001", stringsAsFactors = FALSE)
  ev <- suppressMessages(adam_to_evidence(adsl, adsl_meta(), make_ctx()))
  expect_true(all(ev$asset_id == "ADSL"))
})

test_that("ambiguous metadata requires an explicit dataset_name", {
  meta <- rbind(adsl_meta(),
                data.frame(dataset = "ADAE", variable = "AESEQ",
                           label = "Seq", type = "integer",
                           stringsAsFactors = FALSE))
  adsl <- data.frame(USUBJID = "01-001", stringsAsFactors = FALSE)
  expect_error(
    suppressMessages(adam_to_evidence(adsl, meta, make_ctx())),
    "dataset_name"
  )
})
