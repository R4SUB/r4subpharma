make_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
}

test_that("metacore_to_evidence emits documentation and derivation indicators", {
  meta <- data.frame(
    dataset  = "ADSL",
    variable = c("USUBJID", "TRTSDT"),
    label    = c("Unique Subject Identifier", "Date of First Exposure"),
    type     = c("text", "integer"),
    origin   = c("Predecessor", "Derived"),
    derivation = c(NA, "First dosing date from EX"),
    stringsAsFactors = FALSE
  )

  ev <- suppressMessages(metacore_to_evidence(meta, make_ctx()))

  expect_true(all(c("Q-DEFINE-002", "Q-DEFINE-003") %in% ev$indicator_id))
  # one documentation row per variable, one derivation row for the derived var
  expect_equal(sum(ev$indicator_id == "Q-DEFINE-002"), 2L)
  expect_equal(sum(ev$indicator_id == "Q-DEFINE-003"), 1L)
  expect_true(all(ev$indicator_domain == "quality"))
  expect_true(all(ev$result == "pass"))
})

test_that("undocumented variables fail or warn appropriately", {
  meta <- data.frame(
    dataset  = "ADSL",
    variable = c("A", "B", "C"),
    label    = c("Labelled", NA, "Only label"),
    type     = c("text", NA, NA),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(metacore_to_evidence(meta, make_ctx()))
  doc <- ev[ev$indicator_id == "Q-DEFINE-002", ]

  expect_equal(doc$result[doc$location == "ADSL:A"], "pass")
  expect_equal(doc$result[doc$location == "ADSL:B"], "fail")
  expect_equal(doc$result[doc$location == "ADSL:C"], "warn")
})

test_that("a derived variable without derivation text fails Q-DEFINE-003", {
  meta <- data.frame(
    dataset = "ADSL", variable = "TRTSDT", label = "Trt Start",
    type = "integer", origin = "Derived", derivation = NA,
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(metacore_to_evidence(meta, make_ctx()))
  deriv <- ev[ev$indicator_id == "Q-DEFINE-003", ]

  expect_equal(nrow(deriv), 1L)
  expect_equal(deriv$result, "fail")
  expect_equal(deriv$severity, "high")
})

test_that("metadata with no derived variables emits only documentation rows", {
  meta <- data.frame(
    dataset = "ADSL", variable = c("USUBJID", "SEX"),
    label = c("Subject", "Sex"), type = c("text", "text"),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(metacore_to_evidence(meta, make_ctx()))
  expect_false("Q-DEFINE-003" %in% ev$indicator_id)
  expect_equal(nrow(ev), 2L)
})

test_that("empty metadata is rejected", {
  meta <- data.frame(dataset = character(0), variable = character(0))
  expect_error(suppressMessages(metacore_to_evidence(meta, make_ctx())),
               "no variables")
})
