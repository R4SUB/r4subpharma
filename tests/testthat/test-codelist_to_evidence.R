make_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
}

test_that("codelist_to_evidence emits one Q-DEFINE-005 row per codelist reference", {
  meta <- data.frame(
    dataset  = c("ADSL", "ADSL", "ADAE"),
    variable = c("SEX", "RACE", "AEDECOD"),
    code_id  = c("CL.SEX", "CL.RACE", "CL.AEDICT"),
    n_terms  = c(2L, 6L, NA),
    external = c(FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))

  expect_true(all(ev$indicator_id == "Q-DEFINE-005"))
  expect_equal(nrow(ev), 3L)
  expect_true(all(ev$indicator_domain == "quality"))
  expect_true(all(ev$asset_type == "define"))
  expect_true(all(ev$result == "pass"))
})

test_that("a documented codelist passes, an empty one warns, a dangling one fails", {
  meta <- data.frame(
    dataset  = "ADSL",
    variable = c("SEX", "RACE", "ETHNIC"),
    code_id  = c("CL.SEX", "CL.EMPTY", "CL.MISSING"),
    n_terms  = c(2L, 0L, NA),
    external = FALSE,
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))

  pass <- ev[ev$location == "ADSL:SEX", ]
  warn <- ev[ev$location == "ADSL:RACE", ]
  fail <- ev[ev$location == "ADSL:ETHNIC", ]

  expect_equal(pass$result, "pass")
  expect_equal(pass$severity, "info")
  expect_equal(pass$metric_value, 1)
  expect_match(pass$message, "2 terms")

  expect_equal(warn$result, "warn")
  expect_equal(warn$severity, "medium")
  expect_equal(warn$metric_value, 0.5)
  expect_match(warn$message, "no terms")

  expect_equal(fail$result, "fail")
  expect_equal(fail$severity, "high")
  expect_equal(fail$metric_value, 0)
  expect_match(fail$message, "not documented")
})

test_that("an external-dictionary reference passes without enumerated terms", {
  meta <- data.frame(
    dataset  = "ADAE",
    variable = "AEDECOD",
    code_id  = "CL.AEDICT",
    n_terms  = NA_integer_,
    external = TRUE,
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  expect_equal(ev$result, "pass")
  expect_match(ev$message, "external dictionary")
})

test_that("has_terms and codelist_type are accepted as aliases", {
  meta <- data.frame(
    dataset       = c("ADSL", "ADAE"),
    variable      = c("SEX", "AEDECOD"),
    code_id       = c("CL.SEX", "CL.AEDICT"),
    has_terms     = c(TRUE, FALSE),
    codelist_type = c("code_decode", "external_library"),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  expect_true(all(ev$result == "pass"))
})

test_that("variables that reference no codelist are dropped", {
  meta <- data.frame(
    dataset  = "ADSL",
    variable = c("USUBJID", "SEX"),
    code_id  = c(NA, "CL.SEX"),
    n_terms  = c(NA, 2L),
    stringsAsFactors = FALSE
  )
  cl <- as_codelist_metadata(meta)
  expect_equal(nrow(cl), 1L)
  expect_equal(cl$variable, "SEX")

  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  expect_equal(nrow(ev), 1L)
})

test_that("metadata with no codelist references is rejected", {
  meta <- data.frame(
    dataset = "ADSL", variable = "USUBJID", code_id = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressMessages(codelist_to_evidence(meta, make_ctx())),
    "no variables that reference a codelist"
  )
})

test_that("missing required columns are rejected", {
  expect_error(
    as_codelist_metadata(data.frame(dataset = "ADSL", variable = "SEX")),
    "code_id"
  )
})

test_that("as_codelist_metadata unpacks a real metacore object", {
  skip_if_not_installed("metacore")
  load(metacore::metacore_example("pilot_ADaM.rda"))
  cl <- as_codelist_metadata(metacore)

  expect_setequal(names(cl), codelist_metadata_columns())
  expect_true(nrow(cl) > 0L)
  expect_true(all(!is.na(cl$code_id)))
  # The pilot references an external MedDRA dictionary for AE terms.
  expect_true(any(cl$external))
  # Enumerated codelists carry a positive term count.
  expect_true(any(!is.na(cl$n_terms) & cl$n_terms > 0L))
})

test_that("codelist_to_evidence runs on a real metacore object", {
  skip_if_not_installed("metacore")
  load(metacore::metacore_example("pilot_ADaM.rda"))
  ev <- suppressMessages(codelist_to_evidence(metacore, make_ctx()))

  expect_silent(r4subcore::validate_evidence(ev))
  expect_true(all(ev$indicator_id == "Q-DEFINE-005"))
  expect_true(all(ev$result %in% c("pass", "warn", "fail")))
  # The pilot's referenced codelists are all documented (enumerated or external).
  expect_true(all(ev$result == "pass"))
})

test_that("a bad context is rejected", {
  meta <- data.frame(dataset = "ADSL", variable = "SEX", code_id = "CL.SEX",
                     n_terms = 2L, stringsAsFactors = FALSE)
  expect_error(
    suppressMessages(codelist_to_evidence(meta, ctx = "nope")),
    "r4sub_run_context"
  )
})
