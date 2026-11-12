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

# --- Added coverage ----------------------------------------------------------

test_that("an enumerated codelist with a single term passes and reads singular", {
  meta <- data.frame(
    dataset = "ADSL", variable = "SEXPOOL", code_id = "CL.ONE",
    n_terms = 1L, external = FALSE, stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  expect_equal(ev$result, "pass")
  expect_equal(ev$severity, "info")
  expect_equal(ev$metric_value, 1)
  # Singular: "1 term", not "1 terms".
  expect_match(ev$message, "documented with 1 term\\b")
  expect_no_match(ev$message, "1 terms")
})

test_that("the external-dictionary pass carries info severity and metric 1", {
  meta <- data.frame(
    dataset = "ADAE", variable = "AEDECOD", code_id = "CL.MEDDRA",
    n_terms = NA_integer_, external = TRUE, stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  expect_equal(ev$result, "pass")
  expect_equal(ev$severity, "info")
  expect_equal(ev$metric_value, 1)
})

test_that("the has_terms alias drives the pass/warn outcome, not just presence", {
  meta <- data.frame(
    dataset   = c("ADSL", "ADSL"),
    variable  = c("DOCD", "BARE"),
    code_id   = c("CL.DOCD", "CL.BARE"),
    has_terms = c(TRUE, FALSE),      # TRUE -> 1 term -> pass; FALSE -> 0 -> warn
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  docd <- ev[ev$location == "ADSL:DOCD", ]
  bare <- ev[ev$location == "ADSL:BARE", ]

  expect_equal(docd$result, "pass")
  expect_equal(docd$metric_value, 1)

  # has_terms = FALSE resolves to an entry documenting zero terms: a warn.
  expect_equal(bare$result, "warn")
  expect_equal(bare$severity, "medium")
  expect_equal(bare$metric_value, 0.5)
  expect_match(bare$message, "no terms")
})

test_that("codelist_type == 'external_library' is an external pass with its message", {
  meta <- data.frame(
    dataset       = "ADAE", variable = "AEBODSYS", code_id = "CL.MEDDRA",
    codelist_type = "external_library",
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  expect_equal(ev$result, "pass")
  expect_equal(ev$metric_value, 1)
  expect_match(ev$message, "external dictionary")
})

test_that("a blank-string code_id is treated as no reference and dropped", {
  meta <- data.frame(
    dataset  = "ADSL",
    variable = c("USUBJID", "SEX"),
    code_id  = c("   ", "CL.SEX"),   # whitespace-only: not a reference
    n_terms  = c(NA, 2L),
    stringsAsFactors = FALSE
  )
  cl <- as_codelist_metadata(meta)
  expect_equal(nrow(cl), 1L)
  expect_equal(cl$variable, "SEX")

  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))
  expect_equal(nrow(ev), 1L)
  expect_equal(ev$location, "ADSL:SEX")
})

test_that("an empty (zero-row) metadata data.frame is rejected", {
  empty <- data.frame(
    dataset  = character(0),
    variable = character(0),
    code_id  = character(0),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(as_codelist_metadata(empty)), 0L)
  expect_error(
    suppressMessages(codelist_to_evidence(empty, make_ctx())),
    "no variables that reference a codelist"
  )
})

test_that("data.frame-sourced evidence validates and carries Q-DEFINE-005", {
  meta <- data.frame(
    dataset  = c("ADSL", "ADSL", "ADAE", "ADSL"),
    variable = c("SEX", "RACE", "AEDECOD", "ETHNIC"),
    code_id  = c("CL.SEX", "CL.EMPTY", "CL.MEDDRA", "CL.GONE"),
    n_terms  = c(2L, 0L, NA, NA),
    external = c(FALSE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(codelist_to_evidence(meta, make_ctx()))

  expect_silent(r4subcore::validate_evidence(ev))
  expect_true(all(ev$indicator_id == "Q-DEFINE-005"))
  expect_setequal(ev$result, c("pass", "warn", "fail"))
  expect_true(all(ev$metric_unit == "score"))
  expect_true(all(ev$location == paste0(ev$asset_id, ":", c("SEX", "RACE", "AEDECOD", "ETHNIC"))))
})

# A plain list classed as 'Metacore' exercises the metacore unpack path without
# requiring the metacore package to be installed.
fake_metacore <- function(value_spec, codelist) {
  structure(list(value_spec = value_spec, codelist = codelist),
            class = "Metacore")
}

test_that("value_spec dataset is recovered from the qualified DATASET.VARIABLE key", {
  mc <- fake_metacore(
    value_spec = data.frame(
      # No dataset column: it must be recovered from the qualified key.
      variable = c("ADSL.SEX", "ADAE.AEDECOD", "ADSL.USUBJID"),
      code_id  = c("C66731", "MEDDRA", ""),   # blank -> dropped
      stringsAsFactors = FALSE
    ),
    codelist = data.frame(
      code_id = c("C66731", "MEDDRA"),
      type    = c("code_decode", "external_library"),
      stringsAsFactors = FALSE
    )
  )
  mc$codelist$codes <- list(
    data.frame(code = c("M", "F"), decode = c("Male", "Female")),
    NA
  )

  cl <- as_codelist_metadata(mc)
  expect_setequal(names(cl), codelist_metadata_columns())
  expect_equal(nrow(cl), 2L)
  expect_setequal(cl$dataset, c("ADSL", "ADAE"))
  expect_equal(cl$variable, c("SEX", "AEDECOD"))

  sex <- cl[cl$variable == "SEX", ]
  ae  <- cl[cl$variable == "AEDECOD", ]
  expect_equal(sex$dataset, "ADSL")
  expect_equal(sex$n_terms, 2L)
  expect_false(sex$external)
  expect_equal(ae$dataset, "ADAE")
  expect_true(ae$external)
  expect_true(is.na(ae$n_terms))

  ev <- suppressMessages(codelist_to_evidence(mc, make_ctx()))
  expect_equal(ev$location[ev$evidence_payload != ""][1], "ADSL:SEX")
  expect_true(all(ev$result == "pass"))
})

test_that("a dangling code_id (no matching codelist entry) fails", {
  mc <- fake_metacore(
    value_spec = data.frame(
      variable = c("ADSL.SEX", "ADSL.XX"),
      code_id  = c("C66731", "C_NOT_DEFINED"),
      stringsAsFactors = FALSE
    ),
    codelist = data.frame(
      code_id = "C66731", type = "code_decode", stringsAsFactors = FALSE
    )
  )
  mc$codelist$codes <- list(data.frame(code = c("M", "F"), decode = c("a", "b")))

  cl <- as_codelist_metadata(mc)
  dangling <- cl[cl$variable == "XX", ]
  expect_true(is.na(dangling$n_terms))
  expect_false(dangling$external)

  ev <- suppressMessages(codelist_to_evidence(mc, make_ctx()))
  fail <- ev[ev$location == "ADSL:XX", ]
  expect_equal(fail$result, "fail")
  expect_equal(fail$severity, "high")
  expect_equal(fail$metric_value, 0)
  expect_match(fail$message, "not documented")
})

test_that("pilot_ADaM evidence is all documented, correctly keyed, and validates", {
  skip_if_not_installed("metacore")
  load(metacore::metacore_example("pilot_ADaM.rda"))
  ev <- suppressMessages(codelist_to_evidence(metacore, make_ctx()))

  expect_silent(r4subcore::validate_evidence(ev))
  expect_true(nrow(ev) > 0L)
  expect_true(all(ev$indicator_id == "Q-DEFINE-005"))
  expect_true(all(ev$indicator_name == "Codelist Documented in Metadata"))
  # location is always dataset:variable and never dangling punctuation.
  expect_true(all(grepl("^[^:]+:[^:]+$", ev$location)))
  # Every row the pilot produces is documented (pass), so info severity throughout.
  expect_true(all(ev$result == "pass"))
  expect_true(all(ev$severity == "info"))
  expect_true(all(ev$metric_value == 1))
})
