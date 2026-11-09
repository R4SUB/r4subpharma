make_ctx <- function() {
  suppressMessages(r4subcore::r4sub_run_context("STUDY01", "DEV"))
}

demo_meta <- function() {
  data.frame(
    dataset    = "ADSL",
    variable   = c("USUBJID", "AGE", "SEX"),
    label      = c("Unique Subject Identifier", "Age", "Sex"),
    type       = c("text", "integer", "text"),
    origin     = c("Predecessor", "Derived", "Predecessor"),
    derivation = c(NA, "Age at informed consent", NA),
    structure  = "one record per subject",
    n_keys     = 1L,
    stringsAsFactors = FALSE
  )
}

# created_at is a wall-clock stamp set per adapter call, so it differs between
# the wrapper's internal calls and a second set of calls in the test. Compare
# every other column.
without_created_at <- function(ev) {
  ev <- ev[, setdiff(names(ev), "created_at"), drop = FALSE]
  rownames(ev) <- NULL
  ev
}

test_that("pharma_evidence equals the row-bind of the metadata adapters", {
  meta <- demo_meta()
  ctx  <- make_ctx()

  combined <- suppressMessages(pharma_evidence(meta, ctx))

  expected <- rbind(
    suppressMessages(metacore_to_evidence(meta, ctx)),
    suppressMessages(dataset_metadata_to_evidence(meta, ctx))
  )
  rownames(expected) <- NULL

  expect_equal(without_created_at(combined), without_created_at(expected))
})

test_that("pharma_evidence adds the ADaM adapter rows when data is supplied", {
  meta <- demo_meta()
  ctx  <- make_ctx()
  adsl <- data.frame(USUBJID = "01-001", AGE = 54, SEX = "M",
                     stringsAsFactors = FALSE)

  combined <- suppressMessages(
    pharma_evidence(meta, ctx, data = adsl, dataset_name = "ADSL")
  )

  expected <- rbind(
    suppressMessages(metacore_to_evidence(meta, ctx)),
    suppressMessages(dataset_metadata_to_evidence(meta, ctx)),
    suppressMessages(adam_to_evidence(adsl, meta, ctx, dataset_name = "ADSL"))
  )
  rownames(expected) <- NULL

  expect_equal(without_created_at(combined), without_created_at(expected))
  # The ADaM indicators appear only on the data path.
  expect_true(all(c("Q-DEFINE-002", "Q-DEFINE-004", "T-ADAM-001") %in%
                    combined$indicator_id))
})

test_that("the data path is a strict superset of the metadata-only path", {
  meta <- demo_meta()
  ctx  <- make_ctx()
  adsl <- data.frame(USUBJID = "01-001", AGE = 54, SEX = "M",
                     stringsAsFactors = FALSE)

  meta_only <- suppressMessages(pharma_evidence(meta, ctx))
  with_data <- suppressMessages(
    pharma_evidence(meta, ctx, data = adsl, dataset_name = "ADSL")
  )

  expect_equal(
    without_created_at(with_data)[seq_len(nrow(meta_only)), ],
    without_created_at(meta_only)
  )
  expect_gt(nrow(with_data), nrow(meta_only))
})

test_that("source labels are forwarded to the composed adapters", {
  meta <- demo_meta()
  ctx  <- make_ctx()
  adsl <- data.frame(USUBJID = "01-001", AGE = 54, SEX = "M",
                     stringsAsFactors = FALSE)

  ev <- suppressMessages(pharma_evidence(
    meta, ctx,
    data = adsl, dataset_name = "ADSL",
    source_name = "metacore-x", source_version = "1.2",
    adam_source_name = "adam-x", adam_source_version = "3.4"
  ))

  meta_rows <- ev[ev$indicator_id %in% c("Q-DEFINE-002", "Q-DEFINE-003", "Q-DEFINE-004"), ]
  adam_rows <- ev[grepl("ADAM", ev$indicator_id), ]
  expect_true(all(meta_rows$source_name == "metacore-x"))
  expect_true(all(meta_rows$source_version == "1.2"))
  expect_true(all(adam_rows$source_name == "adam-x"))
  expect_true(all(adam_rows$source_version == "3.4"))
})

test_that("pharma_evidence rejects a bad context", {
  expect_error(
    suppressMessages(pharma_evidence(demo_meta(), ctx = "nope")),
    "r4sub_run_context"
  )
})

test_that("pharma_evidence composes a real metacore object", {
  skip_if_not_installed("metacore")
  load(metacore::metacore_example("pilot_ADaM.rda"))
  ctx <- make_ctx()

  combined <- suppressMessages(pharma_evidence(metacore, ctx))
  expected <- rbind(
    suppressMessages(metacore_to_evidence(metacore, ctx)),
    suppressMessages(dataset_metadata_to_evidence(metacore, ctx))
  )
  rownames(expected) <- NULL

  expect_equal(without_created_at(combined), without_created_at(expected))
  expect_true(all(c("Q-DEFINE-002", "Q-DEFINE-004") %in% combined$indicator_id))
})
