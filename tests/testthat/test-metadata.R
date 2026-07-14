test_that("as_variable_metadata completes a data.frame and infers is_derived", {
  meta <- as_variable_metadata(data.frame(
    dataset  = "ADSL",
    variable = c("USUBJID", "TRTSDT"),
    label    = c("Unique Subject Identifier", "Date of First Exposure"),
    type     = c("text", "integer"),
    origin   = c("Predecessor", "Derived"),
    stringsAsFactors = FALSE
  ))

  expect_setequal(names(meta), metadata_columns())
  expect_equal(nrow(meta), 2L)
  expect_false(meta$is_derived[1])
  expect_true(meta$is_derived[2])
})

test_that("as_variable_metadata treats blank strings as missing", {
  meta <- as_variable_metadata(data.frame(
    dataset = "ADSL", variable = "AGE", label = "  ", type = "",
    stringsAsFactors = FALSE
  ))
  expect_true(is.na(meta$label))
  expect_true(is.na(meta$type))
})

test_that("as_variable_metadata validates its input", {
  expect_error(as_variable_metadata(data.frame(variable = "AGE")), "dataset")
  expect_error(as_variable_metadata(42), "Metacore")
})

test_that("type normalization maps to character/numeric families", {
  expect_equal(normalize_type(c("text", "integer", "float")),
               c("character", "numeric", "numeric"))
  expect_true(is.na(normalize_type("datetime")))
  expect_equal(observed_type(1:3), "numeric")
  expect_equal(observed_type(letters), "character")
})
