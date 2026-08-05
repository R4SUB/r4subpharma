# Tests for the metacore-object path of as_variable_metadata (R/metadata.R).
# A synthetic Metacore-classed object lets us exercise the qualified
# "DATASET.VARIABLE" join without depending on a specific metacore build.

fake_metacore <- function(qualified = TRUE) {
  vkey <- if (qualified) c("ADSL.AGE", "ADSL.SEX") else c("AGE", "SEX")
  structure(
    list(
      ds_vars = data.frame(
        dataset = c("ADSL", "ADSL"), variable = c("AGE", "SEX"),
        stringsAsFactors = FALSE
      ),
      var_spec = data.frame(
        variable = vkey, label = c("Age", "Sex"),
        type = c("integer", "text"), stringsAsFactors = FALSE
      ),
      value_spec = data.frame(
        dataset = c("ADSL", "ADSL"), variable = vkey,
        origin = c("Derived", "CRF"),
        derivation_id = c("MD1", NA), stringsAsFactors = FALSE
      ),
      derivations = data.frame(
        derivation_id = "MD1", derivation = "Age from BRTHDTC",
        stringsAsFactors = FALSE
      )
    ),
    class = "Metacore"
  )
}

test_that("qualified DATASET.VARIABLE keys resolve all attributes", {
  meta <- as_variable_metadata(fake_metacore(qualified = TRUE))
  age <- meta[meta$variable == "AGE", ]
  expect_equal(age$label, "Age")
  expect_equal(age$type, "integer")
  expect_equal(age$origin, "Derived")
  expect_equal(age$derivation, "Age from BRTHDTC")
  expect_true(age$is_derived)

  sex <- meta[meta$variable == "SEX", ]
  expect_equal(sex$origin, "CRF")
  expect_false(sex$is_derived)
})

test_that("plain variable keys resolve too", {
  meta <- as_variable_metadata(fake_metacore(qualified = FALSE))
  age <- meta[meta$variable == "AGE", ]
  expect_equal(age$label, "Age")
  expect_equal(age$type, "integer")
  expect_equal(age$derivation, "Age from BRTHDTC")
})
