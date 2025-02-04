test_that("string is returned without brackets", {
  new_string <- .autoescape_brackets("This string has {brackets}")
  expect_identical(new_string, "This string has brackets")
})
