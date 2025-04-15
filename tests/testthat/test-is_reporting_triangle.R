test_that("Perfect reporting triangle returns TRUE", {
  mat <- matrix(
    c(
      1, 2, 3,
      4, 5, NA,
      6, NA, NA
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_true(.is_reporting_triangle(mat))
})

test_that("NA outside bottom right returns FALSE", {
  mat <- matrix(c(
    1, NA, 3,
    4, 5, NA,
    6, NA, NA
  ), nrow = 3, byrow = TRUE)
  expect_false(.is_reporting_triangle(mat))
})

test_that("Non-NA in bottom right returns FALSE", {
  mat <- matrix(c(
    1, 2, 3,
    4, 5, 9,
    6, NA, NA
  ), nrow = 3, byrow = TRUE)
  expect_false(.is_reporting_triangle(mat))
})

test_that("All NAs returns FALSE", {
  mat <- matrix(NA, nrow = 3, ncol = 3)
  expect_false(.is_reporting_triangle(mat))
})

test_that("No NAs returns FALSE", {
  mat <- matrix(1:9, nrow = 3)
  expect_false(.is_reporting_triangle(mat))
})

test_that("Non-square matrix returns correct result", {
  mat <- matrix(c(
    1, 2, 3, 4,
    5, 6, 7, NA,
    8, 9, NA, NA
  ), nrow = 3, byrow = TRUE)
  expect_false(.is_reporting_triangle(mat))

  matlong <- matrix(c(
    1, 2, 3,
    4, 5, 7,
    5, 6, NA,
    8, NA, NA
  ), nrow = 4, byrow = TRUE)
  expect_true(.is_reporting_triangle(matlong))
})
