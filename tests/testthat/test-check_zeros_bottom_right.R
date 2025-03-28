test_that("Check bottom right zeros work correctly", {
  # Test 1: Diagonal zeros pattern (should return TRUE)
  mat1 <- matrix(c(
    1, 1, 1,
    1, 1, 0,
    1, 0, 0
  ), nrow = 3, byrow = TRUE)
  expect_true(.check_zeros_bottom_right(mat1))

  # Test 2: All mask zeros with extra zeros (should return TRUE)
  mat2 <- matrix(c(
    0, 1, 1,
    1, 0, 0,
    0, 0, 0
  ), nrow = 3, byrow = TRUE)
  expect_true(.check_zeros_bottom_right(mat2))

  # Test 3: Non-zero in mask area (should return FALSE)
  mat3 <- matrix(c(
    1, 1, 1,
    1, 1, 1,
    1, 1, 1
  ), nrow = 3, byrow = TRUE)
  expect_false(.check_zeros_bottom_right(mat3))

  # Test 4: Most but not all are 0s (should return FALSE)
  mat4 <- matrix(c(
    5, 2, 3, 4,
    1, 5, 0, 0,
    9, 0, 0, 0
  ), nrow = 3, byrow = TRUE)
  expect_false(.check_zeros_bottom_right(mat4))

  # Test 5: Single element matrix (edge case)
  mat5 <- matrix(0)
  expect_true(.check_zeros_bottom_right(mat5))
})

test_that("Special cases handled correctly", {
  # Test matrix with NA values
  mat_na <- matrix(
    c(
      1, NA,
      0, 0
    ),
    nrow = 2
  )
  expect_true(.check_zeros_bottom_right(mat_na))
})
