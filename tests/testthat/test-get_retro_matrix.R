test_that(".get_retro_matrix works correctly", {
  # Setup
  matr_observed <- matrix(
    c(
      65, 46, 21, 7,
      100, 80, 60, 40,
      90, 70, 50, NA,
      80, 60, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )

  # Test 1: Check if function returns a matrix
  expect_true(is.matrix(.get_retro_matrix(1, matr_observed, 4)))

  # Test 2: Check dimensions of returned matrix
  expect_identical(dim(.get_retro_matrix(1, matr_observed, 4)), c(4L, 4L))

  # Test 3: Check content of returned matrix for t = 1
  expected_matrix_t1 <- matrix(
    c(
      65, 46, 21, 7,
      100, 80, 60, NA,
      90, 70, NA, NA,
      80, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_identical(.get_retro_matrix(1, matr_observed, 4), expected_matrix_t1)


  # Test 4: Check if function handles edge case (t = 0)
  expect_identical(.get_retro_matrix(0, matr_observed, 4), matr_observed[2:5, ])

  # Test 6: Check if function handles n_history_delay = 1
  expected_matrix_single_row <- matrix(c(80, NA, NA, NA), nrow = 1)
  expect_identical(
    .get_retro_matrix(1, matr_observed, 1),
    expected_matrix_single_row
  )
})
