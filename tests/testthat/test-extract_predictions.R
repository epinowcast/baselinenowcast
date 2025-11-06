test_that(
  ".extract_predictions: function returns correct matrix for the example case",
  { # nolint
    pt_nowcast_mat <- matrix(
      c(
        1, 3, 5, 7,
        4, 7, 8, 9,
        9, 10, 3, 5,
        3, 4, 8, 5
      ),
      nrow = 4,
      byrow = TRUE
    )

    reporting_matrix <- matrix(
      c(
        1, 3, 5, 7,
        4, 7, 8, NA,
        9, 10, NA, NA,
        3, NA, NA, NA
      ),
      nrow = 4,
      byrow = TRUE
    )

    # Expected result - NA where reporting_matrix has values, pt_nowcast_mat
    # values elsewhere
    expected_result <- matrix(
      c(
        NA, NA, NA, NA,
        NA, NA, NA, 9,
        NA, NA, 3, 5,
        NA, 4, 8, 5
      ),
      nrow = 4,
      byrow = TRUE
    )

    result <- .extract_predictions(pt_nowcast_mat, reporting_matrix)

    expect_identical(result, expected_result)
  }
)

test_that(".extract_predictions: function throws error when appropriate", {
  pt_nowcast_mat <- matrix(
    c(
      1, 3, 5, 7,
      4, 7, 8, 9,
      9, 10, 3, 5,
      3, 4, 8, 5
    ),
    nrow = 4,
    byrow = TRUE
  )
  reporting_matrix_mismatch <- matrix(
    c(
      1, 5, 5, 7,
      4, 7, 8, NA,
      9, 10, NA, NA,
      3, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_error(.extract_predictions(pt_nowcast_mat, reporting_matrix_mismatch))
})
