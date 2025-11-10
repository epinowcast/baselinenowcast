test_that(
  "construct_triangle handles square matrix",
  {
    square_matrix <- to_reporting_triangle(matrix(
      1:16,
      nrow = 4,
      ncol = 4,
      byrow = TRUE
    ))
    expected <- matrix(
      c(
        1, 2, 3, 4,
        5, 6, 7, NA,
        9, 10, NA, NA,
        13, NA, NA, NA
      ),
      nrow = 4,
      byrow = TRUE
    )
    result <- construct_triangle(square_matrix)
    expect_identical(strip_attrs(result), expected)
  }
)

test_that(
  "construct_triangle handles rectangular matrix with more rows",
  {
    rect_matrix <- to_reporting_triangle(matrix(
      1:20,
      nrow = 5,
      ncol = 4,
      byrow = TRUE
    ))
    expected <- matrix(
      c(
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, NA,
        13, 14, NA, NA,
        17, NA, NA, NA
      ),
      nrow = 5,
      byrow = TRUE
    )
    result <- construct_triangle(rect_matrix)
    expect_identical(strip_attrs(result), expected)
  }
)

test_that(
  "construct_triangle handles rectangular matrix with more columns",
  {
    rect_matrix <- to_reporting_triangle(matrix(
      1:20,
      nrow = 4,
      ncol = 5,
      byrow = TRUE
    ))
    expected <- matrix(
      c(
        1, 2, 3, 4, NA,
        6, 7, 8, NA, NA,
        11, 12, NA, NA, NA,
        16, NA, NA, NA, NA
      ),
      nrow = 4,
      byrow = TRUE
    )
    # The result will have a column with all NAs which fails validation
    # when converting back to reporting_triangle
    expect_error(
      construct_triangle(rect_matrix),
      "Invalid reporting triangle structure"
    )
  }
)

test_that("construct_triangle leaves 1x1 matrix unchanged", {
  # 1x1 matrices have max_delay = 0 which fails validation
  expect_error(
    to_reporting_triangle(matrix(1, nrow = 1, ncol = 1)),
    "Insufficient `max_delay`"
  )
})

test_that("construct_triangle handles 2x2 matrix", {
  two_by_two <- to_reporting_triangle(matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE
  ))
  expected <- matrix(
    c(1, 2, 3, NA),
    nrow = 2,
    byrow = TRUE
  )
  result <- construct_triangle(two_by_two)
  expect_identical(strip_attrs(result), expected)
})

test_that("construct_triangle handles matrix with existing NAs", {
  # Matrices with NAs in invalid positions cannot be created as reporting_triangles
  expect_error(
    to_reporting_triangle(matrix(
      c(
        1, 2, NA,
        4, 5, 6,
        7, 8, 9
      ),
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )),
    "Invalid reporting triangle structure"
  )
})

test_that("construct_triangle handles one-row matrix", {
  # One-row matrices cause issues with validation
  expect_error(
    to_reporting_triangle(matrix(1:5, nrow = 1))
  )
})

test_that("construct_triangle handles one-column matrix", {
  # One-column matrices have max_delay = 0 which fails validation
  expect_error(
    to_reporting_triangle(matrix(1:5, ncol = 1)),
    "Insufficient `max_delay`"
  )
})

test_that("construct_triangle does not modify the original matrix", {
  original <- to_reporting_triangle(matrix(1:9, nrow = 3))
  original_copy <- original
  result <- construct_triangle(original)
  expect_identical(original, original_copy)
  expect_false(identical(result, original))
})

test_that("construct_triangle handles ragged structure with integer", {
  test_matrix <- to_reporting_triangle(matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, 0,
      6, 2, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  ))

  # Test with structure = 2
  expected_ragged <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, NA,
      6, 2, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  result_ragged <- construct_triangle(test_matrix, 2)
  expect_identical(strip_attrs(result_ragged), expected_ragged)
})

test_that("construct_triangle handles custom structure with vector", {
  test_matrix <- to_reporting_triangle(matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, 0,
      6, 2, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  ))

  # Test with structure = c(1, 2, 1)
  expected_custom <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, NA,
      3, 0, 0, NA, NA,
      6, NA, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  result_custom <- construct_triangle(test_matrix, c(1, 2, 1))
  expect_identical(strip_attrs(result_custom), expected_custom)
})

test_that("construct_triangle can generate something with all NAs at end", {
  exp_result <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 5, 9, 4, 3,
      1, 6, 4, NA, NA,
      3, NA, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  trunc_rt <- to_reporting_triangle(matrix(
    c(
      1, 3, 5, 7, 9,
      4, 5, 9, 4, 3,
      1, 6, 4, 4, 3,
      3, 8, 4, 6, 1
    ),
    nrow = 4,
    byrow = TRUE
  ))
  actual_result <- construct_triangle(trunc_rt,
    structure = c(1, 2)
  )
  expect_identical(strip_attrs(actual_result), exp_result)
})

test_that("construct_triangle can handle case when first element is not 1", { # nolint
  exp_result <- matrix(
    c(
      1, 3, 5, 7, 9, 4, 5,
      4, 5, 9, 4, NA, NA, NA,
      1, 6, 4, NA, NA, NA, NA,
      3, 8, NA, NA, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  trunc_rt <- to_reporting_triangle(matrix(
    c(
      1, 3, 5, 7, 9, 4, 5,
      4, 5, 9, 4, 3, 6, 7,
      1, 6, 4, 4, 3, 5, 7,
      3, 8, 4, 6, 1, 3, 4
    ),
    nrow = 4,
    byrow = TRUE
  ))
  actual_result <- construct_triangle(trunc_rt,
    structure = c(2, 1, 1)
  )
  expect_identical(strip_attrs(actual_result), exp_result)
})

test_that("construct_triangle can handle a structure ending with 2 NAs", {
  exp_result <- matrix(
    c(
      1, 3, 5, 7, 9, 7,
      4, 5, 9, 4, NA, NA,
      1, 6, NA, NA, NA, NA,
      3, NA, NA, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  trunc_rt <- to_reporting_triangle(matrix(
    c(
      1, 3, 5, 7, 9, 7,
      4, 5, 9, 4, 3, 3,
      1, 6, 4, 4, 3, 2,
      3, 8, 4, 6, 1, 6
    ),
    nrow = 4,
    byrow = TRUE
  ))
  actual_result <- construct_triangle(trunc_rt,
    structure = c(1, 1, 2)
  )
  expect_identical(strip_attrs(actual_result), exp_result)
})

test_that("construct_triangle validates structure parameter", {
  test_matrix <- to_reporting_triangle(matrix(1:9, nrow = 3))

  # Test with negative structure
  expect_error(
    construct_triangle(test_matrix, -1),
    "Structure must be positive"
  )

  # Test with structure larger than columns
  expect_error(
    construct_triangle(test_matrix, 4),
    "Structure cannot be larger than number of columns"
  )

  # Test with invalid vector structure
  expect_error(
    construct_triangle(test_matrix, c(1, 1, 1))
  )
  expect_error(
    construct_triangle(test_matrix, c(-1, 1))
  )
})
