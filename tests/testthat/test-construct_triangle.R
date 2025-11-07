#' Expect valid triangle (matrix with potential NAs in bottom-right)
#' @keywords internal
expect_valid_triangle <- function(object, has_nas = TRUE) {
  expect_is(object, "matrix")
  if (has_nas) {
    expect_true(anyNA(object))
  } else {
    expect_false(anyNA(object))
  }
  invisible(object)
}

test_that(
  "construct_triangle handles square matrix",
  {
    square_matrix <- matrix(
      1:16,
      nrow = 4,
      ncol = 4,
      byrow = TRUE
    )
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
    expect_valid_triangle(result, has_nas = TRUE)
    expect_identical(result, expected)
  }
)

test_that(
  "construct_triangle handles rectangular matrix with more rows",
  {
    rect_matrix <- matrix(
      1:20,
      nrow = 5,
      ncol = 4,
      byrow = TRUE
    )
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
    expect_valid_triangle(result, has_nas = TRUE)
    expect_identical(result, expected)
  }
)

test_that(
  "construct_triangle handles rectangular matrix with more columns",
  {
    rect_matrix <- matrix(
      1:20,
      nrow = 4,
      ncol = 5,
      byrow = TRUE
    )
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
    result <- construct_triangle(rect_matrix)
    expect_valid_triangle(result, has_nas = TRUE)
    expect_identical(result, expected)
  }
)

test_that("construct_triangle leaves 1x1 matrix unchanged", {
  single_cell <- matrix(1, nrow = 1, ncol = 1)
  result <- construct_triangle(single_cell)
  expect_valid_triangle(result, has_nas = FALSE)
  expect_identical(result, single_cell)
})

test_that("construct_triangle handles 2x2 matrix", {
  two_by_two <- matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE
  )
  expected <- matrix(
    c(1, 2, 3, NA),
    nrow = 2,
    byrow = TRUE
  )
  result <- construct_triangle(two_by_two)
  expect_valid_triangle(result, has_nas = TRUE)
  expect_identical(result, expected)
})

test_that("construct_triangle handles matrix with existing NAs", {
  na_matrix <- matrix(
    c(
      1, 2, NA,
      4, 5, 6,
      7, 8, 9
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  expected <- matrix(
    c(
      1, 2, NA,
      4, 5, NA,
      7, NA, NA
    ),
    nrow = 3,
    byrow = TRUE
  )
  result <- construct_triangle(na_matrix)
  expect_valid_triangle(result, has_nas = TRUE)
  expect_identical(result, expected)
})

test_that("construct_triangle handles one-row matrix", {
  one_row <- matrix(1:5, nrow = 1)
  expected <- matrix(
    c(1, NA, NA, NA, NA),
    nrow = 1
  )
  result <- construct_triangle(one_row)
  expect_valid_triangle(result, has_nas = TRUE)
  expect_identical(result, expected)
})

test_that("construct_triangle handles one-column matrix", {
  one_col <- matrix(1:5, ncol = 1)
  result <- construct_triangle(one_col)
  expect_valid_triangle(result, has_nas = FALSE)
  expect_identical(result, one_col)
})

test_that("construct_triangle does not modify the original matrix", {
  original <- matrix(1:9, nrow = 3)
  original_copy <- original
  result <- construct_triangle(original)
  expect_valid_triangle(result, has_nas = TRUE)
  expect_identical(original, original_copy)
  expect_false(identical(result, original))
})

test_that("construct_triangle handles ragged structure with integer", {
  test_matrix <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, 0,
      6, 2, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  )

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
  expect_identical(result_ragged, expected_ragged)
})

test_that("construct_triangle handles custom structure with vector", {
  test_matrix <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, 0,
      6, 2, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  )

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
  expect_identical(result_custom, expected_custom)
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

  trunc_rt <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 5, 9, 4, 3,
      1, 6, 4, 4, 3,
      3, 8, 4, 6, 1
    ),
    nrow = 4,
    byrow = TRUE
  )
  actual_result <- construct_triangle(trunc_rt,
    structure = c(1, 2)
  )
  expect_identical(exp_result, actual_result)
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

  trunc_rt <- matrix(
    c(
      1, 3, 5, 7, 9, 4, 5,
      4, 5, 9, 4, 3, 6, 7,
      1, 6, 4, 4, 3, 5, 7,
      3, 8, 4, 6, 1, 3, 4
    ),
    nrow = 4,
    byrow = TRUE
  )
  actual_result <- construct_triangle(trunc_rt,
    structure = c(2, 1, 1)
  )
  expect_identical(exp_result, actual_result)
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

  trunc_rt <- matrix(
    c(
      1, 3, 5, 7, 9, 7,
      4, 5, 9, 4, 3, 3,
      1, 6, 4, 4, 3, 2,
      3, 8, 4, 6, 1, 6
    ),
    nrow = 4,
    byrow = TRUE
  )
  actual_result <- construct_triangle(trunc_rt,
    structure = c(1, 1, 2)
  )
  expect_identical(exp_result, actual_result)
})

test_that("construct_triangle validates structure parameter", {
  test_matrix <- matrix(1:9, nrow = 3)

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
