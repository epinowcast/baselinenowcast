# Sample matrix from the example
test_triangle <- make_test_triangle(data = matrix(
  c(
    65, 46, 21, 7,
    70, 40, 20, 5,
    80, 50, 10, 10,
    100, 40, 31, 20,
    95, 45, 21, NA,
    82, 42, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 7,
  byrow = TRUE
))

test_that("truncate_to_rows returns correct number of truncated matrices with valid input", { # nolint
  n <- 2
  result <- truncate_to_rows(test_triangle, n = n)

  # Verify list length
  expect_length(result, n)

  # Verify matrix properties
  expect_true(all(sapply(result, is.matrix)))
  expect_true(all(sapply(result, \(x) ncol(x) == ncol(test_triangle))))

  # Verify row reduction pattern
  expect_identical(nrow(result[[1]]), nrow(test_triangle) - 1L)
  expect_identical(nrow(result[[2]]), nrow(test_triangle) - 2L)
})

test_that("truncate_to_rows calculates default n from triangle structure", {
  expected_default <- nrow(test_triangle) -
    sum(is.na(rowSums(test_triangle))) - 1
  result <- truncate_to_rows(test_triangle)
  expect_length(result, expected_default)
})


test_that("truncate_to_rows edge cases are handled properly", {
  # n = 0 returns empty list
  expect_length(truncate_to_rows(test_triangle, n = 0), 0)

  # Input validation
  expect_error(
    truncate_to_rows(as.data.frame(test_triangle)),
    "data must be a matrix"
  ) # nolint
  expect_error(
    truncate_to_rows(test_triangle, n = -1),
    "Assertion on 'n' failed: Element 1 is not >= 0."
  )
  expect_error(
    truncate_to_rows(test_triangle, n = 2.5),
    "Assertion on 'n' failed: Must be of type 'integerish'"
  )
})

test_that("truncate_to_rows can handle a range of ns", {
  ncols <- ncol(test_triangle) - 1
  nrows <- nrow(test_triangle) - 1

  expect_silent(truncate_to_rows(test_triangle, n = ncols))
  expect_silent(truncate_to_rows(test_triangle, n = nrows))
  expect_silent(truncate_to_rows(test_triangle, n = 2))
  expect_error(truncate_to_rows(test_triangle, n = -1))
})

test_that(
  "truncate_to_rows replaces values with NA for retrospective snapshots",
  {
<<<<<<< HEAD:tests/testthat/test-truncate_triangles.R
    result <- truncate_triangles(test_triangle, n = 1)[[1]]
=======
    result <- truncate_to_rows(test_triangle, n = 1)[[1]]
>>>>>>> main:tests/testthat/test-truncate_to_rows.R
    # Expect bottom 3 elemets of lower left triangle to be NAs
    expect_true(all(
      anyNA(result[5, 4]),
      anyNA(result[6, 3:4])
    ))
  }
)

test_that("truncate_to_rows truncated matrices preserve original structure", {
  result <- truncate_to_rows(test_triangle, n = 1)[[1]]

  # Verify first rows remain unchanged
  expect_identical(
    result[1:(nrow(result) - 1), ],
    test_triangle[1:(nrow(result) - 1), ]
  )
})

test_that("truncate_to_rows: default works well for ragged triangle", {
  sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1)

  # Generate counts for each reference date
  counts <- c(
    150,
    160, 170, 200, 100
  )

  # Create a complete triangle based on the known delay PMF
  complete_triangle_mat <- lapply(counts, function(x) round(x * sim_delay_pmf))
  complete_triangle_mat <- do.call(rbind, complete_triangle_mat)
  complete_triangle <- make_test_triangle(data = complete_triangle_mat)

  ragged_triangle <- apply_reporting_structure(
    complete_triangle,
    structure = 2
  )

  truncated_triangles <- truncate_to_rows(ragged_triangle)
  expect_length(truncated_triangles, 1L)
})

test_that("truncate_to_rows preserves reporting_triangle class", {
  rep_tri_mat <- matrix(
    c(
      100, 50, 25, 10,
      80, 40, 20, 5,
      90, 45, 15, NA,
      70, 35, NA, NA,
      60, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  ref_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 5)
  rep_tri_obj <- as_reporting_triangle(
    data = rep_tri_mat,
    reference_dates = ref_dates,
    max_delay = 3
  )

  # Truncate to 2 triangles
  result <- truncate_to_rows(rep_tri_obj, n = 2)

  # Check list properties
  expect_length(result, 2)

  # Check first element
  expect_true(is_reporting_triangle(result[[1]]))
  expect_s3_class(result[[1]], "reporting_triangle")
  expect_identical(nrow(result[[1]]), 4L)
  expect_identical(ncol(result[[1]]), 4L)
  expect_identical(get_max_delay(result[[1]]), 3L)
  expect_length(get_reference_dates(result[[1]]), 4L)

  # Check second element
  expect_true(is_reporting_triangle(result[[2]]))
  expect_s3_class(result[[2]], "reporting_triangle")
  expect_identical(nrow(result[[2]]), 3L)
  expect_identical(ncol(result[[2]]), 4L)
  expect_identical(get_max_delay(result[[2]]), 3L)
  expect_length(get_reference_dates(result[[2]]), 3L)
})

test_that("truncate_to_rows with plain matrix errors", {
  plain_mat <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    truncate_to_rows(plain_mat, n = 2),
    "data must have class 'reporting_triangle'"
  )
})
