# Sample matrix from the example
test_triangle <- matrix(
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
)

test_that("truncate_triangles returns correct number of truncated matrices with valid input", { # nolint
  n <- 2
  result <- truncate_triangles(test_triangle, n = n)

  # Verify list length
  expect_length(result, n)

  # Verify matrix properties
  expect_true(all(sapply(result, is.matrix)))
  expect_true(all(sapply(result, \(x) ncol(x) == ncol(test_triangle))))

  # Verify row reduction pattern
  expect_identical(nrow(result[[1]]), nrow(test_triangle) - 1L)
  expect_identical(nrow(result[[2]]), nrow(test_triangle) - 2L)
})

test_that("truncate_triangles default n calculation works correctly", {
  expected_default <- nrow(test_triangle) -
    sum(is.na(rowSums(test_triangle))) - 1
  result <- truncate_triangles(test_triangle)
  expect_length(result, expected_default)
})


test_that("truncate_triangles edge cases are handled properly", {
  # n = 0 returns empty list
  expect_length(truncate_triangles(test_triangle, n = 0), 0)

  # Input validation
  expect_error(
    truncate_triangles(as.data.frame(test_triangle)),
    "Assertion on 'triangle' failed: Must inherit from class 'matrix'"
  ) # nolint
  expect_error(
    truncate_triangles(test_triangle, n = -1),
    "Assertion on 'n' failed: Element 1 is not >= 0."
  )
  expect_error(
    truncate_triangles(test_triangle, n = 2.5),
    "Assertion on 'n' failed: Must be of type 'integerish'"
  )
})

test_that("truncate_triangles can handle a range of ns", {
  ncols <- ncol(test_triangle) - 1
  nrows <- nrow(test_triangle) - 1

  expect_silent(truncate_triangles(test_triangle, n = ncols))
  expect_silent(truncate_triangles(test_triangle, n = nrows))
  expect_silent(truncate_triangles(test_triangle, n = 2))
  expect_error(truncate_triangles(test_triangle, n = -1))
})

test_that("truncate_triangles NA replacement works as expected", {
  result <- truncate_triangles(test_triangle, n = 1)[[1]]
  # Expect bottom 3 elemets of lower left triangle to be NAs
  expect_true(all(
    anyNA(result[5, 4]),
    anyNA(result[6, 3:4])
  ))
})

test_that("truncate_triangles truncated matrices preserve original structure", {
  result <- truncate_triangles(test_triangle, n = 1)[[1]]

  # Verify first rows remain unchanged
  expect_identical(
    result[1:(nrow(result) - 1), ],
    test_triangle[1:(nrow(result) - 1), ]
  )
})

test_that("truncate_triangles: default works well for ragged triangle", {
  sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1)

  # Generate counts for each reference date
  counts <- c(
    150,
    160, 170, 200, 100
  )

  # Create a complete triangle based on the known delay PMF
  complete_triangle <- lapply(counts, function(x) round(x * sim_delay_pmf))
  complete_triangle <- do.call(rbind, complete_triangle)

  ragged_triangle <- construct_triangle(
    complete_triangle,
    structure = 2
  )

  truncated_triangles <- truncate_triangles(ragged_triangle)
  expect_length(truncated_triangles, 1L)
})
