test_that("detect_structure works with small triangle", {
  ragged_triangle <- matrix(
    c(
      1, 3, 5, 7, 9, 4, 5,
      4, 5, 9, 4, NA, NA, NA,
      1, 6, 4, NA, NA, NA, NA,
      3, 8, NA, NA, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  exp_structure <- c(2, 1, 1)
  detected_structure <- detect_structure(ragged_triangle)

  expect_identical(exp_structure, detected_structure)


  ragged_triangle2 <- matrix(
    c(
      1, 3, 5, 7, 9, 7,
      4, 5, 9, 4, NA, NA,
      1, 6, NA, NA, NA, NA,
      3, NA, NA, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  exp_structure2 <- c(1, 1, 2)

  detected_structure2 <- detect_structure(ragged_triangle2)
  expect_identical(exp_structure2, detected_structure2)

  ragged3 <- matrix(
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
  exp_structure3 <- c(1, 2, 1)
  detected_structure3 <- detect_structure(ragged3)

  expect_identical(exp_structure3, detected_structure3)
})

test_that("detect_structure detects a symmetric reporting triangle", {
  rep_tri <- matrix(
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

  exp_structure <- 1
  detected_structure <- detect_structure(rep_tri)
  expect_identical(exp_structure, detected_structure)
})

test_that("detect_structure returns 0 if there are no NAs", {
  rep_mat <- matrix(
    data = 5,
    nrow = 7,
    ncol = 4
  )
  exp_structure <- 0
  detected_structure <- detect_structure(rep_mat)

  expect_identical(exp_structure, detected_structure)
})
