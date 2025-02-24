test_that("generate_retrospective_data works correctly", {
  # Setup
  triangle <- matrix(
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

  # Test 1: Check if the function returns a list
  expect_type(generate_retrospective_data(triangle), "list")

  # Test 2: Check if the number of retrospective triangles is correct
  expect_length(generate_retrospective_data(triangle), 3)

  # Test 3: Check that the dimensions of the returned triangles are all
  # the same
  retro_triangles <- generate_retrospective_data(triangle)
  expect_identical(dim(retro_triangles[[1]]), c(4L, 4L))
  expect_identical(dim(retro_triangles[[2]]), c(4L, 4L))
  expect_identical(dim(retro_triangles[[3]]), c(4L, 4L))

  # Test 4: Check if the content of the first retrospective triangle is correct
  expect_identical(retro_triangles[[1]], triangle)

  # Test 5: Check if the content of the last retrospective triangle is correct
  expected_last_triangle <- matrix(
    c(
      65, 46, 21, 7,
      70, 40, 20, 5,
      80, 50, 10, 10,
      100, 40, 31, NA,
      95, 45, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  expect_equal(retro_triangles[[3]], expected_last_triangle)

  # Test 6: Check if the function handles custom n_history_uncertainty
  custom_retro <- generate_retrospective_data(triangle, n_history_uncertainty = 2)
  expect_length(custom_retro, 2)

  # Test 7: Check if the function handles custom n_history_delay
  custom_delay_retro <- generate_retrospective_data(triangle, n_history_delay = 5)
  expect_equal(dim(custom_delay_retro[[1]]), c(7, 4))
  expect_equal(dim(custom_delay_retro[[2]]), c(6, 4))

  # Test 8: Check if the function throws an error when n_history_uncertainty + n_history_delay > nrow(triangle)
  expect_error(generate_retrospective_data(triangle, n_history_uncertainty = 5, n_history_delay = 3))
})
