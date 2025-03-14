test_that("generate_triangles works correctly", {
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
  expect_type(generate_triangles(triangle, 2), "list")

  # Test 2: Check if the number of retrospective triangles is correct
  expect_length(generate_triangles(triangle, 2), 2)

  # Test 3: Check if the number of retro triangles is correct without specifying
  expect_length(generate_triangles(triangle), 2)

  # Test 4: Check that you get a warning if you
  # ask for more triangles than will be able to generate a nowcast
  expect_warning(generate_triangles(triangle, 4))


  # Test 3: Check that the dimensions of the returned triangles are as
  # expected
  retro_triangles <- generate_triangles(triangle)
  expect_identical(dim(retro_triangles[[1]]), c(6L, 4L))
  expect_identical(dim(retro_triangles[[2]]), c(5L, 4L))

  # Test 4: Check if the content of the first retrospective triangle is correct
  expect_identical(
    retro_triangles[[1]],
    .replace_lower_right_with_NA(triangle[1:6, ])
  )

  # Test 5: Check if the content of the last retrospective triangle is correct
  expected_last_triangle <- matrix(
    c(
      65, 46, 21, 7,
      70, 40, 20, 5,
      80, 50, 10, NA,
      100, 40, NA, NA,
      95, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  expect_identical(retro_triangles[[2]], expected_last_triangle)
})
