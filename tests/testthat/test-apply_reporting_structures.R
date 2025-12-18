test_that(
  "apply_reporting_structures constructs retrospective triangles with structure 1", # nolint
  { # nolint
    # Setup
    triangle1 <- matrix(
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

    triangle2 <- matrix(
      c(
        65, 46, 21, 7,
        70, 40, 20, 5,
        80, 50, 10, 10,
        100, 40, 31, 20,
        95, 45, 21, NA,
        82, 42, NA, NA
      ),
      nrow = 6,
      byrow = TRUE
    )

    trunc_triangles <- list(
      make_test_triangle(data = triangle1),
      make_test_triangle(data = triangle2)
    )

    # Test 1: Check if the function returns a list
    expect_type(apply_reporting_structures(trunc_triangles), "list")

    # Test 2: Check if the number of retrospective triangles is correct
    expect_length(apply_reporting_structures(trunc_triangles), 2)

    # Test 3: Check that the dimensions of the returned triangles are as
    # expected
    retro_triangles <- apply_reporting_structures(trunc_triangles)
    expect_identical(dim(retro_triangles[[1]]), c(7L, 4L))
    expect_identical(dim(retro_triangles[[2]]), c(6L, 4L))

    # Test 4: Check if the content of the first retrospective triangle is
    # correct
    expect_identical(
      retro_triangles[[1]],
      apply_reporting_structure(make_test_triangle(data = triangle1))
    )

    # Test 5: Check if the content of the last retrospective triangle is correct
    expected_last_triangle <- make_test_triangle(data = matrix(
      c(
        65, 46, 21, 7,
        70, 40, 20, 5,
        80, 50, 10, 10,
        100, 40, 31, NA,
        95, 45, NA, NA,
        82, NA, NA, NA
      ),
      nrow = 6,
      byrow = TRUE
    ))
    expect_identical(retro_triangles[[2]], expected_last_triangle)


    expect_error(
      apply_reporting_structures(data.frame(trunc_triangles[1])),
      "data must be a matrix"
    )
  }
)

test_that(
  "apply_reporting_structures structure parameter is passed through correctly",
  {
    # Create test triangles
    triangle1 <- matrix(
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

    triangle2 <- matrix(
      c(
        65, 46, 21, 7,
        70, 40, 20, 5,
        80, 50, 10, 10,
        100, 40, 31, 20,
        95, 45, 21, NA,
        82, 42, NA, NA
      ),
      nrow = 6,
      byrow = TRUE
    )

    trunc_triangles <- list(
      make_test_triangle(data = triangle1),
      make_test_triangle(data = triangle2)
    )

    retro_triangles_custom <- apply_reporting_structures(trunc_triangles,
      structure = 2
    )

    expected_triangle1_struct2 <- make_test_triangle(data = matrix(
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
    expected_triangle1_struct2[5:7, 4] <- NA
    expected_triangle1_struct2[6:7, 3] <- NA
    expected_triangle1_struct2[7, 2] <- NA

    expect_identical(retro_triangles_custom[[1]], expected_triangle1_struct2)
  }
)
