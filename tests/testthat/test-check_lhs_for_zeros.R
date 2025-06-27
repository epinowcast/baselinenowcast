invalid_mat <- matrix(
  c(
    0, 0, 1, 2, 3,
    0, 0, NA, NA, NA
  ),
  byrow = TRUE,
  nrow = 2
)
valid_mat1 <- matrix(
  c(
    0, 0, 1, 2, 3,
    0, 1, NA, NA, NA
  ),
  byrow = TRUE,
  nrow = 2
)
valid_mat2 <- matrix(
  c(
    0, 0, 1, 2, 3,
    1, 0, NA, NA, NA
  ),
  byrow = TRUE,
  nrow = 2
)

invalid_mat2 <- matrix(
  c(
    0, 0, 4, 5, 6,
    0, 0, 1, 2, 3,
    0, 0, NA, NA, NA
  ),
  byrow = TRUE,
  nrow = 3
)
valid_mat3 <- matrix(
  c(
    0, 0, 4, 5, 6,
    0, 0, 1, 2, 3,
    0, 0, 1, 2, 3
  ),
  byrow = TRUE,
  nrow = 3
)
valid_mat4 <- matrix(
  c(
    4, 1, 4, 5, 6,
    0, 0, 1, 2, 3,
    0, 0, NA, NA, NA
  ),
  byrow = TRUE,
  nrow = 3
)


test_that(".check_lhs_for_zeros correctly handles matrix with 0s", {
  expect_false(.check_lhs_for_zeros(invalid_mat))
  expect_false(.check_lhs_for_zeros(invalid_mat2))
  expect_true(.check_lhs_for_zeros(valid_mat1))
  expect_true(.check_lhs_for_zeros(valid_mat2))
  expect_true(.check_lhs_for_zeros(valid_mat3))
  expect_true(.check_lhs_for_zeros(valid_mat4))
})
