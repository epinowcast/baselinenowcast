df <- data.frame(
  group = c("A", "A", "B", "B"),
  value = 1:4
)

df2 <- data.frame(
  group1 = c("A", "A", "B", "B"),
  group2 = c("X", "Y", "X", "Y"),
  value = 1:4
)

test_that(".split_df_by_cols splits by single column", {
  result <- .split_df_by_cols(df, "group")

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("A", "B"))
  expect_identical(nrow(result$A), 2L)
  expect_identical(nrow(result$B), 2L)
  expect_equal(result$A$value, c(1, 2), tol = 0.0001)
  expect_equal(result$B$value, c(3, 4), tol = 0.0001)
})

test_that(".split_df_by_cols splits by multiple columns", {
  result <- .split_df_by_cols(df2, c("group1", "group2"))

  expect_type(result, "list")
  expect_length(result, 4)
  expect_true(all(names(result) %in% c("A___X", "A___Y", "B___X", "B___Y")))
  expect_identical(nrow(result$`A___X`), 1L)
  expect_equal(result$`A___X`$value, 1, tol = 0.0001)
  expect_equal(result$`B___Y`$value, 4, tol = 0.0001)
})

test_that(".split_df_by_cols NULL col_names returns single-element list", {
  result <- .split_df_by_cols(df, NULL)

  expect_type(result, "list")
  expect_length(result, 1)
  expect_identical(result[[1]], df)
})

test_that(".split_df_by_cols converts columns to factors", {
  result <- .split_df_by_cols(df, "group")

  # Check that group column in result is now a factor
  expect_s3_class(result$A$group, "factor")
  expect_s3_class(result$B$group, "factor")
})

test_that(".split_df_by_cols handles numeric columns", {
  df3 <- data.frame(
    group = c(1, 1, 2, 2),
    value = 1:4
  )

  result <- .split_df_by_cols(df3, "group")

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("1", "2"))
})

test_that(".split_df_by_cols preserves all columns", {
  df4 <- df2
  df4$group3 <- 1:4

  result <- .split_df_by_cols(df4, c("group1", "group2"))

  expect_identical(names(result$`A___X`), names(df4))
})
