# Test-specific constants
df <- data.frame(
  group = c("A", "A", "B", "B"),
  value = 1:4,
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  group1 = c("A", "A", "B", "B"),
  group2 = c("X", "Y", "X", "Y"),
  value = 1:4,
  stringsAsFactors = FALSE
)

test_that(".split_df_by_cols splits by single column", {
  result <- .split_df_by_cols(df, "group")

  expect_list_structure(result, expected_length = 2, expected_names = c(
    "A",
    "B"
  ))
  expect_identical(nrow(result$A), 2L)
  expect_identical(nrow(result$B), 2L)
  expect_identical(result$A$value, 1:2)
  expect_identical(result$B$value, 3:4)
})

test_that(".split_df_by_cols splits by multiple columns", {
  result <- .split_df_by_cols(df2, c("group1", "group2"))

  expected_names <- c("A___X", "A___Y", "B___X", "B___Y")
  expect_list_structure(result, expected_length = 4)
  expect_true(all(names(result) %in% expected_names))
  expect_identical(nrow(result[["A___X"]]), 1L)
  expect_identical(result[["A___X"]]$value, 1L)
  expect_identical(result[["B___Y"]]$value, 4L)
})

test_that(".split_df_by_cols NULL col_names returns single-element list", {
  result <- .split_df_by_cols(df, NULL)

  expect_list_structure(result, expected_length = 1)
  expect_identical(result[[1]], df)
})

test_that(".split_df_by_cols converts columns to factors", {
  result <- .split_df_by_cols(df, "group")

  expect_s3_class(result$A$group, "factor")
  expect_s3_class(result$B$group, "factor")
})

test_that(".split_df_by_cols handles numeric columns", {
  df3 <- data.frame(
    group = c(1, 1, 2, 2),
    value = 1:4,
    stringsAsFactors = FALSE
  )

  result <- .split_df_by_cols(df3, "group")

  expect_list_structure(result, expected_length = 2, expected_names = c(
    "1",
    "2"
  ))
})

test_that(".split_df_by_cols preserves all columns", {
  df4 <- df2
  df4$group3 <- 1:4

  result <- .split_df_by_cols(df4, c("group1", "group2"))

  expect_named(result[["A___X"]], names(df4))
})
