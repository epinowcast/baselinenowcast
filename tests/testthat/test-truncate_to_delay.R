rt <- make_test_triangle(nrow = 5, ncol = 4, with_nas = TRUE)

test_that("truncate_to_delay rejects invalid inputs", {
  cases <- list(
    list(
      x = matrix(1:12, nrow = 3),
      max_delay = 2,
      regex = "class 'reporting_triangle'"
    ),
    list(
      x = rt,
      max_delay = "two",
      regex = "single numeric value"
    ),
    list(
      x = rt,
      max_delay = c(1, 2),
      regex = "single numeric value"
    ),
    list(
      x = rt,
      max_delay = -1,
      regex = "non-negative"
    ),
    list(
      x = rt,
      max_delay = get_max_delay(rt) + 1,
      regex = "greater than current maximum delay"
    )
  )
  for (case in cases) {
    expect_error(
      truncate_to_delay(case$x, case$max_delay),
      regexp = case$regex
    )
  }
})

test_that("truncate_to_delay is a no-op when max_delay is current", {
  result <- truncate_to_delay(rt, max_delay = get_max_delay(rt))
  expect_identical(result, rt)
})

test_that("truncate_to_delay reduces delay columns and preserves class", {
  result <- suppressMessages(truncate_to_delay(rt, max_delay = 1))
  expect_true(is_reporting_triangle(result))
  expect_identical(get_max_delay(result), 1L)
  expect_identical(ncol(result), 2L)
  expect_identical(nrow(result), nrow(rt))
})
