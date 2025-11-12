test_that("safelydoesit returns result for successful function and NULL for errors", {
  # Test with a function that works correctly
  add <- function(x, y) {
    return(x + y)
  }
  safe_add <- .safelydoesit(add)

  # Test successful execution
  result1 <- safe_add(2, 3)
  expect_identical(result1$result, 5)
  expect_null(result1$error)

  # Test with a function that throws an error
  divide <- function(x, y) {
    if (y == 0) stop("Division by zero", call. = FALSE)
    return(x / y)
  }
  safe_divide <- .safelydoesit(divide)

  # Test handling of error
  result2 <- safe_divide(10, 0)
  expect_null(result2$result)
  expect_s3_class(result2$error, "error")
  expect_match(result2$error$message, "Division by zero")

  # Test successful case of error-throwing function
  result3 <- safe_divide(10, 2)
  expect_identical(result3$result, 5)
  expect_null(result3$error)

  # Test with wrong input - not a function
  expect_error(.safelydoesit("not a function"))

  # Test with a function that has side effects
  counter <- 0
  increment <- function() {
    counter <<- counter + 1 # nolint
    return(counter)
  }
  safe_increment <- .safelydoesit(increment)

  # Verify side effects still happen
  result4 <- safe_increment()
  expect_identical(result4$result, 1)
  result5 <- safe_increment()
  expect_identical(result5$result, 2)

  # Test with a function that returns NULL normally
  return_null <- function() NULL
  safe_null <- .safelydoesit(return_null)
  result6 <- safe_null()
  expect_null(result6$result)
  expect_null(result6$error)

  # Test with a function that takes multiple arguments
  complex_fun <- function(x, y, z = 1, ...) {
    dots <- list(...)
    if (length(dots) > 0) {
      return(x + y + z + sum(unlist(dots)))
    }
    return(x + y + z)
  }
  safe_complex <- .safelydoesit(complex_fun)

  # Test with different argument patterns
  expect_identical(safe_complex(1, 2)$result, 4)
  expect_identical(safe_complex(1, 2, 3)$result, 6)
  expect_identical(safe_complex(1, 2, 3, 4, 5)$result, 15)
})

test_that("safelydoesit handles nested errors correctly", {
  # Define a function that calls another function that might error
  outer_fun <- function(x) {
    inner_fun <- function(y) {
      if (y < 0) stop("Negative value not allowed", call. = FALSE)
      return(y * 2)
    }
    safe_inner <- .safelydoesit(inner_fun)
    result <- safe_inner(x)

    # Handle the inner error
    if (!is.null(result$error)) {
      return(0) # default value on error
    }
    return(result$result)
  }

  # Test with valid and invalid inputs
  expect_identical(outer_fun(5), 10)
  expect_identical(outer_fun(-5), 0)
})
