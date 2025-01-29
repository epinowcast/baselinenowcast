library(testthat)
library(withr)
library(baselinenowcast)

test_results <- test_check("baselinenowcast")

if (any(as.data.frame(test_results)$warning > 0)) {
  stop("tests failed with warnings")
}
