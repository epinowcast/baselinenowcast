test_that(".allocate_training_volume works as expected when user specifies training volume precisely and correctly. ",{ #nolint
  tv <- .allocate_training_volume(n_ref_times = 12,
                            max_delay = 4,
                            n_history_delay = 5,
                            n_retrospective_nowcasts = 7)
  expect_identical(5, tv$n_history_delay)
  expect_identical(7, tv$n_retrospective_nowcasts)

  tv2 <- .allocate_training_volume(n_ref_times = 12,
                                  max_delay = 4,
                                  n_history_delay = 8,
                                  n_retrospective_nowcasts = 4)
  expect_identical(8, tv2$n_history_delay)
  expect_identical(4, tv2$n_retrospective_nowcasts)

  # Example where not using all of them
  tv3 <- .allocate_training_volume(n_ref_times = 14,
                                   max_delay = 3,
                                   n_history_delay = 8,
                                   n_retrospective_nowcasts = 4)
  expect_identical(8, tv3$n_history_delay)
  expect_identical(4, tv3$n_retrospective_nowcasts)

}
)

test_that(".allocate_training_volume allocates properly with no user specifications", {
  tv <- .allocate_training_volume(n_ref_times = 12,
                                 max_delay = 4)
  expect_identical(6, tv$n_history_delay)
  expect_identical(6, tv$n_retrospective_nowcasts)

  # Odd numbered and more than enough ref times
  tv2 <- .allocate_training_volume(n_ref_times = 13,
                                  max_delay = 4)
  expect_identical(6, tv2$n_history_delay)
  expect_identical(6, tv2$n_retrospective_nowcasts)

  # Less than enough ref times --> split extra between delay and uncertainty
  tv3 <- .allocate_training_volume(n_ref_times = 10,
                                   max_delay = 4)
  expect_identical(7, tv3$n_history_delay)
  expect_identical(3, tv3$n_retrospective_nowcasts)

  # Allocate to uncertainty first once have met requirement for delay estimation
  tv4 <- .allocate_training_volume(n_ref_times = 9,
                                   max_delay = 6)
  expect_identical(7, tv4$n_history_delay)
  expect_identical(2, tv4$n_retrospective_nowcasts)

  # Handle larger numbers
  tv5 <- .allocate_training_volume(n_ref_times = 60,
                                   max_delay = 40)
  expect_identical(50, tv5$n_history_delay)
  expect_identical(10, tv5$n_retrospective_nowcasts)

  # Handle larger less clean numbers
  tv6 <- .allocate_training_volume(n_ref_times = 55,
                                   max_delay = 39)
  expect_identical(47, tv6$n_history_delay)
  expect_identical(8, tv6$n_retrospective_nowcasts)

  # Handle even larger numbers even splot
  tv7 <- .allocate_training_volume(n_ref_times = 120,
                                   max_delay = 40)
  expect_identical(60, tv7$n_history_delay)
  expect_identical(60, tv7$n_retrospective_nowcasts)

  # Same but inc training volume
  tv8 <- .allocate_training_volume(n_ref_times = 140,
                                   max_delay = 40)
  expect_identical(60, tv8$n_history_delay)
  expect_identical(60, tv8$n_retrospective_nowcasts)
}
)

test_that("Errors approriately when insufficient reference times and no training volume is specified", { #nolint
  expect_error(.allocate_training_volume(n_ref_times = 15,
                                  max_delay = 13),
               regexp = "Insufficient reference times in reporting triangle for delay and uncertainty estimation.") #nolint

})
