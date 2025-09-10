test_that(".allocate_training_volume works as expected when user specifies training volume precisely and correctly. ", { # nolint
  tv <- .allocate_training_volume(
    n_ref_times = 12,
    max_delay = 4,
    n_history_delay = 5,
    n_retrospective_nowcasts = 7
  )
  expect_identical(5, tv$n_history_delay)
  expect_identical(7, tv$n_retrospective_nowcasts)

  tv2 <- .allocate_training_volume(
    n_ref_times = 12,
    max_delay = 4,
    n_history_delay = 8,
    n_retrospective_nowcasts = 4
  )
  expect_identical(8, tv2$n_history_delay)
  expect_identical(4, tv2$n_retrospective_nowcasts)

  # Example where not using all of them
  tv3 <- .allocate_training_volume(
    n_ref_times = 14,
    max_delay = 3,
    n_history_delay = 8,
    n_retrospective_nowcasts = 4
  )
  expect_identical(8, tv3$n_history_delay)
  expect_identical(4, tv3$n_retrospective_nowcasts)

  expect_error(
    .allocate_training_volume(
      n_ref_times = 15,
      max_delay = 4,
      n_history_delay = 10,
      n_retrospective_nowcasts = 6
    ),
    regexp = "Insufficient reference times in reporting triangle for specified training volume." # nolint
  )
})

test_that(".allocate_training_volume allocates properly with no user specifications", {
  tv <- .allocate_training_volume(
    n_ref_times = 12,
    max_delay = 4
  )
  expect_identical(6, tv$n_history_delay)
  expect_identical(6, tv$n_retrospective_nowcasts)

  # Odd numbered and more than enough ref times
  tv2 <- .allocate_training_volume(
    n_ref_times = 13,
    max_delay = 4
  )
  expect_identical(6, tv2$n_history_delay)
  expect_identical(6, tv2$n_retrospective_nowcasts)

  # Less than enough ref times --> split extra between delay and uncertainty
  tv3 <- .allocate_training_volume(
    n_ref_times = 10,
    max_delay = 4
  )
  expect_identical(7, tv3$n_history_delay)
  expect_identical(3, tv3$n_retrospective_nowcasts)

  # Allocate to uncertainty first once have met requirement for delay estimation
  tv4 <- .allocate_training_volume(
    n_ref_times = 9,
    max_delay = 6
  )
  expect_identical(7, tv4$n_history_delay)
  expect_identical(2, tv4$n_retrospective_nowcasts)

  # Handle larger numbers
  tv5 <- .allocate_training_volume(
    n_ref_times = 60,
    max_delay = 40
  )
  expect_identical(50, tv5$n_history_delay)
  expect_identical(10, tv5$n_retrospective_nowcasts)

  # Handle larger less clean numbers
  tv6 <- .allocate_training_volume(
    n_ref_times = 55,
    max_delay = 39
  )
  expect_identical(47, tv6$n_history_delay)
  expect_identical(8, tv6$n_retrospective_nowcasts)

  # Handle even larger numbers even splot
  tv7 <- .allocate_training_volume(
    n_ref_times = 120,
    max_delay = 40
  )
  expect_identical(60, tv7$n_history_delay)
  expect_identical(60, tv7$n_retrospective_nowcasts)

  # Same but inc training volume
  tv8 <- .allocate_training_volume(
    n_ref_times = 140,
    max_delay = 40
  )
  expect_identical(60, tv8$n_history_delay)
  expect_identical(60, tv8$n_retrospective_nowcasts)
})

test_that(".allocate_training_volume errors approriately when insufficient reference times and no training volume is specified", { # nolint
  expect_error(
    .allocate_training_volume(
      n_ref_times = 15,
      max_delay = 13
    ),
    regexp = "Insufficient reference times in reporting triangle for delay and uncertainty estimation."
  ) # nolint

  expect_error(
    .allocate_training_volume(
      n_ref_times = 5,
      max_delay = 3
    ),
    regexp = "Insufficient reference times in reporting triangle for delay and uncertainty estimation."
  ) # nolint
})

test_that(".allocate_training_volume handles specified `n_history_delay` and unspecified `n_retrospective_nowcasts` appropriately", {
  tv <- .allocate_training_volume(
    n_ref_times = 12,
    max_delay = 4,
    n_history_delay = 8
  )
  expect_identical(8, tv$n_history_delay)
  expect_identical(4, tv$n_retrospective_nowcasts)

  tv2 <- .allocate_training_volume(
    n_ref_times = 16,
    max_delay = 4,
    n_history_delay = 6
  )
  expect_identical(6, tv2$n_history_delay)
  expect_identical(6, tv2$n_retrospective_nowcasts)

  # By default uses 3* max delay for training volume if its available
  tv3 <- .allocate_training_volume(
    n_ref_times = 16,
    max_delay = 4,
    n_history_delay = 5
  )
  expect_identical(5, tv3$n_history_delay)
  expect_identical(7, tv3$n_retrospective_nowcasts)

  # If 3*max_delay not available, by default uses the remainder for n_retrospective nowcasts #nolint
  tv4 <- .allocate_training_volume(
    n_ref_times = 11,
    max_delay = 4,
    n_history_delay = 5
  )
  expect_identical(5, tv4$n_history_delay)
  expect_identical(6, tv4$n_retrospective_nowcasts)


  # Error if n_history delay is too small
  expect_error(
    .allocate_training_volume(
      n_ref_times = 12,
      max_delay = 4,
      n_history_delay = 4
    ),
    regexp = "User-specified `n_history_delay` is insufficient for delay estimation."
  ) # nolint

  # Error if n_ref_times is too low
  expect_error(
    .allocate_training_volume(
      n_ref_times = 11,
      max_delay = 4,
      n_history_delay = 10
    ),
    regexp = "Insufficient reference times for uncertainty estimation." # nolint
  )
})

test_that(".allocate_training_volume handles specified `n_retrospective_nowcasts` and unspecified `n_history_delay` appropriately", { # nolint
  tv <- .allocate_training_volume(
    n_ref_times = 12,
    max_delay = 4,
    n_retrospective_nowcasts = 6
  )
  expect_identical(6, tv$n_history_delay)
  expect_identical(6, tv$n_retrospective_nowcasts)

  # Default is us 3* max delay for training volume so distribute rest to n_history_delay
  tv1 <- .allocate_training_volume(
    n_ref_times = 14,
    max_delay = 4,
    n_retrospective_nowcasts = 4
  )
  expect_identical(8, tv1$n_history_delay)
  expect_identical(4, tv1$n_retrospective_nowcasts)

  # Same as above but with odd numbers
  tv2 <- .allocate_training_volume(
    n_ref_times = 14,
    max_delay = 3,
    n_retrospective_nowcasts = 4
  )
  expect_identical(5, tv2$n_history_delay)
  expect_identical(4, tv2$n_retrospective_nowcasts)

  # Make sure it hits max delay
  tv3 <- .allocate_training_volume(
    n_ref_times = 14,
    max_delay = 3,
    n_retrospective_nowcasts = 8
  )
  expect_identical(4, tv3$n_history_delay)
  expect_identical(8, tv3$n_retrospective_nowcasts)

  # Error if n_retrospective_nowcasts is too small
  expect_error(
    .allocate_training_volume(
      n_ref_times = 12,
      max_delay = 4,
      n_retrospective_nowcasts = 1
    ),
    regexp = "Insufficient reference times for uncertainty estimation."
  ) # nolint

  # Error if not enough rows overall
  expect_error(
    .allocate_training_volume(
      n_ref_times = 6,
      max_delay = 4,
      n_retrospective_nowcasts = 2
    ),
    regexp = "5 reference times are required for delay estimation, and this plus 2 retrospective nowcasts is more than the available 6 reference times."
  ) # nolint

  # Error if n_retrospective nowccasts is too high
  expect_error(
    .allocate_training_volume(
      n_ref_times = 12,
      max_delay = 6,
      n_retrospective_nowcasts = 8
    ),
    regexp = "7 reference times are required for delay estimation, and this plus 8 retrospective nowcasts is more than the available 12 reference times."
  ) # nolint
})
