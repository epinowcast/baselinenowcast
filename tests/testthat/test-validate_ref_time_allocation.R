testthat(".validate_ref_time_allocations doesn't error when inputs are sufficient", { # nolint
  # Defaults
  expect_no_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 4,
      n_history_delay = 6,
      n_retrospective_nowcasts = 6
    )
  )

  # Defaults with more data
  expect_no_error(
    .validate_ref_time_allocation(
      n_ref_times = 20,
      size_min_ref_times_delay = 4,
      n_history_delay = 6,
      n_retrospective_nowcasts = 6
    )
  )

  # more delays
  expect_no_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 4,
      n_history_delay = 10,
      n_retrospective_nowcasts = 2
    )
  )

  # more uncertainty
  expect_no_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 4,
      n_history_delay = 4,
      n_retrospective_nowcasts = 8
    )
  )
})

testthat(".validate_ref_time_allocations doesn't error when ref times mismatch inputs", { # nolint

  expect_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 4,
      n_history_delay = 8,
      n_retrospective_nowcasts = 6
    ),
    regexp = "Insufficient reference times in reporting triangle for specified" # nolint
  )

  expect_error(
    .validate_ref_time_allocation(
      n_ref_times = -12,
      size_min_ref_times_delay = 4,
      n_history_delay = 8,
      n_retrospective_nowcasts = 6
    ),
    regexp = "Assertion on 'n_ref_times' failed: Element 1 is not >= 0"
  )

  expect_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = -4,
      n_history_delay = 8,
      n_retrospective_nowcasts = 6
    ),
    regexp = "Assertion on 'size_min_ref_times_delay' failed: Element 1 is not >= 0" # nolint
  )

  expect_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 4,
      n_history_delay = -8,
      n_retrospective_nowcasts = 6
    ),
    regexp = "Assertion on 'n_history_delay' failed: Element 1 is not >= 0"
  )

  expect_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 4,
      n_history_delay = 8,
      n_retrospective_nowcasts = -6
    ),
    regexp = "Assertion on 'n_retrospective_nowcasts' failed: Element 1 is not >= 0" # nolint
  )
})

test_that(".validate_ref_time_allocation errors when n_history_delay is too small", { # nolint
  expect_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 6,
      n_history_delay = 5,
      n_retrospective_nowcasts = 6
    ),
    regexp = "Insufficient `n_history_delay`."
  )
})

test_that(".validate_ref_time_allocation errors when n_retrospective_nowcasts is too small", { # nolint
  expect_error(
    .validate_ref_time_allocation(
      n_ref_times = 12,
      size_min_ref_times_delay = 6,
      n_history_delay = 6,
      n_retrospective_nowcasts = 1
    ),
    regexp = "Insufficient `n_retrospective_nowcasts`."
  )
})
