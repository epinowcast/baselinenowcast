test_that("allocate_reference_times works as expected when user specifies training volume precisely and correctly. ", { # nolint
  tv <- allocate_reference_times(
    reporting_triangle = matrix(
      data = 1,
      nrow = 12,
      ncol = 5
    ) |> construct_triangle(),
    max_delay = 4,
    scale_factor = 3,
    prop_delay = 0.5
  )
  expect_identical(tv$n_history_delay, 6)
  expect_identical(tv$n_retrospective_nowcasts, 6)

  # Example where not using all of them
  tv2 <- allocate_reference_times(
    reporting_triangle = matrix(
      data = 1,
      nrow = 14,
      ncol = 5
    ) |> construct_triangle(),
    max_delay = 4,
    scale_factor = 3,
    prop_delay = 2 / 3,
  )
  expect_identical(tv2$n_history_delay, 8)
  expect_identical(tv2$n_retrospective_nowcasts, 4)

  # Scale factor is higher than number of rows, but otherwise prop_delay still
  # works
  tv3 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 10,
        ncol = 5
      ) |> construct_triangle(),
      max_delay = 4,
      scale_factor = 3,
      prop_delay = 0.5
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified" #nolint
  )
  expect_identical(tv3$n_history_delay, 7)
  expect_identical(tv3$n_retrospective_nowcasts, 3)
})

test_that("allocate_reference_times warns when user or defaults don't meet minimum requirements, and reallocates accordingly.", { # nolint

  # Test the default works when their is less than 3* max delay of data
  tv5 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 10,
        ncol = 5
      ) |> construct_triangle(),
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified" #nolint
  )
  expect_identical(tv5$n_history_delay, 7)
  expect_identical(tv5$n_retrospective_nowcasts, 3)

  # Enough to run but not enough for prop delay or scale factor defaults
  tv6 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 8,
        ncol = 5
      ) |> construct_triangle(),
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified" #nolint
  )
  expect_identical(tv6$n_history_delay, 6)
  expect_identical(tv6$n_retrospective_nowcasts, 2)

  # Reallocate to ensure we have enough for n_retrospective_nowcasts
  tv7 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 7,
        ncol = 5
      ) |> construct_triangle(),
      prop_delay = 0.9
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  ) # nolint
  expect_identical(tv7$n_history_delay, 5)
  expect_identical(tv7$n_retrospective_nowcasts, 2)

  # Prop delay is too low
  tv8 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 7,
        ncol = 5
      ) |> construct_triangle(),
      prop_delay = 0.3
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  ) # nolint
  expect_identical(tv8$n_history_delay, 5)
  expect_identical(tv8$n_retrospective_nowcasts, 2)
})

test_that("allocate_reference_times errors when data is insufficient. ", { # nolint
  # Reporting triangle is sufficient but the scale factor makes it insufficient
  # Q: Do we want this to error or to just use all the reference times and warn?
  expect_error(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 10,
        ncol = 5
      ) |> construct_triangle(),
      max_delay = 4,
      scale_factor = 6 / 4,
      prop_delay = 0.4
    ),
    regexp = "Insufficient reference times specified by `scale_factor`"
  ) # nolint

  # Reporting triangle isn't big enough for both, this should error
  expect_error(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 6,
        ncol = 5
      ) |> construct_triangle(),
      max_delay = 4,
      scale_factor = 3,
      prop_delay = 0.5
    ),
    regexp = "Insufficient reference times in reporting triangle for the both delay" #nolint
  )
})

test_that("allocate_reference_times allocates properly with no user specifications (using defaults)", { # nolint
  tv <- allocate_reference_times(
    reporting_triangle = matrix(
      data = 1,
      nrow = 12,
      ncol = 5
    ) |> construct_triangle(),
  )
  expect_identical(tv$n_history_delay, 6)
  expect_identical(tv$n_retrospective_nowcasts, 6)

  # Odd numbered and more than enough ref times
  tv2 <- allocate_reference_times(
    reporting_triangle = matrix(
      data = 1,
      nrow = 13,
      ncol = 5
    ) |> construct_triangle()
  )
  expect_identical(tv2$n_history_delay, 6)
  expect_identical(tv2$n_retrospective_nowcasts, 6)

  # Enough reference times to hit prop_delay -- > split by prop delay
  tv3 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 10,
        ncol = 5
      ) |> construct_triangle()
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified"
  ) # nolint
  expect_identical(tv3$n_history_delay, 7)
  expect_identical(tv3$n_retrospective_nowcasts, 3)

  # Not enough reference times to hit prop delay -->  hit minimum for delay,
  # rest go to uncertainty
  tv3 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 8,
        ncol = 5
      ) |> construct_triangle()
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )
  expect_identical(tv3$n_history_delay, 6)
  expect_identical(tv3$n_retrospective_nowcasts, 2)

  # Same idea as above
  tv4 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 9,
        ncol = 6
      ) |> construct_triangle()
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )
  expect_identical(tv4$n_history_delay, 7)
  expect_identical(tv4$n_retrospective_nowcasts, 2)

  # Handle larger numbers
  tv5 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 60,
        ncol = 40
      ) |> construct_triangle()
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )

  expect_identical(tv5$n_history_delay, 50)
  expect_identical(tv5$n_retrospective_nowcasts, 10)

  # Handle larger less clean numbers
  tv6 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = matrix(
        data = 1,
        nrow = 55,
        ncol = 39
      ) |> construct_triangle()
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )
  expect_identical(tv6$n_history_delay, 47)
  expect_identical(tv6$n_retrospective_nowcasts, 8)

  # Handle even larger numbers even splot
  tv7 <- allocate_reference_times(
    reporting_triangle = matrix(
      data = 1,
      nrow = 120,
      ncol = 41
    ) |> construct_triangle()
  )
  expect_identical(tv7$n_history_delay, 60)
  expect_identical(tv7$n_retrospective_nowcasts, 60)

  # Same but inc training volume
  tv8 <- allocate_reference_times(
    reporting_triangle = matrix(
      data = 1,
      nrow = 140,
      ncol = 41
    ) |> construct_triangle()
  )
  expect_identical(tv8$n_history_delay, 60)
  expect_identical(tv8$n_retrospective_nowcasts, 60)
})
