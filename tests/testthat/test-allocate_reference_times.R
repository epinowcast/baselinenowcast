test_that("allocate_reference_times works as expected when user specifies training volume precisely and correctly. ", { # nolint
  rep_tri <- matrix(
    data = 1,
    nrow = 12,
    ncol = 5
  ) |> construct_triangle()
  tv <- allocate_reference_times(
    reporting_triangle = rep_tri,
    max_delay = 4,
    scale_factor = 3,
    prop_delay = 0.5
  )
  expect_identical(tv$n_history_delay, 6)
  expect_identical(tv$n_retrospective_nowcasts, 6)

  # Example where not using all of them
  rep_tri2 <- matrix(
    data = 1,
    nrow = 14,
    ncol = 5
  ) |> construct_triangle()
  tv2 <- allocate_reference_times(
    reporting_triangle = rep_tri2,
    max_delay = 4,
    scale_factor = 3,
    prop_delay = 2 / 3
  )
  expect_identical(tv2$n_history_delay, 8)
  expect_identical(tv2$n_retrospective_nowcasts, 4)

  # Scale factor is higher than number of rows, but otherwise prop_delay still
  # works
  rep_tri3 <- matrix(
    data = 1,
    nrow = 10,
    ncol = 5
  ) |> construct_triangle()
  tv3 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri3,
      max_delay = 4,
      scale_factor = 3,
      prop_delay = 0.5
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified" # nolint
  )
  expect_identical(tv3$n_history_delay, 7)
  expect_identical(tv3$n_retrospective_nowcasts, 3)
})

test_that("allocate_reference_times properly scales delay and total training amount", { # nolint
  rep_tri <- matrix(
    data = 1,
    nrow = 20,
    ncol = 5
  ) |> construct_triangle()

  tv <- allocate_reference_times(
    rep_tri,
    scale_factor = 2,
    prop_delay = 0.5
  )
  expect_equal(tv$n_history_delay, 5)
  expect_equal(tv$n_retrospective_nowcasts, 3)

  tv2 <- allocate_reference_times(
    rep_tri,
    scale_factor = 4,
    prop_delay = 0.5
  )
  expect_equal(tv2$n_history_delay, 8)
  expect_equal(tv2$n_retrospective_nowcasts, 8)

  tv3 <- allocate_reference_times(
    rep_tri,
    scale_factor = 5,
    prop_delay = 0.25
  )
  expect_equal(tv3$n_history_delay, 5)
  expect_equal(tv3$n_retrospective_nowcasts, 15)

  tv4 <- allocate_reference_times(
    rep_tri,
    scale_factor = 3,
    prop_delay = 0.25
  )
  # As close as you can to prop delay
  expect_equal(tv3$n_history_delay, 5)
  expect_equal(tv3$n_retrospective_nowcasts, 7)

  tv5 <- allocate_reference_times(
    rep_tri,
    scale_factor = 3,
    prop_delay = 0.75
  )
  # Can hit exaxtly prop delay
  expect_equal(tv5$n_history_delay, 9)
  expect_equal(tv5$n_retrospective_nowcasts, 3)
})

test_that("allocate_reference_times warns when user or defaults don't meet minimum requirements, and reallocates accordingly.", { # nolint

  # Test the default works when their is less than 3* max delay of data
  rep_tri5 <- matrix(
    data = 1,
    nrow = 10,
    ncol = 5
  ) |> construct_triangle()
  tv5 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri5
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified" # nolint
  )
  expect_identical(tv5$n_history_delay, 7)
  expect_identical(tv5$n_retrospective_nowcasts, 3)

  # Enough to run but not enough for prop delay or scale factor defaults
  rep_tri6 <- matrix(
    data = 1,
    nrow = 8,
    ncol = 5
  ) |> construct_triangle()
  tv6 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri6
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified" # nolint
  )
  expect_identical(tv6$n_history_delay, 6)
  expect_identical(tv6$n_retrospective_nowcasts, 2)

  # Reallocate to ensure we have enough for n_retrospective_nowcasts
  rep_tri7 <- matrix(
    data = 1,
    nrow = 7,
    ncol = 5
  ) |> construct_triangle()
  tv7 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri7,
      prop_delay = 0.9
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  ) # nolint
  expect_identical(tv7$n_history_delay, 5)
  expect_identical(tv7$n_retrospective_nowcasts, 2)

  # Prop delay is too low
  rep_tri8 <- matrix(
    data = 1,
    nrow = 7,
    ncol = 5
  ) |> construct_triangle()
  tv8 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri8,
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
  rep_tri1 <- matrix(
    data = 1,
    nrow = 10,
    ncol = 5
  ) |> construct_triangle()
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri1,
      max_delay = 4,
      scale_factor = 6 / 4,
      prop_delay = 0.4
    ),
    regexp = "Insufficient reference times specified by `scale_factor`"
  ) # nolint

  # Reporting triangle isn't big enough for both, this should error
  rep_tri2 <- matrix(
    data = 1,
    nrow = 6,
    ncol = 5
  ) |> construct_triangle()
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri2,
      max_delay = 4,
      scale_factor = 3,
      prop_delay = 0.5
    ),
    regexp = "Insufficient reference times in reporting triangle for the both delay" # nolint
  )
})

test_that("allocate_reference_times allocates properly with no user specifications (using defaults)", { # nolint
  rep_tri3 <- matrix(
    data = 1,
    nrow = 12,
    ncol = 5
  ) |> construct_triangle()
  tv <- allocate_reference_times(
    reporting_triangle = rep_tri3
  )
  expect_identical(tv$n_history_delay, 6)
  expect_identical(tv$n_retrospective_nowcasts, 6)

  # Odd numbered and more than enough ref times
  rep_tri2 <- matrix(
    data = 1,
    nrow = 13,
    ncol = 5
  ) |> construct_triangle()
  tv2 <- allocate_reference_times(
    reporting_triangle = rep_tri2
  )
  expect_identical(tv2$n_history_delay, 6)
  expect_identical(tv2$n_retrospective_nowcasts, 6)

  # Enough reference times to hit prop_delay -- > split by prop delay
  rep_tri3 <- matrix(
    data = 1,
    nrow = 10,
    ncol = 5
  ) |> construct_triangle()
  tv3 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri3
    ),
    regexp = "Insufficient reference times in reporting triangle for the specified" # nolint
  )
  expect_identical(tv3$n_history_delay, 7)
  expect_identical(tv3$n_retrospective_nowcasts, 3)

  # Not enough reference times to hit prop delay -->  hit minimum for delay,
  # rest go to uncertainty
  rep_tri4 <- matrix(
    data = 1,
    nrow = 8,
    ncol = 5
  ) |> construct_triangle()
  tv4 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri4
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )
  expect_identical(tv4$n_history_delay, 6)
  expect_identical(tv4$n_retrospective_nowcasts, 2)

  # Same idea as above
  rep_tri5 <- matrix(
    data = 1,
    nrow = 9,
    ncol = 6
  ) |> construct_triangle()
  tv5 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri5
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )
  expect_identical(tv5$n_history_delay, 7)
  expect_identical(tv5$n_retrospective_nowcasts, 2)

  # Handle larger numbers
  rep_tri6 <- matrix(
    data = 1,
    nrow = 60,
    ncol = 40
  ) |> construct_triangle()
  tv6 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri6
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )

  expect_identical(tv6$n_history_delay, 50)
  expect_identical(tv6$n_retrospective_nowcasts, 10)

  # Handle larger less clean numbers
  rep_tri7 <- matrix(
    data = 1,
    nrow = 55,
    ncol = 39
  ) |> construct_triangle()
  tv7 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri7
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )
  expect_identical(tv7$n_history_delay, 47)
  expect_identical(tv7$n_retrospective_nowcasts, 8)

  # Handle even larger numbers even splot
  rep_tri8 <- matrix(
    data = 1,
    nrow = 120,
    ncol = 41
  ) |> construct_triangle()
  tv8 <- allocate_reference_times(
    reporting_triangle = rep_tri8
  )
  expect_identical(tv8$n_history_delay, 60)
  expect_identical(tv8$n_retrospective_nowcasts, 60)

  # Same but inc training volume
  rep_tri9 <- matrix(
    data = 1,
    nrow = 140,
    ncol = 41
  ) |> construct_triangle()
  tv9 <- allocate_reference_times(
    reporting_triangle = rep_tri9
  )
  expect_identical(tv9$n_history_delay, 60)
  expect_identical(tv9$n_retrospective_nowcasts, 60)
})

test_that("allocate_reference_times warns and reallocates appropriately when sufficient ref times but not enough retro nowcasts", { # nolint
  # Handle larger numbers
  rep_tri <- matrix(
    data = 1,
    nrow = 14,
    ncol = 4
  ) |> construct_triangle()
  tv <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      prop_delay = 0.95
    ),
    regexp = "`prop_delay` specified is not equivalent to `prop_delay` used."
  )
})

test_that("allocate_reference_times errors when inputs are invalid", {
  rep_tri <- matrix(
    data = 1,
    nrow = 14,
    ncol = 4
  ) |> construct_triangle()

  expect_error(
    allocate_reference_times(
      reporting_triangle = 6
    )
  )

  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      size_min_retro_nowcasts = 0.1
    )
  )
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      scale_factor = c(4, 5)
    )
  )
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      scale_factor = -3
    )
  )
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      prop_delay = c(4, 5)
    )
  )
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      prop_delay = 2.3
    )
  )
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      prop_delay = -0.4
    )
  )
})
