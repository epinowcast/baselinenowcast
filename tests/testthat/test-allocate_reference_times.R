test_that("allocate_reference_times works as expected when user specifies training volume precisely and correctly. ", { # nolint
  rep_tri <- make_test_triangle(nrow = 12, ncol = 5) |>
    construct_triangle()
  tv <- allocate_reference_times(
    reporting_triangle = rep_tri,
    max_delay = 4,
    scale_factor = 3,
    prop_delay = 0.5
  )
  expect_identical(tv$n_history_delay, 6)
  expect_identical(tv$n_retrospective_nowcasts, 6)

  # Example where not using all of them
  rep_tri2 <- make_test_triangle(nrow = 14, ncol = 5) |>
    construct_triangle()
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
  rep_tri3 <- make_test_triangle(nrow = 10, ncol = 5) |>
    construct_triangle()
  tv3 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri3,
      max_delay = 4,
      scale_factor = 3,
      prop_delay = 0.5
    ),
    regexp = "10 reference times available and 12 are specified." # nolint
  )
  expect_identical(tv3$n_history_delay, 7)
  expect_identical(tv3$n_retrospective_nowcasts, 3)
})

test_that("allocate_reference_times properly scales delay and total training amount", { # nolint
  rep_tri <- make_test_triangle(nrow = 20, ncol = 5) |>
    construct_triangle()

  tv <- expect_message(allocate_reference_times(
    rep_tri,
    scale_factor = 2,
    prop_delay = 0.5
  ))
  expect_identical(tv$n_history_delay, 5)
  expect_identical(tv$n_retrospective_nowcasts, 3)

  tv2 <- allocate_reference_times(
    rep_tri,
    scale_factor = 4,
    prop_delay = 0.5
  )
  expect_identical(tv2$n_history_delay, 8)
  expect_identical(tv2$n_retrospective_nowcasts, 8)

  tv3 <- allocate_reference_times(
    rep_tri,
    scale_factor = 5,
    prop_delay = 0.25
  )
  expect_identical(tv3$n_history_delay, 5)
  expect_identical(tv3$n_retrospective_nowcasts, 15)

  tv4 <- expect_message(allocate_reference_times(
    rep_tri,
    scale_factor = 3,
    prop_delay = 0.25
  ))
  # As close as you can to prop delay
  expect_identical(tv4$n_history_delay, 5)
  expect_identical(tv4$n_retrospective_nowcasts, 7)

  tv5 <- allocate_reference_times(
    rep_tri,
    scale_factor = 3,
    prop_delay = 0.75
  )
  # Can hit exactly prop delay
  expect_identical(tv5$n_history_delay, 9)
  expect_identical(tv5$n_retrospective_nowcasts, 3)
})

test_that("allocate_reference_times handles rounding with a warning", {
  rep_tri <- make_test_triangle(nrow = 20, ncol = 6) |>
    construct_triangle()

  tv <- expect_message(allocate_reference_times(
    rep_tri,
    scale_factor = 3,
    prop_delay = 0.5
  ))
  expect_identical(tv$n_history_delay, 7)
  expect_identical(tv$n_retrospective_nowcasts, 8)

  rep_tri2 <- make_test_triangle(nrow = 100, ncol = 31) |>
    construct_triangle()

  # Don't warn when prop delay is basically equivalent
  tv2 <- expect_no_warning(allocate_reference_times(
    rep_tri2,
    scale_factor = 3,
    prop_delay = 0.667
  ))
  expect_identical(tv2$n_history_delay, 60)
  expect_identical(tv2$n_retrospective_nowcasts, 30)
})

test_that("allocate_reference_times warns when user or defaults don't meet minimum requirements, and reallocates accordingly.", { # nolint

  # Test the default works when their is less than 3* max delay of data
  rep_tri5 <- make_test_triangle(nrow = 10, ncol = 5) |>
    construct_triangle()
  tv5 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri5
    ),
    regexp = "10 reference times available and 12 are specified." # nolint
  )
  expect_identical(tv5$n_history_delay, 7)
  expect_identical(tv5$n_retrospective_nowcasts, 3)

  # Enough to run but not enough for prop delay or scale factor defaults
  rep_tri6 <- make_test_triangle(nrow = 8, ncol = 5) |>
    construct_triangle()
  tv6 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri6
    ),
    regexp = "8 reference times available and 12 are specified." # nolint
  )
  expect_identical(tv6$n_history_delay, 6)
  expect_identical(tv6$n_retrospective_nowcasts, 2)

  # Reallocate to ensure we have enough for n_retrospective_nowcasts
  rep_tri7 <- make_test_triangle(nrow = 7, ncol = 5) |>
    construct_triangle()
  tv7 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri7,
      prop_delay = 0.9
    ),
    regexp = "7 reference times available and 12 are specified."
  ) # nolint
  expect_identical(tv7$n_history_delay, 5)
  expect_identical(tv7$n_retrospective_nowcasts, 2)

  # Prop delay is too low
  rep_tri8 <- make_test_triangle(nrow = 7, ncol = 5) |>
    construct_triangle()
  tv8 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri8,
      prop_delay = 0.3
    ),
    regexp = "7 reference times available and 12 are specified." # nolint
  ) # nolint
  expect_identical(tv8$n_history_delay, 5)
  expect_identical(tv8$n_retrospective_nowcasts, 2)
})

test_that("allocate_reference_times errors when data is insufficient. ", { # nolint
  # Reporting triangle is sufficient but the scale factor makes it insufficient
  # Q: Do we want this to error or to just use all the reference times and warn?
  rep_tri1 <- make_test_triangle(nrow = 10, ncol = 5) |>
    construct_triangle()
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri1,
      max_delay = 4,
      scale_factor = 6 / 4,
      prop_delay = 0.4
    ),
    regexp = "6 reference times specified and 7 are needed"
  ) # nolint

  # Reporting triangle isn't big enough for both, this should error
  rep_tri2 <- make_test_triangle(nrow = 6, ncol = 5) |>
    construct_triangle()
  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri2,
      max_delay = 4,
      scale_factor = 3,
      prop_delay = 0.5
    ),
    regexp = "" # nolint
  )
})

test_that("allocate_reference_times allocates properly with no user specifications (using defaults)", { # nolint
  rep_tri3 <- make_test_triangle(nrow = 12, ncol = 5) |>
    construct_triangle()
  tv <- allocate_reference_times(
    reporting_triangle = rep_tri3
  )
  expect_identical(tv$n_history_delay, 6)
  expect_identical(tv$n_retrospective_nowcasts, 6)

  # Odd numbered and more than enough ref times
  rep_tri2 <- make_test_triangle(nrow = 13, ncol = 5) |>
    construct_triangle()
  tv2 <- allocate_reference_times(
    reporting_triangle = rep_tri2
  )
  expect_identical(tv2$n_history_delay, 6)
  expect_identical(tv2$n_retrospective_nowcasts, 6)

  # Enough reference times to hit prop_delay -- > split by prop delay
  rep_tri3 <- make_test_triangle(nrow = 10, ncol = 5) |>
    construct_triangle()
  tv3 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri3
    ),
    regexp = "10 reference times available and 12 are specified." # nolint
  )
  expect_identical(tv3$n_history_delay, 7)
  expect_identical(tv3$n_retrospective_nowcasts, 3)

  # Not enough reference times to hit prop delay -->  hit minimum for delay,
  # rest go to uncertainty
  rep_tri4 <- make_test_triangle(nrow = 8, ncol = 5) |>
    construct_triangle()
  tv4 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri4
    ),
    regexp = "8 reference times available and 12 are specified." # nolint
  )
  expect_identical(tv4$n_history_delay, 6)
  expect_identical(tv4$n_retrospective_nowcasts, 2)

  # Same idea as above
  rep_tri5 <- make_test_triangle(nrow = 9, ncol = 6) |>
    construct_triangle()
  tv5 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri5
    ),
    regexp = "9 reference times available and 15 are specified." # nolint
  )
  expect_identical(tv5$n_history_delay, 7)
  expect_identical(tv5$n_retrospective_nowcasts, 2)

  # Handle larger numbers
  rep_tri6 <- make_test_triangle(nrow = 60, ncol = 40) |>
    construct_triangle()
  tv6 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri6
    ),
    regexp = "60 reference times available and 117 are specified." # nolint
  )

  expect_identical(tv6$n_history_delay, 50)
  expect_identical(tv6$n_retrospective_nowcasts, 10)

  # Handle larger less clean numbers
  rep_tri7 <- make_test_triangle(nrow = 55, ncol = 39) |>
    construct_triangle()
  tv7 <- expect_warning(
    allocate_reference_times(
      reporting_triangle = rep_tri7
    ),
    regexp = "55 reference times available and 114 are specified." # nolint
  )
  expect_identical(tv7$n_history_delay, 47)
  expect_identical(tv7$n_retrospective_nowcasts, 8)

  # Handle even larger numbers even splot
  rep_tri8 <- make_test_triangle(nrow = 120, ncol = 41) |>
    construct_triangle()
  tv8 <- allocate_reference_times(
    reporting_triangle = rep_tri8
  )
  expect_identical(tv8$n_history_delay, 60)
  expect_identical(tv8$n_retrospective_nowcasts, 60)

  # Same but inc training volume
  rep_tri9 <- make_test_triangle(nrow = 140, ncol = 41) |>
    construct_triangle()
  tv9 <- allocate_reference_times(
    reporting_triangle = rep_tri9
  )
  expect_identical(tv9$n_history_delay, 60)
  expect_identical(tv9$n_retrospective_nowcasts, 60)
})

test_that("allocate_reference_times warns and reallocates appropriately when sufficient ref times but not enough retro nowcasts", { # nolint
  # Handle larger numbers
  rep_tri <- make_test_triangle(nrow = 14, ncol = 4) |>
    construct_triangle()
  tv <- expect_message(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      prop_delay = 0.95
    ),
    regexp = "0.95 reference times were specified for delay estimation but" # nolint
  )
})

test_that("allocate_reference_times errors when inputs are invalid", {
  rep_tri <- make_test_triangle(nrow = 14, ncol = 4) |>
    construct_triangle()

  expect_error(
    allocate_reference_times(
      reporting_triangle = 6
    )
  )

  expect_error(
    allocate_reference_times(
      reporting_triangle = rep_tri,
      n_min_retro_nowcasts = 0.1
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
