# Integration tests for full workflow with negative PMF entries
# These tests verify the complete pipeline works when using preprocess = NULL
# with data that produces negative PMF entries

test_that(
  "estimate_delay with preprocess = NULL produces negative PMF entries",
  {
    # Use the example data
    triangle <- example_downward_corrections_mat

    # Estimate delay with preprocess = NULL
    delay_pmf <- estimate_delay(
      reporting_triangle = triangle,
      max_delay = 3,
      n = 5,
      preprocess = NULL
    )

    # PMF should have at least one negative entry
    expect_true(any(delay_pmf < 0))

    # PMF should still sum to 1
    expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)

    # Identify which entry is negative (should be delay 2)
    negative_indices <- which(delay_pmf < 0)
    expect_gt(length(negative_indices), 0)

    # Compute CDF and verify it has at least one decreasing step
    delay_cdf <- cumsum(delay_pmf)
    cdf_diffs <- diff(delay_cdf)
    expect_true(any(cdf_diffs < 0))

    # But CDF should still end at 1
    expect_equal(delay_cdf[length(delay_cdf)], 1, tolerance = 1e-6)
  }
)

test_that(
  "apply_delay with negative PMF fills NA cells correctly",
  {
    triangle <- example_downward_corrections_mat

    # Define a delay PMF with a negative value
    delay_pmf_negative <- c(0.7, 0.4, -0.15, 0.05)

    # Apply delay
    nowcast <- apply_delay(
      reporting_triangle = triangle,
      delay_pmf = delay_pmf_negative
    )

    # Verify nowcast is a matrix with same dimensions
    expect_is(nowcast, "matrix")
    expect_identical(dim(nowcast), dim(triangle))

    # Verify all NA cells have been filled
    expect_false(anyNA(nowcast))

    # Check that observed values are preserved
    # First 5 rows should be complete, so they should be unchanged
    expect_identical(nowcast[1:5, ], triangle[1:5, ])

    # Rows 6-8 should have been completed (previously had NAs)
    expect_false(anyNA(nowcast[6:8, ]))

    # All values should be finite
    expect_true(all(is.finite(nowcast)))

    # Verify specific NA cells were filled with non-NA values
    # Row 6, col 4 was NA
    expect_false(is.na(nowcast[6, 4]))
    # Row 7, cols 3-4 were NA
    expect_false(is.na(nowcast[7, 3]))
    expect_false(is.na(nowcast[7, 4]))
    # Row 8, cols 2-4 were NA
    expect_false(is.na(nowcast[8, 2]))
    expect_false(is.na(nowcast[8, 3]))
    expect_false(is.na(nowcast[8, 4]))
  }
)

test_that(
  "Full workflow estimate_delay then apply_delay completes with negative PMF",
  {
    triangle <- example_downward_corrections_mat

    # Estimate delay with preprocess = NULL
    delay_pmf <- estimate_delay(
      reporting_triangle = triangle,
      max_delay = 3,
      n = 5,
      preprocess = NULL
    )

    # Verify PMF has negative entries
    expect_true(any(delay_pmf < 0))

    # Apply delay to get nowcast
    nowcast <- apply_delay(
      reporting_triangle = triangle,
      delay_pmf = delay_pmf
    )

    # Verify nowcast completed successfully
    expect_is(nowcast, "matrix")
    expect_identical(dim(nowcast), dim(triangle))
    expect_false(anyNA(nowcast))
  }
)

test_that(
  "Uncertainty estimation works when partial sums stay non-negative",
  {
    # Create a triangle where negative PMF entries don't cause issues
    # for uncertainty estimation
    triangle <- matrix(
      c(
        100, 60, 20, 10,
        120, 70, 25, 15,
        110, 65, 22, 12,
        130, 75, 28, 18,
        115, 68, 24, 14,
        125, 72, 26, NA,
        105, 62, NA, NA,
        95, NA, NA, NA
      ),
      nrow = 8,
      byrow = TRUE
    )

    # Create retrospective nowcasts using default preprocessing
    # (to avoid negative predictions in uncertainty estimation)
    trunc_rts <- truncate_triangles(triangle, n = 3)
    retro_rts <- construct_triangles(trunc_rts)
    retro_nowcasts <- fill_triangles(retro_rts, n = 5)

    # Estimate uncertainty should work
    disp_params <- estimate_uncertainty(
      point_nowcast_matrices = retro_nowcasts,
      truncated_reporting_triangles = trunc_rts,
      retro_reporting_triangles = retro_rts,
      n = 3
    )

    # Should return valid dispersion parameters
    expect_type(disp_params, "double")
    expect_true(all(disp_params > 0))
    expect_true(all(is.finite(disp_params)))
  }
)

test_that(
  "estimate_delay with default preprocessing produces non-negative PMF",
  {
    triangle <- example_downward_corrections_mat

    # Use default preprocessing
    delay_pmf_default <- estimate_delay(
      reporting_triangle = triangle,
      max_delay = 3,
      n = 5
    )

    # Should NOT have negative entries
    expect_true(all(delay_pmf_default >= 0))

    # Should sum to 1
    expect_equal(sum(delay_pmf_default), 1, tolerance = 1e-6)

    # Apply delay should work
    nowcast_default <- apply_delay(
      reporting_triangle = triangle,
      delay_pmf = delay_pmf_default
    )

    expect_false(anyNA(nowcast_default))
  }
)

test_that(
  "preprocess = NULL vs default preprocessing produces different PMFs",
  {
    triangle <- example_downward_corrections_mat

    # Get PMF with NULL preprocessing
    pmf_null <- estimate_delay(
      reporting_triangle = triangle,
      max_delay = 3,
      n = 5,
      preprocess = NULL
    )

    # Get PMF with default preprocessing
    pmf_default <- estimate_delay(
      reporting_triangle = triangle,
      max_delay = 3,
      n = 5
    )

    # They should be different
    expect_false(identical(pmf_null, pmf_default))

    # NULL version has negatives
    expect_true(any(pmf_null < 0))

    # Default version does not
    expect_true(all(pmf_default >= 0))

    # Both sum to 1
    expect_equal(sum(pmf_null), 1, tolerance = 1e-6)
    expect_equal(sum(pmf_default), 1, tolerance = 1e-6)
  }
)

test_that(
  "baselinenowcast with preprocess = NULL produces point nowcast",
  {
    # Convert matrix to reporting_triangle object
    reference_dates <- seq(
      from = as.Date("2025-01-01"),
      length.out = nrow(example_downward_corrections_mat),
      by = "day"
    )
    triangle <- as_reporting_triangle(
      data = example_downward_corrections_mat,
      reference_dates = reference_dates,
      max_delay = 3
    )

    # Test that baselinenowcast() completes with preprocess = NULL
    # Using output_type = "point" since uncertainty estimation
    # does not support negative predictions from negative PMF
    result <- expect_no_error(
      suppressWarnings(
        baselinenowcast(
          data = triangle,
          preprocess = NULL,
          output_type = "point"
        )
      )
    )

    # Verify output structure
    expect_s3_class(result, "data.frame")
    expected_cols <- c("reference_date", "pred_count", "draw", "output_type")
    expect_true(all(expected_cols %in% colnames(result)))

    # Verify nowcast values exist
    expect_false(anyNA(result$pred_count))

    # Verify output has rows
    expect_gt(nrow(result), 0)

    # Verify output_type is correct
    expect_identical(unique(result$output_type), "point")
  }
)
