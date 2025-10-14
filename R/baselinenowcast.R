#' @title Create a reporting triangle
#'
#' @param data Reporting triangle to be nowcasted
#' @inheritParams sample_nowcast
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @param output_type Character string indicating whether the output should be
#'   samples (`"samples"`)from the estimate with full uncertainty or whether to
#'   return the point estimate (`"point"`). Default is `"samples"`.
#' @param draws Integer indicating the number of probabilistic draws to include
#'    if `output_type` is `"samples"`. Default is 1000.
#' @param ... Additional arguments passed to methods.
#' @returns `nowcast_df` Data.frame of class `nowcast_df`
#' @family nowcast_df
#' @export
baselinenowcast <- function(data,
                            scale_factor = 3,
                            prop_delay = 0.5,
                            output_type = "samples",
                            draws = 1000,
                            uncertainty_model = fit_by_horizon,
                            uncertainty_sampler = sample_nb,
                            ...) {
  UseMethod("baselinenowcast")
}

#' @title Creating a dataframe of nowcast results from a single reporting
#'    triangle
#'
#' @param data `reporting_triangle` class object to be nowcasted.
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `data$reporting_triangle_matrix`. Default is to
#'   estimate from the reporting triangle matrix in `data`,
#'   `estimate_delay(data$reporting_triangle_matrix)`.
#' @param uncertainty_params Vector of uncertainty parameters ordered from
#'   horizon 1 to the maximum horizon. Default is `NULL`, which will
#'   result in computing the uncertainty parameters from the `data`.
#' @param ... Additional arguments passed to
#'    \code{\link{estimate_uncertainty}}
#'    and \code{\link{sample_nowcast}}.
#' @inheritParams baselinenowcast
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @family nowcast_df
#' @export
#' @method baselinenowcast reporting_triangle
#' @returns A data.frame of class `nowcast_df`
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#' nowcast_df <- baselinenowcast(rep_tri)
#' nowcast_df
baselinenowcast.reporting_triangle <- function(
    data,
    scale_factor = 3,
    prop_delay = 0.5,
    output_type = "samples",
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    delay_pmf = estimate_delay(data$reporting_triangle_matrix),
    uncertainty_params = NULL,
    ...) {
  tri <- data$reporting_triangle_matrix

  assert_choice(output_type, choices = c("samples", "point"))
  # check for delay pmf being the right length/format
  .validate_delay(tri, delay_pmf)


  tv <- allocate_reference_times(tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay
  )
  pt_nowcast <- apply_delay(tri, delay_pmf)

  if (output_type == "point") {
    nowcast_df <- data.frame(
      time = seq_len(nrow(pt_nowcast)),
      pred_count = rowSums(pt_nowcast)
    )
    if (!is.null(uncertainty_params)) {
      cli_warn(
        message =
          "`uncertainty_params` passed in but point estimate was specified as an output type. `uncertainty params` will not be used."
      ) # nolint
    }
  } else { # estimate uncertainty or sample from passed in uncertainty
    if (is.null(uncertainty_params)) {
      nowcast_df <- estimate_and_apply_uncertainty(
        point_nowcast_matrix = pt_nowcast,
        reporting_triangle = tri,
        n_history_delay = tv$n_history_delay,
        n_retrospective_nowcast = tv$n_retrospective_nowcasts,
        max_delay = ncol(tri) - 1,
        draws = draws,
        uncertainty_model = uncertainty_model,
        uncertainty_sampler = uncertainty_sampler,
        ...
      )
    } else { # uncertainty parameters passed in and not a point estimate
      # check for uncertainty params being the right length/format
      .validate_uncertainty(tri, uncertainty_params)
      nowcast_df <- sample_nowcasts(
        point_nowcast_matrix = pt_nowcast,
        reporting_triangle = tri,
        uncertainty_params = uncertainty_params,
        draws = draws,
        uncertainty_sampler = uncertainty_sampler,
        ...
      )
    }
  }


  result_df <- new_nowcast_df(nowcast_df,
    strata_map = data$strata_map,
    reference_dates = data$reference_date
  )


  return(result_df)
}

#' Combine data from a nowcast dataframe, strata, and reference dates
#' @description Combines data from a nowcast dataframe, a named list of the
#'    strata associated with the nowcast dataframe, and a vector of reference
#'    dates corresponding to the time column in the `nowcast_df`
#'
#' @param nowcast_df Data.frame containing information for multiple draws with
#'  columns for the reference time (`time`), the predicted counts
#'  (`pred_count`), and optionally the draw number (`draw`).
#' @inheritParams as_reporting_triangle
#' @param reference_dates Vector of reference dates corresponding to the
#'    reference times in the `nowcast_df`.
#'
#' @returns An object of class `nowcast_df` which is a data.frame indexed by
#'  the reference dates and with columns for
#'  each of the named elements in the named list `strata`.
#' @export
new_nowcast_df <- function(nowcast_df,
                           strata_map,
                           reference_dates) {
  spine_df <- data.frame(
    reference_date = reference_dates,
    time = seq_along(reference_dates)
  )

  nowcast_df_dates <- merge(nowcast_df,
    spine_df,
    by = "time",
    all.x = TRUE
  )

  if (!is.null(strata_map)) {
    for (strata_name in names(strata_map)) {
      nowcast_df_dates[[strata_name]] <- strata_map[[strata_name]]
    }
  }
  nowcast_df_dates$time <- NULL

  result <- structure(
    data.frame(nowcast_df_dates),
    class = c(class(nowcast_df_dates), "nowcast_df")
  )

  return(result)
}
