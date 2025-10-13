#' @title Create a reporting triangle
#'
#' @param data Either a `reporting_triangle` object or a `reporting_triangles`
#'    object.
#' @inheritParams apply_delay
#' @inheritParams sample_prediction
#' @inheritParams allocate_reference_times
#' @inheritParams sample_nowcast
#' @param include_draws Boolean indicating whether or not to include uncertainty
#'   in the output, default is TRUE.
#' @param draws Integer indicating the number of probabilistic draws to include
#'    if `include_draws` is TRUE. Default is 1000.
#' @param ... Additional arguments passed to methods. For the
#'     `reporting_triangles` method: `strata`, `strata_sharing`.
#'
#
#' @returns `reporting_triangle` class object which is a list containing:
#'    - A matrix with which rows are reference times and columns are delays and
#'    entries are incident cases at each reference time and delay.
#'    - An integer indicating the maximum delay used to create the reporting
#'    triangle
#'    - A vector of the same length as the rows of the matrix indicating the
#'    dates corresponding to the reference times in the rows of the reporting
#'    triangle.
#'    - A character string indicating the strata.
#'    - A vector indicating the "structure" of the reporting triangle.
#'    - A character string indicating the unit of the delays.
#' @export
#'
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#' nowcast_obj <- baselinenowcast(rep_tri)
#' @importFrom lubridate time_length days ymd
baselinenowcast <- function(data,
                            delay_pmf = NULL,
                            uncertainty_params = NULL,
                            scale_factor = 3,
                            prop_delay = 0.5,
                            include_draws = TRUE,
                            draws = 1000,
                            ...) {
  UseMethod("baselinenowcast")
}

#' @title Creating a dataframe of nowcast results from a single reporting
#'    triangle
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @rdname baselinenowcast
#' @export
#' @method baselinenowcast reporting_triangle
baselinenowcast.reporting_triangle <- function(
    data,
    delay_pmf = NULL,
    uncertainty_params = NULL,
    scale_factor = 3,
    prop_delay = 0.5,
    include_draws = TRUE,
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    ...) {
  tri <- data$reporting_triangle_matrix

  if (is.null(delay_pmf)) {
    delay_pmf <- estimate_delay(tri)
  } else {
    # check for delay pmf being the right length/format
    .validate_delay(tri, delay_pmf)
  }

  tv <- allocate_reference_times(tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay
  )
  pt_nowcast <- apply_delay(tri, delay_pmf)

  if (is.null(uncertainty_params)) {
    trunc_rep_tris <- truncate_triangles(tri, n = tv$n_retrospective_nowcasts)
    retro_rep_tris <- construct_triangles(trunc_rep_tris,
      structure = data$structure
    )
    pt_nowcasts <- fill_triangles(retro_rep_tris,
      n = tv$n_history_delay,
      max_delay = data$max_delay
    )
    uncertainty_params <- estimate_uncertainty(
      pt_nowcasts,
      trunc_rep_tris,
      retro_rep_tris,
      n = tv$n_retrospective_nowcasts,
      uncertainty_model = uncertainty_model,
      ...
    )
  } else {
    # check for uncertainty params being the right length/format
    .validate_uncertainty(tri, uncertainty_params)
  }

  if (include_draws) {
    nowcast_df <- sample_nowcasts(
      pt_nowcast,
      tri,
      uncertainty_params,
      draws,
      uncertainty_sampler = uncertainty_sampler,
      ...
    )
  } else {
    nowcast_df_wide <- as.data.frame(as.matrix(tri))
    nowcast_df_wide$time <- seq_len(nrow(nowcast_df))

    nowcast_df <- reshape(nowcast_df_wide,
      direction = "long",
      varying = setdiff(names(nowcast_df), "time"),
      v.names = "pred_count",
      timevar = "delay",
      times = setdiff(names(nowcast_df), "time"),
      idvar = "time"
    )
    rownames(nowcast_df) <- NULL
  }

  result_df <- .combine_data(nowcast_df,
    strata_list = data$strata,
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
#'  (`pred_count`), and the draw number (`draw`).
#' @param strata_list Named list where each entry should be the name of the strata
#'    and the name of the corresponding strata.
#' @param reference_dates Vector of reference dates corresponding to the
#'    reference times in the `nowcast_df`.
#'
#' @returns Data.frame indexed by the reference dates and with columns for
#'   each of the named elements in the named list `strata`.
.combine_data <- function(nowcast_df,
                          strata_list,
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

  if (!is.null(strata_list)) {
    for (strata_name in names(strata_list)) {
      nowcast_df_dates[[strata_name]] <- strata_list[[strata_name]]
    }
  }
  nowcast_df_dates$time <- NULL

  return(nowcast_df_dates)
}
