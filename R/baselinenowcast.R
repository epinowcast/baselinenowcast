#' @title Create a reporting triangle
#'
#' @param data Reporting triangle to be nowcasted
#' @inheritParams apply_delay
#' @inheritParams sample_prediction
#' @inheritParams allocate_reference_times
#' @inheritParams sample_nowcast
#' @param include_draws Boolean indicating whether or not to include uncertainty
#'   in the output, default is TRUE.
#' @param draws Integer indicating the number of probabilistic draws to include
#'    if `include_draws` is TRUE. Default is 1000.
#' @param ... Additional arguments passed to methods.
#' @returns `nowcast_df` Data.frame of class `nowcast_df`
#' @family nowcast_df
#' @export
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
#'
#' @param data `reporting_triangle` class object to be nowcasted.
#' @param ... Additional arguments passed to
#'    `\code{\link{estimate_uncertainty}}`.
#' @inheritParams baselinenowcast
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
    nowcast_df <- data.frame(
      time = seq_len(nrow(pt_nowcast)),
      pred_count = rowSums(pt_nowcast)
    )
  }

  result_df <- new_nowcast_df(nowcast_df,
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
#'  (`pred_count`), and optionally the draw number (`draw`).
#' @param strata_list Named list where each entry should be the name of the
#'   strata and the name of the corresponding strata.
#' @param reference_dates Vector of reference dates corresponding to the
#'    reference times in the `nowcast_df`.
#'
#' @returns An object of class `nowcast_df` which is a data.frame indexed by
#'  the reference dates and with columns for
#'  each of the named elements in the named list `strata`.
#' @export
new_nowcast_df <- function(nowcast_df,
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

  result <- structure(
    data.frame(nowcast_df_dates),
    class = c(class(nowcast_df_dates), "nowcast_df")
  )

  return(result)
}
