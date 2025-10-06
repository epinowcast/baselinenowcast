#' @title Create a reporting triangle
#'
#' @param data Either a `reporting_triangle` object or a `reporting_triangles`
#'    object.
#' @inheritParams apply_delay
#' @inheritParams sample_prediction
#' @inheritParams allocate_reference_times
#' @param include_draws Boolean indicating whether or not to include uncertainty
#'   in the output, default is TRUE.
#' @param draws Integer indicating the number of probabilistic draws to include
#'    if `include_draws` is TRUE. Default is 1000.
#' @param strata_sharing Character string indicating whether uncertainty or
#'    delay estimates are being shared. Default is NULL, for neither.

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
                            delay = NULL,
                            uncertainty_params = NULL,
                            scale_factor = 3,
                            prop_delay = 0.5,
                            include_draws = TRUE,
                            draws = 1000,
                            ...) {
  UseMethod("baselinenowcast")
}

baselinenowcast.reporting_triangle <- function(data,
                                               delay = NULL,
                                               uncertainty_params = NULL,
                                               scale_factor = 3,
                                               prop_delay = 0.5,
                                               include_draws = TRUE,
                                               draws = 1000,
                                               ...){

  tri <- data$reporting_triangle_matrix

  if(is.NULL(delay_pmf){
    delay_pmf <- estimate_delay(tri)
  }else{
    # check for delay pmf being the right length/format
  }

  tv <- allocate_reference_times(tri, scale_factor, prop_delay)
  pt_nowcast <- apply_delay(tri, delay_pmf)

  if is.NULL(uncertainty_params){
    trunc_rep_tris <- truncate_triangles(try, n = tv$n_retrospective_nowcasts)
    retro_rep_tris <- construct_triangles(trunc_rep_tris,
                                          structure = data$structure)
    pt_nowcasts <- fill_triangles(retro_rep_tris,
                                  n = $n_history_delay,
                                  max_delay = data$max_delay)
    uncertainty_params <- estimate_uncertainty(
      pt_nowcast,
      trunc_rep_tris,
      retro_rep_tris,
      n = tv$n_retrospective_nowcasts)
   }else{
     # check for uncertainty params being the right length/format
   }

  if(include_draws){
    nowcast_df <- sample_nowcasts(
      pt_nowcast,
      tri,
      uncertainty_params,
      draws,
      ...
    )
  }else{
    nowcast_df <- as.data.frame(as.matrix(tri)) |>
      mutate(time = row_number()) |>
      pivot_longer(!time,
                   names_to = "delay")
  }

  results <- combine_data(nowcast_df,
                          strata = data$strata,
                          reference_dates = data$reference_date)


  return(nowcast_result)

}
