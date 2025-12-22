#' Get strata column names from reporting_triangle_df
#'
#' @param x A [reporting_triangle_df] object
#' @return Character vector of strata column names, or NULL if no strata
#' @family reporting_triangle_df
#' @export
#' @examples
#' # No strata
#' rt_df <- as_reporting_triangle_df(syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ])
#' get_strata(rt_df) # NULL
#'
#' # With strata
#' rt_df_strata <- as_reporting_triangle_df(
#'   germany_covid19_hosp,
#'   by = c("age_group", "location")
#' )
#' get_strata(rt_df_strata) # c("age_group", "location")
get_strata <- function(x) {
  if (!is_reporting_triangle_df(x)) {
    cli_abort(message = "x must have class 'reporting_triangle_df'")
  }
  return(attr(x, "strata"))
}
