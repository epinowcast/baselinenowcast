#' Convert reporting_triangle_df to list of reporting_triangles
#'
#' Splits a [reporting_triangle_df] by its strata attribute and converts each
#' stratum to a [reporting_triangle] object. Returns a named list.
#'
#' @param data A [reporting_triangle_df] object. If it has no strata, returns
#'   a list with a single unnamed reporting_triangle. If it has strata, returns
#'   a named list with one reporting_triangle per unique stratum combination.
#'
#' @return Named list of [reporting_triangle] objects. Names are constructed
#'   from the strata values separated by "___".
#' @family reporting_triangle_df
#' @export
#' @examples
#' # Single stratum returns list with one element
#' rt_df <- as_reporting_triangle_df(syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ])
#' rt_list <- as_reporting_triangles(rt_df)
#' length(rt_list) # 1
#'
#' # Multiple strata returns named list
#' rt_df_strata <- as_reporting_triangle_df(
#'   germany_covid19_hosp,
#'   by = c("age_group", "location")
#' )
#' rt_list <- as_reporting_triangles(rt_df_strata)
#' names(rt_list)
as_reporting_triangles <- function(data) {
  assert_reporting_triangle_df(data)

  strata <- get_strata(data)
  delays_unit <- attr(data, "delays_unit")

  if (is.null(strata)) {
    # Single stratum - convert directly
    rt <- as_reporting_triangle(
      data,
      delays_unit = delays_unit
    )
    return(list(rt))
  }

  # Multiple strata - split and convert
  # Convert to plain data.frame first to avoid class issues with split
  data_plain <- as.data.frame(data)

  list_of_dfs <- .split_df_by_cols(
    long_df = data_plain,
    col_names = strata
  )

  # Convert each data.frame to reporting_triangle
  list_of_triangles <- lapply(list_of_dfs, function(df) {
    # Select only required columns for reporting_triangle
    df_subset <- df[, c("reference_date", "report_date", "count"), drop = FALSE]
    as_reporting_triangle(
      df_subset,
      delays_unit = delays_unit
    )
  })

  return(list_of_triangles)
}
