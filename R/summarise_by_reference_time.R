#' Summarise a dataframe containing draws of reporting squares
#'
#' This function ingests a long tidy dataframe containing many draws of
#'    reporting squares across each reference time, by summing over all the
#'    counts reported at each delay `d`.
#'
#' @param nowcast_draws_df Dataframe containing draws of a reporting square.
#'    Must contain the following columns: `time`, `delay`, `draw`, `count`.
#'
#' @returns `summary_df` Dataframe containing the nowcasted expected observed
#'    total observations at each reference time.
#' @export
#' @importFrom checkmate check_names
#'
#' @examples
#' nowcast_draws_df <- data.frame(
#'   time = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   delay = c(1, 2, 1, 2, 1, 2, 1, 2),
#'   draw = c(1, 1, 2, 2, 1, 1, 2, 2),
#'   count = c(3, 6, 4, 7, 1, 2, 2, 3)
#' )
#' summary_df <- summarise_by_reference_time(nowcast_draw_df)
#' print(summary_df)
summarise_by_reference_time <- function(nowcast_draws_df) {
  check_names(colnames(nowcast_draws_df),
    must.include = c("time", "delay", "draw", "count")
  )
  summary_df <- aggregate(count ~ time + draw,
    data = nowcast_draws_df,
    FUN = sum
  )
  colnames(summary_df)[3] <- "total_count"
  return(summary_df)
}


#' @rdname summarise_by_reference_time
#' @export
summarize_by_reference_time <- summarise_by_reference_time
