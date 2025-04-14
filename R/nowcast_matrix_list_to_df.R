#' Generate probabilistic nowcast dataframe
#'
#' This function ingests a list of nowcast matrixes, and generates a long tidy
#'    dataframe indexed by time and delay.
#'
#' @param nowcast_matrix_list List of probabilistic nowcast matrices
#' @importFrom cli cli_abort
#' @returns `nowcast_df` Dataframe containing observations and probabilistic
#'    nowcasts indexed by reference time and delay.
#' @export
#'
#' @examples
#' point_nowcast_matrix <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 17.8,
#'     80, 40, 23.2, 15.9,
#'     70, 35, 20.3, 19.9
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#'
#' nowcast_matrix_list <- add_uncertainty(
#'   point_nowcast_matrix = point_nowcast_matrix,
#'   disp = c(8, 1.4, 4),
#'   n_draws = 10
#' )
#'
#' nowcast_df <- nowcast_matrix_list_to_df(
#'   nowcast_matrix_list = nowcast_matrix_list
#' )
nowcast_matrix_list_to_df <- function(nowcast_matrix_list) {
  # Check if all elements are matrices
  if (!all(sapply(nowcast_matrix_list, is.matrix))) {
    cli_abort("All elements in the list must be matrices.")
  }
  if (length(nowcast_matrix_list) < 1) {
    cli_abort("List of nowcasts is empty")
  }

  # Convert each matrix to a data frame with an index column
  combined_df <- do.call(
    rbind,
    lapply(
      seq_along(nowcast_matrix_list),
      function(i) {
        return(nowcast_matrix_to_df(
          nowcast_matrix_list[[i]],
          draw = i
        ))
      }
    )
  )

  return(combined_df)
}

#' Convert a reporting square matrix to a long data frame
#'
#' @param matrix Matrix in the form of a reporting triangle/square, where the
#'    rows indicate the reference time and the columns indicate the delay.
#' @param draw integer tracking the draw that the reporting matrix
#'    corresponds to. Default is `NULL` which will not return a draw column.
#' @importFrom checkmate assert_integerish
#'
#' @returns A long dataframe of the length of the product of the number of
#'    columns and the number of rows, with information on what time and delay
#'    the observation corresponds to.
#' @export
#'
#' @examples
#' nowcast_matrix <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 18,
#'     80, 40, 24, 16,
#'     70, 35, 21, 19,
#'     67, 34, 15, 9
#'   ),
#'   nrow = 6,
#'   byrow = TRUE
#' )
#'
#' long_df <- nowcast_matrix_to_df(nowcast_matrix)
#' print(long_df)
nowcast_matrix_to_df <- function(matrix,
                                 draw = NULL) {
  df_wide <- as.data.frame(matrix)

  df_long <- data.frame(
    time = rep(seq_len(nrow(df_wide)), each = ncol(df_wide)),
    delay = rep(seq_len(ncol(df_wide)), times = nrow(df_wide)),
    count = as.vector(t(df_wide))
  )

  if (!is.null(draw)) {
    assert_integerish(draw, lower = 0)
    df_long$draw <- rep(draw, times = nrow(df_wide) * ncol(df_wide))
  }

  return(df_long)
}
