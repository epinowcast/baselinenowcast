#' Get a draw of only the predicted elements of the nowcast vector
#'
#' @param point_nowcast_pred_matrix Matrix containing only the elements that
#'    have not yet been observed as of the final reference date.
#' @param disp Vector of dispersion parameters indexed by horizon from minus
#'    one to the maximum delay.
#' @param fun_to_aggregate Function to pass to
#'    `rollapply`, which will operate along the nowcast vectors after summing
#'    across delays. Eventually, we can add things like mean, but for now since
#'    we are only providing a negative binomial observation model, we can only
#'    allow sum (min or max would work but wouldn't make much sense).
#' @param k Integer indicating the number `width` of the call to `rollapply`.
#'    Default is 1.
#'
#' @returns Vector of predicted draws at each reference time, for all reference
#'    times in the input `point_nowcast_pred_matrix`.
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom stats rnbinom
#' @importFrom zoo rollapply
#' @examples
#' point_nowcast_pred_matrix <-
#'   matrix(
#'     c(
#'       NA, NA, NA, NA,
#'       NA, NA, NA, NA,
#'       NA, NA, NA, 16.8,
#'       NA, NA, 21.2, 19.5,
#'       NA, 34.5, 15.4, 9.1
#'     ),
#'     nrow = 4,
#'     byrow = TRUE
#'   )
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draw <- get_nowcast_pred_draw(
#'   point_nowcast_pred_matrix,
#'   disp
#' )
#' nowcast_pred_draw
get_nowcast_pred_draw <- function(point_nowcast_pred_matrix,
                                  disp,
                                  fun_to_aggregate = sum,
                                  k = 1) {
  if (length(disp) > nrow(point_nowcast_pred_matrix)) {
    cli_abort(
      message = c(
        "Vector of dispersion parameters is greater than the number ",
        "of reference times in `point_nowcast_pred_matrix`. Check ",
        "to make sure this is expected behavior and truncate ",
        "dispersion to eliminate the horizons that are not in ",
        "the matrix."
      )
    )
  }

  if (!anyNA(point_nowcast_pred_matrix)) {
    cli_warn(
      message = c(
        "No NAs detected in `point_pred_matrix`. Make sure ",
        " that matrix contains NAs for the elements alredy observed ",
        "as of the final reference time."
      )
    )
  }

  n_horizons <- length(disp)
  max_t <- nrow(point_nowcast_pred_matrix)
  mean_pred <- rollapply(
    rowSums(point_nowcast_pred_matrix, na.rm = TRUE)[(max_t - n_horizons + 1):max_t], # nolint
    k,
    fun_to_aggregate
  ) # nolint
  # Nowcast predictions only (these are reversed, first element is horizon 0)
  draw_pred <- rnbinom(n = n_horizons, size = rev(disp), mu = mean_pred)
  # Pad with 0s for the fully observed rows, which are before
  # the max_t - n_horizons
  draw_pred_long <- c(rep(0, max_t - n_horizons), draw_pred)
  return(draw_pred_long)
}

#' Get a dataframe of multiple draws of only the predicted elements of the
#'    nowcast vector
#'
#' @inheritParams get_nowcast_pred_draw
#' @param n_draws Integer indicating the number of draws of the predicted
#'    nowcast vector to generate. Default is `1000`.
#'
#' @returns Dataframe containing the predicted point nowcast vectors indexed by
#'    reference time (`pred_count`), reference time (`time`), and the draw index
#'    (`draw`).
#' @export
#'
#' @examples
#' point_nowcast_pred_matrix <-
#'   matrix(
#'     c(
#'       NA, NA, NA, NA,
#'       NA, NA, NA, NA,
#'       NA, NA, NA, 16.8,
#'       NA, NA, 21.2, 19.5,
#'       NA, 34.5, 15.4, 9.1
#'     ),
#'     nrow = 4,
#'     byrow = TRUE
#'   )
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draws_df <- get_nowcast_pred_draws(
#'   point_nowcast_pred_matrix,
#'   disp,
#'   500
#' )
#' nowcast_pred_draws_df
get_nowcast_pred_draws <- function(point_nowcast_pred_matrix,
                                   disp,
                                   n_draws = 1000,
                                   fun_to_aggregate = sum,
                                   k = 1) {
  # Define allowed functions
  allowed_functions <- list(
    sum = sum
  )

  # Validate function
  fun_name <- deparse(substitute(fun_to_aggregate))
  if (is.name(fun_to_aggregate)) {
    fun_name <- as.character(fun_to_aggregate)
  }

  # Check if function is in allowed list
  if (!identical(fun_to_aggregate, allowed_functions[[fun_name]]) &&
    !any(sapply(allowed_functions, identical, fun_to_aggregate))) {
    allowed_names <- toString(names(allowed_functions), collapse = ", ")
    stop(sprintf("'fun_to_aggregate' should be one of: %s", allowed_names),
      call. = FALSE
    )
  }

  assert_integerish(n_draws, lower = 1)

  for (i in 1:n_draws) {
    pred_nowcast_vec <- get_nowcast_pred_draw(
      point_nowcast_pred_matrix,
      disp,
      fun_to_aggregate,
      k
    )
    df_i <- data.frame(
      pred_count = pred_nowcast_vec,
      time = seq_along(pred_nowcast_vec),
      draw = i
    )
    if (i == 1) {
      df_pred_draws <- df_i
    } else {
      df_pred_draws <- rbind(df_pred_draws, df_i)
    }
  }

  return(df_pred_draws)
}
