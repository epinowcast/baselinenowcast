#' Estimate and apply delay from a reporting triangle
#'
#' This function generates a point nowcast by estimating a delay distribution
#'   from the reporting triangle and applying it to complete the triangle. If a
#'   delay distribution is specified, this will be used to generate the
#'   nowcast, otherwise, a delay distribution will be estimated from the
#'   `reporting_triangle`.
#'
#' @inheritParams estimate_delay
#' @inheritParams assert_reporting_triangle
#' @param delay_pmf Vector of delays assumed to be indexed
#'    starting at the first delay column in `reporting_triangle`.
#'    Default is `NULL`, which will estimate a delay from the
#'    `reporting_triangle`.
#'
#' @returns `pt_nowcast_matrix` A `reporting_triangle` object of point nowcasts
#'   with the same structure as the input
#' @family workflow_wrappers
#' @export
#' @importFrom cli cli_abort
#' @examples
#' # Estimate and apply delay using default parameters
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = example_reporting_triangle
#' )
#' pt_nowcast_matrix
#'
#' # Use downward correction example with specific rows for delay estimation
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = example_downward_corr_rt,
#'   n = 5
#' )
#' pt_nowcast_matrix
#'
#' # Provide a pre-computed delay PMF
#' delay_pmf <- estimate_delay(
#'   reporting_triangle = example_reporting_triangle
#' )
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = example_reporting_triangle,
#'   delay_pmf = delay_pmf
#' )
#' pt_nowcast_matrix
estimate_and_apply_delay <- function(reporting_triangle,
                                     n = nrow(reporting_triangle),
                                     delay_pmf = NULL,
                                     validate = TRUE) {
  assert_reporting_triangle(reporting_triangle, validate)

  if (n > nrow(reporting_triangle)) {
    cli_abort(
      message = c(
        "The number of observations (rows) specified for delay estimation ",
        "must be less than or equal to the number of rows of the reporting ",
        "triangle. Either remove the reporting triangles that do ",
        "not contain sufficient data, or lower `n`."
      )
    )
  }

  tri_mat <- tail(reporting_triangle, n = n)
  has_complete_row <- any(rowSums(is.na(tri_mat)) == 0)
  if (isFALSE(has_complete_row)) {
    cli_abort(
      message = c(
        "The rows used for delay estimation in the reporting triangle must ",
        "contain at least one row with no missing observations."
      )
    )
  }

  if (is.null(delay_pmf)) {
    delay_pmf <- estimate_delay(
      reporting_triangle = reporting_triangle,
      n = n,
      validate = FALSE
    )
  }

  point_nowcast_matrix <- apply_delay(reporting_triangle, delay_pmf,
    validate = FALSE
  )

  return(point_nowcast_matrix)
}

#' Estimate and apply delays to generate retrospective nowcasts
#'
#' This function ingests a list of incomplete reporting triangles and
#'   generates a list of point nowcast matrices, based on the delay estimated
#'   in each triangle or the corresponding delay passed in. It uses the
#'   specified `n` number of reference times to estimate the delay in each
#'   retrospective reporting triangle.
#'
#' @inheritParams estimate_delay
#' @inheritParams assert_reporting_triangle
#' @param retro_reporting_triangles List of reporting triangles to generate
#'    nowcasts for. Typically created by [apply_reporting_structures()].
#' @param n Integer indicating the number of reference times
#'    (number of rows) to use to estimate the delay distribution for each
#'    reporting triangle. Default is the minimum of the number of rows of
#'    all the matrices in `retro_reporting_triangles`.
#' @param delay_pmf Vector or list of vectors of delays assumed to be indexed
#'    starting at the first delay column in each of the matrices in
#'    `retro_reporting_triangles`. If a list, must be of the same length as
#'    `retro_reporting_triangles`, with elements aligning. Default is `NULL`.
#'
#' @returns `point_nowcast_matrices` List of the same number of elements as
#'    the input `retro_reporting_triangles` but with each reporting triangle
#'    filled in based on the delay estimated in that reporting triangle.
#' @family workflow_wrappers
#' @export
#' @importFrom cli cli_abort cli_alert_danger cli_alert_info
#' @examples
#' # Generate retrospective nowcasts using larger triangle
#' data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data_as_of) |>
#'   truncate_to_delay(max_delay = 25) |>
#'   tail(n = 50)
#' trunc_rts <- truncate_to_rows(rep_tri, n = 2)
#' retro_rts <- apply_reporting_structures(trunc_rts)
#' retro_pt_nowcast_mat_list <- estimate_and_apply_delays(retro_rts, n = 30)
#' retro_pt_nowcast_mat_list[1:2]
#'
#' # Using a pre-computed delay PMF
#' delay <- estimate_delay(rep_tri, n = 30)
#' retro_pt_nowcast_mat_list <- estimate_and_apply_delays(
#'   retro_rts,
#'   n = 30,
#'   delay_pmf = delay
#' )
#' retro_pt_nowcast_mat_list[1:2]
estimate_and_apply_delays <- function(retro_reporting_triangles,
                                      n = min(
                                        sapply(retro_reporting_triangles, nrow)
                                      ),
                                      delay_pmf = NULL,
                                      validate = TRUE) {
  if (is.list(delay_pmf)) {
    delay_pmf_list <- delay_pmf
    if (length(delay_pmf_list) != length(retro_reporting_triangles)) {
      cli_abort(message = c(
        "List of `delay_pmf` is not the same length as ",
        "`retro_reporting_triangles`."
      ))
    }
  } else {
    delay_pmf_list <- rep(list(delay_pmf), length(retro_reporting_triangles))
  }

  safe_estimate_and_apply_delay <- .safelydoesit(estimate_and_apply_delay)

  # Use the safe version in mapply, iterating through each item in both
  # lists of reporting triangles and delay PMFs
  point_nowcast_matrices <- mapply(
    function(triangle, pmf, ind) {
      result <- safe_estimate_and_apply_delay(
        reporting_triangle = triangle,
        n = n,
        delay_pmf = pmf,
        validate = validate
      )
      if (!is.null(result$error)) {
        # Print the index and the error message
        error_msg <- sprintf(
          "Error at index %d: Point nowcast matrix could not be generated. This may be acceptable for a few point nowcast matrices used to estimate the uncertainty. \n%s", # nolint
          ind,
          result$error$message
        )
        message(error_msg)
        return(NULL)
      } else {
        # Return the result if successful
        return(result$result)
      }
    },
    retro_reporting_triangles,
    delay_pmf_list,
    seq_along(retro_reporting_triangles),
    SIMPLIFY = FALSE
  )

  # After running, filter the results to find error indices
  error_indices <- which(vapply(
    point_nowcast_matrices, is.null, logical(1)
  ))
  # Print summary
  if (length(error_indices) == length(retro_reporting_triangles)) {
    cli_abort(
      message = c(sprintf(
        "\nErrors occurred in all %s reporting triangles. Check if input triangles have valid data structure or contain zeros in the first column.", # nolint
        length(retro_reporting_triangles)
      ))
    )
  } else if (length(error_indices) > 0) {
    cli_alert_danger(
      text = sprintf(
        "\nErrors occurred at indices: %s\n",
        toString(error_indices,
          collapse = ", "
        )
      )
    )
    cli_alert_info(
      text = sprintf(
        "Successfully processed %d out of %d matrices\n",
        length(retro_reporting_triangles) - length(error_indices),
        length(retro_reporting_triangles)
      )
    )
  }


  return(point_nowcast_matrices)
}

#' Safe iterator
#'
#' @param fun Function to wrap around
#'
#' @returns Function that returns a list with `result` and `error` components.
#'   On success: `result` contains the function output and `error` is NULL.
#'   On failure: `result` is NULL and `error` contains the error object.
#' @keywords internal
.safelydoesit <- function(fun) {
  stopifnot(is.function(fun))
  return(
    function(...) {
      return(tryCatch(
        list(result = fun(...), error = NULL),
        error = function(e) {
          return(list(result = NULL, error = e))
        }
      ))
    }
  )
}
