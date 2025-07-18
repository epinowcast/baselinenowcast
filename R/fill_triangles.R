#' Generate retrospective nowcasts
#'
#' This function ingests a list of incomplete reporting triangles and
#'   generates a list of point nowcast matrices, based on the delay estimated in
#'   each triangle or the corresponding delay passed in. It uses the specified
#'   `n` number of reference times to estimate the delay in each retrospective
#'   reporting triangle.
#'
#' @inheritParams estimate_uncertainty
#' @inheritParams estimate_delay
#' @param n Integer indicating the number of reference times
#'    (number of rows) to use to estimate the delay distribution for each
#'    reporting triangle. Default is the minimum of the number of rows of
#'    all the matrices in the `list_of_rts`.
#' @param delay_pmf Vector or list of vectors of delays assumed to be indexed
#'    starting at the first delay column in each of the matrices in
#'     `retro_reporting_triangles`. If a list, must of the same length as
#'     `retro_reporting_triangles`, with elements aligning. Default is `NULL`
#'
#' @returns `pt_nowcast_matr_list` List of the same number of elements as the
#'    input `retro_reporting_triangles`but with each reporting triangle filled
#'    in based on the delay estimated in that reporting triangle.
#' @export
#' @importFrom cli cli_abort cli_alert_danger cli_alert_info
#' @examples
#' triangle <- matrix(
#'   c(
#'     65, 46, 21, 7,
#'     70, 40, 20, 5,
#'     80, 50, 10, 10,
#'     100, 40, 31, 20,
#'     95, 45, 21, NA,
#'     82, 42, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 7,
#'   byrow = TRUE
#' )
#'
#' trunc_rts <- truncate_triangles(triangle)
#' retro_rts <- construct_triangles(trunc_rts)
#' retro_pt_nowcast_mat_list <- fill_triangles(retro_rts)
#' retro_pt_nowcast_mat_list[1:3]
fill_triangles <- function(retro_reporting_triangles,
                           max_delay = min(
                             sapply(retro_reporting_triangles, ncol)
                           ) - 1,
                           n = min(
                             sapply(retro_reporting_triangles, nrow)
                           ),
                           delay_pmf = NULL) {
  if (is.list(delay_pmf)) { # name as a list and check length of elements
    delay_pmf_list <- delay_pmf
    if (length(delay_pmf_list) != length(retro_reporting_triangles)) {
      cli_abort(message = c(
        "List of `delay_pmf` is not the same length as ",
        "`retro_reporting_triangles`."
      ))
    }
  } else { # create a list with the same pmf
    delay_pmf_list <- rep(list(delay_pmf), length(retro_reporting_triangles))
  }

  safe_fill_triangle <- .safelydoesit(fill_triangle)

  # Use the safe version in mapply, iterating through each item in both
  # lists of reporting triangles and delay PMFs
  pt_nowcast_mat_list <- mapply(
    function(triangle, pmf, ind) {
      result <- safe_fill_triangle(
        reporting_triangle = triangle,
        delay_pmf = pmf,
        n = n,
        max_delay = max_delay
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
  error_indices <- which(sapply(pt_nowcast_mat_list, is.null))
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


  return(pt_nowcast_mat_list)
}

#' Generate point nowcast
#'
#' This function ingests a reporting triangle matrix and optionally, a delay
#'   distribution, and returns a completed reporting square which represents
#'   the point nowcast. If a delay distribution is specified, this will be
#'   used to generate the nowcast, otherwise, a delay distribution will be
#'   estimated from the `reporting_triangle`.
#' @param delay_pmf Vector of delays assumed to be indexed
#'    starting at the first delay column in `reporting_triangle`.
#'    Default is `NULL`, which will estimate a delay from the
#'    `reporting_triangle`.
#' @inheritParams estimate_delay
#' @returns `point_nowcast_matrix` Matrix of the same number of rows and
#'   columns as the `reporting_triangle` but with the missing values filled
#'   in as point estimates.
#' @export
#'
#' @examples
#' triangle <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' point_nowcast_matrix <- fill_triangle(
#'   reporting_triangle = triangle
#' )
#' point_nowcast_matrix
fill_triangle <- function(reporting_triangle,
                          max_delay = ncol(reporting_triangle) - 1,
                          n = nrow(reporting_triangle),
                          delay_pmf = NULL) {
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
  n_rows <- nrow(reporting_triangle)
  has_complete_row <- any(
    rowSums(is.na(reporting_triangle[(n_rows - n + 1):n_rows, ])) == 0
  )
  if (isFALSE(has_complete_row)) {
    cli_abort(
      message = c(
        "The rows used for delay estimation in the reporting triangle must ",
        "contain at least one row with no missing observations."
      )
    )
  }
  .validate_triangle(reporting_triangle)
  if (is.null(delay_pmf)) {
    delay_pmf <- estimate_delay(
      reporting_triangle = reporting_triangle,
      max_delay = max_delay,
      n = n
    )
  }

  point_nowcast_matrix <- apply_delay(reporting_triangle, delay_pmf)
  return(point_nowcast_matrix)
}
