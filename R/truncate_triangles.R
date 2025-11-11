#' Generate truncated reporting triangles
#'
#' This function ingests a reporting triangle/matrix and the number of
#'   truncated reporting triangles we want to create, `n`, and iteratively
#'   truncates the reporting triangle, working from the latest reference time
#'   (bottom) to the older reference times (top) for `n`
#'   snapshots.
#'
#' @param n Integer indicating the number of retrospective
#'   truncated triangles to be generated, always starting from the most
#'   recent reference time. Default is to generate truncated matrices for
#'   each row up until there are insufficient rows to generate nowcasts
#'   from, where the minimum requirement is one more than the  number of
#'   horizon rows (rows containing NAs).
#' @inheritParams estimate_delay
#' @returns `trunc_rep_tri_list` List of `n` truncated `reporting_triangle`
#'   objects with as many rows as available given the truncation, and the same
#'   number of columns as the input `reporting_triangle`.
#' @importFrom checkmate assert_integerish
#' @family generate_retrospective_data
#' @export
#' @examples
#' # Generate multiple truncated triangles
#' truncated_rts <- truncate_triangles(example_reporting_triangle, n = 2)
#' truncated_rts[1:2]
truncate_triangles <- function(reporting_triangle,
                               n = nrow(reporting_triangle) -
                                 sum(is.na(rowSums(reporting_triangle))) - 1) {
  assert_reporting_triangle(reporting_triangle)
  assert_integerish(n, lower = 0)
  trunc_rep_tri_list <- lapply(
    seq_len(n),
    truncate_triangle,
    reporting_triangle
  )

  return(trunc_rep_tri_list)
}

#' Get a single truncated triangle
#'
#' This function takes in a integer `t` and a reporting triangle and generates
#'   a truncated reporting triangle, removing the last `t` observations.
#'
#' @param t Integer indicating the number of timepoints to truncate off the
#'   bottom of the original reporting triangle.
#' @inheritParams estimate_delay
#' @returns `trunc_rep_tri` A `reporting_triangle` object with `t` fewer rows
#'   than the input. The class and metadata are preserved with updated reference
#'   dates.
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
#' @family generate_retrospective_data
#' @export
#' @examples
#' # Generate single truncated triangle
#' trunc_rep_tri <- truncate_triangle(t = 1, reporting_triangle = example_reporting_triangle)
#' trunc_rep_tri
truncate_triangle <- function(t,
                              reporting_triangle) {
  assert_reporting_triangle(reporting_triangle)
  assert_integerish(t, lower = 0)

  n_obs <- nrow(reporting_triangle)
  if (t >= n_obs) {
    cli_abort(
      message = c(
        "The as of time point is greater than or equal to the number of ",
        "rows in the original triangle."
      )
    )
  }

  # Extract the matrix portion and subset using head()
  n_rows <- n_obs - t
  rep_tri_mat <- head(
    as.matrix(reporting_triangle),
    n = n_rows
  )

  # Extract and update metadata
  ref_dates <- head(get_reference_dates(reporting_triangle), n = n_rows)
  delays_unit <- get_delays_unit(reporting_triangle)

  # Create new reporting_triangle with updated metadata
  rep_tri_trunc <- new_reporting_triangle(
    reporting_triangle_matrix = rep_tri_mat,
    reference_dates = ref_dates,
    delays_unit = delays_unit
  )

  return(rep_tri_trunc)
}
