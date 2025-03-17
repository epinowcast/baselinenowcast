#' Generate retrospective reporting triangles
#'
#' This function ingests a reporting triangle/matrix and the number of
#'   retrospective reporting triangles we want to create, `n`, and iteratively
#'   generates the reporting triangle that would have been available as of the
#'   maximum reference time, working from bottom to top for `n` snapshots.
#'
#' @param truncated_triangles List of truncated reporting triangle matrices.
#' @returns List of retrospective reporting triangle matrices, generated
#'   by removing the bottom right observations from the truncated reporting
#'   triangle matrices.
#' @importFrom cli cli_abort
#' @export
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
#' trunc_rts <- truncate_triangles(
#'   triangle = triangle,
#'   n = 2
#' )
#' retro_rts <- generate_triangles(
#'   truncated_triangles = trunc_rts
#' )
#' print(retro_rts[[1]])
#' print(retro_rts[[2]])
generate_triangles <- function(
    truncated_triangles) {
  # Check that input is a list of matrices
  if (typeof(truncated_triangles) != "list") {
    cli_abort(
      message = "`truncated_triangles` must be a list"
    )
  }
  if (!is.matrix(truncated_triangles[[1]])) {
    cli_abort(
      message = "The elements of `truncated_triangles`must be matrices"
    )
  }

  results <- lapply(
    truncated_triangles,
    .replace_lower_right_with_NA
  )

  return(results)
}
