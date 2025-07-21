extract_obs_nowcast_comps <- function(
    pt_nowcast_matrices,
    trunc_reporting_triangles,
    retro_reporting_triangles,
    n) {
  # Check that number of matrices being used are at least as long as specified n
  assert_integerish(n, lower = 0)
  .check_list_length(pt_nowcast_matrices, "pt_nowcast_matrices", n)
  .check_list_length(trunc_reporting_triangles, "trunc_reporting_triangles", n)
  .check_list_length(
    retro_reporting_triangles,
    "retro_reporting_triangles",
    n,
    empty_check = FALSE
  )

  # Truncate to only n nowcasts and extract only non-null elements of both lists
  non_null_indices <- which(!sapply(pt_nowcast_matrices[1:n], is.null))
  n_iters <- length(non_null_indices)
  list_of_ncs <- pt_nowcast_matrices[non_null_indices]
  list_of_obs <- trunc_reporting_triangles[non_null_indices]
  list_of_rts <- retro_reporting_triangles[non_null_indices]

  if (n_iters == 0) {
    cli_abort(
      message = c(
        "No valid retrospective nowcasts were found, therefore ",
        "uncertainty can not be estimated using the reporting ",
        "triangle passed in. This may be due to invalid data ",
        "in reporting triangles, such as zeros in the first column."
      )
    )
  }

  # Check that the sets of matrices are the same dimensions
  dims_ncs <- lapply(list_of_ncs, dim)
  dims_obs <- lapply(list_of_obs, dim)
  all_identical <- all(mapply(identical, dims_ncs, dims_obs))
  if (!all_identical) {
    cli_abort(message = c(
      "Dimensions of the first `n` matrices in `pt_nowcast_matrices` and ",
      "`trunc_reporting_triangles` are not the same."
    ))
  }
  # each item is a retrospective nowcast date
  nowcast_list <- list()
  obs_list <- list()
  for (i in seq_len(n_iters)) {
    nowcast_i <- list_of_ncs[[i]]
    nowcast_component <- nowcast_i
    # Remove the last i observations
    trunc_matr_observed <- list_of_obs[[i]]
    obs_component <- trunc_matr_observed
    triangle_observed <- list_of_rts[[i]]
    indices_nowcast <- is.na(triangle_observed)
    indices_observed <- !is.na(trunc_matr_observed)

    condition <- as.matrix(indices_nowcast * indices_observed)
    nowcast_component[!condition] <- NA
    obs_component[!condition] <- NA
    nowcast_list[[i]] <- nowcast_component
    obs_list[[i]] <- obs_component
  }
  # stuck here because neither of these contain the required already observed
  # as of the forecast date components that are needed for any transformations
  test <- list(nowcast_list, obs_list)
  return(test)
}


#' Extract observations and predictions by retrospective nowcast date and
#'    horizon
#'
#' @param pt_nowcast_matrices List of point nowcast matrices where rows
#'    represent reference time points and columns represent delays.
#' @param trunc_reporting_triangles List of truncated reporting matrices,
#'    containing all observations as of the latest reference time. Elements of
#'    list are paired with elements of `pt_nowcast_matrices`.
#' @param retro_reporting_triangles List of `n` truncated reporting triangle
#'   matrices with as many rows as available given the truncation.
#' @param n Integer indicating the number of reporting matrices to use to
#'    estimate the dispersion parameters.
#' @param fun_to_aggregate Function that will operate along the nowcast
#'    vectors after summing across delays. Eventually, we can add things like
#'    mean, but for now since we are only providing a negative binomial
#'    observation model, we can only allow sum. Currently supported
#'    functions: `sum`.
#' @param k Integer indicating the number of reference times to apply the
#'    `fun_to_aggregate` over to create target used to compute the nowcast
#'    errors.
#'
#' @returns List of matrices labelled as `obs` and `pred` corresponding to the
#'    observed components that had yet to be observed as of the retrospective
#'    nowcast time.
#'
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
#' trunc_rts <- truncate_triangles(triangle, n = 2)
#' retro_rts <- construct_triangles(trunc_rts)
#'
#' retro_nowcasts <- fill_triangles(retro_rts, n = 5)
#' obs_and_pred_list <- extract_obs_and_pred(
#'   pt_nowcast_matrices = retro_nowcasts,
#'   trunc_reporting_triangles = trunc_rts,
#'   retro_reporting_triangles = retro_rts,
#'   n = 2
#' )
extract_obs_and_pred <- function(
    pt_nowcast_matrices,
    trunc_reporting_triangles,
    retro_reporting_triangles,
    n = length(pt_nowcast_matrices),
    fun_to_aggregate = sum,
    k = 1) {
  .validate_aggregation_function(fun_to_aggregate)
  assert_integerish(n, lower = 0)
  .check_list_length(pt_nowcast_matrices, "pt_nowcast_matrices", n)
  .check_list_length(trunc_reporting_triangles, "trunc_reporting_triangles", n)
  .check_list_length(
    retro_reporting_triangles,
    "retro_reporting_triangles",
    n,
    empty_check = FALSE
  )

  # Truncate to only n nowcasts and extract only non-null elements of both lists
  non_null_indices <- which(!sapply(pt_nowcast_matrices[1:n], is.null))
  n_iters <- length(non_null_indices)
  list_of_ncs <- pt_nowcast_matrices[non_null_indices]
  list_of_obs <- trunc_reporting_triangles[non_null_indices]
  list_of_rts <- retro_reporting_triangles[non_null_indices]
  if (n_iters == 0) {
    cli_abort(
      message = c(
        "No valid retrospective nowcasts were found, therefore ",
        "uncertainty can not be estimated using the reporting ",
        "triangle passed in. This may be due to invalid data ",
        "in reporting triangles, such as zeros in the first column."
      )
    )
  }



  # Check that nowcasts has no NAs, trunc_rts has some NAs
  if (any(sapply(list_of_ncs, anyNA))) {
    cli_abort(
      message =
        "`pt_nowcast_matrices` contains NAs"
    )
  }
  if (!any(sapply(list_of_obs, anyNA))) {
    cli_warn(
      message =
        "`trunc_reporting_triangles` does not contain any NAs"
    )
  }
  # Check that the sets of matrices are the same dimensions
  dims_ncs <- lapply(list_of_ncs, dim)
  dims_obs <- lapply(list_of_obs, dim)
  all_identical <- all(mapply(identical, dims_ncs, dims_obs))
  if (!all_identical) {
    cli_abort(message = c(
      "Dimensions of the first `n` matrices in `pt_nowcast_matrices` and ",
      "`trunc_reporting_triangles` are not the same."
    ))
  }


  n_possible_horizons <- ncol(list_of_ncs[[1]]) - 1
  # Each row is retrospective nowcast date, each column is a horizon (i.e
  # columns are not delays, but hoirzons, and each cell contains a total
  # value corresponding to that horizon -- the total expected value to add
  exp_to_add <-
    to_add_already_observed <- matrix(NA, nrow = n, ncol = n_possible_horizons)
  for (i in seq_len(n_iters)) { # Seq along retrospective forecast dates
    # Rretrospective nowcast as of i delays ago
    nowcast_i <- list_of_ncs[[i]]
    # Remove the last i observations
    trunc_matr_observed <- list_of_obs[[i]]
    triangle_observed <- list_of_rts[[i]]
    max_t <- nrow(trunc_matr_observed)
    n_horizons <- min(max_t - k + 1, n_possible_horizons)
    if (i == 1 && n_horizons < n_possible_horizons) {
      cli_abort(
        message = c(
          sprintf(
            "Requested window size k=%i is too large to generate sufficient nowcasts for the required forecast horizons for all available nowcast matrices.", # nolint
            k
          )
        )
      )
    }
    if (n_horizons < n_possible_horizons) {
      cli_warn(message = c(
        sprintf(
          "Requested window size k=%i is too large to generate nowcasts for all forecast horizons in matrix %i. ", # nolint
          k,
          i
        )
      ))
    }
    # Take the reporting triangle and look at one row at a time, which
    # corresponds to one horizon
    for (d in 1:n_horizons) {
      start_row <- max_t - d - k + 2
      end_row <- max_t - d + 1
      obs <- trunc_matr_observed[start_row:end_row, ]
      nowcast <- nowcast_i[start_row:end_row, ]
      indices_nowcast <- is.na(triangle_observed[start_row:end_row, ])
      indices_observed <- !is.na(obs)
      # Function to aggregate is always applied after the matrix has been
      # summed across delays
      exp_to_add[i, d] <- fun_to_aggregate(
        rowSums(as.matrix(nowcast *
          indices_nowcast * indices_observed), na.rm = TRUE),
        na.rm = TRUE
      )
      to_add_already_observed[i, d] <- fun_to_aggregate(
        rowSums(as.matrix(
          obs * indices_nowcast * indices_observed
        ), na.rm = TRUE),
        na.rm = TRUE
      )
    } # end loop over forecast horizons
  } # end loop over retrospective nowcast times

  obs_and_pred_list <- list(pred = exp_to_add, obs = to_add_already_observed)
  return(obs_and_pred_list)
}
