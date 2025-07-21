extract_observed_nowcast_components <- function(
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

  n_possible_horizons <- ncol(list_of_ncs[[1]]) - 1
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
