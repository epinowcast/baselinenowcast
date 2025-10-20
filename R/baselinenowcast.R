#' @title Generate a nowcast
#'
#' @description This function ingests data to be nowcasted and generates a
#'   a \code{\link{baselinenowcast_df}} which contains a probabilistic or point
#'   estimate of the final case counts at each reference date in the `data`.
#'   See \code{\link{baselinenowcast.reporting_triangle}} for details on the
#'   input requirements.
#'
#' @param data Data to be nowcasted
#' @inheritParams sample_nowcast
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @param output_type Character string indicating whether the output should be
#'   samples (`"samples"`)from the estimate with full uncertainty or whether to
#'   return the point estimate (`"point"`). Default is `"samples"`.
#' @param draws Integer indicating the number of probabilistic draws to include
#'    if `output_type` is `"samples"`. Default is 1000.
#' @param ... Additional arguments passed to methods.
#' @returns Data.frame of class \code{\link{baselinenowcast_df}}
#' @family baselinenowcast_df
#' @export
baselinenowcast <- function(data,
                            scale_factor = 3,
                            prop_delay = 0.5,
                            output_type = c("samples", "point"),
                            draws = 1000,
                            uncertainty_model = fit_by_horizon,
                            uncertainty_sampler = sample_nb,
                            ...) {
  UseMethod("baselinenowcast")
}

#' @title Create a dataframe of nowcast results from a single reporting triangle
#'
#' @description This function ingests a single
#'    \code{\link{reporting_triangle}} object and generates a nowcast in the
#'    form of a \code{\link{baselinenowcast_df}} object. This function will by
#'    default estimate uncertainty using past retrospective nowcast errors and
#'    generate probabilistic nowcasts, which are samples from the predictive
#'    distribution of the estimated final case count at each reference date.
#'    This method specifically computes a nowcast for a single reporting
#'    triangle. See documentation for the arguments of this function which
#'    can be used to set the model specifications (things like number of
#'    reference times for delay and uncertainty estimation, the observation
#'    model, etc.).
#'
#' @param data \code{\link{reporting_triangle}} class object to be nowcasted.
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `data$reporting_triangle_matrix`. Default is NULL,
#'   which will estimate the delay from the reporting triangle matrix in `data`,
#'   See \code{\link{estimate_delay}} for more details.
#' @param uncertainty_params Vector of uncertainty parameters ordered from
#'   horizon 1 to the maximum horizon. Default is `NULL`, which will
#'   result in computing the uncertainty parameters from the reporting
#'   triangle matrix `data`. See \code{\link{estimate_uncertainty}} for more
#'   details.
#' @param ... Additional arguments passed to
#'    \code{\link{estimate_uncertainty}}
#'    and \code{\link{sample_nowcast}}.
#' @inheritParams baselinenowcast
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @importFrom rlang arg_match
#' @family baselinenowcast_df
#' @export
#' @method baselinenowcast reporting_triangle
#' @returns Data.frame of class \code{\link{baselinenowcast_df}}
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#' nowcast_df <- baselinenowcast(rep_tri)
#' nowcast_df
baselinenowcast.reporting_triangle <- function(
    data,
    scale_factor = 3,
    prop_delay = 0.5,
    output_type = c("samples", "point"),
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    delay_pmf = NULL,
    uncertainty_params = NULL,
    ...) {
  tri <- data$reporting_triangle_matrix
  output_type <- arg_match(output_type)
  assert_integerish(draws, null.ok = TRUE)

  tv <- allocate_reference_times(tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay
  )

  if (is.null(delay_pmf)) {
    delay_pmf <- estimate_delay(
      reporting_triangle = data$reporting_triangle_matrix,
      n = tv$n_history_delay
    )
  }
  # check for delay pmf being the right length/format
  .validate_delay(tri, delay_pmf)

  pt_nowcast <- apply_delay(tri, delay_pmf)

  if (output_type == "point") {
    nowcast_df <- data.frame(
      time = seq_len(nrow(pt_nowcast)),
      pred_count = rowSums(pt_nowcast)
    )
    nowcast_df$draw <- 1
    result_df <- new_baselinenowcast_df(nowcast_df,
      reference_dates = data$reference_dates,
      output_type = output_type
    )
    return(result_df)
  }

  if (is.null(uncertainty_params)) {
    nowcast_df <- estimate_and_apply_uncertainty(
      point_nowcast_matrix = pt_nowcast,
      reporting_triangle = tri,
      n_history_delay = tv$n_history_delay,
      n_retrospective_nowcasts = tv$n_retrospective_nowcasts,
      max_delay = ncol(tri) - 1,
      draws = draws,
      uncertainty_model = uncertainty_model,
      uncertainty_sampler = uncertainty_sampler,
      ...
    )
  } else {
    .validate_uncertainty(tri, uncertainty_params)
    nowcast_df <- sample_nowcasts(
      point_nowcast_matrix = pt_nowcast,
      reporting_triangle = tri,
      uncertainty_params = uncertainty_params,
      draws = draws,
      uncertainty_sampler = uncertainty_sampler,
      ...
    )
  }

  result_df <- new_baselinenowcast_df(nowcast_df,
    reference_dates = data$reference_dates,
    output_type = output_type
  )

  return(result_df)
}

#' @title Create a dataframe of nowcast results from a dataframe of cases
#'   indexed by reference date and report date
#'
#' @description This function ingests a data.frame with the number of incident
#'    cases indexed by reference date and report date for potentially multiple
#'    strata (e.g. age groups or locations) and returns a data.frame containing
#'    nowcasts by reference dates for each of the specified strata.
#'    For each strata, this function will by default estimate uncertainty using
#'    past retrospective nowcast errors and generate probabilistic nowcasts,
#'    which are samples from the predictive distribution of the estimated final
#'    case count at each reference date. See documentation for the arguments of
#'    this function which can be used to set the model specifications (things
#'    like number of reference times for delay and uncertainty estimation,
#'    the observation model, etc.). The function expects that each strata in
#'    the dataframe has the same maximum delay and the same number of reference
#'    dates.
#'
#' @param data Data.frame in a long tidy format with counts by reference date
#'    and report date for one or more strata. Must contain the following
#'    columns:
#' .    - Column of type `date` or character with the dates of
#'     the primary event occurrence (reference date).
#'    - Column of type `date` or character with the dates of
#'     report of the primary event (report_date).
#'    - Column of numeric or integer indicating the new confirmed counts
#'     pertaining to that reference and report date (count).
#'  Additional columns can be included, and the user can specify which columns
#'  set the unit of a single nowcast  ( i.e. the combination of columns that
#'  uniquely define a single nowcast with the `nowcast_unit` argument.).
#' @param nowcast_unit Vector of character strings indicting the names of the
#'   columns in `data` (after any renaming of columns) that denote the unit of
#'    a single nowcast.Within a nowcast unit, there can be no repeated unique
#'    combinations of reference dates and report dates. Default is `NULL` which
#'    assumes that all columns that are not required columns form the unit of
#'    a single forecast. This may lead to unexpected behavior, so setting the
#'    nowcast unit explicitly can help make the code easier to debug and easier
#'    to read. If specified, all columns that are not part of the forecast unit
#'    (or required columns) will be removed.
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay. Default is NULL,
#'   which will estimate the delay from the reporting triangle in each
#'   nowcast unit in `data`. See \code{\link{estimate_delay}} for more details.
#' @param uncertainty_params Vector of uncertainty parameters ordered from
#'   horizon 1 to the maximum horizon. Default is `NULL`, which will
#'   estimate the uncertainty parameters from the reporting triangle in each
#'   nowcast unit in `data`. See \code{\link{estimate_uncertainty}} for more
#'   details.
#' @param ... Additional arguments passed to
#'    \code{\link{estimate_uncertainty}}
#'    and \code{\link{sample_nowcast}}.
#' @inheritParams baselinenowcast
#' @inheritParams as_reporting_triangle.data.frame
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @family baselinenowcast_df
#' @export
#' @method baselinenowcast data.frame
#' @returns Data.frame of class \code{\link{baselinenowcast_df}}
#' @examples
#' nowcasts_df <- baselinenowcast(germany_covid_hosp,
#'   max_delay = 40,
#'   nowcast_unit = c("age_group", "location")
#' )
#' nowcasts_df
baselinenowcast.data.frame <- function(
    data,
    max_delay,
    delays_unit = "days",
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count",
    nowcast_unit = NULL,
    strata_sharing = NULL,
    scale_factor = 3,
    prop_delay = 0.5,
    output_type = "samples",
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    delay_pmf = NULL,
    uncertainty_params = NULL,
    ...) {
  assert_choice(strata_sharing,
    choices = c("delay", "uncertainty"),
    null.ok = TRUE
  )
  # Extract the additional columns not in the required columns
  if (is.null(nowcast_unit)) {
    nowcast_unit <- colnames(data)[!colnames(data) %in%
      c(
        {{ reference_date }},
        {{ report_date }},
        {{ count }}
      )]
  }

  # Split dataframe into a list of dataframes for each nowcast unit
  list_of_dfs <- split(data, data[nowcast_unit])
  # Make sure they are all the same training volume length and that
  # they have the required columns etc
  # .validate_list_of_dfs(list_of_dfs, max_delay, reference_date, report_date, count, delays_unit) #nolint

  # Get the training volume for all reporting triangles
  rep_tri1 <- as_reporting_triangle.data.frame(
    data = list_of_dfs[[1]],
    max_delay = max_delay,
    delays_unit = delays_unit,
    reference_date = reference_date,
    report_date = report_date,
    count = count
  )
  tv <- allocate_reference_times(
    reporting_triangle = rep_tri1$reporting_triangle_matrix,
    scale_factor = scale_factor,
    prop_delay = prop_delay
  )

  # Apply strata sharing if specified
  if (!is.null(strata_sharing)) {
    pooled_triangle <- combine_triangles(data)
    if ("delay" %in% strata_sharing) {
      # Estimate delay once on pooled data
      delay_pmf <- estimate_delay(
        reporting_triangle = pooled_triangle$reporting_triangle_matrix,
        n = tv$n_history_delay
      )
    }
    if ("uncertainty" %in% strata_sharing) {
      # Estimate uncertainty once on pooled data
      uncertainty_params <- estimate_uncertainty(
        reporting_triangle = pooled_triangle$reporting_triangle_matrix,
        n = tv$n_history_uncertainty
      )
    }
  }

  # Process each nowcast unit
  nowcasts_df <- data.frame()
  for (i in 1:length(list_of_dfs)) {
    rep_tri_df <- list_of_dfs[[i]]

    rep_tri <- as_reporting_triangle.data.frame(
      data = rep_tri_df,
      max_delay = max_delay,
      delays_unit = delays_unit,
      reference_date = reference_date,
      report_date = report_date,
      count = count
    )

    nowcast_df <- baselinenowcast.reporting_triangle(
      data = rep_tri,
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      output_type = output_type,
      draws = draws,
      uncertainty_model = uncertainty_model,
      uncertainty_sampler = uncertainty_sampler,
      delay_pmf = delay_pmf,
      uncertainty_params = uncertainty_params
    )
    # ,...)

    nowcast_df$strata <- names(list_of_dfs[1])

    # lapply version
    # split_result <- lapply(list_of_dfs, function(rep_tri_df) {
    #   rep_tri <- as_reporting_triangle.data.frame(
    #     data = rep_tri_df,
    #     max_delay = max_delay,
    #     delays_unit = delays_unit,
    #     reference_date = reference_date,
    #     report_date = report_date,
    #     count = count
    #   )
    #
    #   nowcast_df <- baselinenowcast.reporting_triangle(
    #     data = rep_tri,
    #     scale_factor = scale_factor,
    #     prop_delay = prop_delay,
    #     output_type = output_type,
    #     draws = draws,
    #     uncertainty_model = uncertainty_model,
    #     uncertainty_sampler = uncertainty_sampler,
    #     delay_pmf = delay_pmf,
    #     uncertainty_params = uncertainty_params,
    #     ...
    #   )
    #   return(nowcast_df)
    # })

    # Use purrr to bind output based on the names of the list
    nowcasts_df <- rbind(nowcasts_df, nowcast_df)
  }
  return(nowcasts_df)
}
