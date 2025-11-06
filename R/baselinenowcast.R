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
#'   The `data$reporting_triangle_matrix` must contain missing observations
#'   in the form of NAs in order to generate an output from this function.
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
      structure = data$structure,
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
#'    cases indexed by reference date and report date for one or multiple
#'    strata, which define the unit of a single nowcast (e.g. age groups or
#'    locations). It returns a data.frame containing nowcasts by reference
#'    date for each strata, which are by default estimated independently.
#'    This function will by default estimate uncertainty using
#'    past retrospective nowcast errors and generate probabilistic nowcasts,
#'    which are samples from the predictive distribution of the estimated final
#'    case count at each reference date. See documentation for the arguments of
#'    this function which can be used to set the model specifications (things
#'    like number of reference times for delay and uncertainty estimation,
#'    the observation model, etc.). The function expects that each strata in
#'    the dataframe has the same maximum delay. If sharing estimates across
#'    all strata, the shared estimates will be made using the shared set of
#'    reference and report dates across strata.
#'
#' @param data Data.frame in a long tidy format with counts by reference date
#'    and report date for one or more strata. Must contain the following
#'    columns:
#'     - `reference_date`: Column of type `Date` containing the dates
#'      of the primary event occurrence.
#'    - `report_date`: Column of type `Date` containing the dates of
#'     report of the primary event.
#'    - `count`: Column of numeric or integer indicating the new confirmed
#'     counts pertaining to that reference and report date.
#'  Additional columns indicating the columns which set the unit of a single
#'  can be included. The user can specify these columns with the
#'  `strata_cols` argument, otherwise it will be assumed that the `data`
#'  contains only data for a single strata.
#' @param strata_cols Vector of character strings indicating the names of the
#'   columns in `data` that determine how to stratify the data for nowcasting.
#'   The unique combinations of the entries in the `strata_cols` denote the
#'   unit of a single nowcast. Within a strata, there can be no repeated
#'   unique combinations of reference dates and report dates. Default is `NULL`
#'   which assumes that the data.frame being passed in represents a single
#'   strata (only one nowcast will be produced). All columns that are not
#'   part of the `strata_cols` will be removed.
#' @param strata_sharing Vector of character strings. Indicates if and what
#'   estimates should be shared for different nowcasting steps. Options are
#'   `"none"` for no sharing (each `strata_cols` is fully independent),
#'   `"delay"` for delay sharing and `"uncertainty"` for uncertainty sharing.
#'   Both `"delay"` and `"uncertainty"` can be passed at the same time.
#' @param ... Additional arguments passed to
#'    \code{\link{estimate_uncertainty}}
#'    and \code{\link{sample_nowcast}}.
#' @inheritParams baselinenowcast
#' @inheritParams as_reporting_triangle.data.frame
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @importFrom purrr set_names imap_dfr
#' @importFrom checkmate assert_subset assert_character assert_names
#'   assert_date
#' @family baselinenowcast_df
#' @export
#' @method baselinenowcast data.frame
#' @returns Data.frame of class \code{\link{baselinenowcast_df}}
#' @examples
#' covid_data_to_nowcast <- germany_covid19_hosp[
#'   germany_covid19_hosp$report_date <
#'     max(germany_covid19_hosp$reference_date),
#' ] # nolint
#' nowcasts_df <- baselinenowcast(covid_data_to_nowcast,
#'   max_delay = 40,
#'   strata_cols = c("age_group", "location")
#' )
#' nowcasts_df
baselinenowcast.data.frame <- function(
    data,
    scale_factor = 3,
    prop_delay = 0.5,
    output_type = c("samples", "point"),
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    max_delay,
    delays_unit = "days",
    strata_cols = NULL,
    strata_sharing = "none",
    ...) {
  output_type <- arg_match(output_type)
  assert_names(colnames(data),
    must.include = c("reference_date", "report_date", "count")
  )
  assert_character(delays_unit)
  assert_character(strata_sharing)
  assert_character(output_type)
  assert_subset(strata_sharing,
    choices = c("delay", "uncertainty", "none")
  )
  if (length(strata_sharing) == 2 && "none" %in% strata_sharing) {
    cli_abort(
      message = c("`strata_sharing` cannot be both 'none' and 'delay'/'uncertainty'") # nolint
    )
  }
  # Filter to max delay
  data$delay <- as.numeric(
    difftime(
      as.Date(data$report_date),
      as.Date(data$reference_date),
      units = delays_unit
    )
  )
  data_clean <- data[data$delay <= max_delay, ]

  .validate_strata_cols(
    strata_cols,
    data_clean
  )
  assert_date(data_clean$reference_date)
  assert_date(data_clean$report_date)

  # Split dataframe into a list of dataframes for each nowcast unit
  if (length(strata_cols) != 0) {
    data[strata_cols] <- lapply(data_clean[strata_cols], as.factor)
    list_of_dfs <- split(data_clean, data_clean[strata_cols],
      sep = "___",
      drop = TRUE
    )
  } else {
    list_of_dfs <- list(data_clean)
  }

  # Create a list of reporting triangles
  list_of_rep_tris <- lapply(list_of_dfs,
    as_reporting_triangle,
    max_delay = max_delay,
    delays_unit = delays_unit
  )
  # Combine if needed
  shared_delay_pmf <- NULL
  shared_uncertainty_params <- NULL
  if (all(strata_sharing != "none")) {
    pooled_df <- .combine_triangle_dfs(
      data = data_clean,
      strata_cols = strata_cols
    )
    pooled_triangle <- as_reporting_triangle(pooled_df,
      max_delay = max_delay
    )
    # Get the training volume for all reporting triangles
    tv <- allocate_reference_times(
      reporting_triangle = pooled_triangle$reporting_triangle_matrix,
      scale_factor = scale_factor,
      prop_delay = prop_delay
    )
    if ("delay" %in% strata_sharing) {
      # Estimate delay once on pooled data
      shared_delay_pmf <- estimate_delay(
        reporting_triangle = pooled_triangle$reporting_triangle_matrix,
        n = tv$n_history_delay
      )
    }
    if ("uncertainty" %in% strata_sharing) {
      # Estimate uncertainty once on pooled data
      trunc_rep_tris <- truncate_triangles(
        reporting_triangle = pooled_triangle$reporting_triangle_matrix,
        n = tv$n_retrospective_nowcasts
      )
      retro_rep_tris <- construct_triangles(
        trunc_rep_tris,
        structure = pooled_triangle$structure
      )
      retro_pt_nowcasts <- fill_triangles(retro_rep_tris,
        n = tv$n_history_delay
      )
      shared_uncertainty_params <- estimate_uncertainty(
        point_nowcast_matrices = retro_pt_nowcasts,
        truncated_reporting_triangles = trunc_rep_tris,
        retro_reporting_triangles = retro_rep_tris,
        n = tv$n_retrospective_nowcasts,
        uncertainty_model = uncertainty_model,
        ...
      )
    }
  }

  # Nowcast
  combined_result <- imap_dfr(
    list_of_rep_tris,
    \(rep_tri, name) {
      .nowcast_from_rep_tris(
        rep_tri = rep_tri,
        name = name,
        strata_cols = strata_cols,
        scale_factor = scale_factor,
        prop_delay = prop_delay,
        output_type = output_type,
        draws = draws,
        uncertainty_model = uncertainty_model,
        uncertainty_sampler = uncertainty_sampler,
        delay_pmf = shared_delay_pmf,
        uncertainty_params = shared_uncertainty_params
      )
    }
  )
  return(combined_result)
}

#' Produce a nowcast from a named reporting triangle object
#'
#' @param rep_tri Object of reporting triangle class
#' @param name Name of the reporting triangle class object
#' @inheritParams baselinenowcast
#' @inheritParams baselinenowcast.data.frame
#' @inheritParams baselinenowcast.reporting_triangle
#' @inheritParams as_reporting_triangle.data.frame
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @keywords internal
#' @returns Data.frame of nowcasts with columns obtained from the name of the
#'   original list and the nowcast unit.
.nowcast_from_rep_tris <- function(
    rep_tri,
    name,
    strata_cols,
    scale_factor,
    prop_delay,
    output_type,
    draws,
    uncertainty_model,
    uncertainty_sampler,
    delay_pmf,
    uncertainty_params,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE)) {
  nowcast_df <- baselinenowcast(
    data = rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    output_type = output_type,
    draws = draws,
    uncertainty_model = uncertainty_model,
    uncertainty_sampler = uncertainty_sampler,
    delay_pmf = delay_pmf,
    uncertainty_params = uncertainty_params,
    ref_time_aggregator = ref_time_aggregator,
    delay_aggregator = delay_aggregator
  )

  # Split the name of the element in the last and add as a separate column
  # based on nowcast unit entry
  if (length(strata_cols) != 0) {
    split_name <- strsplit(name, "___", fixed = TRUE)[[1]]
    nowcast_df[strata_cols] <- as.list(split_name)
  }

  return(nowcast_df)
}
