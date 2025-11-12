#' @title Generate a nowcast
#'
#' @description This function ingests data to be nowcasted and generates a
#'   a [baselinenowcast_df] which contains a probabilistic or point
#'   estimate of the final case counts at each reference date in the `data`.
#'   See [baselinenowcast.reporting_triangle()] for details on the
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
#' @returns Data.frame of class [baselinenowcast_df]
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
#'  [reporting_triangle] object and generates a nowcast in the
#'  form of a [baselinenowcast_df] object.
#'
#'  This function implements a nowcasting workflow for a single reporting
#'  triangle:
#'  \enumerate{
#'      \item [estimate_delay()] - Estimate a reporting delay PMF
#'      \item [apply_delay()] - Generate a point nowcast using the delay PMF
#'      \item [estimate_and_apply_uncertainty()] - Generate a probabilistic
#'       nowcast from a point nowcast and reporting triangle
#' }
#'
#'    This function will by default estimate the delay from the
#'    [reporting_triangle] and estimate uncertainty using past
#'    retrospective nowcast errors on that [reporting_triangle] to
#'    generate probabilistic nowcasts, which
#'    are samples from the predictive distribution of the estimated final case
#'    count at each reference date.
#'    Alternatives include passing in a separate `delay_pmf` or
#'    `uncertainty_params`.
#'    This method specifically computes a nowcast for a single reporting
#'    triangle. See documentation for the arguments of this function which
#'    can be used to set the model specifications (things like number of
#'    reference times for delay and uncertainty estimation, the observation
#'    model, etc.).
#'
#' @param data [reporting_triangle] class object to be nowcasted.
#'   The matrix must contain missing observations in the form of NAs in order
#'   to generate an output from this function.
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in the reporting triangle. Default is NULL, which will
#'   estimate the delay from the reporting triangle in `data`. See
#'   [estimate_delay()] for more details.
#' @param uncertainty_params Vector of uncertainty parameters ordered from
#'   horizon 1 to the maximum horizon. Default is `NULL`, which will result in
#'   computing the uncertainty parameters from the reporting triangle `data`.
#'   See [estimate_uncertainty()] for more details.
#' @param ... Additional arguments passed to
#'    [estimate_uncertainty()]
#'    and [sample_nowcast()].
#' @inheritParams baselinenowcast
#' @inheritParams estimate_delay
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
#'   data = data_as_of_df
#' ) |>
#'   truncate_to_delay(max_delay = 25)
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
    preprocess = preprocess_negative_values,
    ...) {
  output_type <- arg_match(output_type)
  assert_integerish(draws, null.ok = TRUE)

  reference_dates <- get_reference_dates(data)

  tv <- allocate_reference_times(data,
    scale_factor = scale_factor,
    prop_delay = prop_delay
  )

  if (is.null(delay_pmf)) {
    delay_pmf <- estimate_delay(
      reporting_triangle = data,
      n = tv$n_history_delay,
      preprocess = preprocess
    )
  }
  # check for delay pmf being the right length/format
  .validate_delay(data, delay_pmf)

  pt_nowcast <- apply_delay(data, delay_pmf)

  if (output_type == "point") {
    nowcast_df <- data.frame(
      time = seq_len(nrow(pt_nowcast)),
      pred_count = rowSums(pt_nowcast)
    )
    nowcast_df$draw <- 1
    result_df <- new_baselinenowcast_df(nowcast_df,
      reference_dates = reference_dates,
      output_type = output_type
    )
    return(result_df)
  }

  if (is.null(uncertainty_params)) {
    uncertainty_params <- estimate_uncertainty_retro(
      reporting_triangle = data,
      n_history_delay = tv$n_history_delay,
      n_retrospective_nowcasts = tv$n_retrospective_nowcasts,
      uncertainty_model = uncertainty_model
    )
  }
  .validate_uncertainty(data, uncertainty_params)
  nowcast_df <- sample_nowcasts(
    point_nowcast_matrix = pt_nowcast,
    reporting_triangle = data,
    uncertainty_params = uncertainty_params,
    draws = draws,
    uncertainty_sampler = uncertainty_sampler,
    ...
  )

  result_df <- new_baselinenowcast_df(nowcast_df,
    reference_dates = reference_dates,
    output_type = output_type
  )

  return(result_df)
}

#' @title Create a dataframe of nowcast results from a dataframe of cases
#'   indexed by reference date and report date
#'
#' @description This function ingests a data.frame with the number of incident
#'  cases indexed by reference date and report date for one or multiple
#'  strata, which define the unit of a single nowcast (e.g. age groups or
#'  locations). It returns a data.frame containing nowcasts by reference
#'  date for each strata, which are by default estimated independently.
#'  This function will by default estimate uncertainty using
#'  past retrospective nowcast errors and generate probabilistic nowcasts,
#'  which are samples from the predictive distribution of the estimated final
#'  case count at each reference date.
#'
#'  This function implements the full nowcasting workflow on multiple reporting
#'  triangles, generating estimates of the delay and uncertainty parameters
#'  for all strata using estimates from across strata if specified.
#'  \enumerate{
#'      \item [estimate_delay()] - Estimate a delay PMF across strata if
#'      `strata_sharing` contains `"delay"`
#'      \item [estimate_uncertainty_retro()] - Estimates uncertainty parameters
#'      across strata if `strata_sharing` contains `"uncertainty"`
#'      \item [as_reporting_triangle()] - Generates a reporting triangle object
#'      from a data.frame
#'      \item [baselinenowcast.reporting_triangle()] - Generates point or
#'      probabilistic nowcasts depending on `output_type` for each strata.
#' }
#'
#'  @detail See documentation for the arguments of this function which can be
#'  used to set the model specifications (things like number of reference times
#'  for delay and uncertainty estimation, the observation model, etc.).
#'  The function expects that each strata in
#'  the dataframe has the same maximum delay. If sharing estimates across
#'  all strata, the shared estimates will be made using the shared set of
#'  reference and report dates across strata.
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
#' @param max_delay Maximum delay (in units of `delays_unit`) to include in the
#'   nowcast. If NULL (default), all delays in the data are used. If specified,
#'   only observations with delay <= max_delay are included.
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
#'    [estimate_uncertainty()]
#'    and [sample_nowcast()].
#' @inheritParams baselinenowcast
#' @inheritParams as_reporting_triangle.data.frame
#' @inheritParams estimate_uncertainty
#' @inheritParams estimate_delay
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @importFrom purrr set_names map_dfr
#' @importFrom checkmate assert_subset assert_character assert_names
#'   assert_date
#' @family baselinenowcast_df
#' @export
#' @method baselinenowcast data.frame
#' @returns Data.frame of class \code{\link{baselinenowcast_df}}
#' @examples
#' # Filter data to exclude most recent report dates
#' covid_data_to_nowcast <- germany_covid19_hosp[
#'   germany_covid19_hosp$report_date <
#'     max(germany_covid19_hosp$reference_date),
#' ]
#' # Use max_delay parameter to limit delays included
#' nowcasts_df <- baselinenowcast(covid_data_to_nowcast,
#'   max_delay = 40,
#'   strata_cols = c("age_group", "location")
#' )
#' nowcasts_df
baselinenowcast.data.frame <- function(
    data,
    max_delay = NULL,
    scale_factor = 3,
    prop_delay = 0.5,
    output_type = c("samples", "point"),
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    delays_unit = "days",
    strata_cols = NULL,
    strata_sharing = "none",
    preprocess = preprocess_negative_values,
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
  # Compute delays for later use
  data$delay <- as.numeric(
    difftime(
      as.Date(data$report_date),
      as.Date(data$reference_date),
      units = delays_unit
    )
  )

  # Filter by max_delay if specified
  if (!is.null(max_delay)) {
    data_clean <- data[data$delay <= max_delay, ]
  } else {
    data_clean <- data
  }

  .validate_strata_cols(
    strata_cols,
    data_clean
  )
  assert_date(data_clean$reference_date)
  assert_date(data_clean$report_date)

  # Split dataframe into a list of dataframes for each strata
  list_of_dfs <- .split_df_by_cols(
    long_df = data_clean,
    col_names = strata_cols
  )

  # Create a list of reporting triangles
  list_of_rep_tris <- lapply(list_of_dfs,
    as_reporting_triangle,
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
    pooled_triangle <- as_reporting_triangle(pooled_df)
    # Get the training volume for all reporting triangles
    tv <- allocate_reference_times(
      reporting_triangle = pooled_triangle,
      scale_factor = scale_factor,
      prop_delay = prop_delay
    )
    if ("delay" %in% strata_sharing) {
      # Estimate delay once on pooled data
      shared_delay_pmf <- estimate_delay(
        reporting_triangle = pooled_triangle,
        n = tv$n_history_delay,
        preprocess = preprocess
      )
    }
    if ("uncertainty" %in% strata_sharing) {
      # Estimate uncertainty once on pooled data
      shared_uncertainty_params <- estimate_uncertainty_retro(
        reporting_triangle = pooled_triangle,
        n_history_delay = tv$n_history_delay,
        n_retrospective_nowcasts = tv$n_retrospective_nowcasts,
        uncertainty_model = uncertainty_model,
        preprocess = preprocess
      )
    }
  }

  # Nowcast on each reporting triangle and bind into a long data.frame
  combined_result <- map_dfr(
    list_of_rep_tris,
    # nolint start: brace_linter, unnecessary_nesting_linter
    \(rep_tri){
      baselinenowcast(
        data = rep_tri,
        scale_factor = scale_factor,
        prop_delay = prop_delay,
        output_type = output_type,
        draws = draws,
        uncertainty_model = uncertainty_model,
        uncertainty_sampler = uncertainty_sampler,
        delay_pmf = shared_delay_pmf,
        uncertainty_params = shared_uncertainty_params,
        preprocess = preprocess
      )
    }, # nolint end
    .id = "name"
  )

  # Split the `name` column and assign to columns by strata cols
  split_data <- do.call(
    rbind,
    strsplit(combined_result$name, "___", fixed = TRUE)
  )
  for (i in seq_along(strata_cols)) {
    combined_result[[strata_cols[i]]] <- split_data[, i]
  }
  combined_result$name <- NULL
  return(combined_result)
}

#' Split dataframe into a list of dataframes by the entries in the specified
#'   columns
#'
#' @param long_df Data.frame to be split into a list of dataframes.
#' @param col_names Character string indicating the column names to be used
#'   to create the new data.frames.
#'
#' @returns List of data.frames named by the concatenated entries in col_names
#' @keywords internal
.split_df_by_cols <- function(long_df,
                              col_names) {
  if (length(col_names) != 0) {
    long_df[col_names] <- lapply(long_df[col_names], as.factor)
    list_of_dfs <- split(long_df, long_df[col_names],
      sep = "___",
      drop = TRUE
    )
  } else {
    list_of_dfs <- list(long_df)
  }
  return(list_of_dfs)
}
