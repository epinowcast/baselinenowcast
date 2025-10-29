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
#' @param nowcast_unit Vector of character strings indicating the names of the
#'   columns in `data` that denote the unit of a single nowcast. Within a
#'   nowcast unit, there can be no repeated unique combinations of reference
#'   dates and report dates. Default is `NULL` which assumes that all columns
#'   that are not required columns (reference date, report date, count) form
#'   the unit of a single forecast. This may lead to unexpected behaviour, so
#'   setting the nowcast unit explicitly can help make the code easier to debug
#'   and easier to read. If specified, all columns that are not part of the
#'   forecast unit (or required columns) will be removed.
#' @param strata_sharing Vector of character strings indicating the estimand
#'   for which estimates that are "borrowed" from pooled estimates across all
#'   strata should be used. Options are `"delay"` and/or `"uncertainty"`. NULL
#'   indicates that delay and uncertainty estimates should be computed for
#'   each `nowcast_unit` independently.
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
#' @importFrom purrr imap list_rbind
#' @importFrom checkmate assert_subset
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
#'   nowcast_unit = c("age_group", "location")
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
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count",
    nowcast_unit = NULL,
    strata_sharing = NULL,
    delay_pmf = NULL,
    uncertainty_params = NULL,
    ...) {
  .validate_nowcast_unit(
    nowcast_unit,
    data,
    reference_date,
    report_date,
    count
  )
  # Extract the additional columns not in the required columns
  if (is.null(nowcast_unit)) {
    nowcast_unit <- colnames(data)[!colnames(data) %in%
      c(reference_date, report_date, count)]
  }

  # Split dataframe into a list of dataframes for each nowcast unit
  if (length(nowcast_unit) != 0) {
    list_of_dfs <- split(data, interaction(data[nowcast_unit],
      sep = "___",
      drop = TRUE
    ))
  } else {
    list_of_dfs <- list(data)
  }

  # Apply strata sharing if specified
  if (!is.null(strata_sharing)) {
    assert_subset(strata_sharing,
      choices = c("delay", "uncertainty"),
      empty.ok = TRUE
    )
    pooled_df <- combine_triangle_dfs(
      data = data,
      reference_date = reference_date,
      report_date = report_date,
      count = count
    )
    pooled_triangle <- as_reporting_triangle(pooled_df,
      max_delay = max_delay,
      reference_date = reference_date,
      report_date = report_date,
      count = count
    )
    # Get the training volume for all reporting triangles
    tv <- allocate_reference_times(
      reporting_triangle = pooled_triangle$reporting_triangle_matrix,
      scale_factor = scale_factor,
      prop_delay = prop_delay
    )
    if ("delay" %in% strata_sharing) {
      # Estimate delay once on pooled data
      delay_pmf <- estimate_delay(
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
      retro_rep_tris <- construct_triangles(trunc_rep_tris)
      retro_pt_nowcasts <- fill_triangles(retro_rep_tris,
        max_delay = max_delay,
        n = tv$n_history_delay
      )
      uncertainty_params <- estimate_uncertainty(
        point_nowcast_matrices = retro_pt_nowcasts,
        truncated_reporting_triangles = trunc_rep_tris,
        retro_reporting_triangles = retro_rep_tris,
        n = tv$n_retrospective_nowcasts,
        uncertainty_model = uncertainty_model,
        ...
      )
    }
  }

  combined_result <- imap(list_of_dfs, function(rep_tri_df, name) {
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
      scale_factor = 2 * scale_factor,
      prop_delay = prop_delay,
      output_type = output_type,
      draws = draws,
      uncertainty_model = uncertainty_model,
      uncertainty_sampler = uncertainty_sampler,
      delay_pmf = delay_pmf,
      uncertainty_params = uncertainty_params,
      ...
    )

    # Split the name of the element in the last and add as a separate column
    # based on nowcast unit entry
    if (length(nowcast_unit) != 0) {
      for (i in seq_along(nowcast_unit)) {
        split_name <- strsplit(name, "___", fixed = TRUE)[[1]]
        nowcast_df[[nowcast_unit[i]]] <- split_name[i]
      }
    }

    return(nowcast_df)
  }) |> list_rbind()

  return(combined_result)
}


#' Combine triangle data.frames
#'
#' @description This function ingests a dataframe with case counts indexed by
#' reference dates and report dates for multiple strata and sums all the case
#' counts across the shared set of reference and report dates. It requires that
#' the strata have some overlapping set of reference and report dates,
#' otherwise it will return an error to indicate to the user that strata
#' sharing with the set of strata passed in is not possible. If overlapping
#' sets of reference and report dates are input, it returns a data.frame with
#' a single set of counts for all the reference and report date combinations
#' present in all strata in the original data. Note that this may be a subset
#' of the unique set of reference and report date combinations in the original
#' data.
#'
#' @inheritParams baselinenowcast.data.frame
#' @inheritParams as_reporting_triangle.data.frame
#'
#' @returns `result` Data.frame with the same column names for reference date,
#' report date, and case count as in `data` but summed across all strata in
#' the original data.
#' @export
#' @importFrom stats aggregate as.formula
#' @examples
#' example_data <- data.frame(
#'   ref_date = as.Date(c("2021-04-06", "2021-04-06", "2021-04-06", "2021-04-06")), # nolint
#'   rep_date = as.Date(c("2021-04-08", "2021-04-08", "2021-04-10", "2021-04-10")), # nolint
#'   location = c("DE", "FR", "DE", "FR"),
#'   age_group = c("00+", "00+", "00+", "00+"),
#'   cases = c(50, 30, 20, 40)
#' )
#' example_data
#' combined <- combine_triangle_dfs(
#'   data = example_data,
#'   reference_date = "ref_date",
#'   report_date = "rep_date",
#'   count = "cases"
#' )
#' combined
combine_triangle_dfs <- function(data,
                                 reference_date = "reference_date",
                                 report_date = "report_date",
                                 count = "count") {
  group_cols <- c(reference_date, report_date)
  strata_cols <- setdiff(names(data), c(group_cols, count))
  value_col <- count

  if (length(strata_cols) == 0) {
    # If no strata columns, create stratum_id based on unique date combinations
    data$stratum_id <- as.integer(factor(
      paste(data[[reference_date]], data[[report_date]], sep = "___")
    ))
  } else {
    # Create a unique identifier for each stratum
    data$stratum_id <- do.call(paste, c(data[strata_cols], sep = "_"))
  }

  n_strata <- length(unique(data$stratum_id))

  date_strata <- unique(data[, c(group_cols, "stratum_id")])
  date_counts <- aggregate(
    stratum_id ~ .,
    data = date_strata[, c(group_cols, "stratum_id")],
    FUN = function(x) length(unique(x))
  )

  names(date_counts)[ncol(date_counts)] <- "n_strata"

  common_dates <- date_counts[date_counts$n_strata == n_strata, group_cols]

  if (nrow(common_dates) == 0) {
    cli_abort(
      message = "There is no overlapping set of reference and report dates across all strata. `strata_sharing` is not possible" # nolint
    )
  }

  if (!all(date_counts$n_strata == n_strata)) {
    cli_warn(
      message = c("Not all reference dates and report dates combinations are available for all strata.", # nolint
        "i" = "Only the subset of reference and report dates that are available for all strata will be used to aggregate cases." # nolint
      )
    )
  }

  filtered_data <- merge(data, common_dates, by = group_cols)
  filtered_data$stratum_id <- NULL

  formula_str <- paste(value_col, "~", paste(group_cols, collapse = " + "))
  formula_obj <- as.formula(formula_str)

  result <- aggregate(
    formula_obj,
    data = filtered_data,
    FUN = sum,
    na.rm = TRUE
  )
  return(result)
}
