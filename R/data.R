#' Synthetic data containing daily case counts by reference and report date
#'
#' @description A synthetic dataset containing the number of incident cases
#'   indexed by reference date and report date. While data of this form could
#'   be from any source, this data is meant to represent the output of
#'   pre-processing the [syn_nssp_line_list] dataset, which is a synthetic
#'   patient-level line list data from the United States National Syndromic
#'   Surveillance System (NSSP).
#'
#' @format A data.frame with 3795 rows and 3 columns.
#' \describe{
#'   \item{reference_date}{Date the primary event occurred (e.g. date of
#'    hospital admissions, specimen collection date, symptom onset), formatted
#'    in ISO8601 standards as YYYY-MM-DD.}
#'   \item{report_date}{Date the event was reported into the surveillance
#'   system, formatted as YYYY-MM-DD.}
#'   \item{count}{Number of incident events (e.g. cases) occurring on the
#'   specified reference date and reported on the report date.}
#' }
#' @source Created for package demonstration, made to look like the output after
#'   preprocessing the line-list data to obtain the number of incidence cases
#'   of a specific syndromic surveillance definition, indexed by the date of
#'   admission (reference date) and the date of the diagnoses being reported to
#'   the surveillance system (report date) (e.g. a reporting triangle in
#'   long format).
#' @family example_data
"syn_nssp_df"

#' A synthetic dataset resembling line-list (each row is a patient) data from
#'   the United States' National Syndromic Surveillance System (NSSP)
#'   accessed via the Essence platform. All entries are synthetic, formatted to
#'   look as close to the real raw data as possible.
#'
#'   For an example of how to produce a nowcast from this data, see
#'   `vignette("nssp_nowcast")`.
#'
#' @format A data.frame with 25 rows and 8 columns.
#' \describe{
#'   \item{C_Processed_BioSenseID}{Unique identifier for each patient}
#'   \item{CCDDParsed}{Character string indicating primary symptoms and
#'   corresponding diagnoses codes.}
#'   \item{DischargeDiagnosisMDTUpdates}{Character string formatted as a
#'   dictionary with indices and corresponding time stamps formatted as
#'   YYYY-MM-DD HH:MM:SS.}
#'   \item{DischargeDiagnosisUpdates}{Character string formatted as a
#'   dictionary with indices and corresponding diagnoses codes pertaining to
#'   this diagnosis associated with that event.}
#'   \item{HasBeenAdmitted}{Numeric indicating whether the patient was
#'   admitted (0 for no, 1 for admission).}
#'   \item{C_Visit_Date_Time}{Date-time indicating the time stamp of the
#'   the patient registering in the emergency department, in
#'   YYYY-MM-DD HH:MM:SS format.}
#'   \item{c_race}{Character string indicating the race/ethnicity of the
#'   patient.}
#'   \item{sex}{Character string indicating the sex of the patient.}
#' }
#' @source Created for package demonstration to provide an example of how to
#'    pre-process this dataset to obtain a reporting triangle. This is made to
#'    look like the data that one would pull directly an API to access
#'    patient-level line-list data.
#' @family example_data
"syn_nssp_line_list"

#' Simple example reporting triangle for demonstrations
#'
#' @description A basic [reporting_triangle] object demonstrating standard
#'   structure with complete early reference times and progressively incomplete
#'   recent times. Useful for simple examples and tests.
#'
#' @format A [reporting_triangle] object with 5 reference dates and 4 delays:
#' \describe{
#'   \item{reporting_triangle_matrix}{5x4 matrix with counts}
#'   \item{reference_dates}{5 dates starting from 2024-01-01}
#'   \item{delays_unit}{"days"}
#' }
#'
#' @details
#' This is a simple, clean example without complications like negative values
#' or unusual structures. Ideal for:
#' - Package examples demonstrating basic functionality
#' - Unit tests for standard cases
#' - Vignettes introducing nowcasting concepts
#'
#' Use [example_downward_corr_rt] for examples with data quality corrections.
#'
#' @seealso
#' - [example_downward_corr_rt] for downward corrections example
#' - [as_reporting_triangle()] to create reporting triangles
#'
#' @family example_data
#' @examples
#' # View the example triangle
#' example_reporting_triangle
#'
#' # Use in nowcasting - requires complete rows for delay estimation
#' estimate_delay(example_reporting_triangle, n = 6)
"example_reporting_triangle"

#' Example reporting triangle with downward corrections
#'
#' @description A [reporting_triangle] object demonstrating how to handle
#'   systematic downward corrections in reporting data.
#'   This represents a realistic case where data quality reviews at delay 2
#'   consistently identify false positives or reclassify cases, producing
#'   negative values at that specific delay.
#'
#' @format A [reporting_triangle] object with 8 reference dates and 4 delays:
#' \describe{
#'   \item{reporting_triangle_matrix}{8x4 matrix with negative values at
#'     delay 2}
#'   \item{reference_dates}{8 dates starting from 2024-01-01}
#'   \item{delays_unit}{"days"}
#' }
#'
#' @details
#' Use this example to understand:
#' - How to work with negative corrections in delay distributions
#' - The impact of preprocessing negative values on delay estimation
#' - How PMFs and CDFs behave with systematic downward corrections
#'
#' @seealso
#' - [example_reporting_triangle] for a clean example without corrections
#' - [baselinenowcast.reporting_triangle()] specifically the `preprocess`
#'   argument for a description of how to remove negative values if desired.
#'
#' @family example_data
#' @examples
#' # View the example triangle with downward corrections
#' example_downward_corr_rt
#'
#' # Estimate delay with and without preprocessing
#' delay_raw <- estimate_delay(example_downward_corr_rt, n = 5)
#' delay_processed <- estimate_delay(
#'   preprocess_negative_values(example_downward_corr_rt),
#'   n = 5
#' )
#'
#' # Compare the resulting PMFs
#' delay_raw
#' delay_processed
"example_downward_corr_rt"

#' Weekly influenza hospital admissions in Montana with downward revisions
#'
#' @description Weekly confirmed influenza hospital admissions in Montana,
#'   United States, reported to the National Healthcare Safety Network (NHSN)
#'   during the 2024-2025 season, indexed by reference week and report week.
#'   Montana has a small population, so counts are low and previously reported
#'   counts are sometimes revised downwards. These downward revisions appear as
#'   negative values in `count`.
#'
#'   For an example of how to nowcast this data while retaining the downward
#'   revisions, see the
#'   [downward revisions article](https://baselinenowcast.epinowcast.org/articles/nhsn_downward_revisions.html). # nolint
#'
#' @format A data.frame with 952 rows and 3 columns.
#' \describe{
#'   \item{reference_date}{Date of the Saturday ending the week of hospital
#'   admission, formatted as YYYY-MM-DD. Ranges from 2024-11-16 to
#'   2025-08-30.}
#'   \item{report_date}{Date of the Saturday ending the week of the NHSN
#'   release in which the counts were reported, formatted as YYYY-MM-DD.
#'   Counts are first reported in the week after the reference week.}
#'   \item{count}{Change in the number of admissions for the reference week
#'   between the release on `report_date` and the previous release. Negative
#'   values are downward revisions.}
#' }
#' @details
#' Counts were derived from the cumulative counts in each weekly release of
#'   the preliminary NHSN Hospital Respiratory Data (the
#'   `confirmed_admissions_flu_ew_prelim` signal), as archived by the Delphi
#'   Epidata API, by taking the difference between successive releases for
#'   each reference week. The archive starts with the release in the week
#'   ending 2024-11-23, so only reference weeks with complete release
#'   histories are included. See `data-raw/nhsn_mt_flu.R` in the package
#'   source for the code used to create the dataset.
#' @source NHSN Hospital Respiratory Data, accessed via the Delphi Epidata API
#'   (\url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/nhsn.html}). # nolint
#' @family example_data
#' @examples
#' # Downward revisions
#' nhsn_mt_flu_df[nhsn_mt_flu_df$count < 0, ]
#'
#' # Reporting triangle retaining the downward revisions
#' rep_tri <- as_reporting_triangle(
#'   nhsn_mt_flu_df[nhsn_mt_flu_df$report_date <= "2025-04-05", ],
#'   delays_unit = "weeks"
#' ) |>
#'   truncate_to_delay(max_delay = 8)
#' rep_tri
"nhsn_mt_flu_df"

#' Weekly revisions to influenza ED-visit percentages in Montana
#'
#' @description Percentages of emergency department (ED) visits with an
#'   influenza discharge diagnosis in Montana, reported by the National
#'   Syndromic Surveillance Program (NSSP) during the 2024-2025 season.
#'   Successive published percentages are differenced into percentage-point
#'   increments. Negative increments represent downward revisions, which can
#'   arise from changes to the number of influenza visits or total ED visits.
#'
#' @format A data.frame with 1,283 rows and 3 columns.
#' \describe{
#'   \item{reference_date}{Saturday ending the week of ED visits.}
#'   \item{report_date}{Saturday ending the week of publication. This is a
#'     weekly index derived from the archive issue week, not the exact
#'     publication date.}
#'   \item{count}{Numeric change in the published percentage, in percentage
#'     points, including the initial report. Summing successive increments
#'     for a reference week reconstructs its published percentage.}
#' }
#' @details The unsmoothed `pct_ed_visits_influenza` signal is extracted for
#'   reference weeks 202444-202535 and issue weeks 202445-202545. The first
#'   reference week ends on 2024-11-02 and the last on 2025-08-30. The dataset
#'   retains fractional increments and downward revisions. Use
#'   `preprocess = NULL` and a continuous observation model such as
#'   [fit_normal()] with [sample_normal()] when nowcasting these data.
#'   The `count` column contains percentage-point increments, not case counts
#'   or percentages to be averaged across report dates. See
#'   `data-raw/nssp_mt_flu_pct.R` for extraction and reconstruction checks.
#' @source NSSP ED-visit percentages, accessed via the Delphi Epidata API
#'   (\url{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/nssp.html}). # nolint
#' @family example_data
#' @examples
#' # Downward revisions in percentage points
#' nssp_mt_flu_pct_df[nssp_mt_flu_pct_df$count < 0, ]
"nssp_mt_flu_pct_df"

#' Incident COVID-19 hospitalisations indexed by the date of positive test
#'   (reference date) and report date from Germany in 2021 and 2022.
#'
#' @format A data.frame with 140,630 rows and 6 columns.
#' \describe{
#'   \item{reference_date}{Date of first positive COVID-19 test formatted
#'    in ISO8601 standards as YYYY-MM-DD.}
#'   \item{location}{Character string indicating the location of the case
#'   counts}
#'   \item{age_group}{Character string indicating the age group of the case
#'   counts.}
#'   \item{delay}{Integer specifying the delay, in days, between the reference
#'   date and the report date}
#'   \item{count}{Integer indicating the number of cases indexed by reference
#'   and report date.}
#'   \item{report_date}{Date of case report, formatted in ISO8601 standards as
#'    YYYY-MM-DD.}
#' }
#' @source This data comes directly from the preprocessed data in the
#'   German COVID-19 Nowcast Hub from \url{https://github.com/KITmetricslab/hospitalization-nowcast-hub/blob/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv}. #nolint
#'   It contains incident case counts by age group in Germany.
#' @family example_data
#' @keywords internal
"covid19_data"

#' Incident COVID-19 hospitalisations indexed by the date of positive test
#'   (reference date) and report date from Germany in 2021 and 2022.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @format A data.frame with 140,630 rows and 6 columns.
#' \describe{
#'   \item{reference_date}{Date of first positive COVID-19 test formatted
#'    in ISO8601 standards as YYYY-MM-DD.}
#'   \item{location}{Character string indicating the location of the case
#'   counts}
#'   \item{age_group}{Character string indicating the age group of the case
#'   counts.}
#'   \item{delay}{Integer specifying the delay, in days, between the reference
#'   date and the report date}
#'   \item{count}{Integer indicating the number of cases indexed by reference
#'   and report date.}
#'   \item{report_date}{Date of case report, formatted in ISO8601 standards as
#'    YYYY-MM-DD.}
#' }
#' @source This data comes directly from the preprocessed data in the
#'   German COVID-19 Nowcast Hub from \url{https://github.com/KITmetricslab/hospitalization-nowcast-hub/blob/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv}. #nolint
#'   It contains incident case counts by age group in Germany.
#' @family example_data
"germany_covid19_hosp"
