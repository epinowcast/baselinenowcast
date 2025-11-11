#' A synthetic dataset containing the number of incident cases indexed by
#'   reference date and report date. While data of this form could be from any
#'   source, this data is meant to represent the output of pre-processing the
#'   [syn_nssp_line_list] dataset, which is a synthetic patient-level line list
#'   data from the United State's National Syndromic Surveillance System (NSSP).
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
#' # Use in nowcasting
#' estimate_delay(example_reporting_triangle, n = 3)
"example_reporting_triangle"

#' Example reporting triangle with downward corrections (matrix form)
#'
#' @description A synthetic matrix demonstrating downward corrections at a
#'   specific delay.
#'   This represents a realistic case where data quality reviews at delay 2
#'   consistently identify false positives or reclassify cases, resulting in
#'   net downward adjustments that produce negative values.
#'
#'   When estimated with `preprocess = NULL`, this produces a PMF with
#'   negative entries and a CDF that is not strictly increasing, reflecting
#'   the downward correction process.
#'
#' @format A matrix with 8 rows and 4 columns.
#' \describe{
#'   Rows represent reference dates (time points when events occurred).
#'   Columns represent reporting delays (0 to 3 days).
#'   Values represent counts, with negative values at delay 2 representing
#'     downward corrections.
#'   Lower-right triangle contains NA values (unobserved future reports).
#' }
#'
#' @details
#' This example demonstrates relaxed assumptions for PMF and CDF when working
#' with downward corrections:
#' - With `preprocess = NULL`, the PMF can have negative entries
#' - The CDF may not be strictly increasing
#' - This reflects real reporting processes with systematic downward corrections
#'
#' @seealso
#' - [example_downward_corr_rt] for the [reporting_triangle] version
#' - [estimate_delay()] with `preprocess = NULL` to preserve negative entries
#' - [preprocess_negative_values()] to handle negatives by redistribution
#'
#' @family example_data
#' @keywords internal
"example_downward_corr_mat"

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
#'   \item{reporting_triangle_matrix}{8x4 matrix with negative values at delay 2}
#'   \item{reference_dates}{8 dates starting from 2024-01-01}
#'   \item{delays_unit}{"days"}
#' }
#'
#' @details
#' Use this example to understand:
#' - How to work with negative corrections in delay distributions
#' - The difference between `preprocess = NULL` (preserves negatives) and
#'   `preprocess = preprocess_negative_values` (redistributes)
#' - How PMFs and CDFs behave with systematic downward corrections
#'
#' @seealso
#' - [example_downward_corr_mat] for the raw matrix version
#' - [example_reporting_triangle] for a clean example without corrections
#' - [estimate_delay()] with `preprocess` parameter
#' - [preprocess_negative_values()] to handle negative values
#'
#' @family example_data
#' @examples
#' # View the example triangle with downward corrections
#' example_downward_corr_rt
#'
#' # Estimate delay with and without preprocessing
#' delay_raw <- estimate_delay(example_downward_corr_rt, n = 5,
#'   preprocess = NULL)
#' delay_processed <- estimate_delay(example_downward_corr_rt, n = 5,
#'   preprocess = preprocess_negative_values)
#'
#' # Compare the resulting PMFs
#' delay_raw
#' delay_processed
"example_downward_corr_rt"

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
#'   [German COVID-19 Nowcast Hub](https://github.com/KITmetricslab/hospitalization-nowcast-hub/tree/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv). #nolint
#'   It contains incident case counts by age group in Germany.
#' @family example_data
"germany_covid19_hosp"
