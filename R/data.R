#' A synthetic dataset containing the number of incident cases indexed by
#'   reference date and report date. While data of this form could be from any
#'   source, this data is meant to represent the output of pre-processing the
#'   [syn_nssp_line_list] dataset, which is a synthetic patient-level line list
#'   data from the United State's National Syndromic Surveillance System (NSSP).
#'
#' @format A data.frame with 3795 rows and 3 columns.
#' \describe{
#'   \item{reference_date}{Date of the primary event occurred (e.g. date of
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
"syn_nssp_line_list"
