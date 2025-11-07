# A synthetic dataset resembling line-list (each row is a patient) data from the United States' National Syndromic Surveillance System (NSSP) accessed via the Essence platform. All entries are synthetic, formatted to look as close to the real raw data as possible.

For an example of how to produce a nowcast from this data, see
[`vignette("nssp_nowcast")`](https://baselinenowcast.epinowcast.org/articles/nssp_nowcast.md).

## Usage

``` r
syn_nssp_line_list
```

## Format

A data.frame with 25 rows and 8 columns.

- C_Processed_BioSenseID:

  Unique identifier for each patient

- CCDDParsed:

  Character string indicating primary symptoms and corresponding
  diagnoses codes.

- DischargeDiagnosisMDTUpdates:

  Character string formatted as a dictionary with indices and
  corresponding time stamps formatted as YYYY-MM-DD HH:MM:SS.

- DischargeDiagnosisUpdates:

  Character string formatted as a dictionary with indices and
  corresponding diagnoses codes pertaining to this diagnosis associated
  with that event.

- HasBeenAdmitted:

  Numeric indicating whether the patient was admitted (0 for no, 1 for
  admission).

- C_Visit_Date_Time:

  Date-time indicating the time stamp of the the patient registering in
  the emergency department, in YYYY-MM-DD HH:MM:SS format.

- c_race:

  Character string indicating the race/ethnicity of the patient.

- sex:

  Character string indicating the sex of the patient.

## Source

Created for package demonstration to provide an example of how to
pre-process this dataset to obtain a reporting triangle. This is made to
look like the data that one would pull directly an API to access
patient-level line-list data.

## See also

Example datasets
[`germany_covid19_hosp`](https://baselinenowcast.epinowcast.org/reference/germany_covid19_hosp.md),
[`syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md)
