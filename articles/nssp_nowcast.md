# Nowcasting syndromic surveillance system data: a case study applied to the U.S. National Syndromic Surveillance Program (NSSP) data

## 1 Introduction

Syndromic surveillance data are used by public health departments to
understand trends in clinical encounters from electronic health records.
When these datasets are analysed in real-time, delays from the initial
visit to updated diagnoses codes results in systematic downward bias in
the counts of the primary event, i.e. the patient visit, due to the most
recent dates being only partially observed. Nowcasting uses historical
reporting delays to correct for this downward bias and estimate the
eventual, final observed cases, which provides an estimate of the trend
in cases in real-time. In this vignette, we will pre-process the
timestamped data captured in each NSSP ESSENCE visit record to obtain
data in the format we need for nowcasting, and then we will demonstrate
how to specify and run the `baselinenowcast` workflow. Lastly, we’ll
summarise the nowcast and plot against the later observed data.

### 1.1 About syndromic surveillance system data

Syndromic surveillance system data contains information at the
visit-level on the timing and nature of the patient’s clinical
interactions. In this case study, we will use synthetic data from the
United States’ [National Syndromic Surveillance
Program](https://www.cdc.gov/nssp/index.html) (NSSP) dataset available
for participating jurisdictions through the Centers for Disease Control
and Prevention’s (CDC) ESSENCE platform, to nowcast cases of Broad Acute
Respiratory Incidence (BAR) defined by a set of diagnoses codes. These
could easily be swapped out with another set of diagnoses codes e.g. for
influenza-like illness, COVID-19, etc. The NSSP Emergency Department
(ED) visit dataset is a dataset widely used by public health departments
in the United States, representing many but not all counties in the
country. The dataset contains information at the visit-level about each
clinical encounter recorded in the electronic health record system
during the patient’s hospital stay. Clinical encounters may begin to be
associated with diagnoses codes at different points in the patient or
clinical processing journey (e.g. during registration, triage, clinical
encounter, after laboratory results are returned, or during coding for
billing, etc.), and are captured through update messages to the
syndromic surveillance system once they are entered into a facility’s
electronic health record. The difference between the visit date - when
the patient registers in the emergency department- and the time of the
diagnosis update pertaining to the diagnosis of interest, is used to
compute a reporting delay for each patient. Reporting delays for
diagnoses of interest can vary by a range of factors including by
pathogen/syndrome, season, time of day or week, means of diagnosis, the
electronic health record system, or treating facility. Note that all
visits that originate in the emergency department are used for this
analysis, regardless of eventual inpatient admission.

### 1.2 Load packages

We use the `baselinenowcast` package for nowcasting, `dplyr`, and
`tidyr` for data manipulation, `stringr` for parsing text data,
`lubridate` for formatting dates, `ggplot2` for plotting, and `purrr`
for mapping diagnoses codes to text fields in the data. For
`baselinenowcast`, see the [installation
instructions](https://github.com/epinowcast/baselinenowcast#installation).

``` r
# Load packages
library(baselinenowcast)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(purrr)
```

## 2 NSSP data pre-processing

We will walk through how to preprocess the line list NSSP data details
dataset in order to obtain a [long tidy
dataframe](https://r4ds.had.co.nz/tidy-data.html) containing the
incident counts of “cases” of a particular syndrome by reference date
(in this instance the date the visit started) and report date (the date
the patient’s record was updated with the corresponding diagnosis of
interest). We need the data in this format to estimate the reporting
delay, which we will then apply to the partial observations to produce a
nowcast – an estimate of the final observed cases.

### 2.1 Load in the line list data

This typically will be pulled using an API, but here we provide the
`syn_nssp_line_list` dataset as package data.

``` r
syn_nssp_line_list
```

    ## # A tibble: 25 × 8
    ##    C_Processed_BioSense_ID        CCDDParsed              DischargeDiagnosisMD…¹
    ##    <chr>                          <chr>                   <chr>                 
    ##  1 2024.02.03.23961E_2353110519   COUGH SENT BY UC | ;U0… {1};2024-02-03 13:45:…
    ##  2 2024.02.04.23970E_8016495577   COUGH COVID | ;U071;    {1};2024-02-04 10:29:…
    ##  3 2024.02.09.6146E_MM20716469698 VOMITING NAUSEA | ;U07… {1};2024-02-09 01:50:…
    ##  4 2024.02.08.23960I_3453027660   DIVERTICULITIS | ;R197… {1};2024-02-08 19:05:…
    ##  5 2024.02.02.6170E_HF221066059   PREGNANT COLD SYMPTOMS… {1};2024-02-02 01:15:…
    ##  6 2024.02.09.6148I_230936904054  NAUSEU WEAK BACK PAIN … {1};2024-02-09 16:01:…
    ##  7 2024.02.04.6139I_107268480     SHORTNESS OF BREATH | … {1};2024-02-04 09:17:…
    ##  8 2024.02.09.6131E_MP009028546   FLU LIKE SYMPTOMS | ;U… {1};2024-02-09 13:30:…
    ##  9 2024.02.03.30901E_15694614     SHORTNESS OF BREATH | … {1};2024-02-03 06:57:…
    ## 10 2024.02.05.23956E_315489587    FOOT PAIN FOOT INJURY … {1};2024-02-05 05:49:…
    ## # ℹ 15 more rows
    ## # ℹ abbreviated name: ¹​DischargeDiagnosisMDTUpdates
    ## # ℹ 5 more variables: DischargeDiagnosisUpdates <chr>, HasBeenAdmitted <dbl>,
    ## #   C_Visit_Date_Time <dttm>, c_race <chr>, sex <chr>

**Note:** This dataset does not represent data from real patients, it is
entirely synthetic and designed to mirror the NSSP update fields, which
records the timing of a clinical encounter and the corresponding
diagnoses code, if any. See
[`?syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)
for full documentation.

### 2.2 Define the “syndrome” definition

In this near-real-time emergency department dataset, we use public
health surveillance definitions, or “syndrome” definitions. These
syndrome definitions exist for a range of public health concerns and are
primarily defined by the CDC’s NSSP Community of Practice, who consult
subject matter experts from public health departments. They definitions
typically rely on the presence of diagnosis code(s), specific free text
captured in clinical notes, or a combination of these. In some
instances, exclusion criteria are incorporated into these definitions to
improve their specificity.

Here we will list the diagnosis codes which correspond to Broad Acute
Respiratory, but any sets of diagnosis codes that define a syndrome
could be used interchangeably. To be considered a Broad Acute
Respiratory case, one or more of these codes must be reported.

``` r
diagnoses_codes_defn <- c("A22.1", "A221", "A37", "A48.1", "A481", "B25.0", "B250", "B34.2", "B34.9", "B342", "B349", "B44.0", "B44.9", "B440", "B449", "B44.81", "B4481", "B97.2", "B97.4", "B972", "B974", "J00", "J01", "J02", "J03", "J04", "J05", "J06", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18", "J20", "J21", "J22", "J39.8", "J398", "J40", "J47.9", "J479", "J80", "J85.1", "J851", "J95.821", "J95821", "J96.0", "J96.00", "J9600", "J96.01", "J9601", "J96.02", "J9602", "J96.2", "J960", "J962", "J96.20", "J9620", "J96.21", "J9621", "J9622", "J96.22", "J96.91", "J9691", "J98.8", "J988", "R05", "R06.03", "R0603", "R09.02", "R0902", "R09.2", "R092", "R43.0", "R43.1", "R43.2", "R430", "R431", "R432", "U07.1", "U07.2", "U071", "U072", "022.1", "0221", "034.0", "0340", "041.5", "0415", "041.81", "04181", "079.1", "079.2", "079.3", "079.6", "0791", "0792", "0793", "0796", "079.82", "079.89", "07982", "07989", "079.99", "07999", "117.3", "1173", "460", "461", "462", "463", "464", "465", "466", "461.", "461", "461.", "464.", "465.", "466.", "461", "464", "465", "466", "478.9", "4789", "480.", "482.", "483.", "484.", "487.", "488.", "480", "481", "482", "483", "484", "485", "486", "487", "488", "490", "494.1", "4941", "517.1", "5171", "518.51", "518.53", "51851", "51853", "518.6", "5186", "518.81", "518.82", "518.84", "51881", "51882", "51884", "519.8", "5198", "073.0", "0730", "781.1", "7811", "786.2", "7862", "799.02", "79902", "799.1", "7991", "033", "033.", "033", "780.60", "78060") # nolint
```

### 2.3 Expand the data so that each “event” has its own column

First we will pivot the line-list’s time stamp and diagnosis update
columns into a long format with one row per update.

We will create two datasets which parse the characters in the columns
`DischargeDiagnosisMDTUpdates` and `DischargeDiagnosisUpdates` , which
contain a string listing the time stamp and diagnosis codes
(respectively) of each “event” in the clinical encounter, formatted as:

- `{event number};YYYY-MM-DD HH:MM:SS;|{event number 2};YYYY-MM-DD HH:MM:SS;|`
  for `DischargeDiagnosisMDTUpdates`

- `{event number};{diagnoses codes};|{event number 2}{diagnoses codes};|`
  for `DischargeDiagnosisUpdates`

The timestamp records the timing of diagnosis code updates related to
each clinical encounter, capturing the point in time when each new code
became available within the NSSP system. Later, we will merge the two
datasets back together by the unique patient ID and the event number, so
that we can associate each set of diagnoses codes with a timestamp.

We will use
[`tidyr::separate_wider_delim()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)
to expand these entries, so that each “event” has its own column. Since
patients experience a different number of patient update “events”, there
will be missing values for patients not experiencing many events during
their visit. The columns will be named by the original column name + the
event number, e.g. `DischargeDiagnosisMDTUpdates1`. We’ll write a
function to do this for both the time stamps and diagnoses codes.

``` r
expand_events <- function(line_list, event_col_name) {
  wide_line_list <- separate_wider_delim(line_list,
    {{ event_col_name }},
    delim = "{", names_sep = "", too_few = "align_start"
  )
  return(wide_line_list)
}
```

Expand both the time stamps and diagnoses codes, and remove the column
containing the information on the other.

``` r
syn_nssp_time_stamps_wide <- expand_events(
  line_list = syn_nssp_line_list,
  event_col_name = "DischargeDiagnosisMDTUpdates"
) |>
  select(-DischargeDiagnosisUpdates)

syn_nssp_diagnoses_wide <- expand_events(
  line_list = syn_nssp_line_list,
  event_col_name = "DischargeDiagnosisUpdates"
) |>
  select(-DischargeDiagnosisMDTUpdates)
```

We will write a function that, for each of the diagnoses and time stamps
datasets, find the name of the last update column, and uses that to
pivot the data from wide to long. This creates a long tidy dataframe
where each row is now an event.

``` r
wide_to_long <- function(wide_line_list,
                         event_col_name,
                         values_to,
                         names_to,
                         id_col_name) {
  long_data <- wide_line_list |>
    pivot_longer(
      cols = starts_with({{ event_col_name }}),
      names_to = {{ names_to }},
      values_to = {{ values_to }},
      values_drop_na = FALSE
    ) |>
    mutate(
      event_id = paste(
        .data[[id_col_name]],
        as.numeric(str_extract(as.character(.data[[names_to]]), "[0-9.]+"))
      )
    )
  return(long_data)
}
```

Pivot both datasets from long to wide using the function above,
specifying the name of the column which will hold the values (either
time stamps or diagnoses). We will create a unique event ID using the
event number and the patient ID (`id_col_name`) which in this case is
the `C_Processed_BioSense_ID` column.

``` r
syn_nssp_time_stamps_long <- wide_to_long(
  wide_line_list = syn_nssp_time_stamps_wide,
  event_col_name = "DischargeDiagnosisMDTUpdates",
  values_to = "time_stamp",
  names_to = "column_name",
  id_col_name = "C_Processed_BioSense_ID"
)

syn_nssp_diagnoses_long <- wide_to_long(
  wide_line_list = syn_nssp_diagnoses_wide,
  event_col_name = "DischargeDiagnosisUpdates",
  values_to = "diagnoses_codes",
  names_to = "column_name",
  id_col_name = "C_Processed_BioSense_ID"
)
```

Next, we will clean up the time stamps in the data so that the
`time_stamp` column is formatted as `%Y-%m-%d %H:%M:%S`, format the
visit time the same (`C_Visit_Date_Time`) and then we will filter out an
events that are not present (updates are NAs).

``` r
syn_nssp_time_stamps <-
  syn_nssp_time_stamps_long |>
  mutate(
    time_stamp = as.POSIXct(
      str_remove_all(
        str_remove(time_stamp, ".*\\}"),
        "[|;]+"
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    C_Visit_Date_Time = as.POSIXct(C_Visit_Date_Time)
  ) |>
  drop_na(time_stamp)
```

Clean up the diagnoses codes and remove the empty updates from the
diagnoses dataset. For these, we want to keep the semi-colons and just
remove the numbers since this information is stored in the event ID. We
will only use the event ID and the diagnoses codes, as this will be
merged back into the time stamped dataset.

``` r
syn_nssp_diagnoses <-
  syn_nssp_diagnoses_long |>
  mutate(diagnoses_codes = str_remove(diagnoses_codes, ".*\\}")) |>
  filter(nzchar(diagnoses_codes)) |>
  drop_na() |>
  select(event_id, diagnoses_codes)
```

Merge together the time stamps of events and the diagnoses codes. Filter
to remove empty updates.

``` r
nssp_merged <- merge(syn_nssp_time_stamps,
  syn_nssp_diagnoses,
  by = "event_id"
) |>
  filter(diagnoses_codes != ";;|")
```

Now we have a dataframe where each row is an event, with the patient’s
visit start date (`C_Visit_date_Time`), the patient ID
(`C_Processed_BioSense_ID`), the diagnoses code at the event
(`diagnoses_code`), and the time stamp of the event (`time_stamp`). Next
we will add a column for the time from arrival to each updated
diagnosis, in days.

``` r
nssp_updates <- nssp_merged |>
  mutate(arrival_to_update_delay = as.numeric(difftime(
    time_stamp, C_Visit_Date_Time,
    units = "days"
  )))
```

We next filter through the updates to find the first “hit” that
corresponds to the diagnosis codes in the syndromic surveillance
definition for BAR.

``` r
bar_updates <- nssp_updates |>
  filter(map_lgl(diagnoses_codes, ~ any(str_detect(.x, diagnoses_codes_defn))))
```

Next, we will order these by the delay from visit to the diagnoses, and
for each patient keep only the first update containing the BAR diagnoses
code(s).

``` r
first_bar_diagnosis <- bar_updates |>
  arrange(arrival_to_update_delay) |>
  group_by(C_Processed_BioSense_ID) |>
  slice(1)
```

Label the visit start date, `C_Visit_Date_Time`, as the reference
date,`reference_date` and `time_stamp` as the report date, `report_date`
and remove the other column names that are no longer needed, as each row
now represents a case.

``` r
clean_line_list <- first_bar_diagnosis |>
  mutate(
    reference_date = as.Date(C_Visit_Date_Time),
    report_date = as.Date(time_stamp)
  ) |>
  ungroup()
head(clean_line_list)
```

    ## # A tibble: 6 × 13
    ##   event_id C_Processed_BioSense…¹ CCDDParsed HasBeenAdmitted C_Visit_Date_Time  
    ##   <chr>    <chr>                  <chr>                <dbl> <dttm>             
    ## 1 2024.02… 2024.02.01.23959I_204… ABNORMAL …               1 2024-02-01 13:30:00
    ## 2 2024.02… 2024.02.01.23965V_656… DIFFICULT…               1 2024-02-01 09:26:00
    ## 3 2024.02… 2024.02.01.24119E_H10… COUGH FEV…               1 2024-02-01 13:25:00
    ## 4 2024.02… 2024.02.01.24167I_065… LETHARGY …               1 2024-02-01 11:15:00
    ## 5 2024.02… 2024.02.01.6132E_2260… COVID LAS…               0 2024-02-01 13:36:00
    ## 6 2024.02… 2024.02.01.6133I_2490… HIGH BLLO…               1 2024-02-01 11:04:00
    ## # ℹ abbreviated name: ¹​C_Processed_BioSense_ID
    ## # ℹ 8 more variables: c_race <chr>, sex <chr>, column_name <chr>,
    ## #   time_stamp <dttm>, diagnoses_codes <chr>, arrival_to_update_delay <dbl>,
    ## #   reference_date <date>, report_date <date>

### 2.4 Obtain counts of cases by reference date (visit date) and report date (time of first diagnosis)

For nowcasting, we want to compute the number of incident cases indexed
by reference and report date, so we can aggregate by reference and
report date and compute the delay distribution.

``` r
count_df_raw <- clean_line_list |>
  group_by(reference_date, report_date) |>
  summarise(count = n()) |>
  mutate(delay = as.integer(report_date - reference_date))
```

    ## `summarise()` has grouped output by 'reference_date'. You can override using
    ## the `.groups` argument.

Looking at this data, we can see that there is one case where there is a
negative delay, which indicates that the time stamp of the diagnosis
update was recorded before the start of the visit. Depending on the way
that the data is generated, this could be a true negative value, for
example if the patient had previously been tested elsewhere before
arriving at the ED. In this case, patients interactions with the system
always start in the ED, so this is likely due to a data entry error. For
this reason, we will will choose to exclude all the negative valued
delays, however, the choice of how to handle these should be guided be
guided by the data experts and their understanding of the most likely
reason for the observation to prevent introducing additional bias in
this choice.

``` r
count_df <- filter(count_df_raw, delay >= 0)
head(count_df)
```

    ## # A tibble: 6 × 4
    ## # Groups:   reference_date [3]
    ##   reference_date report_date count delay
    ##   <date>         <date>      <int> <int>
    ## 1 2024-02-01     2024-02-01      4     0
    ## 2 2024-02-01     2024-02-02      1     1
    ## 3 2024-02-01     2024-02-20      1    19
    ## 4 2024-02-02     2024-02-05      1     3
    ## 5 2024-02-02     2024-02-10      1     8
    ## 6 2024-02-03     2024-02-03      1     0

We have now generated data in the format that we need to use the
`baselinenowcast` package, which requires a [long tidy
dataframe](https://r4ds.had.co.nz/tidy-data.html) with incident case
counts indexed by reference date and report date. See the [Getting
Started](https://baselinenowcast.epinowcast.org/articles/baselinenowcast.md)
and [model
definition](https://baselinenowcast.epinowcast.org/articles/model_definition.md)
vignettes for more details on the data format we need for nowcasting.
For demonstration purposes, we will now swap out the data from the
simulated NSSP line-list data with a larger synthetic dataset. In
reality you would proceed straight from this dataset to the subsequent
steps, using the case counts indexed by reference date and report date
to run the nowcasting workflow.

## 3 Pre-processing of larger synthetic dataset

We’ll start by loading in the synthetic reporting triangle dataframe,
which is also provided as package data.

``` r
syn_nssp_df
```

    ## # A tibble: 3,795 × 3
    ##    reference_date report_date count
    ##    <date>         <date>      <dbl>
    ##  1 2025-10-25     2025-10-25    194
    ##  2 2025-10-25     2025-10-26     54
    ##  3 2025-10-25     2025-10-27     26
    ##  4 2025-10-25     2025-10-28     13
    ##  5 2025-10-25     2025-10-29     12
    ##  6 2025-10-25     2025-10-30     13
    ##  7 2025-10-25     2025-10-31      5
    ##  8 2025-10-25     2025-11-04     14
    ##  9 2025-10-25     2025-11-05      9
    ## 10 2025-10-25     2025-11-06     14
    ## # ℹ 3,785 more rows

**Note:** This dataset represents synthetic data on the number of
incident cases indexed by reference date and report date. It was
generated to approximately mirror trends in BAR cases during a season
without using any real data. See
[`?syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md)
for full documentation.

You can see that this larger synthetic dataset has the same format as
the one we generated from the line list NSSP data – with columns for
reference date, report date, and counts.

### 3.1 Exploratory data analysis to identify an appropriate maximum delay

To produce a nowcast with `baselinenowcast`, we will first want to
perform an exploratory data analysis to visualize trends in the delay
distribution. This will help us choose a maximum delay and specify the
number of reference times we want to use for delay and uncertainty
estimation.

Click to expand code to create plots of the delay distributions

``` r
long_df <- syn_nssp_df |>
  mutate(delay = as.integer(report_date - reference_date))

delay_df_t <- long_df |>
  group_by(reference_date) |>
  summarise(mean_delay = sum(count * delay) / sum(count))

delay_summary <- long_df |>
  mutate(mean_delay_overall = sum(count * delay) / sum(count))

avg_delays <- long_df |>
  group_by(delay) |>
  summarise(pmf = sum(count) / sum(long_df$count)) |>
  mutate(cdf = cumsum(pmf))

delay_t <- ggplot(delay_df_t) +
  geom_line(aes(
    x = reference_date,
    y = mean_delay
  )) +
  geom_line(
    data = delay_summary,
    aes(
      x = reference_date,
      y = mean_delay_overall
    ),
    linetype = "dashed"
  ) +
  xlab("") +
  ylab("Mean delay") +
  theme_bw()

cdf_delay <- ggplot(avg_delays) +
  geom_line(aes(x = delay, y = cdf)) +
  geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
  theme_bw()
```

``` r
cdf_delay
```

![](nssp_nowcast_files/figure-html/unnamed-chunk-18-1.png)

``` r
delay_t
```

![](nssp_nowcast_files/figure-html/unnamed-chunk-18-2.png)

Based on this figure, we can set the maximum delay to be 25 days as this
is where 95% of the cases appear to have been reported. In general, we
want to choose a maximum delay that will incorporate the vast majority
of all observed reporting delays so that the estimates of the final
counts account for all eventual cases reported. However, a longer
maximum delay will mean that you will require more historical data for
training the model, which must be considered if the amount of historical
data is limited say in a novel outbreak situation. In order to produce a
nowcast that we can evaluate against later observed data for this
example, we will pretend that we are making a nowcast 30 days before the
last reference date in the dataset, May 5th, 2026. In real-time, we
would just use the latest reference date as our nowcast date, however,
for evaluating the performance of the method we want to look back at the
nowcast we would have made with the data we would have had at past time
points. See the [Getting
Started](https://baselinenowcast.epinowcast.org/articles/baselinenowcast.md)
vignette for another example of evaluating our nowcast, and for a full
quantitative evaluation of the method applied to different case studies,
see our
[pre-print](https://www.medrxiv.org/content/10.1101/2025.08.14.25333653v2)
where we evaluate the method on COVID-19 cases in Germany and norovirus
cases in England.

``` r
max_delay <- 25
nowcast_date <- max(long_df$reference_date) - days(30)
```

### 3.2 Format for `baselinenowcast`

First, we’ll remove any reports after the nowcast date, as these
wouldn’t have been available to us in real-time.

``` r
training_df <- filter(
  long_df,
  report_date <= nowcast_date
)
```

What would happen if we looked at the trend in case counts without
correcting for downward bias due to partial observations? We can
summarise the cases by reference dates and plot.

``` r
training_df_by_ref_date <- training_df |>
  filter(report_date <= nowcast_date) |>
  group_by(reference_date) |>
  summarise(initial_count = sum(count))
```

Click to expand code to create the plot of the initially reported cases

``` r
init_data <- training_df_by_ref_date |>
  filter(reference_date >= nowcast_date - days(60))

plot_inits <- ggplot(init_data) +
  geom_line(aes(x = reference_date, y = initial_count), color = "darkred") +
  theme_bw() +
  ylab("Initially reported BAR cases") +
  xlab("Date of ED visit")
```

``` r
plot_inits
```

![](nssp_nowcast_files/figure-html/unnamed-chunk-23-1.png)

We can see that without nowcasting, the cases appear to be sharply
declining at the most recent dates. We can’t tell from this data alone
if the true trend in cases is really declining rapidly or if it is
plateauing or increasing, which is why we need to use nowcasting to make
an estimate of the eventually reported cases at each visit date.

From here, we have the case counts by reference and report date up until
the nowcast date. We will generate a reporting triangle (see the
[mathematical
model](https://baselinenowcast.epinowcast.org/articles/model_definition.md)
vignette for more details), using the helper function
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md).
This helper function uses the `max_delay` and the reference and report
dates to fill in all combinations of reference dates and delays, filter
to exclude delays beyond the maximum delay, and pivot the data from long
to wide to obtain a reporting triangle where rows are reference dates
and columns are delays. This is returned as a `reporting_triangle` class
object which is ready to be used for nowcasting.

``` r
rep_tri <- as_reporting_triangle(training_df,
  max_delay = max_delay
)
```

You can inspect the reporting triangle matrix itself by accessing the
\$reporting_triangle_matrix element in the object

``` r
rep_tri$reporting_triangle_matrix
```

    ##        [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
    ##   [1,]  194   54   26   13   12   13    5    0    0     0    14     9    14
    ##   [2,]  198   53   29   40   11    5    1    0    0    20     0    22     0
    ##   [3,]  167   58   33   14    0    7    4    8   12     8     0     0     6
    ##   [4,]  145   69   22   11    5   19   23   13    8    15     0     0     0
    ##   [5,]  180  100    0   25   12   15   31    7    4    12     5    12     5
    ##   [6,]  220   48    0   21   11   16   24   14    0     0     0     0     8
    ##   [7,]  326   94   14   33   42   26   27    0    0    20    11     0     2
    ##   [8,]  202   87    0   33   31   18    0    0   26    12     0     0     0
    ##   [9,]  256   75   13   39   19   22    7   11    5     5     5     6     4
    ##  [10,]  213   43    1   33   14   10   23    8    3    12     4     0     8
    ##  [11,]  229  128   28   23   10   26    8   15   31    15     8     0    13
    ##  [12,]  237   38   24   58    1   16   15   11    0    12     0    10     0
    ##  [13,]  242   87    8    0   13   22   15    7    0     0     0     0     0
    ##  [14,]  325   78   36   14   26   10   16   15    7    11    26     6    15
    ##  [15,]  211   72   29   27   25    6   10   16   27    26     0    16     0
    ##  [16,]  340  162   35   28   22   13   11   12   23    22     0     8     9
    ##  [17,]  241  139   14   18   14    8   16   14   25    10     1     3     0
    ##  [18,]  258   98   26   10   13   27    8   33   15     8    21     0    32
    ##  [19,]  279  126   23    8   35    6   33   31   13     0    12     0     0
    ##  [20,]  176   15   11   13   10    9   32    1   19    16     0     8     7
    ##  [21,]  167   41    2   15    0   12   12    8    0     5    12     0     1
    ##  [22,]  246   35   20   15   11   14    9    0   14     6    18     7    18
    ##  [23,]  210  134   53    9   22    3   16   34   22    18    12    10    10
    ##  [24,]  157   83   20    6   16    0   30   22    7     4    13     0     0
    ##  [25,]  202   65   30   19    0   31   12   37    4     1    24     1     0
    ##  [26,]  239   61   21    0    8   20    0   32   13     0     0    19    13
    ##  [27,]  160  109   25   73   32   50   40   26    0    17    11    15    19
    ##  [28,]  310   63   19   15   26   10   18   10   17    11     8    27     1
    ##  [29,]  268  114   32   52   27   10   17    6    8    21     5    10     0
    ##  [30,]  266   80   42   49    6   20    6   30   22    12     6    31     6
    ##  [31,]  287   95   41   32    8   11   11    1   18    12    12    11     0
    ##  [32,]  202   36    7    0   17   22   21    8    0     2     0    10    16
    ##  [33,]  288  146   28    5   41   32   28   12   17    11     0     9     8
    ##  [34,]  223   60   26    5   14    1   11   27   11     0     5    13     7
    ##  [35,]  246   35   11   15   21   27   22   10    0     3     0     0     0
    ##  [36,]  273   22   20   29   35   38    0    0   11     9    17     5     0
    ##  [37,]  289  113   10   37   28   15    0    6   27    27    28    45     0
    ##  [38,]  305   89   21   17   13   13   33   30   21    34    17    10     0
    ##  [39,]  284   92   18   23    9   28   32   32   14    24    10     0     8
    ##  [40,]  279   91   16   14   12   13   31   13    0     0     6    19    17
    ##  [41,]  225   82    8   49   27   17   28    8    3     0     0     9    24
    ##  [42,]  174   60   26   14    7    9   26    7   13    19    15     1     0
    ##  [43,]  286   90   32   25   21   15    0   11    2     3    26     5     1
    ##  [44,]  190  107   45   33   34    6   19   14   10    26    31     6     5
    ##  [45,]  291   72   23   43   11    3   56   48    8    21    14     0    13
    ##  [46,]  323  168   36   26   38   23   50   52   45    41     0     3    16
    ##  [47,]  192   83   11   37   33   26   21   15   10     0     0     1     3
    ##  [48,]  151   70   44   22   38   48   14   17    8     0    13    15     1
    ##  [49,]  213   93   26   21   15   14   14    5    1    14     7    21     3
    ##  [50,]  187   77   28   20    8   10   13    0    6    13    14     9     0
    ##  [51,]  272   56   58   24   36   14    5    9   14    10     8     0     0
    ##  [52,]  205   98   25   23   29    9   25   15    1    15     0     0     9
    ##  [53,]  268   53   47   38   16   10   10    9   12    31     0     0     7
    ##  [54,]  177   96   23   17    8    0   14   13   14     0     0     0     5
    ##  [55,]  213   63   34   15   23   23    0   21    8     0    11    12     1
    ##  [56,]  218  113   25   36   26   24   10   18   16     7    22    28    10
    ##  [57,]  135  101   17   39    9   12   10    8    7     6     4     0     0
    ##  [58,]  161   35   17   25   12    4    7    0    9    23     7     0     7
    ##  [59,]  256   70   14   19   28   25   32   15    9    32    12     3     3
    ##  [60,]  160   75   46   37   17   28   25    5    9     4     0    16    12
    ##  [61,]  207  105    5   12    8   16   25   11   11     1     0    11     0
    ##  [62,]  233   71   22   17    8   18   18   23    1     0    12    11     0
    ##  [63,]  194  128   35   20   19   41   17   21   11    27     0     8     8
    ##  [64,]  233   34   23   15   22    9   12    0   19     0     0    11     0
    ##  [65,]  163   71   39   26   22    8    5    0   20    23    14     0     0
    ##  [66,]  137  142   22   40   15    0   10    0   18    21     0     0     0
    ##  [67,]  210   70   36   21   18   25   11    6   13     0     0     0    24
    ##  [68,]  127   46   21   12   25   17    0   12    0     0     0     0     7
    ##  [69,]  172   61    6   24   16   18   11    0    0     0     0     2     5
    ##  [70,]  119   29   25   15    0    7    0    0    7     6    13    11     0
    ##  [71,]   93   70   29   12    8   26   10    8    6    26     0     3     0
    ##  [72,]  172   63   32   13   20    0    0   18    0     7    19    12     0
    ##  [73,]  126   44   13   19    0   10   38   22    0    17     5     0     0
    ##  [74,]  265   79   31   22   16   12   44   36    7    20    13     7    10
    ##  [75,]  168   48   16   21    6   13   23   17    1     0     2     1    27
    ##  [76,]  176   23    9   14    0    5   12    9    0     0     0     7     0
    ##  [77,]  187   56    8    9    0   41    3    0    8     1     0    10     7
    ##  [78,]  133    5   18   16   28   18    0   12    0     0     8     0     0
    ##  [79,]   96   46   20   22    8    0    9   13    1     0     0     7     0
    ##  [80,]  194  139   56   26   13    4   13   20    0     0    14     0     6
    ##  [81,]  234   76   30   16   12   26   12   15   10    13     7     1     5
    ##  [82,]  225   49   24    7   31   25   18    0    7     0     4     0    15
    ##  [83,]  105   56   10   33   22    8   14    8   10     5    11     8     8
    ##  [84,]  148   27   29   22   15   18   15    0    0    11    13     0     0
    ##  [85,]  162   60   34    9    0   30   10   12    6    20    11     0    12
    ##  [86,]  207   54   55    0   23   13    7    0   11     7     6     9     0
    ##  [87,]  239   69   41   13   12    4   35   34    1    21     8     1     0
    ##  [88,]  189  108   22   24   24    8   17    7   20    22     0    10    15
    ##  [89,]  176   53    9   26   12   11   34   31   12     0     0     0     1
    ##  [90,]  276   25   38   13   42   18   30    9    0     0     7    19     2
    ##  [91,]  260   25   29   11   12   13   20    0    0     7    18     0     4
    ##  [92,]  203   47   16   51   29   17   13   34   26    23    20     1    10
    ##  [93,]  330  147   37   55   13    0   13   30   27    33    28    19     0
    ##  [94,]  331   93   37   28   40   27   37   30   41    14    26     0     7
    ##  [95,]  188  105   27   21   26   23   23   31    9    28     0     0    15
    ##  [96,]  225   72   16   15   26   10   47    4   23     1     0    10     9
    ##  [97,]  275  230   36   46   30   32   30   13    7     0    36    16     6
    ##  [98,]  197  127   35   12   14   16    9    0    3    34    22    17    16
    ##  [99,]  242   87   17   13   20    7   16   13   17    18     8    18     3
    ## [100,]  294  132   25   19   33   24    7   21    6    24     6     0     0
    ## [101,]  252  134   66   71   22   11   22   16    8    24    18     9     0
    ## [102,]  289  105   45   27   13   14   19   30    1     5     0     0    33
    ## [103,]  297   80   85    0   44   16   27   21   27     9     0    20    17
    ## [104,]  237   80   40   46    8   24   48   15    0    13    14     5    16
    ## [105,]  328  145   65   47    1   45   16   27    1    16    42    20    30
    ## [106,]  342   88   34   10   26    8   27    3    0     3    21    23     4
    ## [107,]  444   95   48   32   33    6    0   23   50    18     0     0     8
    ## [108,]  298  225   66   53    7   32   42   25   21    35    23     0    15
    ## [109,]  314  137   19   19   21   34   32   31   24     9     0     1    10
    ## [110,]  344  133   30    8   30   28   16   45   41    18    12     0    13
    ## [111,]  402  228   56   26   34   31   35   19    0     4     6    25    15
    ## [112,]  305   71   38   24    9   34   34    0   14     2     9     8    44
    ## [113,]  344   89   43   32   36   13   12   20    0    10     9     0    43
    ## [114,]  564  122   74   33   48   15   17   21    9    28    34    45     0
    ## [115,]  417  180   51   30   12   21   11   32   23    14    14    14     0
    ## [116,]  345  126   90   67   26   12   29   42   17     9     9    11     0
    ## [117,]  412   88    7    1   18   16   67   28   23     0    13     0    20
    ## [118,]  306  158   20   22   27   34   45   16   11     0     0     6    28
    ## [119,]  406  204   42   34   34   20   29    6    6     2    15     1    11
    ## [120,]  404  151   73   35   34   39   17    9    0    24    15    25    30
    ## [121,]  313  178   47   12   39   30    9   21   17     9    27     1     8
    ## [122,]  615  270   35   81   45   26   63   65   42    51    38    15    14
    ## [123,]  470  243   73   30   35   28   34   25   22     5     0     0     4
    ## [124,]  662  275   50   23   40   13   43   64   13     0     5    20     3
    ## [125,]  534  159   74   76   21   52   22   50    0    10    40    22     6
    ## [126,]  638  196   91   66   38   43   62   20   19    43    39    12    14
    ## [127,]  591  230   71   68   58   36   12   28   30    44    28    21    25
    ## [128,]  556  184   47   48   51   40   41   34    5    20    13    22     0
    ## [129,]  487  252   82   69   23   13   57   33   48    49    25    23     1
    ## [130,]  603  261   96   47   27   58   60   35   43    33    18     7    30
    ## [131,]  411  178   65   16   27   17   23   79   46     0     8     9     0
    ## [132,]  504  224   39   73   50   13   24   17   13    23    22     5    18
    ## [133,]  533   96   24   45   29   36   31   41   15    14    13    12    33
    ## [134,]  310  126   43   33   36   12   12   24   28    24    15    32    15
    ## [135,]  414  215   56   84   46   23   19   24   26    16    35    27     0
    ## [136,]  689  333  101   90   58   47   17   26   64    37    22     7    10
    ## [137,]  477  102   67   21    7   30   20   31   51    11    15    11    20
    ## [138,]  493  185   49   17   44   34   32   57   31     5     6    21    26
    ## [139,]  340  116   69   17   17   31   26   15   12     5    32     0    34
    ## [140,]  251  100   32   42   18   20   37   11   11     0    28     0     8
    ## [141,]  300   99   47   34    5   45   18    0   20    16    20    14    19
    ## [142,]  378   77   22   16   16    1   11   14   38    14     8    13     0
    ## [143,]  508  147   37   37   13   15   32   27   16     6    11     4     6
    ## [144,]  482  172   82    2   35   35   13   30   22     0     0     0     6
    ## [145,]  295  197   53   47   19   18   36    8   10     0     0    20    18
    ## [146,]  274   97   28   31   29    5    9   31    7     5     6    18    14
    ## [147,]  171   36   34   26   13   46   11   12   18     1     8     8    31
    ## [148,]  280  155   83   46   15   22    3    4   25     0    10    11    NA
    ## [149,]  246   95   57   19   38   14   10    0    0    10    18    NA    NA
    ## [150,]  210  131   34   50   35   12    1   25   20     6    NA    NA    NA
    ## [151,]  221   96   22   10   13    6    0    5    9    NA    NA    NA    NA
    ## [152,]  291  129   17   26   42   29   23   25   NA    NA    NA    NA    NA
    ## [153,]  179   96   22   50    9    8   18   NA   NA    NA    NA    NA    NA
    ## [154,]  284   40   41   54   28   12   NA   NA   NA    NA    NA    NA    NA
    ## [155,]  217   78   46   14   39   NA   NA   NA   NA    NA    NA    NA    NA
    ## [156,]  336  161   62   13   NA   NA   NA   NA   NA    NA    NA    NA    NA
    ## [157,]  296   53   55   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
    ## [158,]  210  108   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
    ## [159,]  236   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
    ##        [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25]
    ##   [1,]     0     0     0     6     0     0     0     0     0    12     0    10
    ##   [2,]    10     7     0     0    20     0     0     0     0     0    10     0
    ##   [3,]     0    19     6     9     0     0     0     0     0     0     0     0
    ##   [4,]     3     0    10     0     9     0     0     0     0     9     0     0
    ##   [5,]     0     8     0     0     0     5     5     0     0     0     0     0
    ##   [6,]     0     0     0     0     0     0     0     0     0     0     0     0
    ##   [7,]     5    14     0     5     8     2     0     0     0     0     0     0
    ##   [8,]     0     0     0     5     0     0     0     0     0     0     0     0
    ##   [9,]     0     0     8    12     5     0     0     0    10    13     0     0
    ##  [10,]     5    22     0     0     0     0     0    11    15     0     0     0
    ##  [11,]     0    11     5     0    14     0     0     0    17     5     0     6
    ##  [12,]     0    13     0     1     0     4    19     9     0     9     0     0
    ##  [13,]     1     0     0     0    10     4     0     2     0     0     0     0
    ##  [14,]     0     0     0     1     0     0    13     0     0     0     0     7
    ##  [15,]     0     0     0     9     0     7    10     0     0     6     0    14
    ##  [16,]     9     0     0     0     0     0     0     3    14     0     0     0
    ##  [17,]    18     0     0     1    14     3     5     8     0     0     0     0
    ##  [18,]     8     7    12    24     0     0     4     0     0     0     3     0
    ##  [19,]    14     0     7    20     0    17     8     9     0     0     0     1
    ##  [20,]     8     4     1     0     0     0     0     0     0     0     0     0
    ##  [21,]    15     0     0     0     0     0     0     3     0     0     0     0
    ##  [22,]    12     0    18    12     0     0     0     0     0     6     0     0
    ##  [23,]     0     0     8     0     0     0     0     0     0     9    12     0
    ##  [24,]    21     0     0     8     0     0     0     0     7    10     0     0
    ##  [25,]    11     0     4     0     0     0     0     0     0    12     0     0
    ##  [26,]     7     1    10     0     0     0    10     0     0     0     0     0
    ##  [27,]     0     0     0     0    11    12     1    11     0     0     0     0
    ##  [28,]     1     0     0     0     0     1     0     0     0     0     1     0
    ##  [29,]     9    17     0     5     1     0     4     0     0     0     1     0
    ##  [30,]     9     0    14    21     9     0     0     0     0     0     0     0
    ##  [31,]     6     8     0     7     0     0     0     0     0     0     0     0
    ##  [32,]    15     1    11     6     0     7     5     0     9     5     0     0
    ##  [33,]    17     0     7     9    16     8     6     0     4     0    10     0
    ##  [34,]     0    18     0     0    11     0     0     0     0     0     0     0
    ##  [35,]     0     0     0     0     4    13     0     0     0     0     0     0
    ##  [36,]     0     8     1     0     9     0     0     0     0     0     0     0
    ##  [37,]     2     9     0     1     1    13     0     0     7     2     0     0
    ##  [38,]     1    13     0     0    13     0     4     0     0     0     0    17
    ##  [39,]     7     0     0     0     0     0     0     7     0     0     0     0
    ##  [40,]     5    14     6     0     0    10     4    15     0     7    15     0
    ##  [41,]     3     4    12     0     8     0     0     0     0     0     0     0
    ##  [42,]    10     0     0     7     0     0     7     8     0     0     0     0
    ##  [43,]     0     0     0     0     7     0     0     0     0     0     0     0
    ##  [44,]     0     5     0     0     7     0     0     0     0     0     1    16
    ##  [45,]    10    12     7     8     0     0     8     0     0     0     0     3
    ##  [46,]     6     6     0     0     4     0    11     8     0     0     0    23
    ##  [47,]     1     0    20     9     0     0     8     0    10     0     0     0
    ##  [48,]    11     0     8     0    14    15     0     8     6     0    10     0
    ##  [49,]    10     0     0     0     0     0     0     0     0     0     0    14
    ##  [50,]     0     0     0     0     6     0     6     0     0     0     0     0
    ##  [51,]    14     6     0     0     0     0     0     0     0    15     0     0
    ##  [52,]    15     0    11    14     0     0     7     7     0     0     0     0
    ##  [53,]    10    10     0     0     0     0     0     0     0     0     0     0
    ##  [54,]     0     6     0     0     0     0     0     0     0     0     0     0
    ##  [55,]    24     0     0     0     0     0     0     1     0     0     0     0
    ##  [56,]     0     4     0    13     8     1     0    10     5     0     0    18
    ##  [57,]     0     0    17    14     6     0     0     4     9     0    16     0
    ##  [58,]     0     9     1     0     0     0     0     0    11     0     4     0
    ##  [59,]     1    10     0     0     0    13     0     0     0     0     0    14
    ##  [60,]    13     0     7     4     0     0     0     0     0     0     0     0
    ##  [61,]     0     8     0     0     0     8     1     9     8     0     0     0
    ##  [62,]    12     7     0     0     0     8     0     0     0     0     0     0
    ##  [63,]     0     0     2     0     8     0     6     0     0     0     0     1
    ##  [64,]     0     0     4     0     0     4     0     0     0     0     4     0
    ##  [65,]     0     0     7     0     0     0     0     0     0     0     0     0
    ##  [66,]     0     0     0     0     0     0     0     0     0     0     1     0
    ##  [67,]     6     0     5     0     0     0     0     0     3     0     0     0
    ##  [68,]    12     0     0     0     0     0     0     0     0     0     0     0
    ##  [69,]    30    16     0     0     0     0    10     0     0     0     0     0
    ##  [70,]     0     0     0     0     0     5     0     0     0     0     0     2
    ##  [71,]    14     0    16    14     0     0     0     0     0     0     0     0
    ##  [72,]     0     0     4     0     0     0     0     0     1     0     5     0
    ##  [73,]     0     5     0     0     0     0     0     0     0     0     0     0
    ##  [74,]     6     6     0     8     0     0     8     0     5     0     0     0
    ##  [75,]     0     0     0     0     0     0     0     0     0     0     0     0
    ##  [76,]     0     0     0     0     0     0     0     0     0     0     0     0
    ##  [77,]     1     0    15     3     0    17     0     0     0     0     0     4
    ##  [78,]     0     0     0     0     0     0     7     0     0     0     0     0
    ##  [79,]     0    15     0     0    17     0     0     0     0     0     0     1
    ##  [80,]    16     6    13     1     0    11     0     0     0     0     8     0
    ##  [81,]     0    15    11     0    13     0     0     7     5     8    10     0
    ##  [82,]     0     0    14     0     0     5    11     0     0     4     0     0
    ##  [83,]     9     9     5     0    10     0     0     0     0     0     0     1
    ##  [84,]     1     0     0     0     0     0     0     0     0     0     0     0
    ##  [85,]     0     0     7     8     3     6     0     0     2     0     6     0
    ##  [86,]     0    12    10    11     0     0     0     0     0     0     0     0
    ##  [87,]    15    36     0     0    19     5     9     0     6     0     0     0
    ##  [88,]    19    10     2     6     0     0     0     0     0     0     0     0
    ##  [89,]    11     8     0     0     0     0     0    14     6     0     0     0
    ##  [90,]     8     0     0     6    18     0     8     0     0     0     0     0
    ##  [91,]     5    11     0     0     0     0     0     0     0     0     0     0
    ##  [92,]     6     0     0     0     3     0     6    14     4     0     5     0
    ##  [93,]     0     0     0     0    13     0     0     0     0     0     7     0
    ##  [94,]     6     0    10     4     0     0     0     0     0     0     0     9
    ##  [95,]    12    13     1     2     0     1    14     0     0     0     0     0
    ##  [96,]    16     0     0     0     0     0     1    14     0     6     0     0
    ##  [97,]     0    17     0     0     2     0     5     9     0     0     0     0
    ##  [98,]     0     9     0     1     0     0    11     0    10     8     0     0
    ##  [99,]     0     0     0     9     0    10    10     0     0     0     0     0
    ## [100,]     0    16     5     0     0     4     0     0     0     0     0     0
    ## [101,]     0    16     7     9    21     0     0     0    12    13     0     0
    ## [102,]     7    12     9    10     0     0     0     8     0     0     0     0
    ## [103,]     5    18     0     0     0     1     8     1    13     7     0     0
    ## [104,]    17     3     0     0     0     1     8     0    13     0     0     0
    ## [105,]    19     0     0     0    15     1     0     0     0     0     0     0
    ## [106,]     7     0     0     9     5     5     0     0     5     0     0     0
    ## [107,]     0     0    13    21     0    11     0     0     6     6     0     0
    ## [108,]    16     1     5    21     0     0     0     0     0     0     0     0
    ## [109,]    14    15    17     0     0     0     0     3     7     1     0     0
    ## [110,]    23     3    25    15     0     0     0    21     0     1     0     0
    ## [111,]    12     3    10     0     3    11    20    17    17    14     0     7
    ## [112,]    13    12     0     6     0     7     0     8     0     0     0     9
    ## [113,]     8     3     3    29    21     9     1     0     0     0    13     0
    ## [114,]     0    15    32    10     0     0    12     0     5     0    20     0
    ## [115,]     0    25    32    18    14    14     0     7     8     9     0     0
    ## [116,]    26    19     5     8     0     6    32     5     5    12     3     0
    ## [117,]    12    29    28     0     0    13    21     0     0     0     0     4
    ## [118,]    37    11     6     0     0     0    14     3     6     0     3     0
    ## [119,]    46     0    16     8     9     9     0     5     0     0     0     0
    ## [120,]     0     0     9    26     3     2    13     0     0     0     0     0
    ## [121,]     0     0    13    25    16    29     0     0    10     0    13     9
    ## [122,]     6    10    35    38    51     0     2    10     5    23     3    11
    ## [123,]    16     1    25    32     0     0     0    16     4     0     0     0
    ## [124,]    14    11    17     6     0     0    22    10     6    10     2     1
    ## [125,]    19     0     5     0     3     0    33    18     7     0     1     6
    ## [126,]    18     0    18     0     1     3    11    15     0     9     3     0
    ## [127,]     8     0    14    31    16     8     0     0     0     0     9    13
    ## [128,]     8    10    19     6     8    18     0    19     0     7     0     0
    ## [129,]     9    27    15    10     5    24    17     8     6     5     4    14
    ## [130,]    37    20    18     7    12    13     0     7     0    13     3     0
    ## [131,]    13     0     8     3     0     4     5     0     0    15     9     0
    ## [132,]    28     3     5     0    17    10     0    13     6     8     0     0
    ## [133,]    19     0     0    20    29    31     5     0     0     0     0    16
    ## [134,]    17     7    10     2     8     1    14     0     0     7     0     0
    ## [135,]     1     0     9    10     8     0    15     0     0    10     0     4
    ## [136,]    16    31     0    11    15     0     0    20     6     3    12    NA
    ## [137,]     0    17    31    14     0     0    12     7     6    15    NA    NA
    ## [138,]     6    14     7     0     0     9    15     0     0    NA    NA    NA
    ## [139,]     4     0     4     0    11     9     0     9    NA    NA    NA    NA
    ## [140,]     6     0     7    13     7    14    11    NA    NA    NA    NA    NA
    ## [141,]     0     0     0     0     3     0    NA    NA    NA    NA    NA    NA
    ## [142,]     0     0    16     3    16    NA    NA    NA    NA    NA    NA    NA
    ## [143,]    23     0    10    14    NA    NA    NA    NA    NA    NA    NA    NA
    ## [144,]     0     0     2    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [145,]     2    21    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [146,]    17    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [147,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [148,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [149,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [150,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [151,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [152,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [153,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [154,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [155,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [156,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [157,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [158,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## [159,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##        [,26]
    ##   [1,]     0
    ##   [2,]     6
    ##   [3,]     0
    ##   [4,]     0
    ##   [5,]     0
    ##   [6,]     0
    ##   [7,]     0
    ##   [8,]     0
    ##   [9,]     0
    ##  [10,]     0
    ##  [11,]    13
    ##  [12,]     8
    ##  [13,]     4
    ##  [14,]     0
    ##  [15,]     0
    ##  [16,]     0
    ##  [17,]     1
    ##  [18,]     9
    ##  [19,]     0
    ##  [20,]     0
    ##  [21,]     0
    ##  [22,]     0
    ##  [23,]     0
    ##  [24,]     0
    ##  [25,]     0
    ##  [26,]     0
    ##  [27,]     8
    ##  [28,]     0
    ##  [29,]     2
    ##  [30,]     0
    ##  [31,]     0
    ##  [32,]     0
    ##  [33,]     7
    ##  [34,]     0
    ##  [35,]     0
    ##  [36,]     0
    ##  [37,]     0
    ##  [38,]     0
    ##  [39,]     0
    ##  [40,]     0
    ##  [41,]     0
    ##  [42,]     0
    ##  [43,]     2
    ##  [44,]    11
    ##  [45,]     0
    ##  [46,]     0
    ##  [47,]     0
    ##  [48,]     0
    ##  [49,]     0
    ##  [50,]     0
    ##  [51,]     0
    ##  [52,]     0
    ##  [53,]     0
    ##  [54,]     0
    ##  [55,]     0
    ##  [56,]     0
    ##  [57,]     0
    ##  [58,]     0
    ##  [59,]     0
    ##  [60,]     0
    ##  [61,]     7
    ##  [62,]     0
    ##  [63,]     0
    ##  [64,]     0
    ##  [65,]     5
    ##  [66,]     0
    ##  [67,]     0
    ##  [68,]     0
    ##  [69,]     0
    ##  [70,]     0
    ##  [71,]     0
    ##  [72,]     0
    ##  [73,]     0
    ##  [74,]     0
    ##  [75,]     0
    ##  [76,]     1
    ##  [77,]     6
    ##  [78,]     9
    ##  [79,]     0
    ##  [80,]     0
    ##  [81,]     0
    ##  [82,]     8
    ##  [83,]     0
    ##  [84,]     0
    ##  [85,]     0
    ##  [86,]    16
    ##  [87,]     0
    ##  [88,]     0
    ##  [89,]     0
    ##  [90,]     4
    ##  [91,]     0
    ##  [92,]     0
    ##  [93,]     0
    ##  [94,]     0
    ##  [95,]     0
    ##  [96,]     0
    ##  [97,]     0
    ##  [98,]     7
    ##  [99,]     1
    ## [100,]     0
    ## [101,]     4
    ## [102,]     0
    ## [103,]     0
    ## [104,]     0
    ## [105,]     0
    ## [106,]     0
    ## [107,]     0
    ## [108,]     0
    ## [109,]     1
    ## [110,]     0
    ## [111,]     0
    ## [112,]     0
    ## [113,]     0
    ## [114,]     0
    ## [115,]    13
    ## [116,]     0
    ## [117,]     9
    ## [118,]    19
    ## [119,]    10
    ## [120,]    20
    ## [121,]    42
    ## [122,]    14
    ## [123,]     0
    ## [124,]    11
    ## [125,]     9
    ## [126,]     0
    ## [127,]     0
    ## [128,]    12
    ## [129,]     6
    ## [130,]     0
    ## [131,]     9
    ## [132,]     0
    ## [133,]    10
    ## [134,]     5
    ## [135,]    NA
    ## [136,]    NA
    ## [137,]    NA
    ## [138,]    NA
    ## [139,]    NA
    ## [140,]    NA
    ## [141,]    NA
    ## [142,]    NA
    ## [143,]    NA
    ## [144,]    NA
    ## [145,]    NA
    ## [146,]    NA
    ## [147,]    NA
    ## [148,]    NA
    ## [149,]    NA
    ## [150,]    NA
    ## [151,]    NA
    ## [152,]    NA
    ## [153,]    NA
    ## [154,]    NA
    ## [155,]    NA
    ## [156,]    NA
    ## [157,]    NA
    ## [158,]    NA
    ## [159,]    NA

### 3.3 Specify the `baselinenowcast` model

Next, we’ll specify the number of reference times used for delay and
uncertainty estimation as a factor of the maximum delay. We’ll also
specify the proportion of reference times which will be used for delay
estimation, with the remaining used for estimating uncertainty. We’ll
set these as the default values used in the, but in a real-world setting
we recommend performing an evaluation analysis to identify the optimal
amount of training data to be used for each of these tasks. See the
documentation for
[`?allocate_reference_times`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md)
and the “Default Settings” section of the [mathematical
model](https://baselinenowcast.epinowcast.org/articles/model_definition.md)
vignette for more details.

``` r
scale_factor <- 3
prop_delay <- 0.5
```

Internally, we will call
[`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md)
which uses the reporting triangle matrix and these specifications to
allocate the number of reference times used for delay and uncertainty
given the number of reference times available and the maximum delay.
Since our maximum delay is 25 days, this function will allocate 25\*3
(75) reference times for fitting the model, with about half of them (37)
being used for delay estimation and the other half (38) used for
uncertainty estimation.

## 4 Run the `baselinenowcast` workflow

To produce a nowcast, we can use the
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
function to generate an estimate of the “final” number of cases of BAR
by reference date. This function chains together the `baselinenowcast`
workflow, It handles allocating reference times for training
([`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md)),
estimating a delay
([`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)),
generating a point nowcast
([`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md)),
and estimating and applying uncertainty
([`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md)),
while also managing required bookkeeping to return nowcasts by reference
dates. See
[`?baselinenowcast.reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md)
for more details on each step in the workflow, and check out the
[mathematical_model](https://baselinenowcast.epinowcast.org/articles/model_definition.md)
for details on the mathematical model used for each component. The
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
function can ingest either a reporting triangle object as we made above,
or a data.frame with cases by reference and report date for one or
strata with support for common workflows such as sharing estimates
across strata. See
[`?baselinenowcast.data.frame`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md)
for more details.

By default, the `output_type = "samples"` which means that it will
estimate and apply uncertainty using past nowcast errors to generate
probabilistic nowcast draws. Point nowcasts can also be generated by
specifying `output_type = "point"`. It will by default produce 1000
draws, but this can be specified as more or less using the `draws`
argument.

``` r
nowcast_draws_df <- baselinenowcast(rep_tri,
  scale_factor = scale_factor,
  prop_delay = prop_delay,
  draws = 1000
)
```

    ## ℹ 0.5 reference times were specified for delay estimation but 0.493 of reference times used for delay estimation.

    ## ℹ `prop_delay` not identical to the proportion of reference times used for delay estimation due to rounding.

``` r
head(nowcast_draws_df)
```

    ##     pred_count draw reference_date output_type
    ## 1          382    1     2025-10-25     samples
    ## 160        382    2     2025-10-25     samples
    ## 319        382    3     2025-10-25     samples
    ## 478        382    4     2025-10-25     samples
    ## 637        382    5     2025-10-25     samples
    ## 796        382    6     2025-10-25     samples

Because we specified training volumes that did not result in integer
reference times, we’ll get a message letting us know that 37/75, or
0.493 of the reference times are being used for delay estimation.

## 5 Summarise and plot the nowcast

We now have an estimate of the “final” number of cases of BAR by
reference date with uncertainty. To summarise the uncertainty, we can
compute prediction intervals. Here we will visualize the results using
the 50th and 95th percent prediction intervals, though we suggest
showing more prediction intervals if possible.

``` r
nowcast_summary_df <-
  nowcast_draws_df |>
  group_by(reference_date) |>
  summarise(
    median = median(pred_count),
    q50th_lb = quantile(pred_count, 0.25),
    q50th_ub = quantile(pred_count, 0.75),
    q95th_lb = quantile(pred_count, 0.025),
    q95th_ub = quantile(pred_count, 0.975)
  )
```

In order to compare our estimates to the data that was available as of
the time of the nowcast and the “final” cases (which is what we were
trying to estimate), we will join both the initially reported cases and
the held out, true final cases to our probabilistic nowcasts. We already
summarised the initial reports by reference date from the data used to
train the nowcast model. Next summarise the final reports by reference
date using all the data.

``` r
eval_data <- long_df |>
  filter(
    delay <= max_delay,
    reference_date <= nowcast_date
  ) |>
  group_by(reference_date) |>
  summarise(final_count = sum(count))
```

Lastly, join the initial and final reports to the probabilistic nowcast.

``` r
nowcast_w_data <- nowcast_summary_df |>
  left_join(training_df_by_ref_date,
    by = "reference_date"
  ) |>
  left_join(eval_data,
    by = "reference_date"
  )
head(nowcast_w_data)
```

    ## # A tibble: 6 × 8
    ##   reference_date median q50th_lb q50th_ub q95th_lb q95th_ub initial_count
    ##   <date>          <dbl>    <dbl>    <dbl>    <dbl>    <dbl>         <dbl>
    ## 1 2025-10-25        382      382      382      382      382           402
    ## 2 2025-10-26        432      432      432      432      432           439
    ## 3 2025-10-27        351      351      351      351      351           363
    ## 4 2025-10-28        361      361      361      361      361           364
    ## 5 2025-10-29        426      426      426      426      426           442
    ## 6 2025-10-30        362      362      362      362      362           367
    ## # ℹ 1 more variable: final_count <dbl>

### 5.1 Plot nowcast against later observed “final” data

Next we will make a plot of the nowcasted “final” cases compared to the
initially reported cases and the true “final” reports. The plot shows
that, had we naively summarised the initial reports by reference date
(red), it would have appeared as if the recent cases of BAR were
declining rapidly. With our probabilistic nowcast (gray), we can see an
estimate of the true trend in real-time, which shows that the cases of
BAR are plateauing rather than continuing to decline. Compared to what
we eventually observe (black), we can see that our nowcasts appear
visually to have been relatively accurate. For more quantitative
evaluation, we recommend using proper scoring rules (e.g. using the
[`scoringutils` R package](https://epiforecasts.io/scoringutils/)) to
evaluate nowcasts.

Click to expand code to create the plot of the probabilistic nowcast

``` r
combined_data <- nowcast_w_data |>
  select(reference_date, initial_count, final_count) |>
  distinct() |>
  pivot_longer(
    cols = c(initial_count, final_count),
    names_to = "type",
    values_to = "count"
  ) |>
  mutate(type = case_when(
    type == "initial_count" ~ "Initially observed data",
    type == "final_count" ~ "Final observed data"
  )) |>
  filter(reference_date >= nowcast_date - days(60))

nowcast_data_recent <- nowcast_w_data |>
  filter(reference_date >= nowcast_date - days(60))

plot_prob_nowcast <- ggplot(nowcast_data_recent) +
  geom_line(
    aes(
      x = reference_date, y = median
    ),
    color = "gray"
  ) +
  geom_ribbon(
    aes(
      x = reference_date,
      ymin = q50th_lb, ymax = q50th_ub
    ),
    alpha = 0.5,
    fill = "gray"
  ) +
  geom_ribbon(
    aes(
      x = reference_date,
      ymin = q95th_lb, ymax = q95th_ub
    ),
    alpha = 0.5,
    fill = "gray"
  ) +
  # Add observed data and final data once
  geom_line(
    data = combined_data,
    aes(
      x = reference_date,
      y = count,
      color = type
    )
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Initially observed data" = "darkred",
      "Final observed data" = "black"
    ),
    name = ""
  ) +
  xlab("Date of ED visit") +
  ylab("Number of BAR cases") +
  theme(legend.position = "bottom") +
  ggtitle("Comparison of cases of BAR as of the nowcast date, later observed,\n and generated as a probabilistic nowcast") # nolint
```

``` r
plot_prob_nowcast
```

![](nssp_nowcast_files/figure-html/unnamed-chunk-32-1.png)

## 6 Summary

In this vignette we used `baselinenowcast` to nowcast cases of BAR
starting from synthetic syndromic surveillance system data designed to
mirror the U.S. NSSP dataset. We walked through the process of defining
a syndromic surveillance definition using a list of diagnoses codes, and
using the time stamps of updates in an electronic health record in the
NSSP dataset to create a count of the number of cases of a specific
definition indexed by the date of their visit and the date at which the
patient’s diagnoses was recorded into the surveillance system. From
there we converted the data to a `reporting_triangle` object and then
ran the `baselinenowcast` workflow using the
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
function to generate probabilistic nowcasts. This function chains
together multiple steps, and we describe the modular workflow option in
the [Getting
Started](https://baselinenowcast.epinowcast.org/articles/baselinenowcst.md)
vignette. As a final step, we compared our nowcasts of the eventual
final observed case counts to what we later observed and the
right-truncated initial reports.

Next steps include scoring the nowcasts we generated using proper
scoring rules such as the weighted interval score (WIS) or the
continuous ranked probability score (CRPS) and computing metrics such as
the interval coverage to assess how well the observed data falls within
our uncertainty bands. See the [`scoringutils` R
package](https://baselinenowcast.epinowcast.org/articles/epiforecasts.io/scoringutils/)
for resources on scoring predictions. In this vignette we used the
package’s default settings to specify the model, but the optimal
settings will be dependent on the context and its important to tune the
model for your dataset and needs. The user has a number of choices in
how to specify the model, such as the amount of training data to use for
delay or uncertainty estimation, the choice of observation model,
whether to separately estimate delays by weekday, or whether to borrow
estimates from across different strata such as age groups or locations.
See the [Getting
Started](https://baselinenowcast.epinowcast.org/articles/baselinenowcast.md)
vignette and [model
definition](https://baselinenowcast.epinowcast.org/articles/model_definition.md)
for more details on the different model specifications. In our
[publication](https://wellcomeopenresearch.org/articles/10-614/v1) we
show examples using the various model specifications to produce and
evaluate the performance of age-group specific nowcasts of COVID-19 in
Germany and norovirus cases in England. Here’s a
[link](https://github.com/epinowcast/baselinenowcast-paper) to the code
used to generate those nowcasts if interested in doing something similar
for your own settings.

Visual inspection of the nowcasts produced, as well as visual inspection
of the data such as the mean reporting delay over time and across
weekdays, can help identify which specifications are most likely to
improve performance. We encourage users to test the performance of
different specifications of their model, ideally by producing nowcasts
from different model specifications for a range of past nowcast dates,
using the data that would have been available as of the past nowcast
date, and comparing those nowcasts to later observed data. You could do
this just as we did here when we filtered the long tidy data.frame
indexed by reference and report date, to remove report dates before the
nowcast date.
