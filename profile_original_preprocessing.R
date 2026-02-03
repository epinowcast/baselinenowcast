# Profile original NSSP preprocessing code (from main branch)
library(baselinenowcast)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(purrr)
library(profvis)

# Load data
data("syn_nssp_line_list")

# Define diagnosis codes
diagnoses_codes_defn <- c("A22.1", "A221", "A37", "A48.1", "A481", "B25.0", "B250", "B34.2", "B34.9", "B342", "B349", "B44.0", "B44.9", "B440", "B449", "B44.81", "B4481", "B97.2", "B97.4", "B972", "B974", "J00", "J01", "J02", "J03", "J04", "J05", "J06", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18", "J20", "J21", "J22", "J39.8", "J398", "J40", "J47.9", "J479", "J80", "J85.1", "J851", "J95.821", "J95821", "J96.0", "J96.00", "J9600", "J96.01", "J9601", "J96.02", "J9602", "J96.2", "J960", "J962", "J96.20", "J9620", "J96.21", "J9621", "J9622", "J96.22", "J96.91", "J9691", "J98.8", "J988", "R05", "R06.03", "R0603", "R09.02", "R0902", "R09.2", "R092", "R43.0", "R43.1", "R43.2", "R430", "R431", "R432", "U07.1", "U07.2", "U071", "U072", "022.1", "0221", "034.0", "0340", "041.5", "0415", "041.81", "04181", "079.1", "079.2", "079.3", "079.6", "0791", "0792", "0793", "0796", "079.82", "079.89", "07982", "07989", "079.99", "07999", "117.3", "1173", "460", "461", "462", "463", "464", "465", "466", "461.", "461", "461.", "464.", "465.", "466.", "461", "464", "465", "466", "478.9", "4789", "480.", "482.", "483.", "484.", "487.", "488.", "480", "481", "482", "483", "484", "485", "486", "487", "488", "490", "494.1", "4941", "517.1", "5171", "518.51", "518.53", "51851", "51853", "518.6", "5186", "518.81", "518.82", "518.84", "51881", "51882", "51884", "519.8", "5198", "073.0", "0730", "781.1", "7811", "786.2", "7862", "799.02", "79902", "799.1", "7991", "033", "033.", "033", "780.60", "78060")

# Original preprocessing functions
expand_events <- function(line_list, event_col_name) {
  wide_line_list <- separate_wider_delim(line_list,
    {{ event_col_name }},
    delim = "{", names_sep = "", too_few = "align_start"
  )
  return(wide_line_list)
}

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

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("PROFILING ORIGINAL NSSP PREPROCESSING CODE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Dataset size:", nrow(syn_nssp_line_list), "rows\n\n")

# Function to run the complete preprocessing
run_original_preprocessing <- function() {
  # Step 1: Expand to wide
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

  # Step 2: Convert wide to long
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

  # Step 3: Clean timestamps
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

  # Step 4: Clean diagnoses
  syn_nssp_diagnoses <-
    syn_nssp_diagnoses_long |>
    mutate(diagnoses_codes = str_remove(diagnoses_codes, ".*\\}")) |>
    filter(nzchar(diagnoses_codes)) |>
    drop_na() |>
    select(event_id, diagnoses_codes)

  # Step 5: Merge
  nssp_merged <- merge(syn_nssp_time_stamps,
    syn_nssp_diagnoses,
    by = "event_id"
  ) |>
    filter(diagnoses_codes != ";;|")

  # Step 6: Add delay
  nssp_updates <- nssp_merged |>
    mutate(arrival_to_update_delay = as.numeric(difftime(
      time_stamp, C_Visit_Date_Time,
      units = "days"
    )))

  # Step 7: Filter for BAR diagnoses (ORIGINAL METHOD)
  bar_updates <- nssp_updates |>
    filter(map_lgl(diagnoses_codes, ~ any(str_detect(.x, diagnoses_codes_defn))))

  # Step 8: Keep first diagnosis
  first_bar_diagnosis <- bar_updates |>
    arrange(arrival_to_update_delay) |>
    group_by(C_Processed_BioSense_ID) |>
    slice(1)

  return(first_bar_diagnosis)
}

# Benchmark the code
cat("Running benchmark (5 iterations)...\n")
times <- replicate(5, {
  system.time(run_original_preprocessing())["elapsed"]
})

cat("\nBenchmark Results:\n")
cat("Min:", min(times), "seconds\n")
cat("Median:", median(times), "seconds\n")
cat("Mean:", mean(times), "seconds\n")
cat("Max:", max(times), "seconds\n")

# Profile the preprocessing
cat("\nRunning detailed profiling...\n")
prof <- profvis({
  result <- run_original_preprocessing()
})

# Save profiling results
print(prof)
htmlwidgets::saveWidget(prof, "profile_original.html")

cat("\n✓ Profiling completed!\n")
cat("Results saved to: profile_original.html\n")
cat("Open profile_original.html in a browser to see detailed profiling visualization\n")
