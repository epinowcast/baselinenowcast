# Profile optimized NSSP preprocessing code (from optimize branch)
library(baselinenowcast)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(purrr)
library(data.table)
library(profvis)

# Load data
data("syn_nssp_line_list")

# Define diagnosis codes
diagnoses_codes_defn <- c("A22.1", "A221", "A37", "A48.1", "A481", "B25.0", "B250", "B34.2", "B34.9", "B342", "B349", "B44.0", "B44.9", "B440", "B449", "B44.81", "B4481", "B97.2", "B97.4", "B972", "B974", "J00", "J01", "J02", "J03", "J04", "J05", "J06", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18", "J20", "J21", "J22", "J39.8", "J398", "J40", "J47.9", "J479", "J80", "J85.1", "J851", "J95.821", "J95821", "J96.0", "J96.00", "J9600", "J96.01", "J9601", "J96.02", "J9602", "J96.2", "J960", "J962", "J96.20", "J9620", "J96.21", "J9621", "J9622", "J96.22", "J96.91", "J9691", "J98.8", "J988", "R05", "R06.03", "R0603", "R09.02", "R0902", "R09.2", "R092", "R43.0", "R43.1", "R43.2", "R430", "R431", "R432", "U07.1", "U07.2", "U071", "U072", "022.1", "0221", "034.0", "0340", "041.5", "0415", "041.81", "04181", "079.1", "079.2", "079.3", "079.6", "0791", "0792", "0793", "0796", "079.82", "079.89", "07982", "07989", "079.99", "07999", "117.3", "1173", "460", "461", "462", "463", "464", "465", "466", "461.", "461", "461.", "464.", "465.", "466.", "461", "464", "465", "466", "478.9", "4789", "480.", "482.", "483.", "484.", "487.", "488.", "480", "481", "482", "483", "484", "485", "486", "487", "488", "490", "494.1", "4941", "517.1", "5171", "518.51", "518.53", "51851", "51853", "518.6", "5186", "518.81", "518.82", "518.84", "51881", "51882", "51884", "519.8", "5198", "073.0", "0730", "781.1", "7811", "786.2", "7862", "799.02", "79902", "799.1", "7991", "033", "033.", "033", "780.60", "78060")

# Optimized preprocessing function
parse_events_to_long <- function(line_list, event_col_name, id_col_name) {
  # Use data.table for efficient row expansion
  dt <- as.data.table(line_list)

  # Split events by "{" delimiter and create long format
  long_dt <- dt[, .(
    event_string = unlist(strsplit(.SD[[event_col_name]], "\\{", fixed = FALSE))
  ),
  by = c(id_col_name, "C_Visit_Date_Time"),
  .SDcols = event_col_name
  ][nzchar(event_string)] # Remove empty strings

  # Extract event number from the string (format is: "1};content" or "1;content")
  long_dt[, event_num := as.numeric(sub("[};].*", "", event_string))]

  # Create event_id combining patient ID and event number
  long_dt[, event_id := paste(.SD[[1]], event_num), .SDcols = id_col_name]

  # Extract the content after the event number and delimiter (either }; or ;)
  long_dt[, content := sub("^[0-9]+[};]+", "", event_string)]
  long_dt[, content := sub("\\|$", "", content)] # Remove trailing |
  long_dt[, content := sub(";+$", "", content)] # Remove trailing semicolons

  # Convert back to tibble for dplyr compatibility
  result <- as_tibble(long_dt[, .(
    get(id_col_name),
    C_Visit_Date_Time,
    event_id,
    event_num,
    content
  )])

  # Rename first column back to original ID name
  names(result)[1] <- id_col_name

  return(result)
}

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("PROFILING OPTIMIZED NSSP PREPROCESSING CODE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Dataset size:", nrow(syn_nssp_line_list), "rows\n\n")

# Function to run the complete preprocessing
run_optimized_preprocessing <- function() {
  # Step 1: Parse directly to long format (OPTIMIZED)
  syn_nssp_time_stamps_long <- parse_events_to_long(
    line_list = syn_nssp_line_list,
    event_col_name = "DischargeDiagnosisMDTUpdates",
    id_col_name = "C_Processed_BioSense_ID"
  ) |>
    rename(time_stamp = content)

  syn_nssp_diagnoses_long <- parse_events_to_long(
    line_list = syn_nssp_line_list,
    event_col_name = "DischargeDiagnosisUpdates",
    id_col_name = "C_Processed_BioSense_ID"
  ) |>
    rename(diagnoses_codes = content)

  # Step 2: Clean timestamps
  syn_nssp_time_stamps <-
    syn_nssp_time_stamps_long |>
    mutate(
      time_stamp = as.POSIXct(
        str_remove_all(time_stamp, "[;]+$"), # Remove trailing semicolons
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      C_Visit_Date_Time = as.POSIXct(C_Visit_Date_Time)
    ) |>
    filter(!is.na(time_stamp))

  # Step 3: Clean diagnoses
  syn_nssp_diagnoses <-
    syn_nssp_diagnoses_long |>
    filter(nzchar(diagnoses_codes)) |>
    filter(!is.na(diagnoses_codes)) |>
    select(event_id, diagnoses_codes)

  # Step 4: Merge
  nssp_merged <- merge(syn_nssp_time_stamps,
    syn_nssp_diagnoses,
    by = "event_id"
  ) |>
    filter(diagnoses_codes != ";;|")

  # Step 5: Add delay
  nssp_updates <- nssp_merged |>
    mutate(arrival_to_update_delay = as.numeric(difftime(
      time_stamp, C_Visit_Date_Time,
      units = "days"
    )))

  # Step 6: Filter for BAR diagnoses (OPTIMIZED)
  diagnosis_pattern <- paste(diagnoses_codes_defn, collapse = "|")
  bar_updates <- nssp_updates |>
    filter(str_detect(diagnoses_codes, diagnosis_pattern))

  # Step 7: Keep first diagnosis
  first_bar_diagnosis <- bar_updates |>
    arrange(arrival_to_update_delay) |>
    group_by(C_Processed_BioSense_ID) |>
    slice(1)

  return(first_bar_diagnosis)
}

# Benchmark the code
cat("Running benchmark (5 iterations)...\n")
times <- replicate(5, {
  system.time(run_optimized_preprocessing())["elapsed"]
})

cat("\nBenchmark Results:\n")
cat("Min:", min(times), "seconds\n")
cat("Median:", median(times), "seconds\n")
cat("Mean:", mean(times), "seconds\n")
cat("Max:", max(times), "seconds\n")

# Profile the preprocessing
cat("\nRunning detailed profiling...\n")
prof <- profvis({
  result <- run_optimized_preprocessing()
})

# Save profiling results
print(prof)
htmlwidgets::saveWidget(prof, "profile_optimized.html")

cat("\n✓ Profiling completed!\n")
cat("Results saved to: profile_optimized.html\n")
cat("Open profile_optimized.html in a browser to see detailed profiling visualization\n")
