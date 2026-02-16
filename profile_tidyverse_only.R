# Benchmark: Can we avoid data.table dependency?
# This script tests if we can get most of the speedup by ONLY vectorizing
# the diagnosis filtering, while keeping the tidyverse wide-pivot approach

library(baselinenowcast)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(data.table)

# Load data
data("syn_nssp_line_list")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("BENCHMARK: TIDYVERSE-ONLY vs DATA.TABLE OPTIMIZATION\n")
cat("Question: Can we avoid data.table dependency?\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Create test datasets
cat("Creating test datasets...\n")
datasets <- list(
  small = syn_nssp_line_list,  # 25 rows
  medium = bind_rows(replicate(20, syn_nssp_line_list, simplify = FALSE)),  # 500 rows
  large = bind_rows(replicate(100, syn_nssp_line_list, simplify = FALSE))  # 2500 rows
)

cat("Dataset sizes:\n")
cat("  Small:", nrow(datasets$small), "rows\n")
cat("  Medium:", nrow(datasets$medium), "rows\n")
cat("  Large:", nrow(datasets$large), "rows\n\n")

# Define diagnosis codes
diagnoses_codes_defn <- c("U071", "U07.1", "J00", "J06")  # Simplified for testing

# ============================================================================
# APPROACH 1: ORIGINAL (WIDE PIVOT + ROW-BY-ROW FILTERING)
# ============================================================================
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

run_original <- function(data) {
  # Expand to wide
  syn_nssp_time_stamps_wide <- expand_events(
    line_list = data,
    event_col_name = "DischargeDiagnosisMDTUpdates"
  ) |>
    select(-DischargeDiagnosisUpdates)

  syn_nssp_diagnoses_wide <- expand_events(
    line_list = data,
    event_col_name = "DischargeDiagnosisUpdates"
  ) |>
    select(-DischargeDiagnosisMDTUpdates)

  # Convert wide to long
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

  # Clean
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

  syn_nssp_diagnoses <-
    syn_nssp_diagnoses_long |>
    mutate(diagnoses_codes = str_remove(diagnoses_codes, ".*\\}")) |>
    filter(nzchar(diagnoses_codes)) |>
    drop_na() |>
    select(event_id, diagnoses_codes)

  # Merge and filter
  nssp_merged <- merge(syn_nssp_time_stamps,
    syn_nssp_diagnoses,
    by = "event_id"
  ) |>
    filter(diagnoses_codes != ";;|")

  nssp_updates <- nssp_merged |>
    mutate(arrival_to_update_delay = as.numeric(difftime(
      time_stamp, C_Visit_Date_Time,
      units = "days"
    )))

  # ORIGINAL: row-by-row filtering
  bar_updates <- nssp_updates |>
    filter(map_lgl(diagnoses_codes, ~ any(str_detect(.x, diagnoses_codes_defn))))

  return(nrow(bar_updates))
}

# ============================================================================
# APPROACH 2: TIDYVERSE-ONLY OPTIMIZATION (WIDE PIVOT + VECTORIZED FILTERING)
# This keeps the wide pivot but uses vectorized diagnosis filtering
# ============================================================================
run_tidyverse_optimized <- function(data) {
  # Same preprocessing as original (uses wide pivot)
  syn_nssp_time_stamps_wide <- expand_events(
    line_list = data,
    event_col_name = "DischargeDiagnosisMDTUpdates"
  ) |>
    select(-DischargeDiagnosisUpdates)

  syn_nssp_diagnoses_wide <- expand_events(
    line_list = data,
    event_col_name = "DischargeDiagnosisUpdates"
  ) |>
    select(-DischargeDiagnosisMDTUpdates)

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

  syn_nssp_diagnoses <-
    syn_nssp_diagnoses_long |>
    mutate(diagnoses_codes = str_remove(diagnoses_codes, ".*\\}")) |>
    filter(nzchar(diagnoses_codes)) |>
    drop_na() |>
    select(event_id, diagnoses_codes)

  nssp_merged <- merge(syn_nssp_time_stamps,
    syn_nssp_diagnoses,
    by = "event_id"
  ) |>
    filter(diagnoses_codes != ";;|")

  nssp_updates <- nssp_merged |>
    mutate(arrival_to_update_delay = as.numeric(difftime(
      time_stamp, C_Visit_Date_Time,
      units = "days"
    )))

  # OPTIMIZED: vectorized diagnosis filtering (NO data.table needed!)
  diagnosis_pattern <- paste(diagnoses_codes_defn, collapse = "|")
  bar_updates <- nssp_updates |>
    filter(str_detect(diagnoses_codes, diagnosis_pattern))

  return(nrow(bar_updates))
}

# ============================================================================
# APPROACH 3: DATA.TABLE OPTIMIZATION (DIRECT PARSING + VECTORIZED FILTERING)
# ============================================================================
parse_events_to_long <- function(line_list, event_col_name, id_col_name) {
  dt <- as.data.table(line_list)

  long_dt <- dt[, .(
    event_string = unlist(strsplit(.SD[[event_col_name]], "\\{", fixed = FALSE))
  ),
  by = c(id_col_name, "C_Visit_Date_Time"),
  .SDcols = event_col_name
  ][nzchar(event_string)]

  long_dt[, event_num := as.numeric(sub("[};].*", "", event_string))]
  long_dt[, event_id := paste(.SD[[1]], event_num), .SDcols = id_col_name]
  long_dt[, content := sub("^[0-9]+[};]+", "", event_string)]
  long_dt[, content := sub("\\|$", "", content)]
  long_dt[, content := sub(";+$", "", content)]

  result <- as_tibble(long_dt[, .(
    get(id_col_name),
    C_Visit_Date_Time,
    event_id,
    event_num,
    content
  )])

  names(result)[1] <- id_col_name
  return(result)
}

run_datatable_optimized <- function(data) {
  # Direct parsing (no wide pivot)
  syn_nssp_time_stamps_long <- parse_events_to_long(
    line_list = data,
    event_col_name = "DischargeDiagnosisMDTUpdates",
    id_col_name = "C_Processed_BioSense_ID"
  ) |>
    rename(time_stamp = content)

  syn_nssp_diagnoses_long <- parse_events_to_long(
    line_list = data,
    event_col_name = "DischargeDiagnosisUpdates",
    id_col_name = "C_Processed_BioSense_ID"
  ) |>
    rename(diagnoses_codes = content)

  syn_nssp_time_stamps <-
    syn_nssp_time_stamps_long |>
    mutate(
      time_stamp = as.POSIXct(
        str_remove_all(time_stamp, "[;]+$"),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      C_Visit_Date_Time = as.POSIXct(C_Visit_Date_Time)
    ) |>
    filter(!is.na(time_stamp))

  syn_nssp_diagnoses <-
    syn_nssp_diagnoses_long |>
    filter(nzchar(diagnoses_codes)) |>
    filter(!is.na(diagnoses_codes)) |>
    select(event_id, diagnoses_codes)

  nssp_merged <- merge(syn_nssp_time_stamps,
    syn_nssp_diagnoses,
    by = "event_id"
  ) |>
    filter(diagnoses_codes != ";;|")

  nssp_updates <- nssp_merged |>
    mutate(arrival_to_update_delay = as.numeric(difftime(
      time_stamp, C_Visit_Date_Time,
      units = "days"
    )))

  # Vectorized diagnosis filtering
  diagnosis_pattern <- paste(diagnoses_codes_defn, collapse = "|")
  bar_updates <- nssp_updates |>
    filter(str_detect(diagnoses_codes, diagnosis_pattern))

  return(nrow(bar_updates))
}

# ============================================================================
# BENCHMARKING
# ============================================================================
results <- data.frame(
  dataset = character(),
  n_rows = integer(),
  original = numeric(),
  tidyverse_only = numeric(),
  datatable = numeric(),
  stringsAsFactors = FALSE
)

cat("Running benchmarks...\n\n")

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  n_rows <- nrow(dataset)

  cat("Testing", dataset_name, "dataset (", n_rows, "rows)...\n")

  # Original
  cat("  Original (wide pivot + row-by-row)...")
  original_times <- replicate(3, {
    system.time(run_original(dataset))["elapsed"]
  })
  cat(" Done!\n")

  # Tidyverse-only optimization
  cat("  Tidyverse-only (wide pivot + vectorized)...")
  tidyverse_times <- replicate(3, {
    system.time(run_tidyverse_optimized(dataset))["elapsed"]
  })
  cat(" Done!\n")

  # data.table optimization
  cat("  data.table (direct parsing + vectorized)...")
  datatable_times <- replicate(3, {
    system.time(run_datatable_optimized(dataset))["elapsed"]
  })
  cat(" Done!\n\n")

  # Store results
  results <- rbind(results,
    data.frame(
      dataset = dataset_name,
      n_rows = n_rows,
      original = mean(original_times),
      tidyverse_only = mean(tidyverse_times),
      datatable = mean(datatable_times)
    )
  )
}

# ============================================================================
# RESULTS
# ============================================================================
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("COMPARISON: TIDYVERSE-ONLY vs DATA.TABLE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Calculate speedups
results <- results |>
  mutate(
    tidyverse_speedup = original / tidyverse_only,
    datatable_speedup = original / datatable,
    tidyverse_improvement = (original - tidyverse_only) / original * 100,
    datatable_improvement = (original - datatable) / original * 100,
    tidyverse_vs_datatable = tidyverse_only / datatable
  )

cat("PERFORMANCE COMPARISON:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
cat(sprintf("%-10s | %6s | %10s | %10s | %10s\n",
            "Dataset", "Rows", "Original", "Tidyverse", "data.table"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in 1:nrow(results)) {
  row <- results[i, ]
  cat(sprintf("%-10s | %6d | %8.3fs | %8.3fs | %8.3fs\n",
              row$dataset,
              row$n_rows,
              row$original,
              row$tidyverse_only,
              row$datatable))
}

cat(paste(rep("-", 80), collapse = ""), "\n\n")

cat("SPEEDUP vs ORIGINAL:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
cat(sprintf("%-10s | %6s | %15s | %15s\n",
            "Dataset", "Rows", "Tidyverse", "data.table"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in 1:nrow(results)) {
  row <- results[i, ]
  cat(sprintf("%-10s | %6d | %11.1fx (%4.1f%%) | %11.1fx (%4.1f%%)\n",
              row$dataset,
              row$n_rows,
              row$tidyverse_speedup,
              row$tidyverse_improvement,
              row$datatable_speedup,
              row$datatable_improvement))
}

cat(paste(rep("-", 80), collapse = ""), "\n\n")

cat("TIDYVERSE-ONLY vs DATA.TABLE:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
cat(sprintf("%-10s | %6s | %15s | %20s\n",
            "Dataset", "Rows", "Ratio", "Interpretation"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in 1:nrow(results)) {
  row <- results[i, ]
  interpretation <- if (row$tidyverse_vs_datatable < 1.2) {
    "Minimal difference"
  } else if (row$tidyverse_vs_datatable < 2.0) {
    "data.table faster"
  } else {
    "data.table much faster"
  }
  cat(sprintf("%-10s | %6d | %11.2fx | %20s\n",
              row$dataset,
              row$n_rows,
              row$tidyverse_vs_datatable,
              interpretation))
}

cat(paste(rep("-", 80), collapse = ""), "\n\n")

cat("KEY FINDINGS:\n\n")

avg_tidyverse_speedup <- mean(results$tidyverse_speedup[results$dataset != "small"])
avg_datatable_speedup <- mean(results$datatable_speedup[results$dataset != "small"])
avg_ratio <- mean(results$tidyverse_vs_datatable[results$dataset != "small"])

cat(sprintf("1. Tidyverse-only optimization (vectorized filtering): %.1fx speedup\n", avg_tidyverse_speedup))
cat(sprintf("2. data.table optimization (+ direct parsing):        %.1fx speedup\n", avg_datatable_speedup))
cat(sprintf("3. Tidyverse-only is %.2fx slower than data.table\n\n", avg_ratio))

if (avg_ratio < 1.5) {
  cat("RECOMMENDATION: ✅ Use tidyverse-only approach\n")
  cat("- Avoids new data.table dependency\n")
  cat("- Achieves most of the speedup (vectorized filtering is the key optimization)\n")
  cat("- The additional speedup from data.table is minimal (<50%%)\n")
  cat("- Keeps codebase consistent with existing tidyverse style\n")
} else if (avg_ratio < 2.5) {
  cat("RECOMMENDATION: ⚖️  Consider trade-offs\n")
  cat("- Tidyverse-only gets most of the speedup\n")
  cat("- data.table provides noticeable additional improvement (%.1fx)\n", avg_ratio)
  cat("- Decision depends on: dataset size, performance requirements, dependency preferences\n")
} else {
  cat("RECOMMENDATION: 🚀 Use data.table approach\n")
  cat("- Significantly faster (%.1fx) than tidyverse-only\n", avg_ratio)
  cat("- Worth the additional dependency for large datasets\n")
  cat("- Direct parsing avoids expensive pivot operations\n")
}

cat("\n")
cat("SUMMARY:\n")
cat("The diagnosis vectorization (using paste() + str_detect()) provides the MAJORITY\n")
cat("of the speedup and does NOT require data.table. The question is whether the\n")
cat("additional speedup from avoiding the wide pivot is worth the new dependency.\n")

# Save results
write.csv(results, "tidyverse_vs_datatable_comparison.csv", row.names = FALSE)
cat("\n✓ Results saved to: tidyverse_vs_datatable_comparison.csv\n")
