# Isolated benchmark: Diagnosis code vectorization speedup
# This script isolates the performance improvement from JUST vectorizing
# the diagnosis code filtering, independent of the pivot optimization

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
cat("ISOLATED BENCHMARK: DIAGNOSIS CODE VECTORIZATION SPEEDUP\n")
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
# SHARED PREPROCESSING FUNCTION (using optimized direct parsing)
# This creates the same intermediate data for both filtering approaches
# ============================================================================
preprocess_to_nssp_updates <- function(data) {
  # Use optimized direct parsing (NOT the slow wide pivot)
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

  # Parse both columns
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

  # Clean timestamps
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

  # Clean diagnoses
  syn_nssp_diagnoses <-
    syn_nssp_diagnoses_long |>
    filter(nzchar(diagnoses_codes)) |>
    filter(!is.na(diagnoses_codes)) |>
    select(event_id, diagnoses_codes)

  # Merge and filter
  nssp_merged <- merge(syn_nssp_time_stamps,
    syn_nssp_diagnoses,
    by = "event_id"
  ) |>
    filter(diagnoses_codes != ";;|")

  # Add delay
  nssp_updates <- nssp_merged |>
    mutate(arrival_to_update_delay = as.numeric(difftime(
      time_stamp, C_Visit_Date_Time,
      units = "days"
    )))

  return(nssp_updates)
}

# ============================================================================
# FILTERING APPROACH 1: ROW-BY-ROW (ORIGINAL)
# ============================================================================
filter_diagnoses_rowwise <- function(nssp_updates) {
  # Original approach: iterate through each row
  bar_updates <- nssp_updates |>
    filter(map_lgl(diagnoses_codes, ~ any(str_detect(.x, diagnoses_codes_defn))))

  return(nrow(bar_updates))
}

# ============================================================================
# FILTERING APPROACH 2: VECTORIZED (OPTIMIZED)
# ============================================================================
filter_diagnoses_vectorized <- function(nssp_updates) {
  # Optimized approach: single regex pattern, vectorized
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
  preprocessing_time = numeric(),
  rowwise_filtering_time = numeric(),
  vectorized_filtering_time = numeric(),
  stringsAsFactors = FALSE
)

cat("Running isolated benchmarks...\n\n")

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  n_rows <- nrow(dataset)

  cat("Testing", dataset_name, "dataset (", n_rows, "rows)...\n")

  # Step 1: Preprocess (timed separately to show it's not the bottleneck being tested)
  cat("  Preprocessing to nssp_updates...")
  preprocess_times <- replicate(3, {
    system.time({
      nssp_updates <- preprocess_to_nssp_updates(dataset)
    })["elapsed"]
  })
  preprocess_time <- mean(preprocess_times)
  cat(" Done! (", round(preprocess_time, 4), "s avg)\n")

  # Create nssp_updates once for filtering tests
  nssp_updates <- preprocess_to_nssp_updates(dataset)

  # Step 2: Benchmark row-by-row filtering
  cat("  Row-by-row filtering...")
  rowwise_times <- replicate(5, {
    system.time({
      result <- filter_diagnoses_rowwise(nssp_updates)
    })["elapsed"]
  })
  rowwise_time <- mean(rowwise_times)
  cat(" Done! (", round(rowwise_time, 4), "s avg)\n")

  # Step 3: Benchmark vectorized filtering
  cat("  Vectorized filtering...")
  vectorized_times <- replicate(5, {
    system.time({
      result <- filter_diagnoses_vectorized(nssp_updates)
    })["elapsed"]
  })
  vectorized_time <- mean(vectorized_times)
  cat(" Done! (", round(vectorized_time, 4), "s avg)\n\n")

  # Store results
  results <- rbind(results,
    data.frame(
      dataset = dataset_name,
      n_rows = n_rows,
      preprocessing_time = preprocess_time,
      rowwise_filtering_time = rowwise_time,
      vectorized_filtering_time = vectorized_time
    )
  )
}

# ============================================================================
# RESULTS
# ============================================================================
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("DIAGNOSIS VECTORIZATION SPEEDUP RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Calculate speedup
results <- results |>
  mutate(
    filtering_speedup = rowwise_filtering_time / vectorized_filtering_time,
    filtering_improvement_pct = (rowwise_filtering_time - vectorized_filtering_time) / rowwise_filtering_time * 100,
    filtering_time_saved = rowwise_filtering_time - vectorized_filtering_time
  )

cat("ISOLATED DIAGNOSIS FILTERING PERFORMANCE:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
cat(sprintf("%-10s | %6s | %10s | %10s | %8s | %10s\n",
            "Dataset", "Rows", "Row-by-Row", "Vectorized", "Speedup", "Saved (s)"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in 1:nrow(results)) {
  row <- results[i, ]
  cat(sprintf("%-10s | %6d | %8.4fs | %8.4fs | %6.1fx | %8.4fs\n",
              row$dataset,
              row$n_rows,
              row$rowwise_filtering_time,
              row$vectorized_filtering_time,
              row$filtering_speedup,
              row$filtering_time_saved))
}

cat(paste(rep("-", 80), collapse = ""), "\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("• Vectorization speedup on small dataset  (25 rows):   %.1fx\n",
            results$filtering_speedup[results$dataset == "small"]))
cat(sprintf("• Vectorization speedup on medium dataset (500 rows):  %.1fx\n",
            results$filtering_speedup[results$dataset == "medium"]))
cat(sprintf("• Vectorization speedup on large dataset  (2500 rows): %.1fx\n",
            results$filtering_speedup[results$dataset == "large"]))
cat("\n")

cat("INTERPRETATION:\n")
cat("This benchmark isolates ONLY the diagnosis code filtering step.\n")
cat("The preprocessing (string parsing) is identical for both approaches.\n")
cat("Speedup shows the improvement from vectorized str_detect() vs map_lgl().\n")
cat("\n")

cat("Note: Row-by-row filtering time increases with:\n")
cat("  1. Number of records to filter\n")
cat("  2. Number of diagnosis codes in the pattern\n")
cat("  3. Complexity of regex matching\n")
cat("\n")

# Save results
write.csv(results, "diagnosis_vectorization_results.csv", row.names = FALSE)
cat("✓ Results saved to: diagnosis_vectorization_results.csv\n")
