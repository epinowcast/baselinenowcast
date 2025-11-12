#!/usr/bin/env Rscript
# Benchmark script for baselinenowcast.data.frame example
# Profiles the example to identify performance bottlenecks

# Load package from source
devtools::load_all()

# Setup data as in the example
data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
data_as_of$age_group <- "Total"

cat("Data dimensions:\n")
cat("  Rows:", nrow(data_as_of), "\n")
cat("  Reference dates:",
  length(unique(data_as_of$reference_date)), "\n")
cat("  Max delay:",
  max(data_as_of$report_date - data_as_of$reference_date), "\n\n")

cat("Starting profiling...\n\n")

# Profile the example
p <- profvis::profvis({
  nowcast_df <- baselinenowcast(
    data = data_as_of,
    max_delay = 40,
    draws = 100
  )
})

# Print the profvis object (will show in RStudio or save as HTML)
print(p)

# Also do a simple timing benchmark
cat("\nTiming benchmark (5 runs):\n")
times <- numeric(5)
for (i in 1:5) {
  cat("  Run", i, "... ")
  start_time <- Sys.time()
  nowcast_df <- baselinenowcast(
    data = data_as_of,
    max_delay = 40,
    draws = 100
  )
  end_time <- Sys.time()
  elapsed <- as.numeric(end_time - start_time, units = "secs")
  times[i] <- elapsed
  cat(sprintf("%.2f seconds\n", elapsed))
}

cat("\nSummary:\n")
cat(sprintf("  Mean: %.2f seconds\n", mean(times)))
cat(sprintf("  Median: %.2f seconds\n", median(times)))
cat(sprintf("  Min: %.2f seconds\n", min(times)))
cat(sprintf("  Max: %.2f seconds\n", max(times)))

# Save profiling results
htmlwidgets::saveWidget(p, "baselinenowcast_profile.html")
cat("\nProfile saved to: baselinenowcast_profile.html\n")
