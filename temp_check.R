devtools::load_all()
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
data_as_of_df <- data_as_of_df[
  as.numeric(data_as_of_df$report_date - data_as_of_df$reference_date) <= 25,
]
test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]
rep_tri <- suppressMessages(as_reporting_triangle(test))
cat("nrow:", nrow(rep_tri), "\n")
cat("unique ref dates:", length(unique(test$reference_date)), "\n")
structure <- detect_structure(rep_tri)
cat("structure length:", length(structure), "\n")
cat("structure:", structure, "\n")
