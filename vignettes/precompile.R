# Precompile vignettes that have long run times.
# Source this script with the vignettes directory as the working directory.
# This knits each `*.Rmd.orig` file into a `*.Rmd` file containing the
# evaluated output (and figures), so that the vignette builds quickly on CRAN.

library(knitr)
orig_files <- list.files(pattern = "Rmd.orig$")
for (f in orig_files) {
  knit(f, sub(".orig$", "", f))
}
