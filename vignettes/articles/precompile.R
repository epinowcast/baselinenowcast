# Precompile articles that have long run times.
# Source this script with the vignettes/articles directory as the working
# directory. This knits each `*.Rmd.orig` file into a `*.Rmd` file containing
# the evaluated output (and figures), so that the pkgdown site builds quickly.

library(knitr)
orig_files <- list.files(pattern = "Rmd.orig$")
for (f in orig_files) {
  knit(f, sub(".orig$", "", f))
}
