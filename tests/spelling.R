if ((!nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("RUN_SPELL_CHECK"))) &&
  requireNamespace("spelling", quietly = TRUE)) {
  spelling::spell_check_test(
    vignettes = TRUE,
    error = TRUE,
    skip_on_cran = TRUE,
    lang = "en-GB"
  )
}
