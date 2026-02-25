in_ci <- nzchar(Sys.getenv("CI"))
run_spell <- nzchar(Sys.getenv("RUN_SPELL_CHECK"))
if ((!in_ci || run_spell) &&
  requireNamespace("spelling", quietly = TRUE)) {
  spelling::spell_check_test(
    vignettes = TRUE,
    error = TRUE,
    skip_on_cran = TRUE,
    lang = "en-GB"
  )
}
