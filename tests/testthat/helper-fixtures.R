# The published-plan fixtures (Kim 2005, Appendix B; Hong 1998, Appendix E) are
# excluded from the package archive by `.Rbuildignore` (see
# inst/DATA-PROVENANCE.md). They are present in the source repository, where the
# complete suite runs (`testthat::test_local()`), and absent from an installed or
# built package, where the tests that depend on them are skipped.
skip_if_no_fixture <- function(name) {
  testthat::skip_if_not(
    file.exists(test_path("fixtures", name)),
    paste0("fixture ", name, " is not distributed with the package archive")
  )
}
