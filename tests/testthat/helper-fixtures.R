# The published-plan fixtures (Kim 2005, Appendix B; Hong 1998, Appendix E) are
# local-only files of the maintainer: they are not tracked by git (`.gitignore`)
# and are excluded from the package archive by `.Rbuildignore` (see
# inst/DATA-PROVENANCE.md). The tests that depend on them run when the files are
# present (the maintainer's local `testthat::test_local()`) and are skipped
# otherwise: in the built or installed package, in the continuous-integration
# workflow and in any clone of the repository.
skip_if_no_fixture <- function(name) {
  testthat::skip_if_not(
    file.exists(test_path("fixtures", name)),
    paste0("fixture ", name, " is a local-only file of the maintainer")
  )
}
