.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "mediaPlanR ", utils::packageVersion(pkgname),
    " | Reach and exposure-distribution models for media planning. See ?mediaPlanR"
  )
}
