.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "mediaPlanR ", utils::packageVersion(pkgname),
    " | Reliable cross-media reach and frequency planning. See ?media_plan"
  )
}
