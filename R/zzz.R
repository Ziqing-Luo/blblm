.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package version: ", as.character(utils::packageVersion("blblm")))

  packageStartupMessage(
    "Your computer has :",
    available_workers(), " workers"
  )

  # future::plan(future::sequential)

  packageStartupMessage(
    "Default is not to use parallel",
    "You can set workers by `set_workers(n = 2L)` to make it parallel."
  )
}
