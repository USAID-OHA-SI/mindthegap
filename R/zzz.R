.onLoad <- function (libname, pkgname)
{

  packageStartupMessage("\nTo pull UNAIDS 2023 estimates (1990-2022), use pull_unaids(). See function documentation for further information.\n")


  invisible ()
}

.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    gagglr::oha_check("mindthegap", suppress_success = TRUE)
}
