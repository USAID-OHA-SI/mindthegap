.onLoad <- function (libname, pkgname)
{

  startup_msg()

  invisible ()
}

.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    gagglr::oha_check("mindthegap", suppress_success = TRUE)
}
