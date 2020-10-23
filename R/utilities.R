#' Check if package exists
#'
#' @param pkg package name
#'
#' @export

package_check <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "needed for this function to work. Please install it."),
         call. = FALSE)
  }
}


.onAttach <- function(libname, pkgname) {

  usethis::ui_info("loading mindthegap ...")


}
