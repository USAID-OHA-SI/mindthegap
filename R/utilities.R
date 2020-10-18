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

  check_fonts()


}



#' Check for Imported Fonts
#'
#' @export
#' @importFrom utils stack
#' @importFrom grDevices windowsFonts

check_fonts <- function(){

  #list of all fonts available
  localfonts <- stack(grDevices::windowsFonts())

  #load fonts
  if(nrow(localfonts) < 4)
     extrafont::loadfonts(device = "win", quiet = TRUE)

  #check if fonts are loaded
  localfonts <- stack(grDevices::windowsFonts())
  if(nrow(localfonts) < 4) {
    usethis::ui_warn("Before proceeding, need to import local fonts (only happens once). This may take a few minutes")
    extrafont::font_import(prompt = TRUE)
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }

}




