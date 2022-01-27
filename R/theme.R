#' Mind The Gap Plot Theme
#'
#' @return theme
#' @keywords internal
#'
mtg_theme <- function(){
    ggplot2::theme(text = element_text(family = "GillSans", color = base_gray, size = 12),
                   axis.ticks = element_blank(),
                   legend.position = "none",
                   panel.background = element_blank(),
                   strip.background = element_blank(),
                   strip.text = element_text(face = "bold", size = 11, color = base_gray),
                   plot.title = element_text(size = 15, face = "bold", color = "black"),
                   plot.subtitle =element_text(size = 15, color = base_gray),
                   plot.caption = element_text(size = 8,  color = caption_gray))
}

