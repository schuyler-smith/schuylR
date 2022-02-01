#' Custome ggplot theme
#'
#' This is the theme I like to stat with for creating graphics
#'
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements

#' @export
#' @rdname ggtheme
theme_schuy <- function (
  base_size = 12, 
  base_family = "", 
  base_line_size = base_size/22, 
  base_rect_size = base_size/22) 
{
  theme_bw(
    base_size = base_size, 
    base_family = base_family, 
    base_line_size = base_line_size, 
    base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey20"), 
          panel.grid = element_line(colour = "grey92"), 
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(size = base_size, face = "bold"), 
          legend.text = element_text(size = base_size), 
          legend.spacing.x = unit(0.005, "npc"), 
          legend.key = element_rect(fill = "white", colour = NA), 
          legend.key.size = unit(3.5, "mm"),
          axis.text.x = element_text(size = base_size), 
          axis.text.y = element_text(size = base_size), 
          axis.title.x = element_text(size = base_size, face = "bold"), 
          axis.title.y = element_text(size = base_size, face = "bold"),
          strip.text.x = element_text(size = base_size, face = "bold"), 
          strip.background = element_rect(colour = "black", fill = "grey85", size = 1.4),
          complete = TRUE
          )
}


