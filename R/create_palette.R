#' Function for creating color palettes for graphs.
#'
#' Creates color palettes.
#' @usage create_palette(color_count, colors = 'default', preview = FALSE)
#' @param color_count Number of colors to choose for palette.
#' @param colors Name of a color set from the
#' \link[=RColorBrewer]{RColorBrewer} package or a vector palete of R-accepted
#' colors.
#' @param preview If TRUE, prints a figure showing the color palette.
#' @import RColorBrewer
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics image
#' @return palette
#' @export
#' 

create_palette <- function (color_count, colors = "default", preview = FALSE) 
{
  if (any(colors %in% "default")) {
    colors <- c("#757575", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#9EDA8F", 
                "#DE9861", "#565656", "#A6CBE0", "#B275D8", "#82BB47", 
                "#e0503a", "#F5E56C", "#949696", "#4989DE", "#E2E2E2", 
                "#F7B04C", "#696bb2")
    if(color_count < length(colors)){colors <- colors[seq(color_count)]}
  }
  if (any(colors %in% rownames(RColorBrewer::brewer.pal.info))) {
    colors <- RColorBrewer::brewer.pal(min(c(color_count, 
                                             RColorBrewer::brewer.pal.info[rownames(RColorBrewer::brewer.pal.info) == 
                                                                             colors, 1])), colors)
  }
  if (any(colors %in% "viridis")) {
    colors <- c("#ffcf20FF", "#10a53dFF", "#2f9aa0FF", "#3a5e8cFF", "#541352FF")
  }
  if(length(colors) != color_count){
    colors <- (grDevices::colorRampPalette(colors))(color_count)
  }
  colors <- colors[seq(color_count)]
  if (preview) {
    graphics::image(1:length(colors), 1, as.matrix(1:length(colors)), 
                    col = colors, xlab = "", ylab = "", xaxt = "n", 
                    yaxt = "n", bty = "n")
  }
  return(colors)
}
