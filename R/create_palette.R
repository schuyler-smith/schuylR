#' Create color palettes.
#'
#' Create color palettes.
#' @usage create_palette(color_count, colors)
#' @param color_count Number of colors to choose for palette.
#' @param colors Name of a color set from the \link[=RColorBrewer]{RColorBrewer} package or a vector palete of R-accepted colors.
#' @import RColorBrewer
#' @import grDevices
#'

create_palette <- function(color_count, colors){
  options(warn = -1)
  cbcolors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  if(colors == 'default'){colors <- cbcolors}
  if(any(!(colors %in% colors()))){
    if(any(colors %in% rownames(brewer.pal.info))){
      getPalette <- colorRampPalette(brewer.pal(min(c(color_count, brewer.pal.info[rownames(brewer.pal.info) == colors, 1])), colors))
    } else { getPalette <- colorRampPalette(colors)}
  } else { getPalette <- colorRampPalette(colors)}
  return(getPalette(color_count))
}