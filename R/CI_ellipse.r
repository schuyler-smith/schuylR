#' Creates confidence interval ellipse for scatterplot
#'
#' Creates confidence interval ellipse for scatterplot. Can be created
#' for each group or entire set.
#' @usage CI_ellipse(points, groups = NULL, level = 0.95)
#' @param points dataframe of the x,y coordinates of the datapoints, along with
#' any metadata such as group
#' @param groups Name of the column in points that has group membership
#' @param level the confidence level to be plotted (default 95\%)
#' @importFrom stats cov.wt qf
#' @return ellipse points

CI_ellipse <- function(points,
                       groups = NULL,
                       level = 0.95) {
  ellipse_df <- data.frame()
  theta <- (0:99) * 2 * pi / 100
  unit_circle <- cbind(cos(theta), sin(theta))
  if (!is.null(groups)) {
    for (group in unique(points[, groups])) {
      sub_points <- points[points[, groups] == group, c('x', 'y')]
      ellipse_info <- cov.wt(sub_points[, c('x', 'y')])
      shape <- ellipse_info$cov
      center <- ellipse_info$center
      radius <- sqrt(2 * qf(level, 2, length(sub_points$x) - 1))
      Q <- chol(shape, pivot = TRUE)
      order <- order(attr(Q, "pivot"))
      ellipse <- t(center + radius * t(unit_circle %*% Q[, order]))
      ellipse_df <- rbind(ellipse_df, cbind(ellipse, group))
    }
  } else {
    ellipse_info <- cov.wt(points[, c('x', 'y')])
    shape <- ellipse_info$cov
    center <- ellipse_info$center
    radius <- sqrt(2 * qf(level, 2, length(points$x) - 1))
    Q <- chol(shape, pivot = TRUE)
    order <- order(attr(Q, "pivot"))
    ellipse <- t(center + radius * t(unit_circle %*% Q[, order]))
    ellipse_df <- cbind(ellipse)
  }
  return(ellipse_df)
}