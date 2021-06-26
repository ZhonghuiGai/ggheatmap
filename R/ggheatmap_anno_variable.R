#' Draw a heatmap of the grouped variables, and add grouped information to the result of ggheatmap
#'
#' @param data a data.frame containing the variables information and the grouping information
#'
#' @return a ggplot2 type of heatmap containing only one collumn
#' @export
#' @author Zhonghui Gai
#' @examples
#' data <- rowname_to_col(iris)
#' data <- data[,-6]
#' data$samples <- factor(data$samples, levels = data$samples)
#' p <- ggheatmap(data = data, midpoint = 2, scale = TRUE)
#' anno.variables <- data.frame(variables = colnames(iris)[1:4],
#' group = c(rep("grp1", 2), rep("grp2", 2)))
#'
#' p.variable <- ggheatmap_anno_variable(data = anno.variables) + ggsci::scale_fill_lancet()
#' p %>% aplot::insert_left(p.variable, width = 0.03)
ggheatmap_anno_variable <- function(data){
  p <- ggplot(data = data, aes(x = rep(NA, nrow(data)),
                               y = variables,
                               fill = group)) +
    geom_tile() + theme_void()
  return(p)
}
