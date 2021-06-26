#' Draw a heatmap of the grouped samples, and add group information to the result of ggheatmap
#'
#' @param data a data.frame containg the samples information and the grouping information
#'
#' @return a ggplot2 type of heatmap containing only one row
#' @export
#' @author Zhonghui Gai
#' @examples
#' data <- rowname_to_col(iris)
#' data <- data[,-6]
#' data$samples <- factor(data$samples, levels = data$samples)
#' p <- ggheatmap(data = data, midpoint = 2, scale = TRUE)
#' anno.samples <- data.frame(group = iris$Species,
#' samples = data$samples)
#' p.sample <- ggheatmap_anno_sample(data = anno.samples) + ggsci::scale_fill_jama()
#' p %>% aplot::insert_top(p.sample, height = 0.01)

ggheatmap_anno_sample <- function(data){
  p <- ggplot(data = data, aes(x = samples,
                          y = rep(NA, nrow(data)),
                          fill = group)) +
    geom_tile() + theme_void() +
    scale_y_discrete(expand = c(0, 0))
  return(p)
}
