#' Draw a simple heatmap based on ggplot2
#'
#' @param data a data.frame with the first collumn as the the sample names usually transformed
#' from the rownames.
#' @param midpoint a numeric indicated the middle value of the heatmap legend.
#' @param scale Boolean value to indicate if scaled using the scale function.
#'
#' @return a heatmap
#' @export
#' @author Zhonghui Gai
#' @examples
#' data <- rowname_to_col(iris)
#' data <- data[,-6]
#' data$samples <- factor(data$samples, levels = data$samples)
#' ggheatmap(data = data, midpoint = 2, scale = TRUE)
ggheatmap <- function(data, midpoint,
                      scale = FALSE){
  if ("samples" %in% colnames(data)) {
    if (scale) {
      df <- subset(data, select = -samples)
      df <- scale(df)
      df <- data.frame(samples = data$samples, df)
    }else{
      df <- data
    }
  }else{
    stop("The first collum must be the sample index with 'samples' as the collum name!")
  }
  df.melt <- reshape2::melt(df, id.var = "samples")
  p <- ggplot(data = df.melt, aes(x = samples, y = variable)) +
    geom_tile(aes(fill = value), color = "white") +
    xlab(NULL) + ylab(NULL) +
    scale_fill_gradient2(name="Value", #limits = range(scale(heatmap.data)),
                         low = "#3c9eff", mid = "white",
                         high = "#ff445d",  midpoint = midpoint) +
    theme_void() +
    theme(legend.position = 0,
          axis.text.x = element_blank()) +
    scale_y_discrete(position = "right") +
    theme(axis.text.y = element_text(size = 14,
                                     face = "bold.italic",
                                     colour = "black", hjust = 0),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          axis.line.x.bottom = element_blank(),
          axis.line.x.top = element_blank(),
          axis.ticks.x.bottom = element_blank(),
          axis.ticks.x.top = element_blank())
  return(p)
}


