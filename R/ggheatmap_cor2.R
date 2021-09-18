#' Correlation heatmap of two matrix
#'
#' @param data1 the numeric matrix
#' @param data2 the numeric matrix
#' @param clust logic value to indicate whether cluster or not
#' @param both Logic value to show both the coefficient and the p values
#' @param method the method of correlation one the "pearson" and "spearman"
#' @param limits the color range, default value is c(-1, 1)
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' ggheatmap_cor2(iris[, -5], iris[, -5])
ggheatmap_cor2 <- function(data1, data2, clust = TRUE, both = FALSE,
                           method = "spearman", limits = c(-1, 1)){
  # step 1 Correlation analysis and Extraction Correlation Coefficient and p-values
  cor <- psych::corr.test(data1, data2, method = method)
  cor.r <- cor$r
  cor.p <- cor$p
  cor.p <- ifelse(cor.p < 0.001, "***",
                  ifelse(cor.p >= 0.001 & cor.p < 0.01, "**",
                         ifelse(cor.p >= 0.01 & cor.p < 0.05, "*", " ")))
  # step 2 Cluster
  if(clust){
    heatmap.tmp <- pheatmap::pheatmap(cor.r)
    row.order <- heatmap.tmp$tree_row$order
    col.order <- heatmap.tmp$tree_col$order
    cor.r.ind <- cor.r[row.order, col.order]
    cor.p.ind <- cor.p[row.order, col.order]
  }
  p <- ggplot(reshape2::melt(cor.r.ind, na.rm = TRUE), aes(Var1, Var2)) +
    geom_tile(aes(fill = value),colour = "white") +
    theme_minimal() +
    scale_fill_gradient2(name="Value", limits = limits,
                         low = "#3c9eff", mid = "white", high = "#ff445d",  midpoint = 0.01)+
    scale_y_discrete(position = "right") + xlab(NULL) + ylab(NULL) +
    coord_fixed(ratio = 1) +
    theme(panel.grid = element_line(color = NA, size = 0.1),
          panel.background = element_rect(color = NA,fill = 'transparent', size = 1),
          plot.title = element_text(size=20, face = "bold"),
          axis.text.y = element_text(size = 13, face = "bold.italic"),
          axis.text.x = element_text(size = 12, face = "bold", vjust = 0.5, hjust = 0.5, angle = 45),
          axis.title.y = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", angle = 0, hjust = 0.5),
          panel.border = element_rect(color = NA,fill = 'transparent'),
          legend.title = element_blank(),
          legend.position = "right")

  if(both){
    cor.both <- paste0(cor.p.ind, "\n", round(cor.r.ind, 3))
    cor.both <- matrix(cor.both, nrow = nrow(cor.r))
    colnames(cor.both) <- colnames(cor.r.ind)
    rownames(cor.both) <- rownames(cor.r.ind)
    p <- p +
      geom_text(data = reshape2::melt(cor.both, na.rm = TRUE),
                aes(Var1, Var2, label = value), color = "black", size = 3, fontface = "bold")+
      theme(legend.position = 0)
  }else{
    p <- p +
      geom_text(data = reshape2::melt(cor.p.ind, na.rm = TRUE),
                aes(Var1, Var2, label = value), color = "black", size = 4.5, fontface = "bold")
  }
  return(p)
}
