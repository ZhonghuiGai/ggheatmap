#' A swap function of pheatmap, the inputs can be one or two matix
#'
#' @param data1 a numeric matrix
#' @param data2 a numeric matrix
#' @param display_numbers logic value
#' @param cellwidth cellwidth
#' @param cellheight cellheight
#' @param cluster logic value to indicate whether to cluster or not
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' pheatmap2(iris[, -5])
#' pheatmap2(iris[c(1:10, 60:70, 100:110), -5])
#' pheatmap2(iris[, -5], cluster = T)
#' pheatmap2(iris[, -5], iris[, -5])
#' pheatmap2(iris[c(1:10, 60:70, 100:110), -5], display_numbers = T)
#' pheatmap2(t(iris[c(1:10, 60:70, 100:110), -5]), display_numbers = T, cellwidth = 25, cellheight = 25)
pheatmap2 <- function(data1, data2 = NULL, cluster = FALSE, display_numbers = TRUE,
                      cellwidth = 15, cellheight = 15){
if (!is.null(data2) | cluster) {
  cor <- psych::corr.test(data1, data2, method = "spearman")
  cor.r <- cor$r
  cor.p <- cor$p
  cor.p <- ifelse(cor.p < 0.001, "***",
                  ifelse(cor.p >= 0.001 & cor.p < 0.01, "**",
                         ifelse(cor.p >= 0.01 & cor.p < 0.05, "*", " ")))
  p <- pheatmap::pheatmap(cor.r, scale = "none", cluster_cols = T, cluster_rows = T, display_numbers = cor.p,
                          color = colorRampPalette(colors = c("#3c9eff","gray99","#ff445d"))(100),
                          border_color = "gray99", treeheight_row = 0, treeheight_col = 0,
                          number_color = "#0e0c5b",
                          cellwidth = cellwidth, cellheight = cellheight, legend = 1,
                          cutree_rows =  1, cutree_cols = 1, fontsize = 9,
                          fontsize_number = 9,
                          fontface = 2,
                          fontsize_row = 11, fontsize_col = 11, angle_col = 45)
} else {
  p <- pheatmap::pheatmap(data1, scale = "none", cluster_cols = T, cluster_rows = T, display_numbers = display_numbers,
                          color = colorRampPalette(colors = c("#3c9eff","gray99","#ff445d"))(100),
                          border_color = "gray99", treeheight_row = 0, treeheight_col = 0,
                          number_color = "#0e0c5b",
                          cellwidth = cellwidth, cellheight = cellheight, legend = 1,
                          cutree_rows =  1, cutree_cols = 1, fontsize = 9,
                          fontsize_number = 9,
                          fontface = 2,
                          fontsize_row = 11, fontsize_col = 11, angle_col = 45)
  }
  return(p)
}
