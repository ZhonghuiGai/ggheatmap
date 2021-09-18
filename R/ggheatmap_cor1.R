#' Correlation heatmap of one data frame, Only the lower triangle is shown
#'
#' @param data the numeric matrix
#' @param clust logic value to indicate whether cluster or not
#' @param both Logic value to show both the coefficient and the p values
#' @param method the method of correlation one the "pearson" and "spearman"
#' @param limits the color range, default value is c(-1, 1)
#'
#' @return a ggplot object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' ggheatmap_cor1(data = iris[, -5])
ggheatmap_cor1 <- function(data, clust = TRUE, both = FALSE, method = "spearman",
                           limits = c(-1, 1)){
  trigle <- TRUE
  n <- length(data)
  # step 1 Correlation analysis and Extraction Correlation Coefficient and p-values
  cor <- psych::corr.test(data, method = method)
  cor.r <- cor$r
  cor.p <- cor$p
  cor.p <- ifelse(cor.p < 0.001, "***",
                  ifelse(cor.p >= 0.001 & cor.p < 0.01, "**",
                         ifelse(cor.p >= 0.01 & cor.p < 0.05, "*", " ")))
  # step 2 Cluster, First make a function
  reorder <- function(data = cor.r){
    cor.r.tmp <- 1-data # Convert correlation matrix into similarity matrix
    cor.r.tmp <- cor.r.tmp/2
    dd <- as.dist(cor.r.tmp) # Convert into distance matrix, default method euclidean
    hc <- hclust(dd) # Hierarchical clustering, the default method is "complete"
    ind2 <<- hc$order
  }
  # step 2.2 cluster
  cor.r.ind <- cor.r
  cor.p.ind <- cor.p
  if(clust){
    reorder()
    cor.r.ind <- cor.r.ind[ind2, ind2]
    cor.p.ind <- cor.p.ind[ind2, ind2]
  }
  # step 3 extrat the upper trigle
  if(trigle){
    ind <- !lower.tri(cor.r.ind)
    cor.r.ind[ind] <- NA
    cor.p.ind[ind] <- NA
  }

  # step 4 make the heatmap
  p <- ggplot(reshape2::melt(cor.r.ind, na.rm = TRUE), aes(Var1, Var2)) +
    geom_tile(aes(fill = value),colour = "white") +
    theme_minimal() +
    scale_fill_gradient2(name = "Value", limits = limits,
                         low = "#3c9eff", mid = "white",
                         high = "#ff445d",  midpoint = 0.01) +
    scale_y_discrete(position = "right") + xlab(NULL)+ylab(NULL) +
    coord_fixed(ratio=1) +
    theme(panel.grid = element_line(color = NA, size = 0.1),
          panel.background = element_rect(color = NA, fill = "transparent"),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 0),
          axis.title.y = element_text(size = 14),
          legend.text = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold", angle = 0, hjust = 0.5),
          panel.border = element_rect(color = NA, fill = "transparent"),
          legend.title = element_blank(),
          legend.position = c(0.2, 0.8), legend.direction = "horizontal" )

  if(both){# Both correlation coefficients and significance flags are displayed
    cor.both <- paste0(cor.p.ind, "\n", round(cor.r.ind, 3))
    cor.both <- matrix(cor.both, nrow = nrow(cor.r))
    colnames(cor.both) <- colnames(cor.r.ind)
    rownames(cor.both) <- rownames(cor.r.ind)
    cor.both[ind] <- NA
    p <- p +
      geom_text(data = reshape2::melt(cor.both, na.rm = TRUE),
                aes(Var1, Var2, label = value), color = "black", size = 5,
                fontface = "bold")
  }else{
    p <- p +
      geom_text(data = reshape2::melt(cor.p.ind, na.rm = TRUE),
                aes(Var1, Var2, label = value), color = "black", size = 7, fontface = "bold")
  }
  return(p)
}

