#' Move the rownames of a data.frame to the first collum of the data.frame
#'
#' @param data a data.frame
#' @param rowname.rm a boolean to choose to remove the rownames
#'
#' @return a data.frame
#' @export
#' @author Zhonghui Gai
#' @examples
#' rowname_to_col(iris)
rowname_to_col <- function(data,
                           rowname.rm = FALSE){
  if (!inherits(data, "data.frame")) {
    stop("The input must be the class of 'data.frame'")
  }
  df <- data.frame(samples = rownames(data), data)
  if (rowname.rm) {
    rownames(df) <- NULL
  }
  return(df)
}

