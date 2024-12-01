convert_to_LaTeX <- function(plot_obj, new_xlab = NULL, new_ylab = NULL, new_title = NULL){
  if (!requireNamespace('latex2exp', quietly = TRUE)){
    install.packages('latex2exp')
    library(latex2exp)
  }
  if (!is.null(new_title)){
    plot_obj$labels$title  <- TeX(new_title)
  }
  if (!is.null(new_xlab)){
    plot_obj$labels$x  <- TeX(new_xlab)
  }
  if (!is.null(new_ylab)){
    plot_obj$labels$y  <- TeX(new_ylab)
  }
  return(plot_obj)
}