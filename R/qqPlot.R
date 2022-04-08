#' Depicting QQ-plot for GWAS without ggplot2
#'
#' A simple plot function to draw a QQ-plot for GWAS results.
#' @param p A vector of p-values ranging from 0 to 1.
#' @param ... Other arguments passed to plot().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @export
qqPlot = function(p,...) {
  o <- -log(sort(p,decreasing=FALSE),10)
  e <- -log(stats::ppoints(length(p)),10)

  args <- list(...)
  args$x <- e
  args$y <- o
  args$bty <- "n"
  args$xlab <- "Expected -log10(P)"
  args$ylab <- "Observed -log10(P)"
  do.call(plot,args)
  graphics::abline(a=0,b=1,lty=2,col="grey")
}
