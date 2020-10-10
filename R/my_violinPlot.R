#' Depicting violin jitter plots
#'
#' A simplfied ggplot to project violins and boxes on jitter plots.
#' @param x A numeric vector for the x-axis, factorized within the function.
#' @param y A numeric vector for the y-axis.
#' @param data A data.frame to be specified for a plot.
#' @param xlab Option to change the label of x-axis.
#' @param ylab Option to change the label of y-axis.
#' @param ... Other arguments passed to geom_jitter().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @import ggplot2
#' @export
my_violinPlot = function(x, y, data=NULL, xlab=NULL, ylab=NULL,...) {
  if(is.null(data)==TRUE) {
    p <- ggplot2::ggplot(data=NULL, mapping=ggplot2::aes(x=factor(x),y=y))+
      ggplot2::geom_violin(color="grey")+
      ggplot2::geom_boxplot(width=0.1,outlier.shape=NA)+
      do.call(ggplot2::geom_jitter, list(width=0.1,...))+
      ggplot2::theme_classic()
  } else {
    p <- ggplot2::ggplot(data=data, mapping=ggplot2::aes(x=factor(x),y=y))+
      ggplot2::geom_violin(color="grey")+
      ggplot2::geom_boxplot(width=0.1,outlier.shape=NA)+
      do.call(ggplot2::geom_jitter, list(width=0.1,...))+
      ggplot2::theme_classic()
  }
  if(is.null(xlab)==FALSE) { p <- p+ggplot2::xlab(xlab) }
  if(is.null(ylab)==FALSE) { p <- p+ggplot2::ylab(ylab) }
  print(p)
}
