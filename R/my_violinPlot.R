#' Depicting violin jitter plots
#'
#' A simplfied ggplot function to project violins and boxes on jitter plots.
#' @param x A numeric vector for the x-axis, factorized within the function.
#' @param y A numeric vector for the y-axis.
#' @param data A data.frame to be specified for a plot.
#' @param ... Other arguments passed to geom_jitter().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @import ggplot2
#' @export
my_violinPlot = function(x, y, data=NULL, ...) {
  if(is.null(data)==TRUE) {
    ggplot2::ggplot(data=NULL, mapping=ggplot2::aes(x=factor(x),y=y))+
      ggplot2::geom_violin(color="grey")+
      ggplot2::geom_boxplot(width=0.1,outlier.shape=NA)+
      do.call(ggplot2::geom_jitter, list(width=0.1,...))+
      ggplot2::theme_classic()
  } else {
    ggplot2::ggplot(data=data, mapping=ggplot2::aes(x=factor(x),y=y))+
      ggplot2::geom_violin(color="grey")+
      ggplot2::geom_boxplot(width=0.1,outlier.shape=NA)+
      do.call(ggplot2::geom_jitter, list(width=0.1,...))+
      ggplot2::theme_classic()
  }
}
