#' Depicting a simple biplot
#'
#' A simplfied ggplot to depict a biplot.
#' @param x A numeric vector for the x-axis. Factors are now allowed.
#' @param y A numeric vector for the y-axis. If NULL, the function operates like graphics::plot().
#' @param data A data.frame to be specified for a plot.
#' @param xlab Option to change the label of x-axis.
#' @param ylab Option to change the label of y-axis.
#' @param ... Other arguments passed to geom_point().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @import ggplot2
#' @export
my_biPlot = function(x, y=NULL, data=NULL, xlab=NULL, ylab=NULL,...) {
  if(is.null(data)==TRUE) {
    if(is.null(y)==TRUE) {
      p <- ggplot2::ggplot(data=NULL, mapping=ggplot2::aes(x=c(1:length(x)),y=x))+
        do.call(ggplot2::geom_point, list(...))+
        ggplot2::theme_classic()+
        ggplot2::theme(axis.title.x=ggplot2::element_blank())
    } else {
      p <- ggplot2::ggplot(data=NULL, mapping=ggplot2::aes(x=x,y=y))+
        do.call(ggplot2::geom_point, list(...))+
        ggplot2::theme_classic()
      }
  } else {
    if(is.null(y)==TRUE) {
      p <- ggplot2::ggplot(data=data, mapping=ggplot2::aes(x=c(1:length(x)),y=x))+
        do.call(ggplot2::geom_point, list(...))+
        ggplot2::theme_classic()+
        ggplot2::theme(axis.title.x=ggplot2::element_blank())
    } else {
      p <- ggplot2::ggplot(data=data, mapping=ggplot2::aes(x=x,y=y))+
        do.call(ggplot2::geom_point, list(...))+
        ggplot2::theme_classic()
    }
  }
  if(is.null(xlab)==FALSE) { p <- p+ggplot2::xlab(xlab) }
  if(is.null(ylab)==FALSE) { p <- p+ggplot2::ylab(ylab) }
  print(p)
}
