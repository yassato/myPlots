#' Depicting a simple plot
#'
#' A simplfied ggplot to depict a biplot (for continous x) or violin plot (for factorial x).
#' @param x A numeric vector for the x-axis. Violin plots will be shown if x is a factor.
#' @param y A numeric vector for the y-axis. If NULL, the function operates like graphics::plot().
#' @param data A data.frame to be specified for a plot.
#' @param xlab Option to change the label of x-axis.
#' @param ylab Option to change the label of y-axis.
#' @param ... Other arguments passed to geom_point().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @import ggplot2
#' @export
my_Plot = function(x, y=NULL, data=NULL, xlab=NULL, ylab=NULL,...) {
  switch(class(x),
    "factor" = my_violinPlot(x=x, y=y, data=data, xlab=xlab, ylab=ylab,...),
    "ordered" = my_violinPlot(x=x, y=y, data=data, xlab=xlab, ylab=ylab,...),
    "character" = my_violinPlot(x=x, y=y, data=data, xlab=xlab, ylab=ylab,...),
    my_biPlot(x=x, y=y, data=data, xlab=xlab, ylab=ylab,...)
  )
}
