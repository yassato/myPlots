#' Depicting QQ-plot for GWAS
#'
#' A simple ggplot function to draw a QQ-plot for GWAS results.
#' @param p A vector of p-values ranging from 0 to 1.
#' @param ... Other arguments passed to geom_point().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @import ggplot2
#' @export
my_qqPlot = function(p,...) {
  o <- -log(sort(p,decreasing=FALSE),10)
  e <- -log(stats::ppoints(length(p)),10)

  ggplot2::ggplot(data=NULL, mapping=ggplot2::aes(x=e,y=o))+
    do.call(ggplot2::geom_point,list(...))+
    ggplot2::geom_abline(intercept=0,slope=1,linetype="dashed")+
    ggplot2::theme_classic()+
    ggplot2::xlab("Expected -log10(p)")+ggplot2::ylab("Observed -log10(p)")
}
