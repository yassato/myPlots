#' Depicting Manhattan plots for GWAS
#'
#' A simple ggplot function to draw Manhattan plot for GWAS results.
#' @param chr Chromosome number. It should have the same length as \code{pos} and \code{p}.
#' @param pos Positions on the chromosomes.
#' @param p A vector of p-values ranging from 0 to 1.
#' @param ... Other arguments passed to geom_point().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @import ggplot2
#' @export
my_ManhattanPlot = function(chr, pos, p, ...) {
  if(length(pos)!=length(chr)) stop("chr and pos length differ")
  if(length(chr)!=length(p)) stop("chr and p length differ")

  chr <- as.factor(chr)
  coord <- 0
  M <- 0
  tic <- numeric(nlevels(chr))
  for (i in 1:nlevels(chr)) {
    w <- (chr == levels(chr)[i])
    pos.c <- pos[w]
    coord[w] <- M + pos.c
    mx <- max(pos.c)
    tic[i] <- M + mx/2
    M <- M + mx
  }
  coord <- coord/M
  tic <- tic/M

  th <- 0.05/length(p)

  if(sum((p<=1)&(p>0),na.rm=TRUE)==length(stats::na.omit(p))) {
    p <- -log10(p)
  } else {
    p <- p
  }

  ggplot2::ggplot(data=NULL, mapping=ggplot2::aes(x=coord,y=p))+
    do.call(ggplot2::geom_point,list(...))+
    ggplot2::geom_hline(yintercept=-log10(th),linetype="dashed",colour="grey")+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank())
}
