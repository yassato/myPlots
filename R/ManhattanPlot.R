#' Depicting Manhattan plots for GWAS without ggplot2
#'
#' A simple plot function to draw Manhattan plot for GWAS results.
#' @param chr Chromosome number. It should have the same length as \code{pos} and \code{p}.
#' @param pos Positions on the chromosomes.
#' @param p A vector of p-values ranging from 0 to 1. If it is outside 0 or 1, p is not log10-transformed.
#' @param ... Other arguments passed to plot().
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @export
ManhattanPlot = function(chr, pos, p, ...) {
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
  args <- list(...)
  args$xaxt <- "n"
  args$bty <- "n"
  args$x <- coord
  if(sum((p<=1)&(p>0),na.rm=TRUE)==length(na.omit(p))) {
    args$y <- -log10(p)
  } else {
    args$y <- p
  }
  do.call(plot,args)
  abline(h=-log10(th),lty=2,col="grey")
}
