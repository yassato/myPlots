#' Caluculation of the relative coordination across chromosomes
#'
#' Coordination for the x-axis of GWAS Manhattan plots.
#' @param chr Chromosome number. It should have the same length as \code{pos} and \code{p}.
#' @param pos Positions on the chromosomes.
#' @return A relative chromosomal positions from 0 to 1
#' @author Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
#' @export
coord = function(chr, pos) {
  if(length(pos)!=length(chr)) stop("chr and pos length differ")

  chr <- as.factor(chr)
  coord <- 0
  M <- 0
  for (i in 1:nlevels(chr)) {
    w <- (chr == levels(chr)[i])
    pos.c <- pos[w]
    coord[w] <- M + pos.c
    mx <- max(pos.c)
    M <- M + mx
  }
  coord <- coord/M

  return(coord)
}
