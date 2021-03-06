#' Good Lattice Point Design
#'
#' \code{GLP} returns a \code{n} by \code{k} design matrix generated by good lattice point (GLP)
#'
#' @param n A positive integer, which stands for the number of rows (or run size).
#' @param k A positive integer, which stands for the number of columns (or factor size). \code{k} must be smaller than \code{n}. In GLP designs, \code{k} <= the total number of positive integers that are smaller than and coprime to \code{n}.
#' @param h A vector whose length is \code{k}, with its elements that are smaller than and coprime to \code{n}. The default is set to be a random sample of \code{k} elements between 1 and \code{n-1}.
#'
#' @return If all inputs are logical, then the output will be a \code{n} by \code{k} GLP design matrix.
#'
#' @references Korobov, A.N. (1959) The approximate computation of multiple integrals. \emph{Dokl. Akad. Nauk SSSR}, \strong{124}, 1207-1210.
#'
#' @examples
#' #generate a 5 by 3 GLP design with the default setting
#' try=GLP(n=5,k=3)
#' try
#'
#' #Another example
#' #generate a 8 by 4 GLP design with given h vector
#' try2=GLP(n=8,k=4,h=c(1,3,5,7))
#' try2
#' @export

GLP=function(n,k,h=sample(seq(from=1,to=(n-1)),k)){
  #n and k are the run and factor sizes.
  #k <= the number of positive integers that are smaller than and coprime to n
  #h must be a vector whose length is k, with elements that are smaller than and coprime to n

  if(k>=n){
    stop("k must be smaller than n")
  }

  #if(rand.h==TRUE){h=sample(h)}

  D=matrix(0,nrow=n,ncol=k)

  for (i in 1:n) {
    for (j in 1:k) {

      D[i,j]=(i*h[j])%%n

    }
  }

  D
}
