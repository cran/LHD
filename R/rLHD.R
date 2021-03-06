#' Generate a random Latin Hypercube Design (LHD)
#'
#' \code{rLHD} returns a random \code{n} by \code{k} Latin hypercube design matrix
#'
#' @param n A positive integer, which stands for the number of rows (or run size).
#' @param k A positive integer, which stands for the number of columns (or factor size).
#'
#' @return If all inputs are positive integer, then the output will be a \code{n} by \code{k} design matrix.
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #another example with 9 rows and 2 columns
#' rLHD(9,2)
#' @export


#Generate a random LHD
rLHD=function(n,k){
  #n: number of rows, which is also number of run sizes (rs)
  #k: number of columns, which is also number of factors (fa)
  rs=1:n
  space=NULL
  for (i in 1:k) {
    space=c(space,sample(rs))
  }
  LHD=matrix(space,nrow = n, ncol = k, byrow = F)
  LHD
}
