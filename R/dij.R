#' Calculate the Inter-site Distance
#'
#' \code{dij} returns the inter-site distance of two design points of an LHD
#'
#' @param X A matrix object. In general, \code{X} stands for the design matrix.
#' @param i A positive integer, which stands for the i^{th} row of \code{X}.
#' @param j A positive integer, which stands for the j^{th} row of \code{X}. Both \code{i} and \code{j} should be in [1,nrow(X)] and they should not be equal to each other.
#' @param q The default is set to be 1, and it could be either 1 or 2. If \code{q} is 1, \code{dij} is the Manhattan (rectangular) distance. If \code{q} is 2, \code{dij} is the Euclidean distance.
#'
#' @return If all inputs are logical, then the output will be a positive number indicating the distance. \code{dij =  \\left\\{ \\sum_{k=1}^{m} \\vert x_{ik}-x_{jk}\\vert ^q \\right\\}^{1/q}}
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #Calculate the inter-site distance of the 2nd and the 4th row of toy (with default q)
#' dij(X=toy,i=2,j=4)
#'
#' #Calculate the inter-site distance of the 2nd and the 4th row of toy (with q=2)
#' dij(X=toy,i=2,j=4,q=2)
#' @export

dij=function(X,i,j,q=1){
  #X: must be a matrix
  #the default for q is 1:rectangular distance
  #if type q=2: Euclidean distance

  distance=NULL   #this is the row-wise distance

  for (l in 1:ncol(X)) {

    distance=c(distance,(abs(X[i,l]-X[j,l]))^q)
  }

  result=(sum(distance))^(1/q)
  result
}

