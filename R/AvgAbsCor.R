#' Calculate the Average Absolute Correlation
#'
#' \code{AvgAbsCor} returns the average absolute correlation of an LHD
#'
#' @param X A Matrix.
#'
#' @return If all inputs are logical, then the output will be a positive number indicating average absolute correlation.
#' @details \itemize{
#' \item \code{X} stands for the design matrix.
#' \item The average absolute correlation formula is given by the Note Section below.
#' }
#'
#' @note \code{average absolute correlation = \\frac{2 \\sum_{i=1}^{k-1} \\sum_{j=i+1}^{k}|q_{ij}|}{k(k-1)}}
#'
#' @references Georgiou, S. D. (2009) Orthogonal Latin hypercube designs from generalized orthogonal designs. \emph{Journal of Statistical Planning and Inference}, \strong{139}, 1530-1540.
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #Calculate the average absolute correlation of toy
#' AvgAbsCor(X=toy)
#'
#' @export

AvgAbsCor=function(X){
  p=dim(X)[2]

  corr=NULL  #used for storing all the pairwise correlations

  for (i in 1:(p-1)) {
    for (j in (i+1):p) {

      corr=c(corr,stats::cor(X[,i],X[,j]))

    }
  }

  result=mean(abs(corr))
  result
}
