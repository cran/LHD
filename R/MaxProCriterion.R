#' Calculate the Maximum Projection Criterion
#'
#' \code{MaxProCriterion} returns the maximum projection criterion of an LHD
#'
#' @param X A Matrix.
#'
#' @return If all inputs are logical, then the output will be a positive number indicating maximum projection criterion.
#' @details \itemize{
#' \item \code{X} stands for the design matrix.
#' \item The maximum projection criterion formula is given by the Note Section below.
#' }
#'
#' @note \code{maximum projection criterion = \\Bigg\{ \\frac{1}{{n \\choose 2}} \\sum_{i=1}^{n-1} \\sum_{j=i+1}^{n} \\frac{1}{\\Pi_{l=1}^{k}(x_{il}-x_{jl})^2}  \\Bigg\}^{1/k}}
#'
#' @references Joseph, V. R., Gul, E., and Ba, S. (2015) Maximum projection designs for computer experiments. \emph{Biometrika}, \strong{102}, 371-380.
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #Calculate the maximum projection criterion of toy
#' MaxProCriterion(X=toy)
#'
#' @export

MaxProCriterion=function(X){
  n=dim(X)[1]
  p=dim(X)[2]

  temp=0

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {

      denom=1

      for (k in 1:p) {
        denom=denom*(X[i,k]-X[j,k])^2
      }

      temp=temp+1/denom

    }
  }


  result=(2/(n*(n-1))*temp)^(1/p)
  result
}
