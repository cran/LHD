#' Calculate the phi_p Criterion
#'
#' \code{phi_p} returns the phi_p criterion of an LHD
#'
#' @param X A matrix object. In general, \code{X} stands for the design matrix.
#' @param p A positive integer, which is the parameter in the phi_p formula, and \code{p} is prefered to be large. The default is set to be 15.
#' @param q The default is set to be 1, and it could be either 1 or 2. If \code{q} is 1, \code{dij} is the Manhattan (rectangular) distance. If \code{q} is 2, \code{dij} is the Euclidean distance.
#'
#' @return If all inputs are logical, then the output will be a positive number indicating phi_p. \code{\\phi_p = (\\sum_{i=1}^{n-1}\\sum_{j=i+1}^{n}dij^{-p})^{1/p}}, where \code{dij =  \\left\\{ \\sum_{k=1}^{m} \\vert x_{ik}-x_{jk}\\vert ^q \\right\\}^{1/q}}
#'
#' @references Jin, R., Chen, W., and Sudjianto, A. (2005) An efficient algorithm for constructing optimal design of computer experiments. \emph{Journal of Statistical Planning and Inference}, \strong{134}, 268-287.
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #Calculate the phi_p criterion of toy with default setting
#' phi_p(X=toy)
#'
#' #Calculate the phi_p criterion of toy with p=50 and q=2
#' phi_p(X=toy,p=50,q=2)
#' @export

phi_p=function(X,p=15,q=1){
  #X: must be a matrix
  #the default for q is 1:rectangular distance
  #if type q=2: Euclidean distance

  isd=NULL #this is the inter-site distance

  for (i in 1:(nrow(X)-1)) {

    for (j in (i+1):nrow(X)) {
      isd=c(isd,dij(X=X,i=i,j=j,q=q)^(-p))
    }

  }
  result=(sum(isd))^(1/p)
  result
}
