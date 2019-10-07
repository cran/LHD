#' Transfer an Orthogonal Array (OA) into a LHD
#'
#' \code{OA2LHD} transfers an OA into a LHD with corresponding size
#'
#' @param OA A Matrix.
#' @param n A positive integer.
#' @param m A positive integer.
#' @param s A positive integer.
#' @param r A positive integer.
#'
#' @return If all inputs are logical, then the output will be a \code{n} by \code{m} LHD.
#' @details \itemize{
#' \item \code{OA} stands for the input orthogonal array matrix.
#' \item \code{n} stands for the number of rows of OA.
#' \item \code{m} stands for the number of columns of OA.
#' \item \code{s} stands for the number of levels of OA.
#' \item \code{r} stands for the strength of OA.
#' }
#'
#' @note LHD(n,m)=OA(n,m,n,1)
#'
#' @references Tang, B. (1993) Orthogonal-array-based latin hypercubes. \emph{Journal of the Americal Statistical Association}, \strong{88}, 1392-1397.
#'
#' @examples
#' #create an OA(9,2,3,2)
#' OA=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = FALSE);OA
#'
#' #Transfer the "OA" above into a LHD according to Tang (1993)
#' tryOA=OA2LHD(OA=OA,9,2,3,2)
#' OA;tryOA
#' @export


OA2LHD=function(OA,n,m,s,r){
  #OA: OA must be an orthogonal array
  #n: number of rows of OA
  #m: number of columns of OA
  #s: number of levels of OA
  #r: strength of OA
  LHD=OA

  #Create a 3-dimentional array named k, which corresponds to k=1,2,...,s
  k=rep(0,n)
  dim(k)=c(n/s,1,s)

  for (j in 1:m) {
    for (i in 1:s) {
      k[,,i]=seq(from=(i-1)*n/s+1,to=(i-1)*n/s+n/s,1) #This is the formula from Tang (1993)
      k[,,i]=sample(k[,,i])

      LHD[,j][LHD[,j]==i]=k[,,i]*100

    }

  }
  LHD=LHD/100
  LHD
}
