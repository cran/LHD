#' Transfer an Orthogonal Array (OA) into an LHD
#'
#' \code{OA2LHD} transfers an OA into an LHD with corresponding size
#'
#' @param OA A Matrix.
#'
#' @return If the input is logical, then the output will be an LHD whose sizes are the same as input OA.
#' @details \itemize{
#' \item \code{OA} stands for the input orthogonal array matrix.
#' }
#'
#' @note The assumption is that the elements of OAs must be positive.
#'
#' @references Tang, B. (1993) Orthogonal-array-based latin hypercubes. \emph{Journal of the Americal Statistical Association}, \strong{88}, 1392-1397.
#'
#' @examples
#' #create an OA(9,2,3,2)
#' OA=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = FALSE);OA
#'
#' #Transfer the "OA" above into a LHD according to Tang (1993)
#' tryOA=OA2LHD(OA)
#' OA;tryOA
#' @export


OA2LHD=function(OA){
  n=dim(OA)[1]
  m=dim(OA)[2]
  s=length(unique(OA[,1]))

  #OA: OA must be an orthogonal array
  #n: number of rows of OA
  #m: number of columns of OA
  #s: number of levels of OA
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
