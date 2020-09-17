#' Modified Williams Transformation
#'
#' \code{MWT} returns a matrix after implementing the modified Williams transformation
#'
#' @param X A matrix object. In general, \code{X} stands for the design matrix, e.g. an LHD or a GLP design.
#'
#' @return If all inputs are logical, then the output will be a matrix whose sizes are the same as input matrix. Note that the output matrix is not an LHD.
#'
#' @references Wang, L., Xiao, Q., and Xu, H. (2018)  Optimal maximin $L_{1}$-distance Latin hypercube designs based on good lattice point designs. \emph{The Annals of Statistics}, \strong{46}(6B), 3741-3766.
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #Implementing the modified Williams transformation on toy:
#' MWT(toy)
#'
#' @export

MWT=function(X){
  ### Modified Williams transformation of a matrix

  n=dim(X)[1]
  k=dim(X)[2]

  elements=unique(X[,1])

  #minimum element
  ME=min(elements)

  #transfer input matrix into a standard william's code matrix
  if(ME!=0){X=X-ME}

  #The matrix after WT
  Y=X

  for (i in 1:n){
    for (j in 1:k){

      if(X[i,j]<(n/2)){Y[i,j]=2*X[i,j]}
      if(X[i,j]>=(n/2)){Y[i,j]=2*(n-X[i,j])}

    }
  }

  Y
}
