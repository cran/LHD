#' Williams Transformation
#'
#' \code{WT} returns a matrix after implementing the Williams transformation
#'
#' @param X A matrix object. In general, \code{X} stands for the design matrix, e.g. an LHD or a GLP design.
#' @param baseline A integer, which defines the minimum value for each column of the matrix. The default is set to be 1.
#'
#' @return If all inputs are logical, then the output will be a matrix whose sizes are the same as input matrix.
#'
#' @references Williams, E. J. (1949) Experimental designs balanced for the estimation of residual effects of treatments. \emph{Australian Journal of Chemistry}, \strong{2}, 149-168.
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#' toy2=toy-1;toy2  #make elements of "toy" become 0,1,2,3,4
#'
#' #Implementing Williams transformation on both toy and toy2:
#' #The result shows that "WT" function is able to detect the
#' #elements of input matrix and make adjustments.
#' WT(toy)
#' WT(toy2)
#'
#' #Change the baseline
#' WT(toy,baseline=5)
#' WT(toy,baseline=10)
#'
#' @export

WT=function(X,baseline=1){
  n=dim(X)[1]
  k=dim(X)[2]

  elements=unique(X[,1])

  #minimum element
  ME=min(elements)

  #transfer input matrix into a standard william's code matrix
  if(ME!=0){X=X-ME}

  #The matrix after WT
  Y=X

  for (i in 1:n) {
    for (j in 1:k) {

      if(X[i,j]<(n/2)){Y[i,j]=2*X[i,j]+1}
      if(X[i,j]>=(n/2)){Y[i,j]=2*(n-X[i,j])}

    }
  }

  if(baseline!=1){Y=Y+(baseline-1)}

  Y
}
