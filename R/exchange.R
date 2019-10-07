#' Exchange two random elements
#'
#' \code{exchange} returns a new design matrix after two randomly selected elements are switched from a user-defined column.
#'
#' @param X A Matrix.
#' @param j A positive integer.
#'
#' @return If all inputs are logical, then the output will be a new design matrix after the exchange.
#' @details \itemize{
#' \item \code{X} stands for the design matrix.
#' \item \code{j} is the j^th column of the design matrix, and it should be in [1,ncol(X)].
#' }
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #Choose the first column of toy and exchange two randomly selected elements.
#' toy_new=exchange(X=toy,j=1)
#' toy;toy_new
#' @export

exchange=function(X,j){
  #Exchange two randomly selected elements within the column j of matrix X
  #return with a Xnew, which is the matrix after exchange.

  location=sample(1:nrow(X),2)    #randomly selected two elements
  e1=location[1]                  #record the location of the 1st element
  e2=location[2]                  #record the location of the 2nd element
  et=NULL                         #This is a transit element

  Xnew=X
  et=Xnew[e1,j]
  Xnew[e1,j]=Xnew[e2,j]
  Xnew[e2,j]=et

  Xnew
}
