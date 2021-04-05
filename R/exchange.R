#' Exchange two random elements in a matrix
#'
#' \code{exchange} returns a new matrix by switching two randomly selected elements from a user-defined matrix
#'
#'
#' @param X A matrix object. In general, \code{X} stands for a design matrix.
#' @param j A positive integer, which stands for the j^{th} column (or row) of \code{X}, and it should be within [1,ncol(X)] (or [1,nrow(X)]).
#' @param type An exchange type. If \code{type} is "col" (the default setting), two random elements will be exchanged within column \code{j}. If \code{type} is "row", two random elements will be exchanged within row \code{j}.
#'
#' @return If all inputs are logical, then the output will be a new design matrix after the exchange.
#'
#' @examples
#' #create a toy LHD with 5 rows and 3 columns
#' toy=rLHD(n=5,k=3);toy
#'
#' #Choose the first column of toy and exchange two randomly selected elements.
#' try.col=exchange(X=toy,j=1,type="col")
#' toy;try.col
#'
#' #Choose the first row of toy and exchange two randomly selected elements.
#' try.row=exchange(X=toy,j=1,type="row")
#' toy;try.row
#' @export

exchange=function(X,j,type="col"){

  if(type=="col"){
    #Exchange two randomly selected elements within the column j of matrix X
    #return with a new X, which is the matrix after exchange.

    location=sample(1:nrow(X),2)    #randomly select two rows

    e1=X[location[1],j]             #record the 1st element
    e2=X[location[2],j]             #record the 2nd element

    X[location[1],j]=e2
    X[location[2],j]=e1
  }

  if(type=="row"){
    #Exchange two randomly selected elements within the row j of matrix X
    #return with a new X, which is the matrix after exchange.

    location=sample(1:ncol(X),2)    #randomly select two cols

    e1=X[j,location[1]]             #record the 1st element
    e2=X[j,location[2]]             #record the 2nd element

    X[j,location[1]]=e2
    X[j,location[2]]=e1
  }

  X
}
