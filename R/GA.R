#' Genetic Algorithm for LHD
#'
#' \code{GA} returns a maximin distance LHD constructed by genetic algorithm (GA)
#'
#' @param n A positive integer.
#' @param k A positive integer.
#' @param m A positive even integer.
#' @param N A positive integer.
#' @param pmut A probability.
#' @param p A positive integer.
#' @param q The default is set to be 1, and it could be either 1 or 2.
#'
#' @return If all inputs are logical, then the output will be a \code{n} by \code{k} LHD.
#' @details \itemize{
#' \item \code{n} stands for the number of rows (or run size).
#' \item \code{k} stands for the number of columns (or the number of factors).
#' \item \code{m} stands for the number of population and it must be an even number.
#' \item \code{N} stands for the number of iterations.
#' \item \code{pmut} stands for the probability of mutation.
#' \item \code{p} is the parameter in the phi_p formula, and \code{p} is prefered to be large.
#' \item If \code{q} is 1 (the default setting), \code{dij} is the rectangular distance. If \code{q} is 2, \code{dij} is the Euclidean distance.
#' }
#'
#' @references Liefvendahl, M., and Stocki, R. (2006) A study on algorithms for optimization of Latin hypercubes. \emph{Journal of Statistical Planning and Inference}, \strong{136}, 3231-3247.
#'
#' @examples
#' #create a 8 by 3 maximin distance LHD, with # of population and iterations = 10,
#' #the probability of mutation is 1/(k-1)
#' tryGA1=GA(n=8,k=3,m=10,N=10,pmut=1/(3-1),p=50,q=1)
#' tryGA1
#' phi_p(tryGA1,p=50)   #calculate the phi_p of "tryGA1".
#'
#' #Another example with different n and k.
#' tryGA2=GA(n=12,k=2,m=10,N=10,pmut=1/(3-1),p=50,q=1)
#' tryGA2
#' phi_p(tryGA2,p=50)   #calculate the phi_p of "tryGA2".
#' @export

GA=function(n,k,m,N,pmut,p=50,q=1){
  #n and k are the rs and fa.
  #m: the number of population and it must be an even number.
  #N: maximum number of iterations.
  #pmut: the probability of mutation

  C=1  #Initialize counter index

  #step 2 starts, each X[,,i] is the L_i, i=1, ..., m
  X=rep(0,n*k*m)

  dim(X)=c(n,k,m)

  for (i in 1:m) {
    X[,,i]=rLHD(n=n,k=k)
  }
  #step 2 ends

  #step 3 starts
  phip=rep(0,m)

  for (i in 1:m) {
    phip[i]=phi_p(X[,,i],p=50)
  }
  #step 3 ends

  while (C<=N) {

    #step 4 starts: Select survivors (or parients)

    temp=cbind(phip,1:m)

    temp=temp[order(temp[,1]),]

    SI=temp[1:(m/2),2]                 #SI:Survivors Index

    Xnew=rep(0,n*k*(m/2))              #Creat survivors matrix L^{s}

    dim(Xnew)=c(n,k,m/2)

    for (i in 1:(m/2)) {

      Xnew[,,i]=X[,,SI[i]]

    }

    #step 4 ends. Xnew are the survivors matrix

    Xbest=Xnew[,,1]     #step 5 Find the best survivor

    for (i in 2:(m/2)) {    #step 6
      #step 7 and 9 starts
      rcol=sample(1:k,1)

      X[,,i]=Xbest
      X[,rcol,i]=Xnew[,rcol,i]

    } #step 8: end for

    X[,,1]=Xbest   #step 9

    for (i in 2:(m/2)) {    #step 10
      #step 11 and 13 starts
      rcol=sample(1:k,1)

      X[,,(i+(m/2))]=Xnew[,,i]
      X[,rcol,(i+(m/2))]=Xbest[,rcol]

    } #step 12: end for

    X[,,(1+(m/2))]=Xbest   #step 13

    #Until here, the X has been fully updated.

    for (i in 2:m) {      #step 14
      for (j in 1:k) {    #step 15

        z=stats::runif(1,0,1)             #step 16

        if (z<pmut){

          X[,,i]=exchange(X=X[,,i],j=j)   #step 17

        } #step 18: end if

      }   #step 19: end for
    }     #step 20: end for

    #step 21: update phi_p for all the L_i
    for (i in 1:m) {
      phip[i]=phi_p(X[,,i],p=50)
    }

    C=C+1
  }

  temp=cbind(phip,1:m)

  temp=temp[order(temp[,1]),]

  Xbest=X[,,temp[1,2]]

  Xbest

}

