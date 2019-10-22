#' Particle Swarm Optimization for LHD
#'
#' \code{LaPSO} returns a maximin distance LHD constructed by particle swarm optimization algorithm (PSO)
#'
#' @param n A positive integer.
#' @param k A positive integer.
#' @param m A positive integer.
#' @param N A positive integer.
#' @param SameNumP A non-negative integer.
#' @param SameNumG A non-negative integer.
#' @param p0 A probability.
#' @param p A positive integer.
#' @param q The default is set to be 1, and it could be either 1 or 2.
#'
#' @return If all inputs are logical, then the output will be a \code{n} by \code{k} LHD.
#' @details \itemize{
#' \item \code{n} stands for the number of rows (or run size).
#' \item \code{k} stands for the number of columns (or the number of factors).
#' \item \code{m} stands for the number of particles.
#' \item \code{N} stands for the number of iterations.
#' \item \code{SameNumP} stands for how many elements in current column of current particle LHD should be the same as corresponding Personal Best. SameNumP=0, 1, 2, ..., n, and 0 means to skip the "exchange".
#' \item \code{SameNumG} stands for how many elements in current column of current particle LHD should be the same as corresponding Global Best. SameNumP=0, 1, 2, ..., n, and 0 means to skip the "exchange".
#' \item \code{SameNumP} and \code{SameNumG} cannot be 0 at the same time.
#' \item \code{p0} stands the probability of exchange two randomly selected elements in current column of current particle LHD.
#' \item \code{p} is the parameter in the phi_p formula, and \code{p} is prefered to be large.
#' \item If \code{q} is 1 (the default setting), \code{dij} is the rectangular distance. If \code{q} is 2, \code{dij} is the Euclidean distance.
#' }
#'
#' @note Here are some general suggestions about the parameters: \itemize{
#' \item \code{SameNumP} is approximately \code{n}/2 when \code{SameNumG} is 0.
#' \item \code{SameNumG} is approximately \code{n}/4 when \code{SameNumP} is 0.
#' \item \code{p0} * (\code{k} - 1) = 1 or 2 is often sufficient. So \code{p0} = 1/(\code{k} - 1) or 2/(\code{k} - 1).
#' }
#'
#' @references Chen, R.-B., Hsieh, D.-N., Hung, Y., and Wang, W. (2013) Optimizing Latin hypercube designs by particle swarm. \emph{Stat. Comput.}, \strong{23}, 663-676.
#'
#' @examples
#' #create a 8 by 3 maximin distance LHD, with # of particles and iterations = 10, when SameNumG is 0
#' tryLaPSO1=LaPSO(n=8,k=3,m=10,N=10,SameNumP=8/2,SameNumG=0,p0=1/(3-1),p=15,q=1)
#' tryLaPSO1
#' phi_p(tryLaPSO1,p=15,q=1)   #calculate the phi_p of "tryLaPSO1".
#'
#' #create a 8 by 3 maximin distance LHD, with # of particles and iterations = 10, when SameNumP is 0
#' tryLaPSO2=LaPSO(n=8,k=3,m=10,N=10,SameNumP=0,SameNumG=8/4,p0=1/(3-1),p=15,q=1)
#' tryLaPSO2
#' phi_p(tryLaPSO2,p=14,q=1)   #calculate the phi_p of "tryLaPSO2".
#' @export


LaPSO=function(n,k,m,N,SameNumP,SameNumG,p0,p=15,q=1){
  #n and k are the rs and fa.
  #m: the number of particles
  #N: maximum number of iterations.

  #SameNumP: choose how many elements in column j of current LHD should be the same as
  #corresponding Personal Best. SameNumP=0, 1, 2, ..., n, and 0 means not run.
  #SameNumG: choose how many elements in column j of current LHD should be the same as
  #corresponding Global Best. SameNumG=0, 1, 2, ..., n, and 0 means not run.
  #p0: the probability of exchange two randomly selected elements in j of current LHD

  C=1  #Initialize counter index

  #step 2 starts, each X[,,i] is the L_i^{0}, i=1, ..., m
  X=rep(0,n*k*m)

  dim(X)=c(n,k,m)

  for (i in 1:m) {
    X[,,i]=rLHD(n=n,k=k)
  }
  #step 2 ends

  #step 3: Initialize personal best for each particle.
  pbest=X

  #step 4 starts: Initialize the global best
  gbest=X[,,1]

  for (i in 2:m) {
    if (phi_p(X[,,i],p=p,q=q)<phi_p(gbest,p=p,q=q)){gbest=X[,,i]}
  }

  #step 4 ends

  Xnew=X     #step 5


  while(C<=N){

    for (i in 1:m) {     #step 6
      for (j in 1:k) {   #step 7

        #step 8 starts

        if (SameNumP>0){
          for (a in 1:SameNumP) {
            rrow=sample(1:n,1)
            e_r=Xnew[rrow,j,i]        #Randomly choose an element in column j, denoted by e_r

            e_p=pbest[rrow,j,i]       #e_p: the element in pbest whose location is same as e_r

            if (e_r!=e_p){

              #locate the row number of e_p in column j of current LHD, L_i^{new}.
              location=cbind(X[,j,i],1:n)[,2][cbind(X[,j,i],1:n)[,1]==e_p]

              Xnew[location,j,i]=e_r

              Xnew[rrow,j,i]=e_p
            }

          }
        }
        #step 8 ends

        #step 9 starts

        if (SameNumG>0){
          for (b in 1:SameNumG) {
            rrow=sample(1:n,1)
            e_r=Xnew[rrow,j,i]        #Randomly choose an element in column j, denoted by e_r

            e_g=gbest[rrow,j]       #e_p: the element in pbest whose location is same as e_r

            if (e_r!=e_g){

              #locate the row number of e_p in column j of current LHD, L_i^{new}.
              location=cbind(X[,j,i],1:n)[,2][cbind(X[,j,i],1:n)[,1]==e_g]

              Xnew[location,j,i]=e_r

              Xnew[rrow,j,i]=e_g
            }

          }
        }
        #step 9 ends


        z=stats::runif(1,0,1)             #step 10

        if (z<p0){

          Xnew[,,i]=exchange(X=X[,,i],j=j)   #step 11

        }          #step 12: end if


      }   #step 13: end for

    }    #step 14: end for


    for (i in 1:m) {     #step 15

      if (phi_p(Xnew[,,i],p=p,q=q)<phi_p(pbest[,,i],p=p,q=q)){pbest[,,i]=Xnew[,,i]}   #step 16-19
      if (phi_p(Xnew[,,i],p=p,q=q)<phi_p(gbest,p=p,q=q)){gbest=Xnew[,,i]}             #step 20-22
    }      #step 23: end for

    C=C+1
  }

  gbest
}
