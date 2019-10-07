#' Simulated Annealing for LHD
#'
#' \code{SA} returns a maximin distance LHD constructed by simulated annealing algorithm (SA)
#'
#' @param n A positive integer.
#' @param k A positive integer.
#' @param N A positive integer.
#' @param T0 A positive number.
#' @param rate A positive percentage.
#' @param Tmin A positive number.
#' @param Imax A positive integer.
#' @param p A positive integer.
#' @param q The default is set to be 1, and it could be either 1 or 2.
#'
#' @return If all inputs are logical, then the output will be a \code{n} by \code{k} LHD.
#' @details \itemize{
#' \item \code{n} stands for the number of rows (or run size).
#' \item \code{k} stands for the number of columns (or the number of factors).
#' \item \code{N} stands for the number of iterations.
#' \item \code{T0} stands for the user-defined initial temperature.
#' \item \code{rate} stands for temperature decrease rate, and it should be in (0,1). For example, rate=0.25 means the temperature decreases by 25\% each time.
#' \item \code{Tmin} stands for the minimium temperature allowed. When current temperature becomes smaller or equal to \code{Tmin}, the stopping criterion for current loop is met.
#' \item \code{Imax} stands for the maximum perturbations the algorithm will try without improvements before temperature is reduced. For the computation complexity consideration, \code{Imax} is recommended to be smaller or equal to 5.
#' \item \code{p} is the parameter in the phi_p formula, and \code{p} is prefered to be large.
#' \item If \code{q} is 1 (the default setting), \code{dij} is the rectangular distance. If \code{q} is 2, \code{dij} is the Euclidean distance.
#' }
#'
#' @references Morris, M.D., and Mitchell, T.J. (1995) Exploratory designs for computer experiments. \emph{Journal of Statistical Planning and Inference}, \strong{43}, 381-402.
#'
#' @examples
#' #create a 5 by 3 maximin distance LHD, with # of iterations =10, initial
#' #temperature is set to be 10, decrease rate is 10%, minimium temperature is 1,
#' #maximum perturbations the algorithm will try without improvements is 5, and p=50
#' try=SA(n=5,k=3,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
#' try
#' phi_p(try,p=50)   #calculate the phi_p of "try".
#'
#' #Another example with different input
#' try2=SA(n=9,k=2,N=50,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
#' try2
#' @export

SA=function(n,k,N,T0,rate,Tmin,Imax,p=50,q=1){
  #n and k are the rs and fa.
  #N: maximum number of iterations.
  #T0: initial temperature
  #rate: temperature decrease rate. 0<rate<1
  #Tmin: minumum temperature for each itertaion,TPmin > 0
  #Imax:# of perturbations the algorithm will try without improvements before Temperature is reduced

  C=1  #step 1: counter index

  X=rLHD(n=n,k=k)   #step 2

  Xbest=X;TP=T0;Flag=1

  while (C<=N) {

    while(Flag==1 & TP>Tmin){
      Flag=0;I=1

      while (I<=Imax) {
        rcol=sample(1:k,1)   #step 3:Randomly choose a column
        Xnew=exchange(X=X,j=rcol)  #step 4:Exchange two randomly elements within the column "rcol"

        a=phi_p(X=Xnew,p=p)       #step 5 begins here
        b=phi_p(X=X,p=p)
        if (a<b){X=Xnew;Flag=1}
        if (a>=b){
          prob=exp((b-a)/TP)
          draw=sample(c(0,1),1,prob=c(1-prob,prob))    #draw==1 means replace
          if(draw==1){X=Xnew;Flag=1}
        }                         #step 5 ends here

        c=phi_p(X=Xbest,p=p)
        if (a<c){Xbest=Xnew;I=1}
        if (a>=c){I=I+1}

      }

      TP=TP*(1-rate)
    }

    C=C+1;TP=T0;Flag=1
  }

  Xbest
}
