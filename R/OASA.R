#' Orthogonal-Array-Based Simulated Annealing
#'
#' \code{OASA} returns a maximin distance LHD constructed by orthogonal-array-based simulated annealing algorithm (OASA)
#'
#' @param OA A Matrix.
#' @param n A positive integer.
#' @param m A positive integer.
#' @param s A positive integer.
#' @param r A positive integer.
#' @param N A positive integer.
#' @param T0 A positive number.
#' @param rate A positive percentage.
#' @param Tmin A positive number.
#' @param Imax A positive integer.
#' @param p A positive integer.
#' @param q The default is set to be 1, and it could be either 1 or 2.
#'
#' @return If all inputs are logical, then the output will be a \code{n} by \code{m} LHD.
#' @details \itemize{
#' \item \code{OA} stands for the input orthogonal array matrix.
#' \item \code{n} stands for the number of rows of OA.
#' \item \code{m} stands for the number of columns of OA.
#' \item \code{s} stands for the number of levels of OA.
#' \item \code{r} stands for the strength of OA.
#' \item \code{N} stands for the number of iterations.
#' \item \code{T0} stands for the user-defined initial temperature.
#' \item \code{rate} stands for temperature decrease rate, and it should be in (0,1). For example, rate=0.25 means the temperature decreases by 25\% each time.
#' \item \code{Tmin} stands for the minimium temperature allowed. When current temperature becomes smaller or equal to \code{Tmin}, the stopping criterion for current loop is met.
#' \item \code{Imax} stands for the maximum perturbations the algorithm will try without improvements before temperature is reduced. For the computation complexity consideration, \code{Imax} is recommended to be smaller or equal to 5.
#' \item \code{p} is the parameter in the phi_p formula, and \code{p} is prefered to be large.
#' \item If \code{q} is 1 (the default setting), \code{dij} is the rectangular distance. If \code{q} is 2, \code{dij} is the Euclidean distance.
#' }
#'
#' @references Leary, S., Bhaskar, A., and Keane, A. (2003) Optimal orthogonal-array-based latin hypercubes. \emph{Journal of Applied Statistics}, \strong{30}, 585-598.
#'
#' @examples
#' #create an OA(9,2,3,2)
#' OA=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = FALSE);OA
#'
#' #Use above "OA" as the input OA, with with # of iterations =10, initial
#' #temperature is set to be 10, decrease rate is 10%, minimium temperature is 1,
#' #maximum perturbations the algorithm will try without improvements is 5, and p=50
#' tryOASA=OASA(OA=OA,9,2,3,2,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
#' tryOASA
#' @export

OASA=function(OA,n,m,s,r,N,T0,rate,Tmin,Imax,p=50,q=1){
  #OA: OA must be an orthogonal array
  #n: number of rows of OA
  #m: number of columns of OA
  #s: number of levels of OA
  #r: strength of OA

  #N: maximum number of iterations.
  #T0: initial temperature
  #rate: temperature decrease rate. 0<rate<1
  #Tmin: minumum temperature for each itertaion,TPmin > 0
  #Imax:# of perturbations the algorithm will try without improvements before Temperature is reduced

  C=1  #step 1: counter index

  #step 2 begins
  X=OA

  #Create a 3-dimentional array named k, which corresponds to k=1,2,...,s
  k=rep(0,n)
  dim(k)=c(n/s,1,s)

  for (j in 1:m) {
    for (i in 1:s) {
      k[,,i]=seq(from=(i-1)*n/s+1,to=(i-1)*n/s+n/s,1) #This is the formula from Tang (1993)
      k[,,i]=sample(k[,,i])

      X[,j][X[,j]==i]=k[,,i]*100

    }
  }
  X=X/100
  #step 2 ends

  Xbest=X;TP=T0;Flag=1

  while (C<=N) {

    while(Flag==1 & TP>Tmin){
      Flag=0;I=1

      while (I<=Imax) {
        rcol=sample(1:m,1)   #step 3:Randomly choose a column

        #step 4 starts
        rrow=sample(1:n,1)         #Randomly choose a row

        e1=X[rrow,rcol]            #locate the randomly chosen element

        group=ceiling(e1/(n/s))        #locate the e1's level group in the OA

        #randomly select the 2nd element whose OA entry agrees with e1:
        e2=sample(k[,,group][k[,,group]!=e1],1)

        Xnew=X

        Xnew[,rcol][Xnew[,rcol]==e2]=e1

        Xnew[rrow,rcol]=e2
        #step 4 ends


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
