#' Simulated Annealing for LHD with Multi-objective Optimization Approach
#'
#' \code{SA2008} returns a \code{n} by \code{k} LHD matrix generated by simulated annealing algorithm with multi-objective optimization approach
#'
#' @param n A positive integer, which stands for the number of rows (or run size).
#' @param k A positive integer, which stands for the number of columns (or factor size).
#' @param N A positive integer, which stands for the number of iterations. The default is set to be 10. A large value of \code{N} will result a high CPU time, and it is recommended to be no greater than 500.
#' @param T0 A positive number, which stands for the user-defined initial temperature. The default is set to be 10.
#' @param rate A positive percentage, which stands for temperature decrease rate, and it should be in (0,1). For example, rate=0.25 means the temperature decreases by 25\% each time. The default is set to be 10\%.
#' @param Tmin A positive number, which stands for the minimium temperature allowed. When current temperature becomes smaller or equal to \code{Tmin}, the stopping criterion for current loop is met. The default is set to be 1.
#' @param Imax A positive integer, which stands for the maximum perturbations the algorithm will try without improvements before temperature is reduced. The default is set to be 5. For the computation complexity consideration, \code{Imax} is recommended to be smaller or equal to 5.
#' @param OC An optimality criterion. The default setting is "phi_p", and it could be one of the following: "phi_p", "AvgAbsCor", "MaxAbsCor", "MaxProCriterion".
#' @param p A positive integer, which is the parameter in the phi_p formula, and \code{p} is prefered to be large. The default is set to be 15.
#' @param q The default is set to be 1, and it could be either 1 or 2. If \code{q} is 1, \code{dij} is the Manhattan (rectangular) distance. If \code{q} is 2, \code{dij} is the Euclidean distance.
#' @param maxtime A positive number, which indicates the expected maximum CPU time given by user, and it is measured by minutes. For example, maxtime=3.5 indicates the CPU time will be no greater than three and half minutes. The default is set to be 5.
#'
#' @return If all inputs are logical, then the output will be a \code{n} by \code{k} LHD. This modified simulated annealing algorithm reduces columnwise correlations and maximizes minimum distance between design points simultaneously, with a cost of more computational complexity.
#'
#' @references Joseph, V.R., and Hung, Y. (2008) Orthogonal-maximin Latin hypercube designs. \emph{Statistica Sinica}, \strong{18}, 171-186.
#'
#' @examples
#' #generate a 5 by 3 maximin distance LHD with the default setting
#' try=SA2008(n=5,k=3)
#' try
#' phi_p(try)   #calculate the phi_p of "try".
#'
#' #Another example
#' #generate a 8 by 4 nearly orthogonal LHD
#' try2=SA2008(n=8,k=4,OC="AvgAbsCor")
#' try2
#' AvgAbsCor(try2)  #calculate the average absolute correlation.
#' @export

SA2008=function(n,k,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,OC="phi_p",p=15,q=1,maxtime=5){
  #n and k are the rs and fa.
  #N: maximum number of iterations.
  #T0: initial temperature
  #rate: temperature decrease rate. 0<rate<1
  #Tmin: minumum temperature for each itertaion,TPmin > 0
  #Imax:# of perturbations the algorithm will try without improvements before Temperature is reduced
  #OC: optimality criterion, the default is "phi_p", along with default p and q

  maxtime=maxtime*60  #convert minutes to seconds
  timeALL=NULL        #record all cpu time

  C=1  #step 1: counter index

  X=rLHD(n=n,k=k)   #step 2

  Xbest=X;TP=T0;Flag=1

  if(OC=="phi_p"){

    progressbar = utils::txtProgressBar(min = 0, max = N, style = 3)

    while (C<=N) {

      time0=Sys.time()

      while(Flag==1 & TP>Tmin){
        Flag=0;I=1

        while (I<=Imax) {

          #step 3 starts
          rho2max=0

          for (i in 1:k) {

            rho2=0

            for (j in seq(1,k)[-i]) {
              rho2=rho2+stats::cor(X[,i],X[,j])^2
            }

            rho2=rho2/(k-1)

            if (rho2>=rho2max){rho2max=rho2;col1=i}

          }
          #step 3 ends


          #step 4 starts
          phipmax=0

          for (i in 1:n) {

            phip=0

            for (j in seq(1,n)[-i]) {
              phip=phip+dij(X,i=i,j=j)^(-p)
            }

            phip=phip^(1/p)

            if (phip>phipmax){phipmax=phip;row1=i}

          }

          e1=X[row1,col1]     #the is the 1st element

          row2=sample(seq(1,n)[-row1],1)  #randomly choose the 2nd element from the same column

          e2=X[row2,col1]   #2nd element

          Xnew=X

          Xnew[row1,col1]=e2
          Xnew[row2,col1]=e1

          #step 4 ends


          a=phi_p(X=Xnew,p=p,q=q)       #step 5 begins here
          b=phi_p(X=X,p=p,q=q)
          if (a<b){X=Xnew;Flag=1}
          if (a>=b){
            prob=exp((b-a)/TP)
            draw=sample(c(0,1),1,prob=c(1-prob,prob))    #draw==1 means replace
            if(draw==1){X=Xnew;Flag=1}
          }                         #step 5 ends here

          c=phi_p(X=Xbest,p=p,q=q)
          if (a<c){Xbest=Xnew;I=1}
          if (a>=c){I=I+1}

        }

        TP=TP*(1-rate)
      }

      time1=Sys.time()
      timediff=time1-time0
      timeALL=c(timeALL,timediff)

      ##########progress bar codes
      utils::setTxtProgressBar(progressbar, C)
      ##########

      if(as.numeric(sum(timeALL)+timediff)<=maxtime){C=C+1}
      if(as.numeric(sum(timeALL)+timediff)>maxtime){C=N+1}
      TP=T0;Flag=1
    }
  }

  if(OC=="AvgAbsCor"){

    progressbar = utils::txtProgressBar(min = 0, max = N, style = 3)

    while (C<=N) {

      time0=Sys.time()

      while(Flag==1 & TP>Tmin){
        Flag=0;I=1

        while (I<=Imax) {

          #step 3 starts
          rho2max=0

          for (i in 1:k) {

            rho2=0

            for (j in seq(1,k)[-i]) {
              rho2=rho2+stats::cor(X[,i],X[,j])^2
            }

            rho2=rho2/(k-1)

            if (rho2>=rho2max){rho2max=rho2;col1=i}

          }
          #step 3 ends


          #step 4 starts
          phipmax=0

          for (i in 1:n) {

            phip=0

            for (j in seq(1,n)[-i]) {
              phip=phip+dij(X,i=i,j=j)^(-p)
            }

            phip=phip^(1/p)

            if (phip>phipmax){phipmax=phip;row1=i}

          }

          e1=X[row1,col1]     #the is the 1st element

          row2=sample(seq(1,n)[-row1],1)  #randomly choose the 2nd element from the same column

          e2=X[row2,col1]   #2nd element

          Xnew=X

          Xnew[row1,col1]=e2
          Xnew[row2,col1]=e1

          #step 4 ends


          a=AvgAbsCor(X=Xnew)       #step 5 begins here
          b=AvgAbsCor(X=X)
          if (a<b){X=Xnew;Flag=1}
          if (a>=b){
            prob=exp((b-a)/TP)
            draw=sample(c(0,1),1,prob=c(1-prob,prob))    #draw==1 means replace
            if(draw==1){X=Xnew;Flag=1}
          }                         #step 5 ends here

          c=AvgAbsCor(X=Xbest)
          if (a<c){Xbest=Xnew;I=1}
          if (a>=c){I=I+1}

        }

        TP=TP*(1-rate)
      }

      time1=Sys.time()
      timediff=time1-time0
      timeALL=c(timeALL,timediff)

      ##########progress bar codes
      utils::setTxtProgressBar(progressbar, C)
      ##########

      if(as.numeric(sum(timeALL)+timediff)<=maxtime){C=C+1}
      if(as.numeric(sum(timeALL)+timediff)>maxtime){C=N+1}
      TP=T0;Flag=1
    }
  }

  if(OC=="MaxAbsCor"){

    progressbar = utils::txtProgressBar(min = 0, max = N, style = 3)

    while (C<=N) {

      time0=Sys.time()

      while(Flag==1 & TP>Tmin){
        Flag=0;I=1

        while (I<=Imax) {

          #step 3 starts
          rho2max=0

          for (i in 1:k) {

            rho2=0

            for (j in seq(1,k)[-i]) {
              rho2=rho2+stats::cor(X[,i],X[,j])^2
            }

            rho2=rho2/(k-1)

            if (rho2>=rho2max){rho2max=rho2;col1=i}

          }
          #step 3 ends


          #step 4 starts
          phipmax=0

          for (i in 1:n) {

            phip=0

            for (j in seq(1,n)[-i]) {
              phip=phip+dij(X,i=i,j=j)^(-p)
            }

            phip=phip^(1/p)

            if (phip>phipmax){phipmax=phip;row1=i}

          }

          e1=X[row1,col1]     #the is the 1st element

          row2=sample(seq(1,n)[-row1],1)  #randomly choose the 2nd element from the same column

          e2=X[row2,col1]   #2nd element

          Xnew=X

          Xnew[row1,col1]=e2
          Xnew[row2,col1]=e1

          #step 4 ends


          a=MaxAbsCor(X=Xnew)       #step 5 begins here
          b=MaxAbsCor(X=X)
          if (a<b){X=Xnew;Flag=1}
          if (a>=b){
            prob=exp((b-a)/TP)
            draw=sample(c(0,1),1,prob=c(1-prob,prob))    #draw==1 means replace
            if(draw==1){X=Xnew;Flag=1}
          }                         #step 5 ends here

          c=MaxAbsCor(X=Xbest)
          if (a<c){Xbest=Xnew;I=1}
          if (a>=c){I=I+1}

        }

        TP=TP*(1-rate)
      }

      time1=Sys.time()
      timediff=time1-time0
      timeALL=c(timeALL,timediff)

      ##########progress bar codes
      utils::setTxtProgressBar(progressbar, C)
      ##########

      if(as.numeric(sum(timeALL)+timediff)<=maxtime){C=C+1}
      if(as.numeric(sum(timeALL)+timediff)>maxtime){C=N+1}
      TP=T0;Flag=1
    }
  }

  if(OC=="MaxProCriterion"){

    progressbar = utils::txtProgressBar(min = 0, max = N, style = 3)

    while (C<=N) {

      time0=Sys.time()

      while(Flag==1 & TP>Tmin){
        Flag=0;I=1

        while (I<=Imax) {

          #step 3 starts
          rho2max=0

          for (i in 1:k) {

            rho2=0

            for (j in seq(1,k)[-i]) {
              rho2=rho2+stats::cor(X[,i],X[,j])^2
            }

            rho2=rho2/(k-1)

            if (rho2>=rho2max){rho2max=rho2;col1=i}

          }
          #step 3 ends


          #step 4 starts
          phipmax=0

          for (i in 1:n) {

            phip=0

            for (j in seq(1,n)[-i]) {
              phip=phip+dij(X,i=i,j=j)^(-p)
            }

            phip=phip^(1/p)

            if (phip>phipmax){phipmax=phip;row1=i}

          }

          e1=X[row1,col1]     #the is the 1st element

          row2=sample(seq(1,n)[-row1],1)  #randomly choose the 2nd element from the same column

          e2=X[row2,col1]   #2nd element

          Xnew=X

          Xnew[row1,col1]=e2
          Xnew[row2,col1]=e1

          #step 4 ends


          a=MaxProCriterion(X=Xnew)       #step 5 begins here
          b=MaxProCriterion(X=X)
          if (a<b){X=Xnew;Flag=1}
          if (a>=b){
            prob=exp((b-a)/TP)
            draw=sample(c(0,1),1,prob=c(1-prob,prob))    #draw==1 means replace
            if(draw==1){X=Xnew;Flag=1}
          }                         #step 5 ends here

          c=MaxProCriterion(X=Xbest)
          if (a<c){Xbest=Xnew;I=1}
          if (a>=c){I=I+1}

        }

        TP=TP*(1-rate)
      }

      time1=Sys.time()
      timediff=time1-time0
      timeALL=c(timeALL,timediff)

      ##########progress bar codes
      utils::setTxtProgressBar(progressbar, C)
      ##########

      if(as.numeric(sum(timeALL)+timediff)<=maxtime){C=C+1}
      if(as.numeric(sum(timeALL)+timediff)>maxtime){C=N+1}
      TP=T0;Flag=1
    }
  }

  avgtime=round(mean(timeALL),2)
  iterations=length(timeALL)

  close(progressbar)
  print(paste0("average CPU time per iteration is: ", avgtime, " seconds"))
  print(paste0("the number of iterations completed is: ", iterations))
  print(paste0("the elements in design matrix is scaled to be 1 to ", n))

  Xbest
}
