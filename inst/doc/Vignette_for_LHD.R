## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
devtools::load_all()
library(LHD)

## ------------------------------------------------------------------------
set.seed(1)
X=rLHD(n=6,k=3);X

## ------------------------------------------------------------------------
dij(X=X,i=2,j=4) #The default setting is the rectangular distance.

#If Euclidean distance is desired, run the following
dij(X=X,i=2,j=4,q=2)

## ------------------------------------------------------------------------
phi_p(X=X,p=50)  

#If Euclidean distance is desired, run the following
phi_p(X=X,p=50,q=2)  

## ------------------------------------------------------------------------
#Suppose we want to exchange two random elements from the 1st column of X.
Xnew=exchange(X=X,j=1)

#Look and compare
X;Xnew

#The SA function
set.seed(1)
trySA=SA(n=6,k=3,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
trySA

#The phi_p of trySA is much smaller than the phi_p of X
phi_p(X=trySA,p=50) 

## ------------------------------------------------------------------------
#create an OA(9,2,3,2)
OA=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = F);OA

#Transfer the OA above into a 9 by 2 LHD
tryOA=OA2LHD(OA=OA,9,2,3,2)
tryOA

#The OASA function
set.seed(1)
tryOASA=OASA(OA=OA,9,2,3,2,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
tryOASA

#The phi_p of tryOASA is smaller than the phi_p of SA with same input parameters
phi_p(X=tryOASA,p=50) 
phi_p(X=SA(n=9,k=2,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1),p=50) 

## ------------------------------------------------------------------------
set.seed(1)
trySA2008_63=SA2008(n=6,k=3,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
trySA2008_63
phi_p(X=trySA2008_63,p=50) 

#Another example with different n and k
trySA2008_92=SA2008(n=9,k=2,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
trySA2008_92
phi_p(X=trySA2008_92,p=50) 

## ------------------------------------------------------------------------
set.seed(1)
trySLHD_63F=SLHD(n=6,k=3,t=2,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1)
trySLHD_63F
phi_p(X=trySLHD_63F,p=50) 

#If the second stage is desired, run the following
trySLHD_63T=SLHD(n=6,k=3,t=2,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1,stage2=TRUE)
trySLHD_63T
phi_p(X=trySLHD_63T,p=50) 

#Another example with different n and k
trySLHD_92T=SLHD(n=9,k=2,t=1,N=10,T0=10,rate=0.1,Tmin=1,Imax=5,p=50,q=1,stage2=TRUE)
trySLHD_92T
phi_p(X=trySLHD_92T,p=50) 


## ------------------------------------------------------------------------
set.seed(1)
#This is LaPSO-P
tryLaPSO_63P=LaPSO(n=6,k=3,m=10,N=10,SameNumP=6/2,SameNumG=0,p0=1/(3-1),p=50,q=1)
tryLaPSO_63P
phi_p(X=tryLaPSO_63P,p=50) 

#This is LaPSO-G
tryLaPSO_63G=LaPSO(n=6,k=3,m=10,N=10,SameNumP=6/4,SameNumG=0,p0=1/(3-1),p=50,q=1)
tryLaPSO_63G
phi_p(X=tryLaPSO_63G,p=50) 

#Another example with different n and k
tryLaPSO_92G=LaPSO(n=9,k=2,m=10,N=10,SameNumP=9/4,SameNumG=0,p0=1/(2-1),p=50,q=1)
tryLaPSO_92G
phi_p(X=tryLaPSO_92G,p=50) 


## ------------------------------------------------------------------------
set.seed(1)
#Setting the probability of mutation is 1/(k-1)
tryGA_63=GA(n=6,k=3,m=10,N=10,pmut=1/(3-1),p=50,q=1)
tryGA_63
phi_p(X=tryGA_63,p=50) 

#Another example with different n and k.
tryGA_92=GA(n=9,k=2,m=10,N=10,pmut=1/(2-1),p=50,q=1)
tryGA_92
phi_p(X=tryGA_92,p=50) 

