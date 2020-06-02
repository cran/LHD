## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
devtools::load_all()
library(LHD)

## -----------------------------------------------------------------------------
set.seed(1)
X=rLHD(n=6,k=3);X

## -----------------------------------------------------------------------------
dij(X=X,i=2,j=4) #The default setting is the rectangular distance.

#If Euclidean distance is desired, run the following
dij(X=X,i=2,j=4,q=2)

## -----------------------------------------------------------------------------
phi_p(X=X,p=15)  

#If Euclidean distance is desired, run the following
phi_p(X=X,p=15,q=2)  

## -----------------------------------------------------------------------------
AvgAbsCor(X=X)  #The average absolute correlation of X

MaxAbsCor(X=X)  #The maximum absolute correlation of X

## -----------------------------------------------------------------------------
MaxProCriterion(X=X)  #The maximum projection criterion of X

## -----------------------------------------------------------------------------
#Suppose we want to exchange two random elements from the 1st column of X.
Xnew=exchange(X=X,j=1)

#Look and compare
X;Xnew

#The SA function with default setting
set.seed(1)
trySA=SA(n=6,k=3)
trySA

#The phi_p of trySA is much smaller than the phi_p of X, which was 0.2517586
phi_p(X=trySA,p=15) 

#Now we try SA function with a different optimality criterion
set.seed(1)
trySA2=SA(n=6,k=3,OC="AvgAbsCor")
AvgAbsCor(trySA2)      #The average absolute correlation is about 0.05

## -----------------------------------------------------------------------------
#create an OA(9,2,3,2)
OA=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = F);OA

#Transfer the OA above into a 9 by 2 LHD
tryOA=OA2LHD(OA=OA)
tryOA

#The OASA function
set.seed(1)
tryOASA=OASA(OA=OA)
tryOASA

#The phi_p of tryOASA is smaller than the phi_p of SA 
phi_p(X=tryOASA,p=15); phi_p(X=SA(n=9,k=2),p=15) 

#Now we try OASA function with a different optimality criterion
tryOASA2=OASA(OA=OA,OC="AvgAbsCor")
AvgAbsCor(tryOASA2)      #The average absolute correlation is 0

## -----------------------------------------------------------------------------
set.seed(1)
trySA2008_63=SA2008(n=6,k=3)
trySA2008_63
phi_p(X=trySA2008_63,p=15) 

#Another example with different n and k
trySA2008_92=SA2008(n=9,k=2)
trySA2008_92
phi_p(X=trySA2008_92,p=15) 

#Now we try SA2008 function with a different optimality criterion
set.seed(1)
trySA2008=SA2008(n=6,k=3,OC="AvgAbsCor")
AvgAbsCor(trySA2008)      #The average absolute correlation is about 0.03

## -----------------------------------------------------------------------------
set.seed(1)
trySLHD_63F=SLHD(n=6,k=3,t=2)   #The default setting (stage2 is FALSE)
trySLHD_63F
phi_p(X=trySLHD_63F,p=15) 

#If the second stage is desired, run the following
trySLHD_63T=SLHD(n=6,k=3,t=2,stage2=TRUE)
trySLHD_63T
phi_p(X=trySLHD_63T,p=15)       #Result is improved (phi_p is smaller than above)

#Now we try SLHD function with a different optimality criterion
set.seed(1)
trySLHD=SLHD(n=6,k=3,OC="AvgAbsCor",stage2=TRUE)
AvgAbsCor(trySLHD)      #The average absolute correlation is about 0.07

## -----------------------------------------------------------------------------
set.seed(1)
tryLaPSO_63=LaPSO(n=6,k=3)
tryLaPSO_63
phi_p(X=tryLaPSO_63,p=15) 

#Another example with different n and k
tryLaPSO_92=LaPSO(n=9,k=2)
tryLaPSO_92
phi_p(X=tryLaPSO_92,p=15) 

#Now we try LaPSO function with a different optimality criterion
set.seed(1)
tryLaPSO=LaPSO(n=6,k=3,OC="AvgAbsCor")
AvgAbsCor(tryLaPSO)      #The average absolute correlation is about 0.1

## -----------------------------------------------------------------------------
set.seed(1)
tryGA_63=GA(n=6,k=3)
tryGA_63
phi_p(X=tryGA_63,p=15) 

#Another example with different n and k.
tryGA_92=GA(n=9,k=2)
tryGA_92
phi_p(X=tryGA_92,p=15) 

#Now we try GA function with a different optimality criterion
set.seed(1)
tryGA=GA(n=6,k=3,OC="AvgAbsCor")
AvgAbsCor(tryGA)      #The average absolute correlation is about 0.07

