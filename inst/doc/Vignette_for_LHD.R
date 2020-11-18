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

## -----------------------------------------------------------------------------
#n by n design when 2n+1 is prime
try=FastMmLHD(8,8)
try
phi_p(try)   #calculate the phi_p of "try".

#n by n design when n+1 is prime
try2=FastMmLHD(12,12)
try2
phi_p(try2)   #calculate the phi_p of "try2".

#n by n-1 design when n is prime
try3=FastMmLHD(7,6)
try3
phi_p(try3)   #calculate the phi_p of "try3".

#General cases
try4=FastMmLHD(24,8)
try4
phi_p(try4)   #calculate the phi_p of "try4".

## -----------------------------------------------------------------------------
#create an orthogonal LHD with m=4. So n=2^m+1=17 and k=2*m-2=6
tryOLHD=OLHD.Y1998(m=4)
tryOLHD
MaxAbsCor(tryOLHD)  #zero columnwise correlations

## -----------------------------------------------------------------------------
#create an orthogonal LHD with m=4. So n=2^m+1=17 and k=4+3=7
tryOLHD=OLHD.C2007(m=4)
tryOLHD
MaxAbsCor(tryOLHD)  #zero columnwise correlations

## -----------------------------------------------------------------------------
#create a 5 by 2 OLHD
OLHD=OLHD.C2007(m=2)

#create an OA(25,6,5,2)
OA=matrix(c(2,2,2,2,2,1,2,1,5,4,3,5,3,2,1,5,4,5,1,5,4,3,2,5,
4,1,3,5,2,3,1,2,3,4,5,2,1,3,5,2,4,3,1,1,1,1,1,1,4,3,2,1,5,5,
5,5,5,5,5,1,4,4,4,4,4,1,3,1,4,2,5,4,3,3,3,3,3,1,3,5,2,4,1,3,
3,4,5,1,2,2,5,4,3,2,1,5,2,3,4,5,1,2,2,5,3,1,4,4,1,4,2,5,3,4,
4,2,5,3,1,4,2,4,1,3,5,3,5,3,1,4,2,4,5,2,4,1,3,3,5,1,2,3,4,2,
4,5,1,2,3,2),ncol=6,nrow=25,byrow=T)

#Construct a 25 by 12 OLHD
tryOLHD=OLHD.L2009(OLHD,OA)
tryOLHD
MaxAbsCor(tryOLHD)  #zero columnwise correlations

## -----------------------------------------------------------------------------
#create an orthogonal LHD with C=3, r=3, type="odd".
#So n=3*2^4+1=49 and k=2^3=8
tryOLHD1=OLHD.S2010(C=3,r=3,type="odd")
tryOLHD1

#create an orthogonal LHD with C=3, r=3, type="even".
#So n=3*2^4=48 and k=2^3=8
tryOLHD2=OLHD.S2010(C=3,r=3,type="even")
tryOLHD2

MaxAbsCor(tryOLHD1)  #zero columnwise correlations
MaxAbsCor(tryOLHD2)  #zero columnwise correlations

## -----------------------------------------------------------------------------
#create an orthogonal LHD with n=11 and k=5
OLHD.B2001(n=11,k=5)

#create an orthogonal LHD with n=7 and k=6
OLHD.B2001(n=7,k=6)

