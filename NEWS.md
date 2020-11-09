\#LHD 1.3.0
Part I - We added the following four functions:

OLHD2001, OLHD2007, OLHD2009, and OLHD2010.

These new functions are construction methods for orthogonal LHDs.

Part II - The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.

\#LHD 1.2.0
Part I - We added the following six functions:

FastMmLHD, GLP, LOO, LPWT, MWT, OLHD1998, and WT.

OLHD1998 is a construction method for orthogonal LHDs. The rest functions are methods for 
maximin distance LHDs under L_1 distance.

Part II - We updated all the existing functions:

1. To be more informative, the documentations of all the existing functions have been updated.

2. A new input argument, maxtime, was added for each of the following algorithm functions:

SA, OASA, SA2008, SLHD, LaPSO, and GA.

Now users can define a maximum CPU time for each of those six algorithm functions. In addition,
a progress bar will show up in R console to keep users informed about the situation of algorithm.

Part III - The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.

\#LHD 1.1.0
This is a big update for the whole package.

Part I - We added the following three optimality criteria functions:

Average absolute correlation (function name: AvgAbsCor)

Maximum absolute correlation (function name: MaxAbsCor)

Maximum projection criterion (function name: MaxProCriterion)

Part II - We updated the following functions:

SA, OA2LHD, OASA, SA2008, SLHD, LaPSO, GA

In the past, only maximin distance LHDs can be generated through these algorithms. Now, the default optimality criterion is the set to be "phi_p" still, which corresponds the maximin distance, but these algorithms are able to generate orthogonal LHDs and maximum projection LHDs, respectively. 

Another modification is that the majority of input arguments now have default settings. For those seeking instant design matrix or quick search, simply input run sizes and factor sizes would be enough. This modification saves times for users.

Part III - The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.

\#LHD 0.1.3
Updated SLHD function: The exchanging logic in Stage II has been updated.

\#LHD 0.1.2

The following functions have been updated:

SA, OASA, SA2008, SLHD, GA, and LaPSO.

The default for p used to be 50, and now is set to be 15. Besides,
modifications were made for each function above to improve accuracy of
result. Correspondingly, documentations (.Rd files), vignettes, and test
files were updated. These updates should also fix the errors from the
CRAN Package Check Results.

\#LHD 0.1.1

The Description field of DESCRIPTION file has been updated.
