# LHD 1.4.1

* Updated .Rbuildignore to make sure LICENSE.md is included in the package tarball (.tar.gz).

# LHD 1.4.0

* Updated maintainer's email address.

* Fixed the NOTEs in CRAN checks.

* Added MIT License file (LICENSE.md).

* Updated testing codes in the testthat folder.

* A minor update in the `FastMmLHD` function.

# LHD 1.3.3

* Updated the `LaPSO` function: the exchanging logic has been updated to provide more accurate results.

# LHD 1.3.2

* The `exchange` function was updated and now it is capable of exchanging elements either column-wise or row-wise.

* The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.

# LHD 1.3.1

* The following five functions were re-named to avoid confusions: `OLHD1998`, `OLHD2001`, `OLHD2007`, `OLHD2009`, and `OLHD2010`.
  - `OLHD1998` now is called `OLHD.Y1998`.
  - `OLHD2001` now is called `OLHD.B2001`.
  - `OLHD2007` now is called `OLHD.C2007`.
  - `OLHD2009` now is called `OLHD.L2009`.
  - `OLHD2010` now is called `OLHD.S2010`.
  
* Functions `LOO`, `LPWT`, and `MWT` are integrated into function `FastMmLHD`.

* The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.
  - Documentations were updated to provide recommendations to new users on how to set input arguments properly.

# LHD 1.3.0

* Part I - We added the following four functions: `OLHD2001`, `OLHD2007`, `OLHD2009`, and `OLHD2010`.

  - These new functions are construction methods for orthogonal LHDs.

* Part II - The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.

# LHD 1.2.0

* Part I - We added the following six functions: `FastMmLHD`, `GLP`, `LOO`, `LPWT`, `MWT`, `OLHD1998`, and `WT`.

  - `OLHD1998` is a construction method for generating orthogonal LHDs. The rest functions are construction methods for maximin distance LHDs under L_1 distance.

* Part II - We updated all the existing functions:

  - To be more informative, the documentations of all the existing functions have been updated.

  - A new input argument, maxtime, was added for each of the following algorithm functions: `SA`, `OASA`, `SA2008`, `SLHD`, `LaPSO`, and `GA`. Now users can define a maximum CPU time using that input argument. In addition, a progress bar will show up in the R console to inform users the overall progress.

* Part III - The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.

# LHD 1.1.0

* Part I - We added the following three optimality criteria functions:

  - Average absolute correlation (function name: `AvgAbsCor`)

  - Maximum absolute correlation (function name: `MaxAbsCor`)

  - Maximum projection criterion (function name: `MaxProCriterion`)

* Part II - We updated the following functions: `SA`, `OA2LHD`, `OASA`, `SA2008`, `SLHD`, `LaPSO`, and `GA`.

  - `SA`, `OASA`, `SA2008`, `SLHD`, `LaPSO`, and `GA` will be able to generate maximin distance LHDs, maximum projection LHDs, and nearly orthogonal LHDs from now on, while they can only generate maximin distance LHDs in the past. 

  - For `OA2LHD`, users no longer need to provide the parameters of input Orthogonal Array. The function itself will take care of that.

  - The majority of input arguments now have default settings. For users who seek instant design matrix or quick search, simply input run sizes and factor sizes would be enough. This modification saves times for users.

* Part III - The DESCRIPTION file, documentations (.Rd files), vignettes, and test files were updated accordingly.

# LHD 0.1.3

* Updated the `SLHD` function: the exchanging logic in Stage II has been updated.

# LHD 0.1.2

* The following functions have been updated: `SA`, `OASA`, `SA2008`, `SLHD`, `GA`, and `LaPSO`. 
  -The default input argument p used to be 50, and now is set to be 15. 

* Correspondingly, documentations (.Rd files), vignettes, and test files were updated. 

* This update also fixed the errors from the CRAN Package Check Results.

# LHD 0.1.1

* The Description field of DESCRIPTION file has been updated.
