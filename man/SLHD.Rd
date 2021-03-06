% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SLHD.R
\name{SLHD}
\alias{SLHD}
\title{Sliced Latin Hypercube Design (SLHD)}
\usage{
SLHD(
  n,
  k,
  t = 1,
  N = 10,
  T0 = 10,
  rate = 0.1,
  Tmin = 1,
  Imax = 3,
  OC = "phi_p",
  p = 15,
  q = 1,
  stage2 = FALSE,
  maxtime = 5
)
}
\arguments{
\item{n}{A positive integer, which stands for the number of rows (or run size).}

\item{k}{A positive integer, which stands for the number of columns (or factor size).}

\item{t}{A positive integer, which stands for the number of slices. \code{n}/\code{t} must be a positive integer, that is, n is divisible by t. \code{t} must be smaller or equal to \code{k} when \code{n} is 9 or larger. \code{t} must be smaller than \code{k} when \code{n} is smaller than 9. Otherwise, the funtion will never stop. The default is set to be 1.}

\item{N}{A positive integer, which stands for the number of iterations. The default is set to be 10. A large value of \code{N} will result a high CPU time, and it is recommended to be no greater than 500.}

\item{T0}{A positive number, which stands for the user-defined initial temperature. The default is set to be 10.}

\item{rate}{A positive percentage, which stands for temperature decrease rate, and it should be in (0,1). For example, rate=0.25 means the temperature decreases by 25\% each time. The default is set to be 10\%.}

\item{Tmin}{A positive number, which stands for the minimium temperature allowed. When current temperature becomes smaller or equal to \code{Tmin}, the stopping criterion for current loop is met. The default is set to be 1.}

\item{Imax}{A positive integer, which stands for the maximum perturbations the algorithm will try without improvements before temperature is reduced. The default is set to be 5. For the computation complexity consideration, \code{Imax} is recommended to be smaller or equal to 5.}

\item{OC}{An optimality criterion. The default setting is "phi_p", and it could be one of the following: "phi_p", "AvgAbsCor", "MaxAbsCor", "MaxProCriterion".}

\item{p}{A positive integer, which is the parameter in the phi_p formula, and \code{p} is prefered to be large. The default is set to be 15.}

\item{q}{The default is set to be 1, and it could be either 1 or 2. If \code{q} is 1, \code{dij} is the Manhattan (rectangular) distance. If \code{q} is 2, \code{dij} is the Euclidean distance.}

\item{stage2}{A logic input argument, and it could be either FALSE or TRUE. If \code{stage2} is FALSE (the default setting), \code{SLHD} will only implement the first stage of the algorithm. If \code{stage2} is TRUE, \code{SLHD} will implement the whole algorithm.}

\item{maxtime}{A positive number, which indicates the expected maximum CPU time given by user, and it is measured by minutes. For example, maxtime=3.5 indicates the CPU time will be no greater than three and half minutes. The default is set to be 5.}
}
\value{
If all inputs are logical, then the output will be a \code{n} by \code{k} LHD. As mentioned from the original paper, the first stage plays a much more important role since it optimizes the slice level. More resources should be given to the first stage if computational budgets are limited. Let m=n/t, where m is the number of rows for each slice, if (m)^k >> n, the second stage becomes optional. That is the reason why we add a \code{stage2} parameter to let users decide if the second stage is needed.
}
\description{
\code{SLHD} returns a \code{n} by \code{k} LHD matrix generated by improved two-stage algorithm
}
\examples{
#generate a 5 by 3 maximin distance LHD with the default setting
try=SLHD(n=5,k=3)
try
phi_p(try)   #calculate the phi_p of "try".

#generate a 5 by 3 maximin distance LHD with stage II
#let stage2=TRUE and other input are the same as above
try2=SLHD(n=5,k=3,stage2=TRUE)
try2
phi_p(try2)   #calculate the phi_p of "try2".

#Another example
#generate a 8 by 4 nearly orthogonal LHD
try3=SLHD(n=8,k=4,OC="AvgAbsCor",stage2=TRUE)
try3
AvgAbsCor(try3)  #calculate the average absolute correlation.
}
\references{
Ba, S., Myers, W.R., and Brenneman, W.A. (2015) Optimal Sliced Latin Hypercube Designs. \emph{Technometrics}, \strong{57}, 479-487.
}
