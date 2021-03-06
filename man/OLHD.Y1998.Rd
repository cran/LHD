% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OLHD.Y1998.R
\name{OLHD.Y1998}
\alias{OLHD.Y1998}
\title{Orthogonal Latin Hypercube Design}
\usage{
OLHD.Y1998(m)
}
\arguments{
\item{m}{A positive integer, and it must be greater than or equal to 2.}
}
\value{
If all inputs are logical, then the output will be an orthogonal LHD with the following run size: \code{n=2^m+1} and the following factor size: \code{k=2m-2}.
}
\description{
\code{OLHD.Y1998} returns a \code{2^m+1} by \code{2m-2} orthogonal Latin hypercube design generated by the construction method of Ye (1998)
}
\examples{
#create an orthogonal LHD with m=3. So n=2^m+1=9 and k=2*m-2=4
OLHD.Y1998(m=3)

#create an orthogonal LHD with m=4. So n=2^m+1=17 and k=2*m-2=6
OLHD.Y1998(m=4)

}
\references{
Ye, K.Q. (1998) Orthogonal column Latin hypercubes and their application in computer experiments. \emph{Journal of the American Statistical Association}, \strong{93}(444), 1430-1439.
}
