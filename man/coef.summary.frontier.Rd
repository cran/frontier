\name{coef.summary.frontier}
\alias{coef.summary.frontier}
\title{coef method for class summary.frontier}

\description{
Extract the coefficients, their standard errors, t-values, and P-values
from stochastic frontier models returned by the \code{summary} method
for objects of class \code{frontier}.
}

\usage{
\method{coef}{summary.frontier}( object, which = "mle", ... )
}

\arguments{
   \item{object}{an object of class \code{\link{summary.frontier}}
      (returned by the \code{summary} method for objects
      of class \code{frontier}}
   \item{which}{character string. Which coefficients should be returned?
      ('ols' for coefficients estimated by OLS
      or 'mle' for coefficients estimated by Maximum Likelihood).}
   \item{...}{currently unused.}
}

\value{
   The \code{coef} method for objects of class \code{summary.frontier} 
   returns a matrix,
   where the four columns contain the estimated
   coefficients, their standard errors, t-values, and P-values.
}

\author{Arne Henningsen \email{ahenningsen@agric-econ.uni-kiel.de}}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   sfa <- frontier( front41Data, "firm", "time", "logOutput",
      c( "logCapital", "logLabour" ) )
   coef( summary( sfa ), which = "ols" )
   coef( summary( sfa ) )
}

\keyword{models}
