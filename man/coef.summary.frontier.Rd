\name{coef.summary.frontier}
\alias{coef.summary.frontier}
\title{coef method for class summary.frontier}

\description{
Extract the coefficients, their standard errors, z-values or t-values,
and (asymptotic) P-values
from stochastic frontier models returned by the \code{summary} method
for objects of class \code{frontier}.
}

\usage{
\method{coef}{summary.frontier}( object, which = "mle", \dots )
}

\arguments{
   \item{object}{an object of class \code{\link{summary.frontier}}
      (returned by the \code{summary} method for objects
      of class \code{frontier}}
   \item{which}{character string. Which coefficients should be returned?
      ('ols' for coefficients estimated by OLS
      or 'mle' for coefficients estimated by Maximum Likelihood).}
   \item{\dots}{currently unused.}
}

\details{
   The standard errors of the estimated parameters
   are taken from the direction matrix
   that is used in the final iteration of the Davidon-Fletcher-Powell procedure
   that is used for maximising the (log) likelihood function.
   
   If argument \code{which} of this method is \code{"mle"} (the default)
   and argument \code{extraPar} of \code{\link{summary.frontier}}
   is set to \code{TRUE},
   some additional parameters, their standard errors, z-values,
   and (asymptotic) P-values are returned
   (see documentation of \code{\link{summary.frontier}}, 
   \code{\link{coef.frontier}}, or \code{\link{vcov.frontier}}).
   The standard errors of the additional parameters
   are obtained by the delta method.
   Please note that the delta method might provide poor approximations
   of the \sQuote{true} standard errors,
   because parameter \eqn{\sigma^2}{sigma^2} is left-censored
   and parameter \eqn{\gamma}{gamma} is both left-censored and right-censored
   so that these parameters cannot be normally distributed.
   
   Please note further
   that the t statistic and the z statistic are not reliable
   for testing the statistical signicance of \eqn{\sigma^2}{sigma^2},
   \eqn{\gamma}{gamma}, and the \sQuote{additional parameters},
   because these parameters are censored and cannot follow 
   a normal distribution or a t distribution.
}

\value{
   The \code{coef} method for objects of class \code{summary.frontier} 
   returns a matrix,
   where the four columns contain the estimated
   coefficients, their standard errors, z-values or t-values,
   and (asymptotic) P-values.
}

\seealso{\code{\link{coef.frontier}}, \code{\link{summary.frontier}},
   \code{\link{vcov.frontier}}, and \code{\link{sfa}}.}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   coef( summary( sfaResult ), which = "ols" )
   coef( summary( sfaResult ) )
   coef( summary( sfaResult, extraPar = TRUE ) )
}

\keyword{models}
