\name{logLik.frontier}
\alias{logLik.frontier}
\title{Extract Log-Likelihood Value}

\description{
Extract the log-likelihood value(s) from stochastic frontier models
returned by \code{\link{frontier}}.
}

\usage{
\method{logLik}{frontier}( object, which = "mle", newParam = NULL, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{which}{character string. Which log-likelihood value
      should be returned?
      'ols' for the log-likelihood value of the parameters estimated by OLS,
      'grid' for the log-likelihood value of the parameters obtained
      by the grid search (only if no starting values were provided),
      'start' for the log-likelihood value of the starting values
      of the parameters specified by the user
      (only if starting values were provided),
      or 'mle' for the log-likelihood values of the parameters estimated
      by Maximum Likelihood.}
   \item{newParam}{optional vector of parameters.
      If this argument is provided by the user, the log-likelihood value
      of the model \code{object} is calculated with these (new) parameters.}
   \item{\dots}{currently unused.}
}

\value{
   \code{logLik.frontier} returns a scalar.
}

\seealso{\code{\link{frontier}}.}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )

   # SFA estimation with starting values obtained from a grid search
   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   logLik( sfaResult, which = "ols" )
   logLik( sfaResult, which = "grid" )
   logLik( sfaResult )

   # SFA estimation with starting values provided by the user
   sfaResult2 <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data, startVal = 0.9 * coef( sfaResult ) )
   logLik( sfaResult2, which = "ols" )
   logLik( sfaResult2, which = "start" )
   logLik( sfaResult2 )

   # evaluate log likelihood function for a user-provided parameter vector
   logLik( sfaResult, newParam = 0.9 * coef( sfaResult ) )
      # equal to  logLik( sfaResult2, which = "start" )

   # log likelihood function for different values of gamma
   plot( t( sapply( seq( 0.05, 0.95, 0.05 ), function(x) c( x,
      logLik( sfaResult, newParam = c( coef( sfaResult )[1:4], x ) ) ) ) ) )
}

\keyword{models}
