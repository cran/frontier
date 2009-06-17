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
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   # SFA estimation with starting values obtained from a grid search
   sfa <- frontier( yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ), data = front41Data )
   logLik( sfa, which = "ols" )
   logLik( sfa, which = "grid" )
   logLik( sfa )

   # SFA estimation with starting values provided by the user
   sfa2 <- frontier( yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ), data = front41Data,
      startVal = 0.9 * coef( sfa ) )
   logLik( sfa2, which = "ols" )
   logLik( sfa2, which = "start" )
   logLik( sfa2 )

   # evaluate log likelihood function for a user-provided parameter vector
   logLik( sfa, newParam = 0.9 * coef( sfa ) )
      # equal to  logLik( sfa2, which = "start" )

   # log likelihood function for different values of gamma
   plot( t( sapply( seq( 0.05, 0.95, 0.05 ), function(x) c( x,
      logLik( sfa, newParam = c( coef( sfa )[1:4], x ) ) ) ) ) )
}

\keyword{models}
