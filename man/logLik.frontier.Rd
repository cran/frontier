\name{logLik.frontier}
\alias{logLik.frontier}
\title{Extract Log-Likelihood Value}

\description{
Extract the log-likelihood value(s) from stochastic frontier models
returned by \code{\link{frontier}}.
}

\usage{
\method{logLik}{frontier}( object, which = "mle", \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{which}{character string. Which log-likelihood value
      should be returned?
      ('ols' for the log-likelihood value of the model estimated by OLS,
      'grid' for the log-likelihood value of the model obtained
      by the grid search (not implemented yet),
      or 'mle' for the log-likelihood values of the model estimated
      by Maximum Likelihood).}
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

   sfa <- frontier( yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ), data = front41Data )
   logLik( sfa, which = "ols" )
   logLik( sfa )
}

\keyword{models}
