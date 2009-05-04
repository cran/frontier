\name{coef.frontier}
\alias{coef.frontier}
\title{coef method for class frontier}

\description{
Extract the coefficients from stochastic frontier models
returned by \code{\link{frontier}}.
}

\usage{
\method{coef}{frontier}( object, which = "mle", \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{which}{character string. Which coefficients should be returned?
      ('start' for starting values provided by the user,
      'ols' for coefficients estimated by OLS,
      'grid' for coefficients obtained by the grid search,
      or 'mle' for coefficients estimated by Maximum Likelihood).}
   \item{\dots}{currently unused.}
}

\value{
   \code{coef.frontier} returns a named vector of the coefficients.
}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   sfa <- frontier( yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ), data = front41Data )
   coef( sfa, which = "ols" )
   coef( sfa, which = "grid" )
   coef( sfa )
}

\keyword{models}
