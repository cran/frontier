\name{vcov.frontier}
\alias{vcov.frontier}
\title{vcov method for class frontier}

\description{
Extract the covariance matrix of the maximum likelihood coefficients
of a stochastic frontier model returned by \code{\link{frontier}}.
}

\usage{
\method{vcov}{frontier}( object, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{\dots}{currently unused.}
}

\value{
   \code{vcov.frontier} returns the covariance matrix 
   of the maximum likelihood coefficients.
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
   vcov( sfa )
}

\keyword{models}
