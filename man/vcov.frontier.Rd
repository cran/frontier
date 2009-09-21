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

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   vcov( sfaResult )
}

\keyword{models}
