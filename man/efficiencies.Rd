\name{efficiencies}
\alias{efficiencies}
\alias{efficiencies.default}

\title{Returning Efficiency Estimates}

\description{
   This method returns efficiency estimates from frontier models.
}

\usage{
efficiencies( object, \dots )
\method{efficiencies}{default}( object, \dots )
}

\arguments{
   \item{object}{a frontier model.}
   \item{\dots}{further arguments for methods.}
}

\details{
   This is a generic function.
   The default method just returns the element
   \code{effic} from \code{object}.
}

\author{Arne Henningsen}

\seealso{\code{\link{efficiencies.frontier}}.}

\keyword{methods}
