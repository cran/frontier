\name{coef.front41Output}
\alias{coef.front41Output}
\alias{coef.summary.front41Output}
\alias{vcov.front41Output}

\title{Coefficients from Frontier 4.1}

\description{
   These methods return the coefficients and their covariance matrix
   from a model estimated by Frontier 4.1.
}

\usage{
   \method{coef}{front41Output}( object, which = "MLE", ... )

   \method{coef}{summary.front41Output}( object, which = "MLE", ... )

   \method{vcov}{front41Output}( object, ... )
}

\arguments{
  \item{object}{an object of class \code{front41Output}
    or \code{summary.front41Output}
    (read/created by \code{\link{front41ReadOutput}}
    or \code{\link{summary.front41Output}}, respectively).}
  \item{which}{character string indication, which coefficients should
    be returned: either 'OLS' (from OLS estimation),
    'GRID' (from grid search), or
    'MLE' (from maximum likelihood estimation).}
  \item{...}{currently ignored.}
}

\value{
The \code{coef} method applied to an object of class \code{front41Output}
returns a vector containing all
coefficients estimated by Frontier 4.1.

The \code{coef} method applied to an object of class
\code{summary.front41Output} returns a matrix containing
the estimates, their standard errors, the \eqn{t} values and \eqn{P} values
of all coefficients estimated by Frontier 4.1.

The \code{vcov} method returns the covariance matrix
of all coefficients estimated by Frontier 4.1.
}

\author{Arne Henningsen, \email{ahenningsen@agric-econ.uni-kiel.de}}

\seealso{\code{\link{front41ReadOutput}}}

\keyword{methods}
