\name{vcov.frontier}
\alias{vcov.frontier}
\title{vcov method for class frontier}

\description{
Extract the covariance matrix of the maximum likelihood coefficients
of a stochastic frontier model returned by \code{\link{frontier}}.
}

\usage{
\method{vcov}{frontier}( object, extraPar = FALSE, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{extraPar}{logical. If \code{TRUE}, the variances and covariances
      of additional parameters are returned:
      \code{sigmaSqU} = \code{sigmaSq} * \code{gamma} 
      (with \eqn{u} ~ \eqn{N^+}{N+}( \code{mu}, \code{sigmaSqU} )),
      \code{sigmaSqV} = \code{sigmaSq} * ( 1 - \code{gamma} )
      (with \eqn{v} ~ N( 0, \code{sigmaSqV} )),
      \code{sigma} = \code{sigmaSq}^0.5,
      \code{sigmaU} = \code{sigmaSqU}^0.5,
      \code{sigmaV} = \code{sigmaSqV}^0.5,
      \code{lambdaSq} = \code{sigmaSqU} / \code{sigmaSqV},
      and \code{lambda} = \code{sigmaU} / \code{sigmaV}.
      Please note that \code{sigmaSqU} and \code{sigmaU} are not
      the variance and standard error, respectively, of \eqn{u}.}
   \item{\dots}{currently unused.}
}

\details{
   The variance-covariance matrix of the estimated parameters
   is taken from the direction matrix
   that is used in the final iteration of the Davidon-Fletcher-Powell procedure
   that is used for maximising the (log) likelihood function.
   
   If argument \code{extraPar} is \code{TRUE},
   the variances and covariances of the additional parameters
   are obtained by the delta method.
   Please note that the delta method might provide poor approximations
   of the \sQuote{true} variances and covariances,
   because parameter \eqn{\sigma^2}{sigma^2} is left-censored
   and parameter \eqn{\gamma}{gamma} is both left-censored and right-censored
   so that these parameters cannot be normally distributed.
   
   Please note further
   that it might not be appropriate to use standard statistical tests
   (e.g. t-tests or other Wald tests)
   that are based on the variances and covariances of \eqn{\sigma^2}{sigma^2},
   \eqn{\gamma}{gamma}, and the \sQuote{additional parameters},
   because these parameters are censored and cannot follow normal distributions.
}

\value{
   \code{vcov.frontier} returns the covariance matrix 
   of the maximum likelihood coefficients.
}

\seealso{\code{\link{coef.frontier}}, \code{\link{coef.summary.frontier}},
   \code{\link{summary.frontier}}, and \code{\link{sfa}}.}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   vcov( sfaResult )
}

\keyword{models}
