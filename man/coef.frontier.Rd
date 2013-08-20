\name{coef.frontier}
\alias{coef.frontier}
\title{coef method for class frontier}

\description{
Extract the coefficients from stochastic frontier models
returned by \code{\link{frontier}}.
}

\usage{
\method{coef}{frontier}( object, which = "mle", extraPar = FALSE, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{which}{character string. Which coefficients should be returned?
      ('start' for starting values provided by the user,
      'ols' for coefficients estimated by OLS,
      'grid' for coefficients obtained by the grid search,
      or 'mle' for coefficients estimated by Maximum Likelihood).}
   \item{extraPar}{logical. If \code{TRUE}, additional parameters are returned:
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
      the variance and standard error, respectively, of \eqn{u}.
      If the model is an error components frontier
      and argument \code{timeEffect} is \code{FALSE},
      also the following additional parameters are returned:
      \code{varU} = the variance of \eqn{u},
      \code{sdU} = \code{varU}^0.5,
      and \code{gammaVar} = \code{varU} / ( \code{varU} + \code{sigmaSqV} ).
      Please note that the variance of \eqn{u}
      usually differs between observations
      if the model is an error component frontier with \sQuote{time effect}
      or an efficiency effects frontier.}
   \item{\dots}{currently unused.}
}

\value{
   \code{coef.frontier} returns a named vector of the coefficients.
}

\seealso{\code{\link{coef.summary.frontier}} and \code{\link{sfa}}.}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   coef( sfaResult, which = "ols" )
   coef( sfaResult, which = "grid" )
   coef( sfaResult )
}

\keyword{models}
