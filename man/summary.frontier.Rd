\name{summary.frontier}
\alias{summary.frontier}
\alias{print.summary.frontier}
\title{summary method for class frontier}

\description{
Create and print summary results of a stochastic frontier analysis
returned by \code{\link{frontier}}.
}

\usage{
\method{summary}{frontier}( object, extraPar = FALSE, effic = FALSE,
   logDepVar = TRUE, effMinusU = farrell, farrell = TRUE, \dots )
\method{print}{summary.frontier}( x, effic = x$printEffic, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{x}{an object of class \code{summary.frontier} 
      (returned by the function \code{summary.frontier}).}
   \item{extraPar}{logical. If \code{TRUE}, some additional parameters,
      their standard errors, z-values, and P values are returned:
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
      If the model is an error components frontier,
      also the following additional parameters are returned:
      \code{varU} = the variance of \eqn{u},
      \code{sdU} = \code{varU}^0.5,
      and \code{gammaVar} = \code{varU} / ( \code{varU} + \code{sigmaSqV} ).
      Please note that the variance of \eqn{u}
      usually differs between observations
      if the model is an error component frontier with \sQuote{time effect}
      or an efficiency effects frontier.}
   \item{effic}{logical. Print the individual efficiency estimates?}
   \item{logDepVar}{logical. Is the dependent variable logged?}
   \item{effMinusU}{logical. If \code{TRUE} (the default), 
      the efficiencies are calculated by E[exp(-u)].
      If \code{FALSE}, 
      the efficiencies are calculated by E[exp(u)].
      For details, see documentation of argument \code{minusU} 
      of \code{\link{efficiencies.frontier}}.}
   \item{farrell}{logical. This argument is only kept for backward compatibility
      and will be removed in the future.}
   \item{\dots}{further arguments to the \code{summary} method 
      are currently ignored;
      further arguments to the \code{print} method
      are forwarded to \code{\link[stats]{printCoefmat}}.}
}

\details{
   The standard errors of the estimated parameters
   are taken from the direction matrix
   that is used in the final iteration of the Davidon-Fletcher-Powell procedure
   that is used for maximising the (log) likelihood function.
   
   If argument \code{extraPar} is \code{TRUE},
   the standard errors of the additional parameters
   are obtained by the delta method.
   Please note that the delta method might provide poor approximations
   of the \sQuote{true} standard errors,
   because parameter \eqn{\sigma^2}{sigma^2} is left-censored
   and parameter \eqn{\gamma}{gamma} is both left-censored and right-censored
   so that these parameters cannot be normally distributed.
   
   Please note further
   that the t statistic and the z statistic are not reliable
   for testing the statistical signicance of \eqn{\sigma^2}{sigma^2},
   \eqn{\gamma}{gamma}, and the \sQuote{additional parameters},
   because these parameters are censored and cannot follow 
   a normal distribution or a t distribution.
}

\value{
   \code{summary.frontier} returns a list of class \code{summary.frontier}
   that is identical to an object returned by \code{\link{frontier}}
   with two modifications and (up to) four additional elements:
   \item{olsParam}{matrix of OLS estimates, their standard errors,
      t-values, and P-values.}
   \item{mleParam}{matrix of ML estimates, their standard errors,
      z-values, and asymptotic P-values.}
   \item{logDepVar}{logical. Argument \code{logDepVar} (see above).}
   \item{printEffic}{argument \code{effic}.}
   \item{effic}{matrix. Efficiency estimates:
      each row corresponds to a cross-section;
      each column corresponds to a time period.}
   \item{efficMean}{numeric scalar. Mean efficiency.}
   \item{efficYearMeans}{numeric vector.
      Mean efficiency for each year in the sample
      (only for panel data but not for the Error Components Frontier
      without time effects).}
}

\author{Arne Henningsen}

\seealso{\code{\link{sfa}}, \code{\link{efficiencies.frontier}}, 
   \code{\link{vcov.frontier}}, and \code{\link{lrtest.frontier}}.}

\examples{
   # example included in FRONTIER 4.1 (cross-section data)
   data( front41Data )

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   summary( sfaResult )

   # rice producers in the Phillipines (panel data)
   data( "riceProdPhil" )
   library( "plm" )
   riceProdPhil <- pdata.frame( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

   # Error Components Frontier
   rice <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil )
   summary( rice )

   # Efficiency Effects Frontier
   rice2 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
      EDYRS + BANRAT, data = riceProdPhil )
   summary( rice2 )
}

\keyword{models}
