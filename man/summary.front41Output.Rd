\name{summary.front41Output}
\alias{summary.front41Output}
\alias{print.summary.front41Output}
\title{Summarizing the Estimation of Frontier 4.1}

\description{
   \code{summary.front41Output} summarizes the estimation results
   of a model estimated by Frontier 4.1..
}

\usage{
\method{summary}{front41Output}( object, \dots )

\method{print}{summary.front41Output}( x, efficiencies = FALSE, \dots )
}

\arguments{
   \item{object}{an object of class \code{front41Output}
      (read/created by \code{\link{front41ReadOutput}}.}
   \item{x}{object of class \code{summary.front41Output}
      (returned by the \code{summary} method for objects of class
      \code{front41ReadOutput}).}
   \item{efficiencies}{logical. Print all efficiency estimates?
      (If \code{FALSE}, only the mean efficiency is printed.)}
   \item{\dots}{currently ignored.}
}

\value{
   The \code{summary} method returns a list of class
   \code{summary.front41Output}
   with the same elements as object returned by \code{\link{front41ReadOutput}}.
   However, the elements \code{olsResults}, \code{gridResults}, and
   \code{mleResults} have an additional culumn with marginal significance
   levels (P values).
   The \eqn{P} values of the OLS estimates are calculated using
   the \eqn{t} distribution,
   while the (asymptotic) \eqn{P} values of the ML estimates
   are calculated based on the assumption
   that their \eqn{t} values follow an (asymptotic) standard normal
   distribution.
}

\seealso{\code{\link{front41ReadOutput}}, \code{\link{front41WriteInput}}.}

\author{Arne Henningsen}

\examples{
   # read the output file that is provided with Frontier 4.1
   outFile <- system.file( "front41/EG1.OUT", package = "frontier" )
   sfa <- front41ReadOutput( outFile )
   summary( sfa )
}

\keyword{models}
