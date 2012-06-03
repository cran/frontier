\name{summary.frontier}
\alias{summary.frontier}
\alias{print.summary.frontier}
\title{summary method for class frontier}

\description{
Create and print summary results of a stochastic frontier analysis
returned by \code{\link{frontier}}.
}

\usage{
\method{summary}{frontier}( object, effic = FALSE,
   logDepVar = TRUE, farrell = TRUE, \dots )
\method{print}{summary.frontier}( x, effic = x$printEffic, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{x}{an object of class \code{summary.frontier} 
      (returned by the function \code{summary.frontier}).}
   \item{effic}{logical. Print the individual efficiency estimates?}
   \item{logDepVar}{logical. Is the dependent variable logged?}
   \item{farrell}{logical. If \code{TRUE}, Farrel-type efficiencies
      that have values between 0 and 1 are used for calculating 
      mean elasticities.
      If \code{FALSE}, efficiencies that have values larger than 
      or equal to one are used for calculating the mean efficiencies.
      The conversion between the two efficiency measures is done by taking
      the inverse (before taking the mean).}
   \item{\dots}{further arguments to the \code{summary} method 
      are currently ignored;
      further arguments to the \code{print} method
      are forwarded to \code{\link[stats]{printCoefmat}}.}
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
   and \code{\link{lrtest.frontier}}.}

\examples{
   # example included in FRONTIER 4.1 (cross-section data)
   data( front41Data )

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   summary( sfaResult )

   # rice producers in the Phillipines (panel data)
   data( riceProdPhil )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

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
