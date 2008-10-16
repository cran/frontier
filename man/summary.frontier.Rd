\name{summary.frontier}
\alias{summary.frontier}
\alias{print.summary.frontier}
\title{summary method for class frontier}

\description{
Create and print summary results of a stochastic frontier analysis
returned by \code{\link{frontier}}.
}

\usage{
\method{summary}{frontier}( object, effic = FALSE, ... )
\method{print}{summary.frontier}( x, effic = x$printEffic, ... )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{x}{an object of class \code{summary.frontier} 
      (returned by the function \code{summary.frontier}).}
   \item{effic}{logical. Print the individual efficiency estimates?}
   \item{...}{currently unused.}
}

\value{
   \code{summary.frontier} returns a list of class \code{summaryfrontier}
   that is identical to an object returned by \code{\link{frontier}}
   with three exceptions:
   \item{olsParam}{matrix of OLS estimates, their standard errors,
      t values, and P values.}
   \item{mleParam}{matrix of ML estimates, their standard errors,
      t values, and P values.}
   \item{printEffic}{argument \code{effic}.}
}

\author{Arne Henningsen \email{ahenningsen@agric-econ.uni-kiel.de}}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   sfa <- frontier( front41Data, "firm", "time", "logOutput",
      c( "logCapital", "logLabour" ) )
   summary( sfa )

   # rice producers in the Phillipines
   data( riceProdPhil )
   riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
   riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
   riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
   riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

   rice <- frontier( riceProdPhil,
      crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
      yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
   summary( rice )

   rice2 <- frontier( riceProdPhil,
      crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
      yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
      zNames = c( "EDYRS", "BANRAT" ) )
   summary( rice2 )
}

\keyword{models}
