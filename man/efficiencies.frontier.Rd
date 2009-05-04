\name{efficiencies.frontier}
\alias{efficiencies.frontier}

\title{Returning Efficiency Estimates}

\description{
   This method returns efficiency estimates from stochastic frontier models
   estimated with \code{\link{frontier}}.
}

\usage{
\method{efficiencies}{frontier}( object, asInData = FALSE, \dots )
}

\arguments{
   \item{object}{a stochastic frontier model
      returned by \code{\link{frontier}}.}
   \item{asInData}{logical. If \code{TRUE},
      the efficiency estimates are returned in the same order
      as the corresponding observations in the data set
      used for the estimation
      (see section \sQuote{value} below).}
   \item{\dots}{currently ignored.}
}

\value{
   If argument \code{asInData} is \code{FALSE} (default),
   a matrix of efficiency estimates is returned,
   where each row corresponds to a firm (cross-section unit)
   and each column corresponds to a time period
   (only if efficiency estimates differ between time periods).

   If argument \code{asInData} is \code{TRUE},
   a vector of efficiency estimates is returned,
   where the efficiency estimates are in the same order
   as the corresponding observations in the data set
   used for the estimation.
}

\author{Arne Henningsen}

\seealso{\code{\link{frontier}}, \code{\link{efficiencies}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( riceProdPhil )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )
   riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
   riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
   riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
   riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

   # Error Components Frontier (Battese & Coelli 1992), no time effect
   rice <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ), data = riceProdPhil )
   efficiencies( rice )
   riceProdPhil$efficiencies <- efficiencies( rice, asInData = TRUE )

   # Error Components Frontier (Battese & Coelli 1992), with time effect
   riceTime <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ), data = riceProdPhil,
      timeEffect = TRUE )
   efficiencies( riceTime )
   riceProdPhil$efficienciesTime <- efficiencies( riceTime, asInData = TRUE )
}

\keyword{methods}
