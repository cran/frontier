\name{residuals.frontier}
\alias{residuals.frontier}

\title{Returning Residuals}

\description{
   This method returns the residuals from stochastic frontier models
   estimated with the \pkg{frontier} package 
   (e.g. function \code{\link{sfa}}).
}

\usage{
\method{residuals}{frontier}( object, asInData = FALSE, \dots )
}

\arguments{
   \item{object}{a stochastic frontier model
      estimated with the \pkg{frontier} package 
      (e.g. function \code{\link{sfa}}).}
   \item{asInData}{logical. If \code{TRUE},
      the residuals are returned in the same order
      as the corresponding observations in the data set
      used for the estimation
      (see section \sQuote{value} below).}
   \item{\dots}{currently ignored.}
}

\value{
   If argument \code{asInData} is \code{FALSE} (default),
   a matrix of the residuals is returned,
   where each row corresponds to a firm (cross-section unit)
   and each column corresponds to a time period.

   If argument \code{asInData} is \code{TRUE},
   a vector of residuals is returned,
   where the residuals are in the same order
   as the corresponding observations in the data set
   used for the estimation.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfa}}, \code{\link{residuals}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( "riceProdPhil" )
   library( "plm" )
   riceProdPhil <- pdata.frame( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

   # Error Components Frontier (Battese & Coelli 1992), no time effect
   rice <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil )
   residuals( rice )
   riceProdPhil$residuals <- residuals( rice, asInData = TRUE )

   # Error Components Frontier (Battese & Coelli 1992), with time effect
   riceTime <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil, timeEffect = TRUE )
   residuals( riceTime )
   riceProdPhil$residualsTime <- residuals( riceTime, asInData = TRUE )
}

\keyword{methods}
