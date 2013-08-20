\name{fitted.frontier}
\alias{fitted.frontier}

\title{Returning Fitted (Frontier) Values}

\description{
   This method returns the fitted \dQuote{frontier} values
   from stochastic frontier models
   estimated with the \pkg{frontier} package 
   (e.g. function \code{\link{sfa}}).
}

\usage{
\method{fitted}{frontier}( object, asInData = FALSE, \dots )
}

\arguments{
   \item{object}{a stochastic frontier model
      estimated with the \pkg{frontier} package 
      (e.g. function \code{\link{sfa}}).}
   \item{asInData}{logical. If \code{TRUE},
      the fitted values are returned in the same order
      as the corresponding observations in the data set
      used for the estimation
      (see section \sQuote{value} below).}
   \item{\dots}{currently ignored.}
}

\value{
   If argument \code{asInData} is \code{FALSE} (default),
   a matrix of the fitted values is returned,
   where each row corresponds to a firm (cross-section unit)
   and each column corresponds to a time period.

   If argument \code{asInData} is \code{TRUE},
   a vector of fitted values is returned,
   where the fitted values are in the same order
   as the corresponding observations in the data set
   used for the estimation.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfa}}, \code{\link{fitted}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( "riceProdPhil" )
   library( "plm" )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

   # Error Components Frontier (Battese & Coelli 1992), no time effect
   rice <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil )
   fitted( rice )
   riceProdPhil$fitted <- fitted( rice, asInData = TRUE )

   # Error Components Frontier (Battese & Coelli 1992), with time effect
   riceTime <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil, timeEffect = TRUE )
   fitted( riceTime )
   riceProdPhil$fittedTime <- fitted( riceTime, asInData = TRUE )
}

\keyword{methods}
