\name{fitted.frontier}
\alias{fitted.frontier}
\alias{predict.frontier}

\title{Fitted and Predicted (Frontier) Values}

\description{
   This method returns the fitted and predicted \dQuote{frontier} values
   from stochastic frontier models
   estimated with the \pkg{frontier} package 
   (e.g. function \code{\link{sfa}}).
}

\usage{
\method{fitted}{frontier}( object, asInData = FALSE, \dots )

\method{predict}{frontier}( object, newdata = NULL, asInData = TRUE, \dots )
}

\arguments{
   \item{object}{a stochastic frontier model
      estimated with the \pkg{frontier} package 
      (e.g. function \code{\link{sfa}}).}
   \item{newdata}{an optional data frame from which the explanatory variables
      are used to calculate the predicted \dQuote{frontier} values.
      If this argument is \code{NULL},
      the fitted values are returned.}
   \item{asInData}{logical. If \code{TRUE},
      the fitted values are returned in the same order
      as the corresponding observations in the data set
      used for the estimation
      (see section \sQuote{value} below).}
   \item{\dots}{currently ignored.}
}

\value{
   If argument \code{asInData} is \code{FALSE},
   a matrix of the fitted or predicted values is returned,
   where each row corresponds to a firm (cross-section unit)
   and each column corresponds to a time period.

   If argument \code{asInData} is \code{TRUE},
   a vector of fitted or predicted values is returned,
   where the fitted values are in the same order
   as the corresponding observations in the data set
   used for the estimation
   or the data set specified by argument \code{newdata}.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfa}}, \code{\link{fitted}}, \code{\link{predict}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( "riceProdPhil" )
   library( "plm" )
   riceProdPhil <- pdata.frame( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

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
