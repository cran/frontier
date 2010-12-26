\name{efficiencies.frontier}
\alias{efficiencies.frontier}

\title{Returning Efficiency Estimates}

\description{
   This method returns efficiency estimates from stochastic frontier models
   estimated with \code{\link{frontier}}.
}

\usage{
\method{efficiencies}{frontier}( object, asInData = FALSE,
   logDepVar = TRUE, farrell = TRUE, margEff = FALSE, \dots )
}

\arguments{
   \item{object}{a stochastic frontier model
      returned by \code{\link{frontier}}.}
   \item{asInData}{logical. If \code{TRUE},
      the efficiency estimates are returned in the same order
      as the corresponding observations in the data set
      used for the estimation
      (see section \sQuote{value} below).}
   \item{logDepVar}{logical. Is the dependent variable logged?}
   \item{farrell}{logical. If \code{TRUE}, Farrel-type efficiencies
      that have values between 0 and 1 are returned.
      If \code{FALSE}, the returned efficiencies have values larger than 
      or equal to one.}
   \item{margEff}{logical. If \code{TRUE}, the marginal effects
      of the \eqn{z} variables (of an Efficiency Effects Frontier, EEF)
      on the efficiency measure are returned as an \sQuote{attribute} 
      to the returned object (i.e. the efficiency estimates).
      Currently, this feature is implemented only for models 
      with logged dependent variables.}
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
   
   If argument \code{margEff} is \code{TRUE},
   and the model is an Efficiency Effects Frontier (EFF) 
   with \eqn{z} variables,
   and the dependent variable is logged,
   the returned efficiency estimates have an attribute \code{"margEff"}
   that contains the marginal effects of the \eqn{z} variables
   on the efficiency measure.
   If argument \code{asInData} is \code{FALSE},
   this attribute is a 3-dimensional array,
   where the first dimension represents the individual firm,
   the second deminsion represents the time period,
   and the third dimension represents the \eqn{z} variables.
   In contrast, if argument \code{asInData} is \code{TRUE},
   this attribute is a matrix,
   where the rows represent the observations
   and the columns represent the \eqn{z} variables.
}

\author{Arne Henningsen \email{arne.henningsen@gmail.com}}

\seealso{\code{\link{sfa}}, \code{\link{summary.frontier}},
   and \code{\link{efficiencies}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( riceProdPhil )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

   # Error Components Frontier (Battese & Coelli 1992), no time effect
   rice <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil )
   efficiencies( rice )
   riceProdPhil$efficiencies <- efficiencies( rice, asInData = TRUE )

   # Error Components Frontier (Battese & Coelli 1992), with time effect
   riceTime <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil, timeEffect = TRUE )
   efficiencies( riceTime )
   riceProdPhil$efficienciesTime <- efficiencies( riceTime, asInData = TRUE )
}

\keyword{methods}
