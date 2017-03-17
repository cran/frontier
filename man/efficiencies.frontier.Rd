\name{efficiencies.frontier}
\alias{efficiencies.frontier}

\title{Returning Efficiency Estimates}

\description{
   This method returns efficiency estimates from stochastic frontier models
   estimated with \code{\link{frontier}}.
}

\usage{
\method{efficiencies}{frontier}( object, asInData = FALSE,
   logDepVar = TRUE, minusU = farrell, farrell = TRUE, 
   margEff = FALSE, \dots )
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
   \item{minusU}{logical. If \code{TRUE} (the default), 
      the efficiencies are calculated by E[exp(-u)],
      i.e. Farrel-type efficiencies are returned for input-oriented models,
      Shepard-type efficiencies are returned for output-oriented models,
      and the returned efficiency estimates have values 
      between zero and one,
      where a one indicates a fully efficient firm
      and a zero indicates a fully inefficient firm.
      If \code{FALSE}, 
      the efficiencies are calculated by E[exp(u)],
      i.e. Shepard-type efficiencies are returned for input-oriented models,
      Farrell-type efficiencies are returned for output-oriented models,
      and the returned efficiency estimates have values larger than 
      or equal to one,
      where a one indicates a fully efficient firm
      and plus infinity indicates a fully inefficient firm.}
   \item{farrell}{logical. This argument is only kept for backward compatibility
      and will be removed in the future.}
   \item{margEff}{logical. If \code{TRUE}, the marginal effects
      of the \eqn{z} variables (of an Efficiency Effects Frontier, EEF)
      on the efficiency measure are returned as an \sQuote{attribute} 
      to the returned object (i.e. the efficiency estimates).
      These marginal effects are calculated by the formula derived in
      Olsen and Henningsen (2011),
      which was slightly adjusted for the differing model specifications.
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
   
   If the dependent variable is logged, 
   the marginal effect of the \eqn{k}th \eqn{z} variable on the efficiency is
   \deqn{\frac{\partial E[ \exp( - \kappa \, u ) ]}{\partial z_{kit}}
   = \frac{  \delta_{k} ( 1 - \gamma ) 
      \exp \left( - \kappa \, \bar{\mu}_{it} 
         + \frac{1}{2} \bar{\sigma}^{2} \right) }{ 
            \Phi \left( \frac{\bar{\mu}_{it}}{ \bar{\sigma} } \right) }}
   \deqn{ \cdot \left( \frac{ \phi \left( -\kappa \, \bar{\sigma} 
         + \frac{ \bar{\mu}_{it} }{ \bar{\sigma} } \right) }{ \bar{\sigma} }
      - \frac{ \Phi \left( -\kappa \, \bar{\sigma} 
               + \frac{ \bar{\mu}_{it} }{ \bar{\sigma} } \right) \,
            \phi \left( \frac{ \bar{\mu}_{it} }{ \bar{\sigma} } \right)}{ 
         \bar{\sigma} \, 
            \Phi \left( \frac{ \bar{\mu}_{it} }{ \bar{\sigma}}\right) }
            - \kappa \, \Phi \left( -\kappa \, \bar{\sigma} 
               + \frac{ \bar{\mu}_{it} }{ \bar{\sigma} } \right) \right), }
   where
   \deqn{ \bar{\mu}_{it} 
      = ( 1 - \gamma ) \, z_{it} ' \delta 
         - \tau \, \gamma \, \epsilon_{it},}
   \deqn{ \bar{\sigma}^{2}
      = \gamma \, ( 1 - \gamma ) \, \sigma^{2},}
   \eqn{\kappa = 1} in case of Farrell efficiencies
   (i.e. efficiencies have values between between 0 and 1),
   whereas \eqn{\kappa = -1} otherwise 
   (i.e. efficiencies have values larger than 1),
   and \eqn{\tau = 1} if inefficiency decreases the dependent variable,
   whereas \eqn{\tau = -1} otherwise
   (see Olsen and Henningsen 2011).

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


\references{
   Olsen, Jakob Vesterlund and Arne Henningsen (2011): 
   Investment utilization and farm efficiency in Danish agriculture. 
   FOI working paper 2011/13, Institute of Food and Resource Economics, 
   University of Copenhagen, 
   \url{http://EconPapers.repec.org/RePEc:foi:wpaper:2011_13}.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfa}}, \code{\link{summary.frontier}},
   and \code{\link{efficiencies}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( "riceProdPhil" )
   library( "plm" )
   riceProdPhil <- pdata.frame( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

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
   
   # Technical Efficiency Effects Frontier (Battese & Coelli 1995)
   rice2 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
      EDYRS + BANRAT - 1, data = riceProdPhil )
   eff <- efficiencies( rice2, margEff = TRUE )
   attr( eff, "margEff" )   # marginal effects
}

\keyword{methods}
