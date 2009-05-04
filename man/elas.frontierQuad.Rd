\name{elas.frontierQuad}
\alias{elas.frontierQuad}
\title{Elasticities of a Quadratic/Translog Frontier}

\description{
   Calculate the elasticities of a quadratic or translog frontier function.
}

\usage{
\method{elas}{frontierQuad}( object, data = NULL, dataLogged = TRUE,
   yObs = FALSE, \dots )
}

\arguments{
   \item{object}{object of class \code{frontierQuad}
      (returned by \code{\link{frontierQuad}}).}
   \item{data}{dataframe containing the data;
      if it is not specified,
      the data frame that was used for the frontier estimation
      is used for calculating elasticities.}
   \item{dataLogged}{logical. Are the variables
      (specified in arguments \code{yName} and \code{xNames}
      and available in argument \code{data})
      already logged?
      (If argument \code{dataLogged} is \code{TRUE},
      the frontier function is of the translog form;
      if argument \code{dataLogged} is \code{FALSE},
      the frontier function is quadratic).}
   \item{yObs}{logical. Use observed values of the endogenous variable.
      If \code{FALSE} (default) predicted values calculated by
      \code{\link[micEcon]{quadFuncCalc}} are used
      (ignored if argument \code{dataLogged} is \code{TRUE}).}
   \item{\ldots}{currently ignored.}
}

\details{
   This method internally calls the functions
   \code{\link[micEcon]{translogEla}} and
   \code{\link[micEcon]{quadFuncEla}}.
}

\value{
   See documentation of \code{\link[micEcon]{translogEla}} and
   \code{\link[micEcon]{quadFuncEla}}.
}

\seealso{
   \code{\link{frontierQuad}}, \code{\link[micEcon]{translogEla}},
   and \code{\link[micEcon]{QuadFuncEla}}.
}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1 (cross-section data)
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   translog <- frontierQuad( yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ),
      data = front41Data )
   elas( translog )$ela
}

\keyword{models}
