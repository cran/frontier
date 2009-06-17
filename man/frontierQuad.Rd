\name{frontierQuad}
\alias{frontierQuad}
\title{Quadratic or Translog Frontiers}

\description{
   This is a convenient interface for estimating
   quadratic or translog stochastic frontier functions
   using \code{\link{frontier}}.
}

\usage{
frontierQuad( yName, xNames, shifterNames = NULL, zNames = NULL,
   data, quadHalf = TRUE, \dots )
}

\arguments{
   \item{yName}{string: name of the endogenous variable.}
   \item{xNames}{a vector of strings containing the names of the X variables
      (exogenous variables of the production or cost function)
      that should be included as linear, quadratic, and interaction terms.}
   \item{shifterNames}{a vector of strings containing the names of the
          X variables that should be included as shifters
          only (not in quadratic or interaction terms).}
   \item{zNames}{a vector of strings containing the names of the Z variables
      (variables explaining the efficiency level).}
   \item{data}{a (panel) data frame that contains the data;
      if \code{data} is a usual data.frame,
      it is assumed that these are cross-section data;
      if \code{data} is a panel data frame
      (created with \code{\link[plm]{plm.data}}),
      it is assumed that these are panel data.}
   \item{quadHalf}{logical. Multiply the quadratic terms by one half?}
   \item{\ldots}{further arguments passed to \code{\link{frontier}}.}
}

\value{
   \code{frontierQuad} returns a list of class \code{frontierQuad}
   (and \code{frontier})
   containing the same elements as returned by \code{\link{frontier}}
   and additionally the element \code{quadHalf}
   which is equal to the argument \code{quadHalf}.
}

\seealso{\code{\link{frontier}}.}

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
   translog
}

\keyword{models}
