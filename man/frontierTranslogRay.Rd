\name{frontierTranslogRay}
\alias{frontierTranslogRay}
\encoding{latin1}
\title{Translog Ray Frontiers}

\description{
   This is a convenient interface for estimating
   translog ray stochastic frontier functions
   using \code{\link{frontier}}.
}

\usage{
frontierTranslogRay( yNames, xNames, shifterNames = NULL,
   zNames = NULL, data, \dots )
}

\arguments{
   \item{yNames}{a vector of two character strings containing
      the names of the two dependent variables.}
   \item{xNames}{a vector of strings containing the names of the X variables
      (exogenous variables of the production or cost function)
      that should be included as linear, quadratic, and interaction terms.}
   \item{shifterNames}{a vector of strings containing the names of the
          X variables that should be included as shifters
          only (not in quadratic or interaction terms).}
   \item{zNames}{a vector of strings containing the names of the Z variables
      (variables explaining the efficiency level).}
   \item{data}{a (panel) data frame that contains the data
      (see documentation of \code{\link{frontier}})
      NOTE: the variables in \code{yNames} and \code{xNames}
      are logarithmized internally and hence must be in natural units,
      while the variables in \code{shifterNames} and \code{zNames}
      are NOT logarithmized internally and hence must be specified
      as they should be used in the model.}
   \item{\ldots}{further arguments passed to \code{\link{frontierQuad}}
      and possibly further to \code{\link{frontier}}.}
}

\value{
   \code{frontierTranslogRay} returns a list of class \code{frontierTranslogRay}
   (as well as \code{frontierQuad} and \code{frontier})
   containing almost the same elements as returned by \code{\link{frontier}}.
   Additionally, it includes following objects:
   \item{distance}{the \dQuote{distance} from the origin (zero)
      to the point of the dependent variables.}
   \item{theta}{the \dQuote{direction} from the origin (zero)
      to the point of the dependent variables.}
}

\references{
   Löthgren, M. (1997)
   Generalized stochastic frontier production models,
   \emph{Economics Letters}, 57, 255-259.

   Löthgren, M. (1997)
   \emph{A Multiple Output Stochastic Ray Frontier Production Model},
   Working Paper Series in Economics and Finance, No. 158,
   Stockholm School of Economics.

   Löthgren, M. (2000)
   Specification and estimation of stochastic multiple-output production
   and technical inefficiency
   \emph{Applied Economics}, 32, 1533-1540.
}

\seealso{\code{\link{frontier}}, \code{\link{frontierQuad}}.}

\author{Arne Henningsen and Geraldine Henningsen}

\examples{
## preparing data
data( germanFarms )
# quantity of crop outputs
germanFarms$qCrop <- germanFarms$vCrop / germanFarms$pOutput
# quantity of animal outputs
germanFarms$qAnimal <- germanFarms$vAnimal / germanFarms$pOutput
# quantity of variable inputs
germanFarms$qVarInput <- germanFarms$vVarInput / germanFarms$pVarInput

# estimate a translog ray production function
estResultRay <- frontierTranslogRay( yNames = c( "qCrop", "qAnimal" ),
   xNames = c( "qLabor", "land", "qVarInput" ),
   data = germanFarms )
summary( estResultRay )
}

\keyword{models}
