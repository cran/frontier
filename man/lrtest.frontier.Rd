\name{lrtest.frontier}
\alias{lrtest.frontier}

\title{Likelihood Ratio test for Stochastic Frontier Models}

\description{
   Testing parameter restrictions in stochastic frontier models
   by a Likelihood Ratio test.
}

\usage{
   \method{lrtest}{frontier}( object, ... )
}

\arguments{
   \item{object}{a fitted model object of class \code{frontier}.}
   \item{\dots}{further fitted model objects of class \code{frontier}.}
}

\details{
   If \code{lrtest.frontier} is called with only one argument/object
   (i.e. argument \code{...} is not used),
   it compares the fitted model to a corresponding model
   without inefficiency (i.e. estimated by OLS).

   If \code{lrtest.frontier} is called with more than one argument/object
   (i.e. argument \code{...} is used),
   it consecutively compares
   the fitted model object \code{object}
   with the models passed in \code{...}.

   The test statistic is
   \code{2 * ( logLik( mu ) - logLik( mr ) )},
   where \code{mu} is the unrestricted model
   and \code{mr} is the restricted model.

   If a Frontier model (estimated by ML) is compared to
   a model without inefficiency (estimated by OLS),
   the test statistic asymptotically has a mixed \eqn{\chi^2} distribution
   under the null hypothesis (see Coelli, 1995).

   If two Frontier models (estimated by ML) are compared,
   the test statistic asymptotically has a \eqn{\chi^2}
   distribution with \eqn{j} degrees of freedom
   under the null hypothesis,
   where \eqn{j} is the number of restrictions.
}

\value{
   An object of class \code{anova},
   which contains the log-likelihood value,
   degrees of freedom, the difference in degrees of freedom,
   likelihood ratio Chi-squared statistic and corresponding p value.
   See documentation of \code{\link[lmtest]{lrtest}}
   in package "lmtest".
}

\references{
   Coelli, T.J. (1995), Estimators and Hypothesis Tests for a Stochastic:
   A Monte Carlo Analysis, \emph{Journal of Productivity Analysis},
   6, 247-268.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfa}}, \code{\link[lmtest]{lrtest}}}

\examples{
# rice producers in the Philippines (panel data)
data( "riceProdPhil" )
library( "plm" )
riceProdPhil <- pdata.frame( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

# Error Components Frontier with truncated normal distribution
# and time effects (unrestricted model)
mu <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   truncNorm = TRUE, timeEffect = TRUE, data = riceProdPhil )

# Error Components Frontier with half-normal distribution
# without time effects (restricted model)
mr <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = riceProdPhil )

## compare the two models by an LR-test
lrtest( mu, mr )

## compare each of the models to a corresponding model without inefficiency
lrtest( mu )
lrtest( mr )
}

\keyword{models}
