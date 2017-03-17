\name{resettestFrontier}
\alias{resettestFrontier}

\title{RESET test for Stochastic Frontier Models}

\description{
   Generalized Ramsey's RESET test (REgression Specification Error Test)
   for misspecification of the functional form
   based on a Likelihood Ratio test.
}

\usage{
   resettestFrontier( object, power = 2:3 )
}

\arguments{
   \item{object}{a fitted model object of class \code{frontier}.}
   \item{power}{a vector indicating the powers of the fitted variables
      that should be included as additional explanatory variables.
      By default, the test is for quadratic or cubic influence
      of the fitted response.}
}

\value{
   An object of class \code{anova}
   as returned by \code{\link{lrtest.frontier}}.
}

\references{
   Ramsey, J.B. (1969), Tests for Specification Error
   in Classical Linear Least Squares Regression Analysis.
   \emph{Journal of the Royal Statistical Society, Series B} 31, 350-371.
}

\author{Arne Henningsen}

\seealso{
   \code{\link{sfa}},  \code{\link[lmtest]{resettest}}, and
   \code{\link{lrtest.frontier}}
}

\examples{
   # load data set
   data( front41Data )

   # estimate a Cobb-Douglas production frontier
   cobbDouglas <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   
   # conduct the RESET test
   resettestFrontier( cobbDouglas )
}

\keyword{models}
