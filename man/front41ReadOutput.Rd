\name{front41ReadOutput}
\alias{front41ReadOutput}
\alias{print.front41Output}
\title{Read output of Frontier 4.1}

\description{
   Read the output file of Tim Coelli's program Frontier 4.1 that
   performs stochastic frontier analysis.
}

\usage{
   front41ReadOutput( file = "front41.out" )

   \method{print}{front41Output}( x, efficiencies = FALSE, \dots )
}

\arguments{
   \item{file}{character variable with the name of the file to read.}
   \item{x}{object of class \code{front41Output}
      (returned by \code{front41ReadOutput}.}
   \item{efficiencies}{logical. Print all efficiency estimates?
      (If \code{FALSE}, only the mean efficiency is printed.)}
   \item{\dots}{currently ignored.}
}

\value{
   a list of class \code{front41Output} containing following objects:
   \item{version}{the version of Frontier 4.1 that produced the output.}
   \item{insFile}{name of the instruction file used by Frontier 4.1.}
   \item{dtaFile}{name of the data file used by Frontier 4.1.}
   \item{modelType}{model type: either 1 for 'Error Components Frontier' or
      2 for 'Tech. Eff. Effects Frontier'.}
   \item{modelTypeName}{model type: 'Error Components Frontier' or 'Tech. Eff.
      Effects Frontier'.}
   \item{functionType}{function type: either 1 for 'production function' or
      2 for 'cost function'.}
   \item{functionTypeName}{function type: 'production function' or
      'cost function'.}
   \item{logDepVar}{logical. Is the dependent variable logged.}
   \item{olsResults}{results of the OLS estimation.}
   \item{nXvars}{number X variables (exogenous variables of the production
      or cost function.}
   \item{olsLogl}{log likelihood value of the OLS estimation.}
   \item{gridResults}{results of the grid search.}
   \item{mleResults}{results of the maximum likelihood estimation.}
   \item{mleLogl}{log likelihood value of the maximum likelihood estimation.}
   \item{mleCov}{coefficient covariance matrix of the maximum likelihood
      estimation.}
   \item{lrTest}{LR test of the one-sided error.}
   \item{lrTestRestrict}{number of restrictions of the LR test.}
   \item{nIter}{number of iterations.}
   \item{maxIter}{maximum number of iterations set.}
   \item{nCross}{number of cross-sections.}
   \item{nPeriods}{umber of time periods.}
   \item{nObs}{total number of observations.}
   \item{nObsMissing}{number of observations that are not in the panel.}
   \item{efficiency}{technical efficiency estimates.}
   \item{meanEfficiency}{mean efficiency.}
}

\details{
   A modified version of Tim Coelli's FRONTIER 4.1
   that can be used non-interactively is available on
   \url{http://frontier.r-forge.r-project.org/front41.html}.
   It can be called from within R using the \code{system} command
   (see example).
   This version is is available as (FORTRAN) source code
   and (executable) binaries for GNU/Linux and MS-Windows.
}

\references{
   Battese, G.E. and T. Coelli (1992), Frontier production functions,
      technical efficiency and panel data: with application to paddy
      farmers in India. \emph{Journal of Productivity Analysis}, 3, 153-169.

   Battese, G.E. and T. Coelli (1995), A model for technical inefficiency effects
      in a stochastic frontier production function for panel data.
      \emph{Empirical Economics}, 20, 325-332.

   Coelli, T. (1996) A Guide to FRONTIER Version 4.1: A Computer
      Program for Stochastic Frontier Production and Cost Function
      Estimation, CEPA Working Paper 96/08,
      \url{http://www.uq.edu.au/economics/cepa/frontier.php},
      University of New England.
}

\seealso{\code{\link{front41WriteInput}}, \code{\link{front41Est}}}

\author{Arne Henningsen}

\examples{
   # read the output file that is provided with Frontier 4.1
   outFile <- system.file( "front41/EG1.OUT", package = "frontier" )
   sfa <- front41ReadOutput( outFile )
   print( sfa, efficiencies = TRUE )

   # perform an SFA and read the output
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   front41WriteInput( front41Data, "firm", yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ), 
      path = tempdir(), insFile = "coelli.ins" )

   \dontrun{
   system( paste0( "cd ", tempdir(), "; front41.bin coelli.ins" ) )
   sfa <- front41ReadOutput( file.path( tempdir(), "coelli.out" ) )
   summary( sfa )
   }
}

\keyword{models}

