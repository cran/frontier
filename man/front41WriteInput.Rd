\name{front41WriteInput}
\alias{front41WriteInput}
\title{Write input files for Frontier 4.1}

\description{
   Write an instruction file, a data file, and a start-up file
   for Tim Coelli's program Frontier 4.1
   that performs stochastic frontier analysis.
}

\usage{
front41WriteInput( data, crossSectionName, timePeriodName = NULL,
   yName, xNames = NULL, qxNames = NULL, zNames = NULL, quadHalf = TRUE,
   modelType = ifelse( is.null( zNames ), 1, 2 ), functionType = 1,
   logDepVar = TRUE, mu = FALSE, eta = FALSE, path = ".",
   insFile = "front41.ins", dtaFile = sub( "\\\\.ins$", ".dta", insFile ),
   outFile = sub( "\\\\.ins$", ".out", insFile ), startUpFile = "front41.000",
   iprint = 5, indic = 1, tol = 0.00001, tol2 = 0.001, bignum = 1.0E+16,
   step1 = 0.00001, igrid2 = 1, gridno = 0.1, maxit = 100, ite = 1 )
}

\arguments{
   \item{data}{data frame that contains the data.}
   \item{crossSectionName}{string: name of the cross section identifier.}
   \item{timePeriodName}{string: name of the time period identifier
      or \code{NULL} in case of cross-section data.}
   \item{yName}{string: name of the endogenous variable.}
   \item{xNames}{a vector of strings containing the names of the X variables
      (exogenous variables of the production or cost function).}
   \item{qxNames}{a vector of strings containing the names of the variables
      to construct quadratic and interaction terms.
      As a shortcut, this argument can be set to \code{"all"}
      for using all variables specified in argument \code{xNames}
      to get a full quadratic or translog model.}
   \item{zNames}{a vector of strings containing the names of the Z variables
      (variables explaining the efficiency level).}
   \item{quadHalf}{logical. Multiply the quadratic terms by one half?}
   \item{modelType}{model type: either 1 for an 'Error Components Frontier'
      or 2 for an 'Efficiency Effects Frontier'.}
   \item{functionType}{function type: either 1 for 'production function' or
      2 for 'cost function'.}
   \item{logDepVar}{logical. Is the dependent variable logged.}
   \item{mu}{logical. Should a 'mu' (in case of an 'Error Components Frontier',
      i.e. modelType = 1)
      or a delta0 (in case of an 'Efficiency Effects Frontier',
      i.e. modelType = 2)
      be included in the estimation.}
   \item{eta}{logical. Should an 'eta' be included in the estimation
      (only in case of an 'Error Components Frontier', i.e. modelType = 1).}
   \item{path}{path in which the instruction file, the data file,
      and the start-up file should be written.}
   \item{insFile}{name of the instruction file.}
   \item{dtaFile}{name of the data file.}
   \item{outFile}{name of the output file.}
   \item{startUpFile}{name of the start-up file. If this argument is NULL,
      no start-up file is written.}
   \item{iprint}{numeric. Print info every \code{iprint} iterations;
      if this argument is 0, do not print.}
   \item{indic}{numeric. Use in unidimensional search procedure:
      indic = 2 says do not scale step length in unidimensional search;
      indic = 1 says scale (to length of last step) only if last step was smaller;
      indic = any other number says scale (to length of last step).}
   \item{tol}{numeric. Convergence tolerance (proportiona).}
   \item{tol2}{numeric. Tolerance used in uni-dimensional search procedure.}
   \item{bignum}{numeric. Used to set bounds on densities and distributions.}
   \item{step1}{numeric. Size of 1st step in search procedure.}
   \item{igrid2}{numeric. 1 = double accuracy, 0 = single accuracy.}
   \item{gridno}{numeric. Steps taken in single accuracy grid search on gamma.}
   \item{maxit}{numeric. Maximum number of iterations permitted}
   \item{ite}{numeric. 1 = print all efficiency estimates;
      0 = print only the mean efficiency.}
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

\value{
   \code{front41WriteInput} writes an instruction file, a data file,
   and a start-up file for Frontier 4.1 to disk
   and it invisibly returns a list of class \code{front41WriteInput}.
   This list contains mainly the arguments with which \code{front41WriteInput}
   was called.
   An exception is element \code{data}, which is \emph{not}
   the argument \code{data} but the data matrix
   that was written into the data file.
   Furthermore, in case of an Efficiency Effects Model,
   the element \code{eta} contains the number of Z variables.
   Additionally, the returned list contains following elements:
   \item{nCrossSection}{number of cross section units.}
   \item{nTimePeriods}{number of time periods.}
   \item{nTotalObs}{total number of observations.}
   \item{nXtotal}{total number of X variables
      (including quadratic and interaction terms).}
   \item{nZvars}{number of Z variables.}
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

\seealso{\code{\link{front41ReadOutput}}, \code{\link{front41Est}}}

\author{Arne Henningsen}

\examples{
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

