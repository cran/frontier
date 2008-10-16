\name{frontier}
\alias{frontier}
\alias{print.frontier}
\title{Stochastic Frontier Analysis}

\description{ Maximum Likelihood Estimation of
   Stochastic Frontier Production and Cost Functions.
   Two specifications are available:
   the error components specification with time-varying efficiencies
   (Battese and Coelli 1992)
   and a model specification in which the firm effects are directly 
   influenced by a number of variables (Battese and Coelli 1995).
   This R package uses the Fortran source code of Frontier 4.1
   (Coelli 1996).
}

\usage{
frontier( data, crossSectionName, timePeriodName = NULL,
   yName, xNames = NULL, qxNames = NULL, zNames = NULL, quadHalf = TRUE,
   modelType = ifelse( is.null( zNames ), 1, 2 ),
   functionType = 1, logDepVar = TRUE, mu = FALSE, eta = FALSE,
   iprint = 0, indic = 1, tol = 0.00001, tol2 = 0.001, bignum = 1.0E+16,
   step1 = 0.00001, igrid2 = 1, gridno = 0.1, maxit = 100,
   startVal = NULL )
\method{print}{frontier}( x, ... )
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
   \item{iprint}{numeric. Print info every \code{iprint} iterations;
      if this argument is 0, do not print.}
   \item{indic}{numeric. Use in unidimensional search procedure:
      indic = 2 says do not scale step length in unidimensional search;
      indic = 1 says scale (to length of last step) only if last step was smaller;
      indic = any other number says scale (to length of last step).}
   \item{tol}{numeric. Convergence tolerance (proportional).}
   \item{tol2}{numeric. Tolerance used in uni-dimensional search procedure.}
   \item{bignum}{numeric. Used to set bounds on densities and distributions.}
   \item{step1}{numeric. Size of 1st step in search procedure.}
   \item{igrid2}{numeric. 1 = double accuracy, 0 = single accuracy.}
   \item{gridno}{numeric. Steps taken in single accuracy grid search on gamma.}
   \item{maxit}{numeric. Maximum number of iterations permitted.}
   \item{startVal}{numeric vector. Optional starting values for the ML 
      estimation.}
   \item{x}{an object of class \code{frontier} 
      (returned by the function \code{frontier}).}
   \item{...}{currently unused.}
}

\value{
   \code{frontier} returns a list of class \code{frontier}
   containing following elements:
   \item{modelType}{model type (see above).}
   \item{functionType}{function type (see above).}
   \item{logDepVar}{logical. Is the dependent variable logged?}
   \item{nn}{number of cross-sections.}
   \item{nt}{number of time periods.}
   \item{nob}{number of observations in total.}
   \item{nb}{number of regressor variables (Xs).}
   \item{mu}{logical. Indicates if a 'mu'/'delta0' is included
      in the estimation (see above).}
   \item{eta}{In case of an error component model: logical,
      indicates if an 'eta' is included in the estimation.
      In case of an efficiency effects model: numeric,
      the number of efficiency effects regressors (Zs).}
   \item{iprint}{numeric. Printing of information
      on the iterations (see above).}
   \item{indic}{numeric. Scaling of unidimensional search procedure
      (see above).}
   \item{tol}{numeric. Convergence tolerance.}
   \item{tol2}{numeric. Tolerance used in uni-dimensional search procedure.}
   \item{bignum}{numeric. Used to set bounds on densities and distributions.}
   \item{step1}{numeric. Size of 1st step in search procedure.}
   \item{igrid2}{numeric. 1 = double accuracy, 0 = single accuracy.}
   \item{gridno}{numeric. Steps taken in single accuracy grid search
      on gamma.}
   \item{maxit}{numeric. Maximum number of iterations permitted.}
   \item{startVal}{numeric vector. Starting values for the ML
      estimation (if specified by user).}
   \item{dataTable}{matrix. Data matrix sent to Frontier 4.1.}
   \item{olsParam}{numeric vector. OLS estimates.}
   \item{olsStdEr}{numeric vector. Standard errors of OLS estimates.}
   \item{olsLogl}{numeric. Log likelihood value of OLS estimation.}
   \item{gridParam}{numeric vector. Parameters obtained from the grid search
      (if no starting values were specified).}
   \item{mleParam}{numeric vector. Parameters obtained from ML estimation.}
   \item{mleCov}{matrix. Covariance matrix of the parameters obtained
      from the OLS estimation.}
   \item{mleLogl}{numeric. Log likelihood value of the ML estimation.}
   \item{lrTestVal}{LR test of the one-sided error
      (this statistic has a mixed chi-square distribution
      with \code{lrTestDf} degrees of freedom.}
   \item{lrTestDf}{numeric. Degrees of freedom of the LR test.}
   \item{nIter}{numeric. Number of iterations of the ML estimation.}
   \item{effic}{matrix. Efficiency estimates:
      each row corresponds to a cross-section;
      each column corresponds to a time period.}
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
      \url{http://www.uq.edu.au/economics/cepa/frontier.htm},
      University of New England.
}

\author{Tim Coelli \email{t.coelli@uq.edu.au},
   Arne Henningsen \email{ahenningsen@agric-econ.uni-kiel.de}}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   # Cobb-Douglas production frontier
   cobbDouglas <- frontier( front41Data, "firm", "time", "logOutput",
      c( "logCapital", "logLabour" ) )
   cobbDouglas

   # Translog production frontier
   translog <- frontier( front41Data, "firm", "time", "logOutput",
      c( "logCapital", "logLabour" ), qxNames = "all" )
   translog

   # rice producers in the Phillipines
   data( riceProdPhil )
   riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
   riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
   riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
   riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

   rice <- frontier( riceProdPhil,
      crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
      yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
   rice

   rice2 <- frontier( riceProdPhil,
      crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
      yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
      zNames = c( "EDYRS", "BANRAT" ) )
   rice
}

\keyword{models}
