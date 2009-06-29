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
frontier( yName, xNames = NULL, zNames = NULL, data,
   ineffDecrease = TRUE, logDepVar = TRUE, truncNorm = FALSE,
   zIntercept = FALSE, timeEffect = FALSE, startVal = NULL,
   tol = 0.00001, maxit = 1000, muBound = 2, bignum = 1.0E+16,
   searchStep = 0.00001, searchTol = 0.001, searchScale = NA,
   gridSize = 0.1, gridDouble = TRUE, printIter = 0 )
\method{print}{frontier}( x, \dots )
}

\arguments{
   \item{yName}{string: name of the endogenous variable.}
   \item{xNames}{a vector of strings containing the names of the X variables
      (exogenous variables of the production or cost function).}
   \item{zNames}{a vector of strings containing the names of the Z variables
      (variables explaining the efficiency level).}
   \item{data}{a (panel) data frame that contains the data;
      if \code{data} is a usual data.frame,
      it is assumed that these are cross-section data;
      if \code{data} is a panel data frame
      (created with \code{\link[plm]{plm.data}}),
      it is assumed that these are panel data.}
   \item{ineffDecrease}{logical. If \code{TRUE},
      inefficiency decreases the endogenous variable
      (e.g. for estimating a production function);
      if \code{FALSE},
      inefficiency increases the endogenous variable
      (e.g. for estimating a cost function).}
   \item{logDepVar}{logical. Is the dependent variable logged.}
   \item{truncNorm}{logical. If \code{TRUE},
      the inefficiencies are assumed to have a truncated normal distribution
      (i.e. parameter \eqn{\mu}{mu} is added);
      if \code{FALSE},
      they are assumed to have a half-normal distribution
      (only relevant for the \sQuote{Error Components Frontier}).}
   \item{zIntercept}{logical. If \code{TRUE},
      an intercept (with parameter \eqn{\delta_0}{delta_0})
      is added to the Z variables
      (only relevant for the \sQuote{Efficiency Effects Frontier}).}
   \item{timeEffect}{logical. If \code{FALSE} (default),
      the efficiency estimates of an \sQuote{Error Components Frontier}
      are time invariant;
      if \code{TRUE}, time is allowed to have an effect on efficiency
      (this argument is ignored in case of an
      \sQuote{Efficiency Effects Frontier}).}
   \item{startVal}{numeric vector. Optional starting values for the ML
      estimation.}
   \item{tol}{numeric. Convergence tolerance (proportional).}
   \item{maxit}{numeric. Maximum number of iterations permitted.}
   \item{muBound}{numeric. Bounds on the parameter \eqn{\mu}{mu}
      (see \sQuote{details} section).}
   \item{bignum}{numeric. Used to set bounds on densities and distributions.}
   \item{searchStep}{numeric. Size of the first step in the Coggin
      uni-dimensional search procedure done each iteration
      to determine the optimal step length for the next iteration
      (see Himmelblau 1972).}
   \item{searchTol}{numeric. Tolerance used in the Coggin
      uni-dimensional search procedure done each iteration
      to determine the optimal step length for the next iteration
      (see Himmelblau 1972).}
   \item{searchScale}{logical or \code{NA}. Scaling in the Coggin
      uni-dimensional search procedure done each iteration
      to determine the optimal step length for the next iteration
      (see Himmelblau 1972):
      if \code{TRUE}, the step length is scaled to the length of the last step;
      if \code{FALSE}, the step length is not scaled;
      if \code{NA}, the step length is scaled (to the length of last step)
         only if the last step was smaller.}
   \item{gridSize}{numeric. The size of the increment
      in the first phase grid search on \eqn{\gamma}{gamma}.}
   \item{gridDouble}{logical. If \code{TRUE},
      a second phase grid search on \eqn{\gamma}{gamma} is conducted
      around the \dQuote{best} value obtained in the first phase
      with an increment of \code{gridSize/10}.}
   \item{printIter}{numeric. Print info every \code{printIter} iterations;
      if this argument is 0, do not print.}
   \item{x}{an object of class \code{frontier}
      (returned by the function \code{frontier}).}
   \item{\dots}{currently unused.}
}

\details{
   One can use the \code{frontier} function
   to calculate the log likelihood value for a given model,
   a given data set, and given parameters
   by using the argument \code{startVal} to specify the parameters
   and using the other arguments to specify the model and the data.
   The log likelihood value can then be retrieved by
   the \code{\link[=logLik.frontier]{logLik}} method
   with argument \code{which} set to \code{"start"}.
   Setting argument \code{maxit} to \code{0} avoids the
   (eventually time-consuming) ML estimation and allows
   to retrieve the log likelihood value
   with the \code{\link[=logLik.frontier]{logLik}} method
   without further arguments.

   The \code{frontier} function uses the Fortran source code of
   Tim Coelli's software FRONTIER 4.1
   (\url{http://www.uq.edu.au/economics/cepa/frontier.htm})
   and hence, provides the same features as FRONTIER 4.1.
   A comprehensive documentation of FRONTIER 4.1 is available
   in the file \code{Front41.pdf}
   that is included in the archive \code{FRONT41-xp1.zip},
   which is available at
   \url{http://www.uq.edu.au/economics/cepa/frontier.htm}.
   It is recommended to read this documentation, 
   because the \code{frontier} function is based on the FRONTIER 4.1 software.

   If argument \code{zNames} is \code{NULL},
   an \sQuote{Error Components Frontier} (ECF, see Battese and Coelli 1992)
   is estimated.
   If argument \code{zNames} is not \code{NULL},
   an \sQuote{Efficiency Effects Frontier} (EEF, see Battese and Coelli 1995)
   is estimated.
   Generally, there should be no reason to estimate an EEF
   without z variable,
   but this can done by setting argument \code{zNames} to \code{NA}.

   In case of an Error Components Frontier (ECF)
   with the inefficiency terms \eqn{u} following a
   truncated normal distribution with mean \eqn{\mu}{mu},
   argument \code{muBound} can be used to restrict \eqn{\mu}{mu}
   to be in the interval \eqn{\pm}{+/-}\code{muBound} * \eqn{\sigma_u}{sigma_u},
   where \eqn{\sigma_u}{sigma_u} is the standard deviation of \eqn{u}.
   If \code{muBound} is infinity, zero, or negative,
   no bounds on \eqn{\mu}{mu} are imposed.
}

\value{
   \code{frontier} returns a list of class \code{frontier}
   containing following elements:
   \item{modelType}{integer.
      A \sQuote{1} denotes an \sQuote{Error Components Frontier} (ECF);
      a \sQuote{2} denotes an \sQuote{Efficiency Effects Frontier} (EFF).}
   \item{ineffDecrease}{logical. Argument \code{ineffDecrease} (see above).}
   \item{logDepVar}{logical. Argument \code{logDepVar} (see above).}
   \item{nn}{number of cross-sections.}
   \item{nt}{number of time periods.}
   \item{nob}{number of observations in total.}
   \item{nb}{number of regressor variables (Xs).}
   \item{truncNorm}{logical. Argument \code{truncNorm}.}
   \item{zIntercept}{logical. Argument \code{zIntercept}.}
   \item{timeEffect}{logical. Argument \code{timeEffect}.}
   \item{printIter}{numeric. Argument \code{printIter} (see above).}
   \item{searchScale}{numeric. Argument \code{searchScale} (see above).}
   \item{tol}{numeric. Argument \code{tol} (see above).}
   \item{searchTol}{numeric. Argument \code{searchTol} (see above).}
   \item{bignum}{numeric. Argument \code{bignum} (see above).}
   \item{searchStep}{numeric. Argument \code{searchStep} (see above).}
   \item{gridDouble}{logical. Argument \code{gridDouble} (see above).}
   \item{gridSize}{numeric. Argument \code{gridSize} (see above).}
   \item{maxit}{numeric.  Argument \code{maxit} (see above).}
   \item{startVal}{numeric vector. Argument \code{startVal}
      (only if specified by user).}
   \item{call}{the matched call.}
   \item{dataTable}{matrix. Data matrix sent to Frontier 4.1.}
   \item{olsParam}{numeric vector. OLS estimates.}
   \item{olsStdEr}{numeric vector. Standard errors of OLS estimates.}
   \item{olsLogl}{numeric. Log likelihood value of OLS estimation.}
   \item{gridParam}{numeric vector. Parameters obtained from the grid search
      (if no starting values were specified).}
   \item{gridLogl}{numeric. Log likelihood value of the parameters
      obtained from the grid search
      (only if no starting values were specified).}
   \item{startLogl}{numeric. Log likelihood value of the starting values
      for the parameters
      (only if starting values were specified).}
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

   Himmelblau, D.M. (1972), \emph{Applied Non-Linear Programming},
      McGraw-Hill, New York.
}

\seealso{\code{\link{frontierQuad}} for quadratic/translog frontiers.}

\author{Tim Coelli and Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1 (cross-section data)
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   # Cobb-Douglas production frontier
   cobbDouglas <- frontier( yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ), data = front41Data )
   summary( cobbDouglas )

   # rice producers in the Philippines (panel data)
   data( riceProdPhil )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )
   riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
   riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
   riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
   riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

   # Error Components Frontier (Battese & Coelli 1992)
   rice <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ), data = riceProdPhil )
   summary( rice )

   # Technical Efficiency Effects Frontier (Battese & Coelli 1995)
   rice2 <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ),
      zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil )
   summary( rice2 )

   # Cost Frontier (with land as quasi-fixed input)
   riceProdPhil$cost <- riceProdPhil$LABOR * riceProdPhil$LABORP +
      riceProdPhil$NPK * riceProdPhil$NPKP
   riceProdPhil$lCost   <- log( riceProdPhil$cost )
   riceProdPhil$lLABORP <- log( riceProdPhil$LABORP )
   riceProdPhil$lNPKP   <- log( riceProdPhil$NPKP )
   riceCost <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
      data = riceProdPhil, ineffDecrease = FALSE )
   summary( riceCost )
}

\keyword{models}
