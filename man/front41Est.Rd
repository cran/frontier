\name{front41Est}
\alias{front41Est}
\title{Estimate a Stochastic Frontier Model by Frontier 4.1}

\description{
   Estimate a stochastic frontier model with a modified version
   of Tim Coelli's program Frontier 4.1
   (NOTE: this program has to be installed separately!).
}

\usage{
   front41Est( command = ifelse( .Platform$OS.type == "windows",
      "front41.exe", "front41.bin" ), \dots )
}

\arguments{
   \item{command}{command to call the modified version of FRONTIER 4.1
      (see details).}
   \item{\dots}{arguments passed to \code{\link{front41WriteInput}}.}
}

\details{
   Using the command \code{front41Est} requires the installation
   of a modified version of Tim Coelli's FRONTIER 4.1.
   It is available on
   \url{http://frontier.r-forge.r-project.org/front41.html}.
   as (FORTRAN) source code
   and (executable) binaries for GNU/Linux and MS-Windows.
}

\value{
   \code{front41Est} returns a list of class \code{front41Output}
   that is returned by \code{\link{front41ReadOutput}}
   with two additional elements:
   \item{input}{object returned by \code{\link{front41WriteInput}}.}
   \item{messages}{messages returned by FRONTIER 4.1.}
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

\seealso{\code{\link{front41WriteInput}}, \code{\link{front41ReadOutput}}}

\author{Arne Henningsen}

\examples{
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   \dontrun{
   front41Est( data = front41Data, crossSectionName = "firm",
      yName = "logOutput", xNames = c( "logCapital", "logLabour" ) )
   }
}

\keyword{models}

