print.summary.frontier <- function( x, effic = x$printEffic, ... ) {

   if( x$modelType == 1 ) {
      cat( "Error Components Frontier (see Battese & Coelli 1992)\n" )
   } else if( x$modelType == 2 ) {
      cat( "Efficiency Effects Frontier (see Battese & Coelli 1995)\n" )
   } else {
      stop( "unknown model type '", x$modelType, "'" )
   }
   if( x$ineffDecrease ) {
      cat( "Inefficiency decreases the endogenous variable",
	"(as in a production function)\n" )
   } else {
      cat( "Inefficiency increases the endogenous variable",
	"(as in a cost function\n" )
   }
   if( x$logDepVar == 1 ) {
      cat( "The dependent variable is logged\n" )
   } else {
      cat( "The dependent variable is not logged\n" )
   }
   if( x$nIter < x$maxit ) {
      cat( "convergence achieved after", x$nIter, "iterations\n" )
   } else {
      cat( "convergence NOT achieved after", x$nIter, "iterations\n" )
   }

   cat( "\nfinal maximum likelihood estimates\n" )
   printCoefmat( coef( x ) )
   cat( "log likelihood value:", x$mleLogl, "\n" )

   if( x$nt == 1 ) {
      cat( "\ncross-sectional data\n" )
      cat( "total number of observations =", x$nob, "\n" )
   } else {
      cat( "\npanel data\n" )
      cat( "number of cross-sections =", x$nn, "\n" )
      cat( "number of time periods =", x$nt, "\n" )
      cat( "total number of observations =", x$nob, "\n" )
      cat( "thus there are", x$nn * x$nt - x$nob,
         "observations not in the panel\n" )
   }

   if( effic ){
      cat( "\nefficiency estimates\n" )
      print( x$effic )
   }

   if( !is.null( x$efficYearMeans ) ) {
      cat( "\nmean efficiency of each year\n" )
      print( x$efficYearMeans )
   }

   cat( "\nmean efficiency:", x$efficMean, "\n" )

   invisible( x )
}