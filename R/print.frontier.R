print.frontier <- function( x, ... ) {

   cat( "Stochastic Frontier Analysis with FRONTIER 4.1\n\n" )
   if( x$modelType == 1 ) {
      cat( "Error Components Frontier (see Battese & Coelli 1992)\n" )
   } else if( x$modelType == 2 ) {
      cat( "Tech. Eff. Effects Frontier (see Battese & Coelli 1995)\n" )
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
   cat( "\nFinal maximum likelihood estimates\n" )
   print( coef( x ) )
   invisible( x )
}