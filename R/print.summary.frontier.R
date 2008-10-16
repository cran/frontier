print.summary.frontier <- function( x, effic = x$printEffic, ... ) {

   cat( "Stochastic Frontier Analysis with FRONTIER 4.1\n\n" )
   if( x$modelType == 1 ) {
      cat( "Error Components Frontier (see Battese & Coelli 1992)\n" )
   } else {
      cat( "Efficiency Effects Frontier (see Battese & Coelli 1995)\n" )
   }
   if( x$functionType == 1 ) {
      cat( "The model is a production function\n" )
   } else {
      cat( "The model is a cost function\n" )
   }
   if( x$logDepVar == 1 ) {
      cat( "The dependent variable is logged\n" )
   } else {
      cat( "The dependent variable is not logged\n" )
   }

   if( is.null( coef.frontier( x, which = "start" ) ) ) {
      cat( "\nOLS estimates\n" )
      printCoefmat( coef( x, which = "ols" ) )
      cat( "log likelihood value:", x$olsLogl, "\n" )
      cat( "\nestimates after the grid search\n" )
      print( as.matrix( coef.frontier( x, which = "grid" ), ncol = 1 ) )
   } else {
      cat( "\nstarting values provided by the user\n" )
      print( as.matrix( coef.frontier( x, which = "start" ), ncol = 1 ) )
   }

   cat( "\nfinal maximum likelihood estimates\n" )
   printCoefmat( coef( x ) )
   cat( "log likelihood value:", x$mleLogl, "\n" )

   cat( "\nLR test of the one-sided error =", x$lrTestVal, "\n" )
   cat( "with number of restrictions =", x$lrTestDf, "\n" )
   cat( "[note that this statistic has a mixed chi-square distribution]\n" )

   cat( "\nnumber of iterations =", x$nIter, "\n" )
   cat( "(maximum number of iterations set at:", x$maxit, ")\n" )

   cat( "\nnumber of cross-sections =", x$nn, "\n" )
   cat( "number of time periods =", x$nt, "\n" )
   cat( "total number of observations =", x$nob, "\n" )
   cat( "thus there are", x$nn * x$nt - x$nob,
      "observations not in the panel\n" )

   if( effic ){
      cat( "\nefficiency estimates\n" )
      print( x$effic )
   }

   if( ncol( x$effic ) > 1 ) {
      cat( "\nmean efficiency of each year\n" )
      print( colMeans( x$effic ) )
   }

   cat( "\nmean efficiency:", mean( x$effic ), "\n" )

   invisible( x )
}