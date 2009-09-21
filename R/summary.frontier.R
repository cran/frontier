summary.frontier <- function( object, effic = FALSE,
      logDepVar = TRUE, ... ) {

   # save variable 'logDepVar'
   object$logDepVar <- logDepVar

   # calculate efficiency estimates
   object$effic <- efficiencies( object, logDepVar = logDepVar )

   # matrix of OLS estimates, their standard errors, t-values and P-values
   olsParam <- matrix( NA, length( object$olsParam ) , 4 )
   rownames( olsParam ) <- names( object$olsParam )
   colnames( olsParam ) <- c( "Estimate", "Std. Error", "t value", 
      "Pr(>|t|)" )
   olsParam[ , 1 ] <- object$olsParam
   olsParam[ , 2 ] <- c( object$olsStdEr, NA )
   olsParam[ , 3 ] <- olsParam[ , 1 ] / olsParam[ , 2 ]
   df <- object$nob - length( object$olsParam )
   olsParam[ , 4 ] <- 2 * pt( abs( olsParam[ , 3 ] ), df, lower.tail = FALSE )
   object$olsParam <- olsParam

   # matrix of ML estimates, their standard errors, t-values and P-values
   mleParam <- matrix( NA, length( object$mleParam ) , 4 )
   rownames( mleParam ) <- names( object$mleParam )
   colnames( mleParam ) <- colnames( olsParam )
   mleParam[ , 1 ] <- object$mleParam
   mleParam[ , 2 ] <- diag( object$mleCov )^0.5
   mleParam[ , 3 ] <- mleParam[ , 1 ] / mleParam[ , 2 ]
   df <- object$nob - length( object$mleParam )
   mleParam[ , 4 ] <- 2 * pt( abs( mleParam[ , 3 ] ), df, lower.tail = FALSE )
   object$mleParam <- mleParam

   object$printEffic <- effic

   if( ncol( object$effic ) > 1 ) {
      object$efficYearMeans <- rep( NA, object$nt )
      resid <- residuals( object )
      for( i in 1:object$nt ) {
         object$efficYearMeans[ i ] <-
            mean( object$effic[ !is.na( resid[ , i ] ), i ] )
      }
      names( object$efficYearMeans ) <- colnames( object$effic )
   }

   if( object$modelType == 1 && !object$timeEffect ) {
      object$efficMean <- mean( object$effic )
   } else {
      object$efficMean <- mean( object$effic[ !is.na( residuals( object ) ) ] )
   }

   class( object ) <- "summary.frontier"
   return( object )
}
