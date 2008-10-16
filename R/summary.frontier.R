summary.frontier <- function( object, effic = FALSE, ... ) {

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

   class( object ) <- "summary.frontier"
   return( object )
}
