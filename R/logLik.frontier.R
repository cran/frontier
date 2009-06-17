logLik.frontier <- function( object, which = "mle", newParam = NULL, ... ) {

   if( is.null( newParam ) ) {
      if( tolower( which ) == "ols" ) {
         result <- object$olsLogl
      } else if( tolower( which ) == "grid" ) {
         result <- object$gridLogl
      } else if( tolower( which ) == "start" ) {
         result <- object$startLogl
      } else if( tolower( which ) == "mle" ) {
         result <- object$mleLogl
      } else {
         stop( "argument 'which' must be either 'ols', 'grid', 'start', or 'mle'" )
      }
      if( is.null( result ) ) {
         result <- NA
      }
   } else {
      if( ! is.vector( newParam ) || ! is.numeric( newParam ) ||
            length( newParam ) != length( coef( object ) ) ) {
         stop( "argument 'newParam' must be a numeric vector of length ",
            length( coef( object ) ) )
      }
      if( tolower( which ) != "mle" ) {
         warning( "argument 'which' has been ignored" )
      }
      if( "frontierQuad" %in% class( object ) ) {
         result <- logLik( frontierQuad(
            yName = eval( object$call$yName ),
            xNames = eval( object$call$xNames ),
            shifterNames = eval( object$call$shifterNames ),
            zNames = eval( object$call$zNames ),
            data = eval( object$call$data ),
            quadHalf = object$quadHalf,
            ineffDecrease = object$ineffDecrease,
            logDepVar = object$logDepVar,
            truncNorm = object$truncNorm,
            zIntercept = object$zIntercept,
            timeEffect = object$timeEffect,
            startVal = newParam,
            maxit = 0 ), which = "start" )
      } else {
         result <- logLik( frontier(
            yName = eval( object$call$yName ),
            xNames = eval( object$call$xNames ),
            zNames = eval( object$call$zNames ),
            data = eval( object$call$data ),
            ineffDecrease = object$ineffDecrease,
            logDepVar = object$logDepVar,
            truncNorm = object$truncNorm,
            zIntercept = object$zIntercept,
            timeEffect = object$timeEffect,
            startVal = newParam,
            maxit = 0 ), which = "start" )
      }
   }
   return( result )
}