coef.frontier <- function( object, which = "mle", ... ) {

   if( tolower( which ) == "start" ){
      return( object$startVal )
   } else if( tolower( which ) == "ols" ) {
      return( object$olsParam )
   } else if( tolower( which ) == "grid" ) {
      return( object$gridParam )
   } else if( tolower( which ) == "mle" ) {
      return( object$mleParam )
   } else {
      stop( "argument 'which' must be either 'start', 'ols', 'grid',",
         " or 'mle'" )
   }
}