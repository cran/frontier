# residuals of frontier models
residuals.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      if( object$nt > 1 ) {
         result <- rep( NA, nrow( object$dataTable ) )
         for( i in 1:length( result ) ) {
            result[ i ] <- object$resid[ object$dataTable[ i , 1 ],
               object$dataTable[ i , 2 ] ]
         }
      } else {
         result <- drop( object$resid )
      }
      names( result ) <- rownames( object$dataTable )
   } else {
      result <- object$resid
   }
   return( result )
}
