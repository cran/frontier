# efficiencies of frontier models
efficiencies.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      data <- eval( object$call$data )
      if( "plm.dim" %in% class( data ) ) {
         result <- rep( NA, nrow( data ) )
         names( result ) <- paste( data[[ 1 ]], data[[ 2 ]], sep = "_" )
         for( i in 1:nrow( data ) ) {
            if( ncol( object$effic ) == 1 ) {
               result[ i ] <- object$effic[ data[[ 1 ]][ i ], 1 ]
            } else {
               result[ i ] <- object$effic[ data[[ 1 ]][ i ], data[[ 2 ]][ i ] ]
            }
         }
      } else {
         result <- drop( object$effic )
      }
   } else {
      result <- object$effic
   }
   return( result )
}
