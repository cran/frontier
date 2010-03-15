frontierTranslogRay <- function( yNames, xNames, shifterNames = NULL,
      zNames = NULL, data, ... ) {

   checkNames( c( yNames, xNames, shifterNames, zNames ), names( data ) )

   if( length( yNames ) != 2 ) {
      stop( "the estimation of translog ray frontiers has not been implemented",
         " with more than two endogenous variables yet" )
   }

   if( any( c( "distance", "theta" ) %in% c( xNames, shifterNames ) ) ) {
      stop( "the variable names in arguments 'xNames' and 'shifterNames'",
         " must not be 'distance' or 'theta'" )
   }

   nInput <- length( xNames )

   logData <- logDataSet( data = data, varNames = xNames )

   distance <- sqrt( data[[ yNames[ 1 ] ]]^2 + data[[ yNames[ 2 ] ]]^2 )

   logData$distance <- log( distance )

   logData$theta <- acos( data[[ yNames[ 1 ] ]] / distance )

   # shifter variables
   for( i in seq( along = shifterNames ) ) {
      logData[[ shifterNames[ i ] ]] <- data[[ shifterNames[ i ] ]]
   }

   # z variables
   for( i in seq( along = zNames ) ) {
      logData[[ zNames[ i ] ]] <- data[[ zNames[ i ] ]]
   }

   result <- frontierQuad( yName = "distance",
      xNames = c( xNames, "theta" ), shifterNames = shifterNames,
      zNames = zNames, data = logData, ... )

   result$call <- match.call()
   result$yName         <- NULL
   result$yNames        <- yNames
   result$xNames        <- xNames
   result$shifterNames  <- shifterNames
   result$distance      <- distance
   result$theta         <- logData$theta

   coefNames <- names( result$mleParam )[
      1:( 1 + ( nInput + 1 ) + ( nInput + 2 ) * ( nInput + 1 ) / 2 ) ]

   coefNames[ coefNames ==
      paste( "b", nInput + 1, nInput + 1, sep = "_" ) ] <- "b_t_t"
   coefNames <- sub( paste( "_", nInput + 1, "$", sep = "" ), "_t",
      coefNames )
   names( result$olsParam )[ 1:length( coefNames ) ] <- coefNames
   names( result$olsStdEr )[ 1:length( coefNames ) ] <- coefNames
   if( ! is.null( result$gridParam ) ) {
      names( result$gridParam )[ 1:length( coefNames ) ] <- coefNames
   }
   names( result$mleParam )[ 1:length( coefNames ) ] <- coefNames
   rownames( result$mleCov )[ 1:length( coefNames ) ] <- coefNames
   colnames( result$mleCov )[ 1:length( coefNames ) ] <- coefNames

   class( result ) <- c( "frontierTranslogRay", class( result ) )

   return( result )
}
