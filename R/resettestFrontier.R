resettestFrontier <- function( object, power = 2:3 ) {
   
   if( !inherits( object, "frontier" ) ) {
      stop( "function resettestFrontier() can only be applied to objects",
         " of class 'frontier'" )
   }
   
   if( object$modelType != 1 ) {
      stop( "function resettestFrontier() can currently only test",
         " 'Error Components Frontiers'",
         " (not yet 'Efficiency Effects Frontiers')" )
   }
   
   estCall <- object$call
   estFunc <- as.character( estCall[[ 1 ]] )
   estArg <- as.list( estCall )[ -1 ]
   
   # add variables to data set
   estData <- eval( estCall$data )
   fitVal <- fitted( object, asInData = TRUE )
   for( i in 1:length( power ) ) {
      estData[[ paste( "fitVal", i, sep = "" ) ]] <- fitVal^power[ i ]
   }
   estArg$data <- estData
   
   # add variables to model specification
   if( estFunc == "sfa" ) {
      estFormula <- eval( estCall$formula )
      estFormulaChar <- paste( as.character( estFormula )[2],
         as.character( estFormula )[3], sep = " ~ " )
      estFormulaChar <- paste( estFormulaChar,
            paste( paste( "fitVal", 1:length( power ), sep = "" ),
               collapse = " + " ), sep = " + " )
      estFormula <- as.formula( estFormulaChar )
      estArg$formula <- estFormula
   } else if( estFunc == "frontier" ) {
      estXNames <- eval( estCall$xNames )
      estXNames <- c( estXNames,
         paste( "fitVal", 1:length( power ), sep = "" ) )
      estArg$xNames <- estXNames
   } else if( estFunc == "frontierQuad" ) {
      estShifterNames <- eval( estCall$shifterNames )
      estShifterNames <- c( estShifterNames,
         paste( "fitVal", 1:length( power ), sep = "" ) )
      estArg$shifterNames <- estShifterNames
      estArg$lrTests <- FALSE
   } else {
      stop( "function resettestFrontier() has not yet been implemented",
         " for stochastic frontier models estimated by function ",
         estFunc, "()" )
   }

   estArg$printIter <- 0
   estArg$startVal <- NULL
   estNew <- do.call( estFunc, estArg )
   
   ### commeted out, because [see above]
   if( logLik( estNew ) < logLik( object ) ) {
      estCoef <- coef( object )
      posSigmaSq <- which( names( estCoef ) == "sigmaSq" )
      startCoef <- c( estCoef[ 1:( posSigmaSq - 1 ) ],
         rep( 0, length( power ) ), estCoef[ posSigmaSq:length( estCoef ) ] ) 
      estArg$startVal <- startCoef
      estNew <- do.call( estFunc, estArg )
   }
   
   result <- lrtest( object, estNew )
   
   objectName <- as.character( match.call()$object )
   heading <- attr( result, "heading" )
   heading <- sub( "object", objectName, heading )
   heading <- sub( "estNew",
      paste( objectName,
         paste( paste( "fit^", power, sep = "" ), collapse = " + " ),
         sep = " + " ), heading )
   attr( result, "heading" ) <- heading
   
   return( result )
}