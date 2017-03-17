frontierDataTable <- function( data, formula, effFormula, mc, mfe ) {

   # preparing model matrix and model response
   m <- match( "data", names( mc ), 0 )
   mf <- mc[ c( 1, m ) ]
   mf$formula <- formula
   attributes( mf$formula ) <- NULL
   mf$na.action <- na.pass
   mf[[ 1 ]] <- as.name( "model.frame" )
   mf <- eval( mf, parent.frame( n = 2 ) )
   mt <- attr( mf, "terms" )
   xMat <- model.matrix( mt, mf )
   xNames <- colnames( xMat )
   yVec <- model.response( mf )
   yName <- as.character( formula )[ 2 ]
   if( length( yVec ) != nrow( xMat ) ) {
      return( paste( "the number of observations of the endogenous variable (",
         length( yVec ), ") is not equal to the number of observations",
         " of the exogenous variables (", nrow( xMat ), ")", sep = "" ) )
   }

   # cross section and time period identifier  (based on code from Kevin Tappe)
   if( inherits( data, "pdata.frame" ) ) {
      # current panel data format from pkg plm
      idVec <- as.integer( index( data )[[ 1 ]] )
      timeVec <- as.integer( index( data )[[ 2 ]] )
   } else if( inherits( data, "plm.dim" ) ) {
      # deprecated panel data format from pkg plm
      idVec <- as.integer( data[[ 1 ]] )
      timeVec <- as.integer( data[[ 2 ]] )
   } else {
      idVec <- 1:length( yVec )
      timeVec <- rep( 1, length( idVec ) )
   }

   # check dependent variable
   if( sum( !is.na( yVec ) & is.finite( yVec ) ) == 0 ) {
      return( "the dependent variable has no valid observations" )
   }

   # explanatory x variables
   if( ncol( xMat ) > 0 ) {
      for( i in 1:ncol( xMat ) ) {
         if( sum( !is.na( xMat[ , i ] ) & is.finite( xMat[ , i ] ) ) == 0 ) {
            return( paste( "regressor '", xNames[ i ], "' has no valid observations",
               sep = "" ) )
         }
      }
   }

   # variables explaining the efficiency level
   if( is.null( effFormula  ) ) {
      zMat <- NULL
      zIntercept <- FALSE
   } else {
      if( class( effFormula ) != "formula" ) {
         stop( "argument 'effFormula' must be a formula" )
      } else if( length( effFormula ) != 2 ) {
         stop( "argument 'formula' must be a 1-sided formula" )
      }
      me <- match( "data", names( mc ), 0 )
      mfe <- mc[ c( 1, me ) ]
      mfe$formula <- effFormula
      attributes( mfe$formula ) <- NULL
      mfe$na.action <- na.pass
      mfe[[ 1 ]] <- as.name( "model.frame" )
      mfe <- eval( mfe, parent.frame( n = 2 ) )
      mte <- attr( mfe, "terms" )
      zMat <- model.matrix( mte, mfe )
      if( ncol( zMat ) > 0 && colnames( zMat )[ 1 ] == "(Intercept)" ) {
         zIntercept <- TRUE
         zMat <- zMat[ , -1, drop = FALSE ]
      } else {
         zIntercept <- FALSE
      }
      if( nrow( zMat ) != nrow( xMat ) ) {
         return( paste( "the number of observations of the variables explaining",
            " efficiency (", nrow( zMat ), ") is not equal to the number",
            " of observations of the (regular) regressors (",
            nrow( xMat ), ")", sep = "" ) )
      }
      if( ncol( zMat ) > 0 ) {
         for( i in 1:ncol( zMat ) ) {
            if( sum( !is.na( zMat[ , i ] ) & is.finite( zMat[ , i ] ) ) == 0 ) {
               return( paste( "the regressor for the inefficiency term '", 
                  colnames( zMat )[ i ], "' has no valid observations", sep = "" ) )
            }
         }
      }
   }

   # detect and remove observations with NAs, NaNs, and INFs
   dataTable <- cbind( idVec, timeVec, yVec, xMat, zMat )
   validObs <- rowSums( is.na( dataTable ) | is.infinite( dataTable ) ) == 0
   rm( dataTable )
   idVec   <- idVec[ validObs ]
   timeVec <- timeVec[ validObs ]
   yVec   <- yVec[ validObs ]
   xMat   <- xMat[ validObs, , drop = FALSE ]
   zMat <- zMat[ validObs, , drop = FALSE ]

   # make sure that the cross-section units are numbered continously
   firmId <- sort( unique( idVec ) )
   idVecNew <- rep( NA, sum( validObs ) )
   for( i in 1:length( firmId ) ) {
      idVecNew[ idVec == firmId[ i ] ] <- i
   }
   idVec <- idVecNew
   
   # check consistency of firm numbers
   if( any( is.na( idVec ) ) ) {
      return( "internal error: at least one firm number is NA" )
   }
   if( min( idVec ) != 1 ) {
      return( "internal error: the smallest firm number must be one" )
   }
   if( max( idVec ) > length( firmId ) ) {
      return( "internal error: a firm number is larger than the number of firms" )
   }
   
   # make sure that the time periods are numbered continously
   timeId <- sort( unique( timeVec ) )
   timeVecNew <- rep( NA, sum( validObs ) )
   for( i in 1:length( timeId ) ) {
      timeVecNew[ timeVec == timeId[ i ] ] <- i
   }
   timeVec <- timeVecNew
   
   # check consistency of time period numbers
   if( any( is.na( timeVec ) ) ) {
      return( "internal error: at least one time period number is NA" )
   }
   if( min( timeVec ) != 1 ) {
      return( "internal error: the smallest time period number must be one" )
   }
   if( max( timeVec ) > length( timeId ) ) {
      return( "internal error: a time period number is larger",
         " than the number of time periods" )
   }
   
   # check for double entries for firm/period combinations
   for( i in 1:length( firmId ) ) {
      for( j in 1:length( timeId ) ) {
         if( sum( idVec == i & timeVec == j ) > 1 ){
            return( paste( "more than one observation for firm '", firmId[ i ],
               "' in period '", timeId[ j ], "'", sep = "" ) )
         }
      }
   }
   
   # obtaining names of the observations
   if( !is.null( rownames( data ) ) ) {
      obsNames <- rownames( data )
   } else if( !is.null( names( yVec ) ) ) {
      obsNames <- names( yVec )
   } else if( !is.null( rownames( xMat ) ) ) {
      obsNames <- rownames( xMat )
   } else if( !is.null( rownames( zMat ) ) ) {
      obsNames <- rownames( zMat )
   } else {
      obsNames <- NULL
   }
   names( validObs ) <- obsNames
   
   returnObj <- list()
   returnObj$idVec       <- idVec
   returnObj$timeVec     <- timeVec
   returnObj$yVec        <- yVec
   returnObj$yName       <- yName
   returnObj$xMat        <- xMat
   returnObj$zMat        <- zMat
   returnObj$validObs    <- validObs
   returnObj$firmId      <- firmId
   returnObj$timeId      <- timeId
   returnObj$zIntercept  <- zIntercept

   return( returnObj )
}
