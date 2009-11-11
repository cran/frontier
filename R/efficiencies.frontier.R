# efficiencies of frontier models
efficiencies.frontier <- function( object, asInData = FALSE,
      logDepVar = TRUE, ... ) {

   resid <- residuals( object )
   fitted <- - resid
   for( i in 1:nrow( object$dataTable ) ) {
      firm <- object$dataTable[ i, 1 ]
      time <- object$dataTable[ i, 2 ]
      fitted[ firm, time ] <- fitted[ firm, time] + object$dataTable[ i, 3 ]
   }
   sigmaSq <- coef( object )[ "sigmaSq" ]
   gamma <- coef( object )[ "gamma" ]
   lambda <- sqrt( gamma / ( 1 - gamma ) )
   if( object$ineffDecrease ) {
      dir <- 1
   } else {
      dir <- -1
   }
   if( object$modelType == 1 ) {
      if( object$timeEffect ) {
         eta <- coef( object )[ "time" ]
         etaStar <- exp( - eta * ( 1:object$nt - object$nt ) )
      } else {
         eta <- 0
         etaStar <- rep( 1, object$nt )
      }
      if( object$nt == 1 ) {
         residStar <- drop( resid )
         tStar <- 1
      } else {
         residStar <- rep( NA, object$nn )
         tStar <- rep( NA, object$nn )
         for( i in 1:object$nn ) {
            residStar[ i ] <- sum( resid[ i, ] * etaStar, na.rm = TRUE )
            tStar[ i ] <- sum( etaStar[ !is.na( resid[ i, ] ) ]^2 )
         }
      }
      if( object$truncNorm ) {
         mu <- coef( object )[ "mu" ]
      } else {
         mu <- 0
      }
      muStar <- ( - dir * gamma * residStar + mu * ( 1 - gamma ) ) /
         ( 1 + ( tStar - 1 ) * gamma )
      sigmaStarSq <- sigmaSq * gamma * ( 1 - gamma ) /
         ( 1 + ( tStar - 1 ) * gamma )
      sigmaStar <- sqrt( sigmaStarSq )
      result <- matrix( NA, nrow = object$nn,
         ncol = object$nt^object$timeEffect )
      if( logDepVar ) {
         for( j in 1:ncol( result ) ) {
            result[ , j ] <- (
               pnorm( - dir * sigmaStar * etaStar[j] + muStar / sigmaStar ) /
               pnorm( muStar / sigmaStar ) ) *
               exp( - dir * muStar * etaStar[j] + 0.5 * sigmaStarSq * etaStar[j]^2 )
         }
      } else {
         fittedStar <- rep( NA, object$nn )
         tInd <- rep( NA, object$nn )
         for( i in 1:object$nn ) {
            fittedStar[ i ] <- sum( fitted[ i, ], na.rm = TRUE )
            tInd[ i ] <- sum( !is.na( resid[ i, ] ) )
         }
         for( j in 1:ncol( result ) ) {
            result[ , j ] <- 1 - dir * etaStar[ j ] * ( muStar + sigmaStar *
               exp( dnorm( muStar / sigmaStar, log = TRUE ) -
                  pnorm( muStar / sigmaStar, log = TRUE ) ) ) /
               ( fittedStar / tInd )
         }
      }
      # set efficiency estimates of missing observations to NA
      if( ncol( result ) > 1 ) {
         result[ is.na( resid ) ] <- NA
      }
   } else if( object$modelType == 2 ) {
      if( object$zIntercept ) {
         zDelta <- coef( object )[ "Z_(Intercept)" ]
      } else {
         zDelta <- 0
      }
      nz <- ncol( object$dataTable ) - 3 - object$nb
      if( nz > 0 ) {
         for( i in 1:nz ) {
            zDelta <- zDelta + object$dataTable[ ,
                  ncol( object$dataTable ) - nz + i ] *
               coef( object )[ object$nb + object$zIntercept + 1 + i  ]
         }
      } else {
         zDelta <- rep( zDelta, nrow( object$dataTable ) )
      }
      zDeltaMat <- matrix( NA, nrow = object$nn, ncol = object$nt )
      for( i in 1:object$nob ) {
         zDeltaMat[ object$dataTable[ i, 1 ], object$dataTable[ i, 2 ] ] <-
            zDelta[ i ]
      }
      sigmaBarSq <- gamma * ( 1 - gamma ) * sigmaSq
      sigmaBar <- sqrt( sigmaBarSq )
      muBar <- ( 1 - gamma ) * zDeltaMat - dir * gamma * resid
      if( logDepVar ) {
         result <- ( pnorm( - dir * sigmaBar + muBar / sigmaBar ) /
               pnorm( muBar / sigmaBar ) ) *
               exp( - dir * muBar + 0.5 * sigmaBarSq )
      } else {
         result <- 1 - dir * ( muBar + sigmaBar *
            exp( dnorm( muBar / sigmaBar, log = TRUE ) -
               pnorm( muBar / sigmaBar, log = TRUE ) ) ) /
            fitted
      }
   } else {
      stop( "internal error: unknow model type '",
         object$modelType, "'" )
   }

   if( object$ineffDecrease ) {
      result[ result > 1 ] <- 1
   } else {
      result[ result < 1 ] <- 1
   }

   rownames( result ) <- rownames( resid )
   if( ncol( result ) > 1 ) {
      colnames( result ) <- colnames( resid )
   } else {
      colnames( result ) <- "efficiency"
   }

   if( asInData ) {
      effic <- rep( NA, length( object$validObs ) )
      if( sum( object$validObs ) != nrow( object$dataTable ) ) {
         stop( "internal error: number of rows of element 'dataTable' is not",
            " equal to number of valid observations" )
      }
      for( i in 1:nrow( object$dataTable ) ) {
         effic[ object$validObs ][ i ] <- result[ object$dataTable[ i , 1 ],
            min( object$dataTable[ i , 2 ], ncol( result ) ) ]
      }
      result <- effic
      names( result ) <- names( object$validObs )
   }
   return( result )
}
