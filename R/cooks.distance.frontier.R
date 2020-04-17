cooks.distance.frontier <- function( model, target = "predict",
  asInData = FALSE, progressBar = TRUE, ... ) {
  
  estCall <- model$call
  estFunc <- as.character( estCall[[ 1 ]] )
  estArg <- as.list( estCall )[ -1 ]
  
  # data set used for the estimation
  estData <- eval( estCall$data )
  if( target == "predict" ) {
    fitVal <- fitted( model, asInData = TRUE )
    residVal <- residuals( model, asInData = TRUE )
  } else if( target == "efficiencies" ) {
    effVal <- efficiencies( model, asInData = TRUE, ... )
  } else {
    stop( "argument 'target' must be either 'predict' or 'efficiencies'" )
  }
  
  # make some checks
  reportMessage <- paste( 
    "Please report this problem to the maintainer of the 'frontier' package",
    "preferable wit R code and data that reproduce this error" )
  if( nrow( estData ) != length( model$validObs ) ) {
    stop( "internal error: the number of rows of 'estData' is not equal to",
      " the number of elements of 'validObs'. ",
      reportMessage )
  }
  if( target == "predict" ) {
    if( length( fitVal ) != length( model$validObs ) ) {
      stop( "internal error: the number of elements of 'fitVal' is not equal to",
        " the number of elements of 'validObs'. ",
        reportMessage )
    }
    if( length( residVal ) != length( model$validObs ) ) {
      stop( "internal error: the number of elements of 'residVal' is not equal to",
        " the number of elements of 'validObs'. ",
        reportMessage )
    }
    if( any( is.na( fitVal[ model$validObs ] ) ) ) {
      stop( "internal error: there are NA values in 'fitVal[ validObs ]'. ",
        reportMessage )
    }
    if( any( is.na( residVal[ model$validObs ] ) ) ) {
      stop( "internal error: there are NA values in 'residVal[ validObs ]'. ",
        reportMessage )
    }
  } else if( target == "efficiencies" ) {
    if( length( effVal ) != length( model$validObs ) ) {
      stop( "internal error: the number of elements of 'effVal' is not equal to",
        " the number of elements of 'validObs'. ",
        reportMessage )
    }
    if( any( is.na( effVal[ model$validObs ] ) ) ) {
      stop( "internal error: there are NA values in 'effVal[ validObs ]'. ",
        reportMessage )
    }
  }

  if( target == "predict" ) {
    # variance of the error term
    sigma2 <- sum( ( residVal[ model$validObs ] - 
        mean( residVal[ model$validObs ] ) )^2 ) /
      ( sum( model$validObs ) - model$nb )
  } else if( target == "efficiencies" ) {
    # variance of the efficiency scores
    sigma2Eff <- sum( ( effVal[ model$validObs ] - 
        mean( effVal[ model$validObs ] ) )^2 ) /
      ( sum( model$validObs ) - 1 )
  }
  
  # do not print output at iteration when re-estimating the SFA models
  estArg$printIter <- 0
  
  # vector for pseudo-Cook's distances
  cooksDist <- rep( NA, sum( model$validObs ) )
  
  # create progress bar
  if( progressBar ) {
    progBar <- txtProgressBar( min = 0, max = length( cooksDist ), style = 3 )
  }
  
  for( i in 1:length( cooksDist ) ) {
    # re-estimate the SFA model without the i-th valid observation
    estArg$data <- estData[ -which( model$validObs )[i], ]
    estNew <- suppressWarnings( do.call( estFunc, estArg ) )
  
    if( target == "predict" ) {
      # obtain predicted values for all observations
      predVal <- predict( estNew, newdata = estData )
      
      # calculate pseudo-Cook's distance for the predict values
      cooksDist[i] <- sum( ( (fitVal - predVal )[ model$validObs ] )^2 ) /
        ( model$nb * sigma2 )
    } else if( target == "efficiencies" ) {
      # obtain efficiency estimates for all observations
      effValNew <- efficiencies( estNew, newdata = estData, asInData = TRUE, 
        ... )
      
      # calculate pseudo-Cook's distance for the efficiency estimates
      cooksDist[i] <- sum( ( ( effVal - effValNew )[ model$validObs ] )^2 ) /
        ( model$nb * sigma2Eff )
    }
    
    # update progress bar
    if( progressBar ) {
      setTxtProgressBar( progBar, i )
    }
  }
  # close progress bar
  if( progressBar ) {
    close( progBar )
  }
  
  if( asInData ) {
    result <- rep( NA, length( model$validObs ) )
    result[ model$validObs ] <- cooksDist
  } else {
    result <- cooksDist
  }
  
  return( result )
}
