predict.frontier <- function( object, newdata = NULL, asInData = TRUE,  ... ) {
  
  if( is.null( newdata ) ) {
    pred <- fitted( object, asInData = asInData )
  } else {
    if( !is.data.frame( newdata ) ) {
      stop( "argument 'newdata' must be of class data.frame")
    }
    estCall <- object$call
    estFunc <- as.character( estCall[[ 1 ]] )
    estArg <- as.list( estCall )[ -1 ]
    estArg$data <- newdata
    
    estArg$maxit <- 0
    estArg$startVal <- object$mleParam
    estNew <- suppressWarnings( do.call( estFunc, estArg ) )
    pred <- fitted( estNew, asInData = asInData )
  }
  return( pred )
}
