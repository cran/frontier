sfa <- function(
      formula, data = sys.frame( sys.parent() ),
      ineffDecrease = TRUE,
      truncNorm = FALSE,
      timeEffect = FALSE,
      startVal = NULL,
      tol = 0.00001,
      maxit = 1000,
      muBound = 2,
      bignum = 1.0E+16,
      searchStep = 0.00001,
      searchTol = 0.001,
      searchScale = NA,
      gridSize = 0.1,
      gridDouble = TRUE,
      restartMax = 10,
      restartFactor = 0.999,
      printIter = 0 ) {

   # determine modelType (im)
   formula <- as.Formula( formula )
   if( length( formula )[2] == 1 ) {
      modelType <- 1
      effFormula <- NULL
   } else if( length( formula )[2] == 2 ) {
      modelType <- 2
      effFormula <- formula( formula, lhs = 0, rhs = 2 )
   } else {
      stop( "argument 'formula' has an inappropriate number of RHS parts" )
   }
   formula <- formula( formula, lhs = 1, rhs = 1 )

   # formula
   if( class( formula ) != "formula" ) {
      stop( "argument 'formula' must be a formula" )
   } else if( length( formula ) != 3 ) {
      stop( "argument 'formula' must be a 2-sided formula" )
   }

   # ineffDecrease (ipc)
   if( !is.logical( ineffDecrease ) || length( ineffDecrease ) != 1 ) {
      stop( "argument 'ineffDecrease' must be a single logical value" )
   }
   # truncNorm (mu)
   if( !is.logical( truncNorm ) ) {
      stop( "argument 'truncNorm' must be logical" )
   }
   if( truncNorm && modelType == 2 ) {
      warning( "argument 'truncNorm' is ignored in",
         " Efficiency Effects Frontiers (EEF)" )
   }
   # timeEffect (eta)
   if( !is.logical( timeEffect ) ) {
      stop( "argument 'timeEffect' must be logical" )
   }
   if( timeEffect && ! "plm.dim" %in% class( data ) ) {
      warning( "argument 'timeEffect' is ignored in case of",
         " cross-sectional data" )
   }
   # printIter (iprint)
   if( !is.numeric( printIter ) ) {
      stop( "argument 'printIter' must be numeric" )
   } else if( printIter != round( printIter ) ) {
      stop( "argument 'printIter' must be an iteger" )
   } else if( printIter < 0 ) {
      stop( "argument 'printIter' must be non-negative" )
   }
   printIter <- as.integer( printIter )
   # searchScale (indic)
   if( length( searchScale ) != 1 ) {
      stop( "argument 'searchScale' must be a single logical value or NA" )
   } else if( is.na( searchScale ) ) {
      indic <- as.integer( 1 )
   } else if( is.logical( searchScale ) ) {
      indic <- as.integer( 2 - 2 * searchScale )
   } else {
      stop( "argument 'searchScale' must be a logical value or NA" )
   }
   # tol
   if( !is.numeric( tol ) ) {
      stop( "argument 'tol' must be numeric" )
   } else if( tol < 0 ) {
      stop( "argument 'tol' must be non-negative" )
   }
   # searchTol (tol2)
   if( !is.numeric( searchTol ) ) {
      stop( "argument 'searchTol' must be numeric" )
   } else if( searchTol < 0 ) {
      stop( "argument 'searchTol' must be non-negative" )
   }
   # muBound
   if( !is.numeric( muBound ) || length( muBound ) != 1 ) {
      stop( "argument 'muBound' must be a numeric scalar" )
   } else if( is.infinite( muBound ) ) {
      muBound <- 0
   }
   # bignum
   if( !is.numeric( bignum ) ) {
      stop( "argument 'bignum' must be numeric" )
   } else if( bignum <= 0 ) {
      stop( "argument 'bignum' must be positive" )
   }
   # searchStep (step1)
   if( !is.numeric( searchStep ) ) {
      stop( "argument 'searchStep' must be numeric" )
   } else if( searchStep <= 0 ) {
      stop( "argument 'searchStep' must be positive" )
   }
   # gridDouble (igrid2)
   if( !is.logical( gridDouble ) || length( gridDouble ) != 1 ) {
      stop( "argument 'gridDouble' must be a single logical value" )
   }
   # gridSize (gridno)
   if( !is.numeric( gridSize ) ) {
      stop( "argument 'gridSize' must be numeric" )
   } else if( gridSize <= 0 ) {
      stop( "argument 'gridSize' must be positive" )
   }
   # maxit
   if( !is.numeric( maxit ) || length( maxit ) != 1 ) {
      stop( "argument 'maxit' must be a single numeric scalar" )
   } else if( maxit != round( maxit ) ) {
      stop( "argument 'maxit' must be an integer" )
   } else if( maxit < 0 ) {
      stop( "argument 'maxit' must not be negative" )
   }
   maxit <- as.integer( maxit )
   # restartMax
   if( !is.numeric( restartMax ) || length( restartMax ) != 1 ) {
      stop( "argument 'restartMax' must be a single numeric scalar" )
   } else if( restartMax != round( restartMax ) ) {
      stop( "argument 'restartMax' must be an integer" )
   } else if( restartMax < 0 ) {
      stop( "argument 'restartMax' must not be negative" )
   }
   restartMax <- as.integer( restartMax )
   # restartFactor
   if( !is.numeric( restartFactor ) || length( restartFactor ) != 1 ) {
      stop( "argument 'restartFactor' must be a numeric scalar" )
   } else if( is.infinite( restartFactor ) ) {
      stop( "argument 'restartFactor' must be finite" )
   }


   # preparing model matrix and model response
   mc <- match.call( expand.dots = FALSE )
   m <- match( "data", names( mc ), 0 )
   mf <- mc[ c( 1, m ) ]
   mf$formula <- formula
   attributes( mf$formula ) <- NULL
   mf$na.action <- na.pass
   mf[[ 1 ]] <- as.name( "model.frame" )
   mf <- eval( mf, parent.frame() )
   mt <- attr( mf, "terms" )
   xMat <- model.matrix( mt, mf )
   if( ncol( xMat ) > 0 && colnames( xMat )[ 1 ] == "(Intercept)" ) {
      xMat <- xMat[ , -1, drop = FALSE ]
   } else {
      stop( "the model cannot be estimated without an intercept" )
   }
   xNames <- colnames( xMat )
   yVec <- model.response( mf )
   yName <- as.character( formula )[ 2 ]
   if( length( yVec ) != nrow( xMat ) ) {
      stop( "the number of observations of the endogenous variable (",
         length( yVec ), ") is not equal to the number of observations",
         " of the exogenous variables (", nrow( xMat ), ")" )
   }

   # cross section and time period identifier
   if( "plm.dim" %in% class( data ) ) {
      dataTable <- matrix( as.integer( data[[ 1 ]] ), ncol = 1 )
      dataTable <- cbind( dataTable, as.integer( data[[ 2 ]] ) )
   } else {
      dataTable <- matrix( 1:length( yVec ), ncol = 1 )
      dataTable <- cbind( dataTable, rep( 1, nrow( dataTable ) ) )
   }
   nXvars <- length( xNames )
   nb <- nXvars

   # endogenous variable
   dataTable <- cbind( dataTable, yVec )
   if( sum( !is.na( yVec ) & is.finite( yVec ) ) == 0 ) {
      stop( "the dependent variable has no valid observations" )
   }

   # exogenous variables
   dataTable <- cbind( dataTable, xMat )
   paramNames <- "(Intercept)";
   if( nXvars > 0 ) {
      for( i in 1:nXvars ) {
         paramNames <- c( paramNames, xNames[ i ] )
         if( sum( !is.na( xMat[ , i ] ) & is.finite( xMat[ , i ] ) ) == 0 ) {
            stop( "regressor '", xNames[ i ], "' has no valid observations" )
         }
      }
   }

   # variables explaining the efficiency level
   if( is.null( effFormula  ) ) {
      zNames <- NULL
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
      mfe <- eval( mfe, parent.frame() )
      mte <- attr( mfe, "terms" )
      zMat <- model.matrix( mte, mfe )
      if( ncol( zMat ) > 0 && colnames( zMat )[ 1 ] == "(Intercept)" ) {
         zIntercept <- TRUE
         zMat <- zMat[ , -1, drop = FALSE ]
      } else {
         zIntercept <- FALSE
      }
      if( nrow( zMat ) != nrow( xMat ) ) {
         stop( "the number of observations of the variables explaining",
            " efficiency (", nrow( zMat ), ") is not equal to the number",
            " of observations of the (regular) regressors (",
            nrow( xMat ), ")" )
      }
      dataTable <- cbind( dataTable, zMat )
      zNames <- colnames( zMat )
      if( length( zNames ) > 0 ) {
         for( i in 1:length( zNames ) ) {
            if( sum( !is.na( zMat[ , i ] ) & is.finite( zMat[ , i ] ) ) == 0 ) {
               stop( "the regressor for the inefficiency term '", zNames[ i ],
                  "' has no valid observations" )
            }
         }
      }
   }
   nZvars <- length( zNames )

   # detect and remove observations with NAs, NaNs, and INFs
   validObs <- rowSums( is.na( dataTable ) | is.infinite( dataTable ) ) == 0
   dataTable <- dataTable[ validObs, ]
   # number of (valid) observations
   nob <- sum( validObs )
   # number of cross-section units
   nn <- length( unique( dataTable[ , 1 ] ) )
   # number of time periods
   nt <- length( unique( dataTable[ , 2 ] ) )

   # mu: truncNorm, zIntercept
   if( modelType == 1 ) {
      mu <- truncNorm
   } else {
      mu <- zIntercept
   }

   # eta: timeEffect, nZvars
   if( modelType == 1 ) {
      eta <- timeEffect
   } else {
      eta <- nZvars
   }

   # adding column names to the data table
   colnames( dataTable ) <- c( "id", "t", yName, xNames, zNames )

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
   rownames( dataTable ) <- obsNames[ validObs ]
   names( validObs ) <- obsNames

   nParamTotal <- nb + 3 + mu + eta
   if( nParamTotal > nob ) {
      stop( "the model cannot be estimated,",
         " because the number of parameters (", nParamTotal,
         ") is larger than the number of",
         ifelse( sum( !validObs ) > 0, " valid", "" ),
         " observations (", nob, ")" )
   }
   if( is.null( startVal ) ) {
      startVal <- 0
   } else {
      if( nParamTotal != length( startVal ) ) {
         stop( "wrong number of starting values (you provided ",
            length( startVal ), " starting values but the model has ",
            nParamTotal, " parameters)" )
      }
   }

   # OLS estimation
   if( nXvars > 0 ) {
      ols <- lm( dataTable[ , 3 ] ~ dataTable[ , 4:( 3 + nb ) ] )
   } else {
      ols <- lm( dataTable[ , 3 ] ~ 1 )
   }
   olsParam <- c( coef( ols ), summary( ols )$sigma^2 )
   olsStdEr <- sqrt( diag( vcov( ols ) ) )
   olsLogl  <- logLik( ols )[ 1 ]

   returnObj <- .Fortran( "front41",
      modelType = as.integer( modelType ),
      ineffDecrease = as.integer( ( !ineffDecrease ) + 1 ),
      nn = as.integer( nn ),
      nt = as.integer( nt ),
      nob = as.integer( nob ),
      nb = as.integer( nb ),
      mu = as.integer( mu ),
      eta = as.integer( eta ),
      printIter = as.integer( printIter ),
      indic = as.integer( indic ),
      tol = as.double( tol ),
      searchTol = as.double( searchTol ),
      bignum = as.double( bignum ),
      searchStep = as.double( searchStep ),
      gridDouble = as.integer( gridDouble ),
      gridSize = as.double( gridSize ),
      maxit = as.integer( maxit ),
      muBound = as.double( muBound ),
      restartMax = as.integer( restartMax ),
      restartFactor = as.double( restartFactor ),
      nRestart = as.integer( 0 ),
      nStartVal = as.integer( length( startVal ) ),
      startVal = as.double( startVal ),
      nRowData = as.integer( nrow( dataTable ) ),
      nColData = as.integer( ncol( dataTable ) ),
      dataTable = matrix( as.double( dataTable ), nrow( dataTable ),
         ncol( dataTable ), dimnames = dimnames( dataTable ) ),
      nParamTotal = as.integer( nParamTotal ),
      olsParam = as.double( c( olsParam, rep( 0, 1 + mu + eta ) ) ),
      gridParam = as.double( rep( 0, nParamTotal ) ),
      startLogl = as.double( 0 ),
      mleParam = as.double( rep( 0, nParamTotal ) ),
      mleCov = matrix( as.double( 0 ), nParamTotal, nParamTotal ),
      mleLogl = as.double( 0 ),
      nIter = as.integer( 0 ),
      code = as.integer( 0 ) )
   returnObj$nStartVal <- NULL
   returnObj$nRowData <- NULL
   returnObj$nColData <- NULL
   returnObj$nParamTotal <- NULL
   returnObj$ineffDecrease <- as.logical( 2 - returnObj$ineffDecrease )
   returnObj$gridDouble <- as.logical( returnObj$gridDouble )
   returnObj$olsParam <- olsParam
   returnObj$olsStdEr <- olsStdEr
   returnObj$olsLogl  <- olsLogl

   ## calculate residuals
   resid <- drop( dataTable[ , 3 ] -
      cbind( rep( 1, nrow( dataTable ) ), xMat[ validObs, ] ) %*%
      returnObj$mleParam[ 1:( nb + 1 ) ] )
   returnObj$resid <- matrix( NA, nrow = nn, ncol = nt )
   if( length( resid ) != nrow( dataTable ) ) {
      stop( "internal error: length of residuals is not equal to",
         " the number of rows of the data table (valid observations)" )
   }
   for( i in 1:length( resid ) ) {
      returnObj$resid[ dataTable[ i, 1 ], dataTable[ i, 2 ] ] <-
         resid[ i ]
   }

   ## skewness of OLS residuals
   returnObj$olsResid <- residuals( ols )
   returnObj$olsSkewness <- skewness( returnObj$olsResid )
   returnObj$olsSkewnessOkay <- returnObj$olsSkewness * ( -1 )^ineffDecrease >= 0


   ## warnings regarding wrong skewness, smaller logLik value, and no convergence
   warnMaxit <- maxit <= returnObj$nIter && maxit > 0
   if( !returnObj$olsSkewnessOkay && returnObj$mleLogl < returnObj$olsLogl ) {
      warning( "the residuals of the OLS estimates are ",
         ifelse( ineffDecrease, "right", "left" ), "-skewed",
         " and the likelihood value of the ML estimation is less",
         " than that obtained using OLS;",
         " this usually indicates that there is no inefficiency",
         " or that the model is misspecified" )
   } else if( !returnObj$olsSkewnessOkay ) {
      warning( "the residuals of the OLS estimates are ",
         ifelse( ineffDecrease, "right", "left" ), "-skewed;",
         " this might indicate that there is no inefficiency",
         " or that the model is misspecified" )
   } else if( returnObj$mleLogl < returnObj$olsLogl && warnMaxit ) {
      warning( "the maximum number of iterations has been reached and",
         " the likelihood value of the ML estimation is less",
         " than that obtained using OLS;",
         " please try again using different starting values and/or",
         " increase the maximum number of iterations" )
      warnMaxit <- FALSE
   } else if( returnObj$mleLogl < returnObj$olsLogl ) {
      warning( "the likelihood value of the ML estimation is less",
         " than that obtained using OLS;",
         " this indicates that the likelihood maximization did not",
         " converge to the global maximum or",
         " that there is no inefficiency",
         " (you could try again using different starting values)" )
   }
   if( warnMaxit ) {
      warning( "the maximum number of iterations has been reached;",
         " please try again using different starting values and/or",
         " increase the maximum number of iterations" )
   }

   # mu: truncNorm, zIntercept
   if( modelType == 1 ) {
      returnObj$truncNorm <- as.logical( returnObj$mu )
      returnObj$zIntercept <- zIntercept
      returnObj$mu <- NULL
   } else {
      returnObj$truncNorm <- truncNorm
      returnObj$zIntercept <- as.logical( returnObj$mu )
      returnObj$mu <- NULL
   }
   # eta: timeEffect, nz
   if( modelType == 1 ) {
      returnObj$timeEffect <- as.logical( returnObj$eta )
   } else {
      returnObj$timeEffect <- timeEffect
   }
   returnObj$eta <- NULL

   if( returnObj$indic == 2 ) {
      returnObj$searchScale <- FALSE
   } else if( returnObj$indic == 1 ) {
      returnObj$searchScale <- NA
   } else {
      returnObj$searchScale <- TRUE
   }
   returnObj$indic <- NULL
   if( length( startVal ) == 1 ){
      if( modelType == 1 ) {
         returnObj$gridParam <- returnObj$gridParam[ 1:( nb + 3 ) ]
      } else {
         returnObj$gridParam <- returnObj$gridParam[
            c( 1:( nb + 1 ), ( nParamTotal - 1 ):nParamTotal ) ]
      }
      names( returnObj )[ names( returnObj ) == "startLogl" ] <- "gridLogl"
   } else {
      returnObj$gridParam <- NULL
   }
   # assign row names and column names to residuals
   if( "plm.dim" %in% class( data ) ) {
      rownames( returnObj$resid ) <- levels( data[[ 1 ]] )
      colnames( returnObj$resid ) <- levels( data[[ 2 ]] )
   } else {
      rownames( returnObj$resid ) <- obsNames
      colnames( returnObj$resid ) <- "residuals"
   }
   if( modelType == 2 ) {
      if( zIntercept ){
         paramNames <- c( paramNames, "Z_(Intercept)" )
      }
      if( nZvars > 0 ) {
         paramNames <- c( paramNames, paste( "Z", zNames, sep = "_" ) )
      }
   }

   if( length( startVal ) == 1 ){
      returnObj$startVal <- NULL
   }
   paramNames <- c( paramNames, "sigmaSq", "gamma" )
   if( modelType == 1 ) {
      if( truncNorm ){
         paramNames <- c( paramNames, "mu" )
      }
      if( timeEffect ){
         paramNames <- c( paramNames, "time" )
      }
   }
   names( returnObj$olsParam ) <- c( paramNames[ 1:( nb + 1 ) ],
      "sigmaSq" )
   names( returnObj$olsStdEr ) <- paramNames[ 1:( nb + 1 ) ]
   if( !is.null( returnObj$gridParam ) ) {
      names( returnObj$gridParam ) <- c( paramNames[ 1:( nb + 1 ) ],
         "sigmaSq", "gamma" )
   }
   names( returnObj$mleParam ) <- paramNames
   rownames( returnObj$mleCov ) <- paramNames
   colnames( returnObj$mleCov ) <- paramNames
   if( !is.null( returnObj$startVal ) ) {
      names( returnObj$startVal ) <- paramNames
   }
   returnObj$call <- match.call()
   returnObj$validObs <- validObs

   class( returnObj ) <- "frontier"
   return( returnObj )
}
