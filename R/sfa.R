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
   if( !is.numeric( maxit ) ) {
      stop( "argument 'maxit' must be numeric" )
   } else if( maxit != round( maxit ) ) {
      stop( "argument 'maxit' must be an integer" )
   } else if( maxit < 0 ) {
      stop( "argument 'maxit' must not be negative" )
   }
   maxit <- as.integer( maxit )

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
      nn <- length( unique( data[[ 1 ]] ) )
      nt <- length( unique( data[[ 2 ]] ) )
   } else {
      dataTable <- matrix( 1:length( yVec ), ncol = 1 )
      dataTable <- cbind( dataTable, rep( 1, nrow( dataTable ) ) )
      nn <- nrow( xMat )
      nt <- 1
   }
   nob <- nrow( dataTable )
   nXvars <- length( xNames )
   nb <- nXvars

   # endogenous variable
   dataTable <- cbind( dataTable, yVec )
   dataTable <- cbind( dataTable, xMat )

   # exogenous variables
   paramNames <- "(Intercept)";
   if( nXvars > 0 ) {
      for( i in 1:nXvars ) {
         paramNames <- c( paramNames, xNames[ i ] )
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
   }
   nZvars <- length( zNames )

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
   rownames( dataTable ) <- obsNames

   nParamTotal <- nb + 3 + mu + eta
   if( is.null( startVal ) ) {
      startVal <- 0
   } else {
      if( nParamTotal != length( startVal ) ) {
         stop( "wrong number of starting values (you provided ",
            length( startVal ), " starting values but the model has ",
            nParamTotal, " parameters)" )
      }
   }

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
      nStartVal = as.integer( length( startVal ) ),
      startVal = as.double( startVal ),
      nRowData = as.integer( nrow( dataTable ) ),
      nColData = as.integer( ncol( dataTable ) ),
      dataTable = matrix( as.double( dataTable ), nrow( dataTable ),
         ncol( dataTable ), dimnames = dimnames( dataTable ) ),
      nParamTotal = as.integer( nParamTotal ),
      olsParam = as.double( rep( 0, nParamTotal ) ),
      olsStdEr = as.double( rep( 0, nParamTotal ) ),
      olsLogl = as.double( 0 ),
      gridParam = as.double( rep( 0, nParamTotal ) ),
      startLogl = as.double( 0 ),
      mleParam = as.double( rep( 0, nParamTotal ) ),
      mleCov = matrix( as.double( 0 ), nParamTotal, nParamTotal ),
      mleLogl = as.double( 0 ),
      nIter = as.integer( 0 ) )
   returnObj$nStartVal <- NULL
   returnObj$nRowData <- NULL
   returnObj$nColData <- NULL
   returnObj$nParamTotal <- NULL
   returnObj$ineffDecrease <- as.logical( 2 - returnObj$ineffDecrease )
   returnObj$gridDouble <- as.logical( returnObj$gridDouble )
   returnObj$olsParam <- returnObj$olsParam[ 1:( nb + 2 ) ]
   returnObj$olsStdEr <- returnObj$olsStdEr[ 1:( nb + 1 ) ]

   ## calculate residuals
   resid <- drop( dataTable[ , 3 ] -
      cbind( rep( 1, nrow( dataTable ) ), xMat ) %*%
      returnObj$mleParam[ 1:( nb + 1 ) ] )
   returnObj$resid <- matrix( NA, nrow = nn, ncol = nt )
   if( length( resid ) != nrow( dataTable ) ) {
      stop( "internal error: length of residuals is not equal to",
         " the number of rows of the data table" )
   }
   for( i in 1:length( resid ) ) {
      returnObj$resid[ dataTable[ i, 1 ], dataTable[ i, 2 ] ] <-
         resid[ i ]
   }

   # check if the maximum number of iterations has been reached
   if( maxit <= returnObj$nIter && maxit > 0 ) {
      warning( "Maximum number of iterations reached" );
   }

   # likelihood ratio test
   returnObj$lrTestVal <- 2 * (returnObj$mleLogl - returnObj$olsLogl )
   if( returnObj$lrTestVal < 0 ) {
      warning( "the likelihood value of the ML estimation is less",
         " than that obtained using ols --",
         " please try again using different starting values" )
   }

   # degrees of freedom of the likelihood ratio test
   if( returnObj$modelType == 1 ) {
      returnObj$lrTestDf <- truncNorm + timeEffect + 1
   } else {
      returnObj$lrTestDf <- zIntercept + nZvars + 1
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

   class( returnObj ) <- "frontier"
   return( returnObj )
}
