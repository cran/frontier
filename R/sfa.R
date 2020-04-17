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
   if( !inherits( formula, "formula" ) ) {
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
   if( timeEffect && !( inherits( data, "pdata.frame" ) ||
         inherits( data, "plm.dim" ) ) ) {
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

   # obtain call with full names of arguments
   mc <- match.call( expand.dots = FALSE )
   # prepare data table, etc
   tmp <- frontierDataTable( formula = formula, effFormula = effFormula, 
      data = data, mc = mc )
   if( is.character( tmp ) ) {
      stop( tmp )
   }
   validObs   <- tmp$validObs
   dataTable  <- cbind( tmp$idVec, tmp$timeVec, tmp$yVec, tmp$xMat, tmp$zMat )
   rownames( dataTable ) <- names( validObs )[ validObs ]
   colnames( dataTable )[1:3] <- c( "id", "t", tmp$yName )
   firmId     <- tmp$firmId
   timeId     <- tmp$timeId
   nb         <- ncol( tmp$xMat )
   nob        <- sum( validObs )
   nn         <- max( tmp$idVec )
   nt         <- max( tmp$timeVec )
   zIntercept <- tmp$zIntercept
   nZvars     <- ncol( tmp$zMat )

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

   nParamTotal <- nb + 2 + mu + eta
   if( nParamTotal > nob && maxit > 0 ) {
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
   if( maxit > 0 ) {
      if( nb > 0 ) {
         ols <- lm( tmp$yVec ~ tmp$xMat - 1 )
      } else if( nb == 0 ) {
         ols <- lm( tmp$yVec ~ -1 )
      }
      if( any( is.na( coef( ols ) ) ) ) {
         stop( "at least one coefficient estimated by OLS is NA: ",
            paste( colnames( tmp$xMat )[ is.na( coef( ols ) ) ], collapse = ", " ),
         ". This may have been caused by (nearly) perfect multicollinearity" )
      }
      olsParam <- c( coef( ols ), summary( ols )$sigma^2 )
      olsStdEr <- sqrt( diag( vcov( ols ) ) )
      olsLogl  <- logLik( ols )[ 1 ]
   } else {
      olsParam <- rep( 0, ncol( tmp$xMat ) + 1 )
      olsStdEr <- rep( NA, ncol( tmp$xMat ) )
      olsLogl  <- NA
   }
   
   # factors for adjusting the parameters in the grid search
   if( maxit > 0 ) {
      if( nb > 0 ) {
         gridAdj <- coef( 
            lm( rep( 1, nrow( dataTable ) ) ~ dataTable[ , 4:( 3 + nb ) ] - 1 ) )
      } else {
         gridAdj <- numeric( 0 )
      }
   } else {
      gridAdj <- rep( 0, nb )
   }
   if( length( gridAdj ) != nb ) {
      stop( "internal error: the length of 'gridAdj' is not equal to 'nb'.",
         " Please contact the maintainer of the frontier package" )
   }

   returnObj <- .Fortran( C_front41,
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
      gridAdj = as.double( gridAdj ),
      gridParam = as.double( rep( 0, nParamTotal ) ),
      startLogl = as.double( 0 ),
      mleParam = as.double( rep( 0, nParamTotal ) ),
      mleCov = matrix( as.double( 0 ), nParamTotal, nParamTotal ),
      mleLogl = as.double( 0 ),
      nIter = as.integer( 0 ),
      code = as.integer( 0 ),
      nFuncEval = as.integer( 0 ) )

   # check if the return code indicates an error
   if( returnObj$code == 101 ) {
      stop( "the total number of observations exceeds the product of",
         " the number of firms by the number of years" )
   } else if( returnObj$code == 102 ) {
      stop( "internal error: calculated variable 'n'",
         " is not equal to argument 'nParamTotal'.",
         " Please contact the maintainer of the 'frontier' package",
         " (arne.henningsen@gmail.com)" )
   } else if( returnObj$code == 103 ) {
      stop( "wrong number of starting values" )
   } else if( returnObj$code == 104 ) {
      stop( "a firm number is < 1" )
   } else if( returnObj$code == 105 ) {
      stop( "a firm number is > number of firms" )
   } else if( returnObj$code == 106 ) {
      stop( "a period number is < 1" )
   } else if( returnObj$code == 107 ) {
      stop( "a period number is > number of periods" )
   } else if( returnObj$code == 108 ) {
      stop( "there are no observations on at least one firm" )
   } else if( returnObj$code == 109 ) {
      stop( "internal error: 2 + nr - nmu * (im-1)",
            " is not equal to argument 'nColData'.",
            " Please contact the maintainer of the 'frontier' package",
            " (arne.henningsen@gmail.com)" )
   } else if( returnObj$code > 100 ) {
      stop( "unknown error.",
         " Please contact the maintainer of the 'frontier' package",
         " (arne.henningsen@gmail.com)" )
   }
   returnObj$nStartVal <- NULL
   returnObj$nRowData <- NULL
   returnObj$nColData <- NULL
   returnObj$nParamTotal <- NULL
   returnObj$ineffDecrease <- as.logical( 2 - returnObj$ineffDecrease )
   returnObj$gridDouble <- as.logical( returnObj$gridDouble )
   returnObj$olsParam <- olsParam
   returnObj$olsStdEr <- olsStdEr
   returnObj$olsLogl  <- olsLogl

   ## calculate fitted "frontier" values
   if( nb == 0 ) {
      fitVal <- rep( 0, sum( validObs ) )
   } else {
      fitVal <- drop( dataTable[ , 4:(3+nb), drop = FALSE ] %*% 
            returnObj$mleParam[ 1:nb ] )
   }
   returnObj$fitted <- matrix( NA, nrow = nn, ncol = nt )
   if( length( fitVal ) != nrow( dataTable ) ) {
      stop( "internal error: length of the fitted values is not equal to",
         " the number of rows of the data table (valid observations)" )
   }
   for( i in 1:length( fitVal ) ) {
      returnObj$fitted[ dataTable[ i, 1 ], dataTable[ i, 2 ] ] <-
         fitVal[ i ]
   }

   ## calculate residuals
   resid <- drop( dataTable[ , 3 ] - fitVal )
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
   if( maxit > 0 ) {
      returnObj$olsResid <- residuals( ols )
   } else {
      returnObj$olsResid <- rep( NA, nob )
   }
   returnObj$olsSkewness <- skewness( returnObj$olsResid )
   returnObj$olsSkewnessOkay <- returnObj$olsSkewness * ( -1 )^ineffDecrease >= 0


   ## warnings regarding wrong skewness, smaller logLik value, and no convergence
   warnMaxit <- maxit <= returnObj$nIter && maxit > 0
   if( !returnObj$olsSkewnessOkay && returnObj$mleLogl < returnObj$olsLogl &&
         maxit > 0 ) {
      warning( "the residuals of the OLS estimates are ",
         ifelse( ineffDecrease, "right", "left" ), "-skewed",
         " and the likelihood value of the ML estimation is less",
         " than that obtained using OLS;",
         " this usually indicates that there is no inefficiency",
         " or that the model is misspecified" )
   } else if( !returnObj$olsSkewnessOkay && maxit > 0 ) {
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
   } else if( returnObj$mleLogl < returnObj$olsLogl && maxit > 0 ) {
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
         idx <- 1:( nb + 2 )
      } else {
         if( nb == 0 ) {
            idx <- NULL
         } else {
            idx <- 1:nb
         }
         idx <- c( idx, ( nParamTotal - 1 ):nParamTotal )
      }
      if( any( returnObj$gridParam[ (1:nParamTotal)[ -idx ] ] != 0 ) ) {
         warning( "internal error: some unused grid-search parameters are",
            " non-zero: ", paste( returnObj$gridParam, collapse = " " ),
            " please contact the maintainer of the 'frontier' package" )
      }
      returnObj$gridParam <- returnObj$gridParam[ idx ]
      names( returnObj )[ names( returnObj ) == "startLogl" ] <- "gridLogl"
   } else {
      returnObj$gridParam <- NULL
   }
   # assign row names and column names to residuals
   if( inherits( data, "pdata.frame" ) || inherits( data, "plm.dim" ) ) {
      rownames( returnObj$resid ) <- levels( index( data )[[ 1 ]] )[ firmId ]
      colnames( returnObj$resid ) <- levels( index( data )[[ 2 ]] )[ timeId ]
   } else {
      rownames( returnObj$resid ) <- names( validObs )[ validObs ]
      colnames( returnObj$resid ) <- "residuals"
   }

   # names of the parameters
   paramNames <- colnames( tmp$xMat )
   if( modelType == 2 ) {
      if( zIntercept ){
         paramNames <- c( paramNames, "Z_(Intercept)" )
      }
      if( nZvars > 0 ) {
         paramNames <- c( paramNames, paste( "Z", colnames( tmp$zMat ), sep = "_" ) )
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
   if( nb >= 1 ) {
      betaNames <- paramNames[ 1:nb ]
   } else {
      betaNames <- NULL
   }
   names( returnObj$olsParam ) <- c( betaNames, "sigmaSq" )
   names( returnObj$olsStdEr ) <- betaNames
   if( !is.null( returnObj$gridParam ) ) {
      names( returnObj$gridParam ) <- c( betaNames, "sigmaSq", "gamma" )
   }
   names( returnObj$mleParam ) <- paramNames
   rownames( returnObj$mleCov ) <- paramNames
   colnames( returnObj$mleCov ) <- paramNames
   if( !is.null( returnObj$startVal ) ) {
      names( returnObj$startVal ) <- paramNames
   }
   returnObj$call <- match.call()
   returnObj$validObs <- validObs

   if( maxit > 0 ) {
      if( ( returnObj$mleParam[ "gamma" ] < 0.01 || 
            returnObj$mleParam[ "gamma" ] > 0.99 ) ) {
         warning( "the parameter 'gamma' is close to the boundary",
                  " of the parameter space [0,1]:",
                  " this can cause convergence problems and",
                  " can negatively affect the validity and reliability",
                  " of statistical tests",
                  " and might be caused by model misspecification" )
      }
   
      if( !semidefiniteness( returnObj$mleCov ) ) {
         warning( "the covariance matrix of the maximum likelihood estimates",
            " is not positive semidefinite" )
      } else {
         testSingularCov <- try( solve( returnObj$mleCov ), silent = TRUE )
         if( inherits( testSingularCov, "try-error" ) ) {
            if( grepl( "singular", testSingularCov[1] ) ) {
               warning( "the covariance matrix of the maximum likelihood estimates",
                  " is singular" )
            } else {
               warning( "the covariance matrix of the maximum likelihood estimates",
                  " is not invertible" )
            }
         }
      }
   }
   
   
   class( returnObj ) <- "frontier"
   return( returnObj )
}
