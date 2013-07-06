library( frontier )
options( digits = 5 )

printAll <- function( x ) {
   for( n in names( x ) ) {
      cat( "$", n, "\n", sep = "" )
      if( n %in% c( "olsParam", "gridParam", "mleParam", "olsStdEr", 
            "mleCov", "resid", "olsResid" ) ) {
         print( round( x[[ n ]], 2 ) )
      } else {
         print( x[[ n ]] )
      }
      cat( "\n" )
   }
   cat( "class\n" )
   print( class( x ) )
}

printME <- function( x ) {
   me <- attr( x, "margEff" )
   attr( x, "margEff" ) <- NULL
   print( round( x, 2 ) )
   if( !is.null( me ) ) {
      cat( "margEff\n" )
      print( round( me, 2 ) )
   }
}

## example data included in FRONTIER 4.1 (cross-section data)
data( front41Data )
row.names( front41Data ) <- paste( "F", row.names( front41Data ), sep = "_" )
front41Data$logOutput  <- log( front41Data$output )
front41Data$logCapital <- log( front41Data$capital )
front41Data$logLabour  <- log( front41Data$labour )
front41Data$firmNo     <- c( 1:nrow( front41Data ) )

## cross-section data, error components frontier
sa1 <- sfa( logOutput ~ logCapital + logLabour, data = front41Data )
Sa1 <- sfa( log( output ) ~ log( capital ) + log( labour ), data = front41Data )
all.equal( Sa1[-39], sa1[-39], check.attributes = FALSE, tol = 1e-4 )
a1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ) )
all.equal( sa1[-39], a1[-39], tol = 1e-4 )
print( a1 )
coef( a1, which = "start" )
round( coef( a1, which = "ols" ), 2 )
round( coef( a1, which = "grid" ), 2 )
round( coef( a1 ), 2 )
round( coef( summary( a1 ), which = "ols" ), 2 )
round( coef( summary( a1 ) ), 2 )
round( vcov( a1 ), 2 )
print( logLik( a1, which = "ols" ), digits = 4 )
print( logLik( a1, which = "grid" ), digits = 4 )
print( logLik( a1 ), digits = 4 )
nobs( a1 )
print( summary( a1 ), digits = 1 )
print( summary( a1, effMinusU = FALSE ), digits = 1 )
lrtest( a1 )
printME( efficiencies( a1, margEff = TRUE ) )
printME( efficiencies( a1, asInData = TRUE ) )
printME( efficiencies( a1, minusU = FALSE ) )
printME( efficiencies( a1, asInData = TRUE, minusU = FALSE ) )
round( residuals( a1 ), 2 )
round( residuals( a1, asInData = TRUE ), 2 )
printAll( a1 )

## cross-section data, error components frontier, truncNorm
sa2 <- sfa( logOutput ~ logCapital + logLabour, data = front41Data,
   truncNorm = TRUE, printIter = 4 )
a2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE )
all.equal( sa2[-c(8,39)], a2[-c(8,39)], tol = 1e-4 )
print( a2, digits = 1 )
coef( a2, which = "start" )
round( coef( a2, which = "ols" ), 2 )
round( coef( a2, which = "grid" ), 2 )
round( coef( a2 ), 2 )
round( coef( summary( a2 ), which = "ols" ), 2 )
round( coef( summary( a2 ) ), 2 )
round( vcov( a2 ), 2 )
print( logLik( a2, which = "ols" ), digits = 4 )
print( logLik( a2 ), digits = 4 )
nobs( a2 )
print( summary( a2 ), digits = 1 )
lrtest( a2 )
round( efficiencies( a2 ), 2 )
round( efficiencies( a2, asInData = TRUE ), 2 )
round( residuals( a2 ), 2 )
round( residuals( a2, asInData = TRUE ), 2 )
printAll( a2 )

## cross-section data, error components frontier, truncNorm, starting values
sa5 <- sfa( logOutput ~ logCapital + logLabour, data = front41Data,
   truncNorm = TRUE, startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
all.equal( sa5[-39], a5[-39], tol = 1e-4 )
print( a5, digits = 1 )
coef( a5, which = "start" )
round( coef( a5, which = "ols" ), 2 )
coef( a5, which = "grid" )
round( coef( a5 ), 2 )
round( coef( summary( a5 ), which = "ols" ), 2 )
round( coef( summary( a5 ) ), 2 )
round( vcov( a5 ), 2 )
print( logLik( a5, which = "ols" ), digits = 4 )
print( logLik( a5 ), digits = 4 )
nobs( a5 )
print( summary( a5 ), digits = 1 )
lrtest( a5 )
round( efficiencies( a5 ), 2 )
round( efficiencies( a5, asInData = TRUE ), 2 )
printAll( a5 )

## cross-section data, efficiency effects frontier
saa1 <- sfa( logOutput ~ logCapital + logLabour | firmNo - 1,
   data = front41Data )
Saa1 <- sfa( log( output ) ~ log( capital ) + log( labour ) | firmNo - 1,
   data = front41Data, printIter = 3 )
all.equal( Saa1[-c(8,39)], saa1[-c(8,39)], check.attributes = FALSE, tol = 1e-4 )
aa1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo" )
all.equal( saa1[-39], aa1[-39], tol = 1e-4 )
print( aa1, digits = 1 )
coef( aa1, which = "start" )
round( coef( aa1, which = "ols" ), 2 )
round( coef( aa1, which = "grid" ), 2 )
round( coef( aa1 ), 2 )
round( coef( summary( aa1 ), which = "ols" ), 2 )
round( coef( summary( aa1 ) ), 2 )
round( vcov( aa1 ), 2 )
nobs( aa1 )
print( summary( aa1 ), digits = 1 )
print( summary( aa1, effMinusU = FALSE ), digits = 1 )
lrtest( aa1 )
printME( aa1eff <- efficiencies( aa1, margEff = TRUE ) )
printME( aa1effD <- efficiencies( aa1, asInData = TRUE, margEff = TRUE ) )
printME( aa1effF <- efficiencies( aa1, minusU = FALSE, margEff = TRUE ) )
printME( aa1effDF <- efficiencies( aa1, asInData = TRUE, minusU = FALSE, 
   margEff = TRUE ) )
aa1m <- aa1
aa1m$dataTable[ , "firmNo" ] <- aa1m$dataTable[ , "firmNo" ] + 1e-6
all.equal( attr( aa1eff, "margEff" )[ , 1, 1 ], 
   ( efficiencies( aa1m ) - aa1eff )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( aa1effD, "margEff" )[ , 1 ], 
   c( efficiencies( aa1m, asInData = TRUE ) - aa1effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( aa1effF, "margEff" )[ , 1, 1 ], 
   ( efficiencies( aa1m, minusU = FALSE ) - aa1effF )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( aa1effDF, "margEff" )[ , 1 ],
   c( efficiencies( aa1m, asInData = TRUE, minusU = FALSE ) - aa1effDF ) / 1e-6,
   tol = 1e-4 )
round( residuals( aa1 ), 2 )
round( residuals( aa1, asInData = TRUE ), 2 )
printAll( aa1 )

## cross-section data, efficiency effects frontier, zIntercept
saa2 <- sfa( logOutput ~ logCapital + logLabour | firmNo,
   data = front41Data )
aa2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE )
all.equal( saa2[-39], aa2[-39], tol = 1e-4 )
print( aa2, digits = 2 )
coef( aa2, which = "start" )
round( coef( aa2, which = "ols" ), 2 )
round( coef( aa2, which = "grid" ), 2 )
round( coef( aa2 ), 2 )
round( coef( summary( aa2 ), which = "ols" ), 2 )
round( coef( summary( aa2 ) ), 2 )
round( vcov( aa2 ), 2 )
nobs( aa2 )
print( summary( aa2 ), digits = 1 )
lrtest( aa2 )
round( efficiencies( aa2 ), 2 )
round( efficiencies( aa2, asInData = TRUE ), 2 )
round( residuals( aa2 ), 2 )
round( residuals( aa2, asInData = TRUE ), 2 )
printAll( aa2 )

## cross-section data, efficiency effects frontier, zIntercept, starting values
saa5 <- sfa( logOutput ~ logCapital + logLabour | firmNo,
   data = front41Data, startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
aa5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE,
   startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
all.equal( saa5[-39], aa5[-39], tol = 1e-4 )
print( aa5, digits = 2 )
coef( aa5, which = "start" )
round( coef( aa5, which = "ols" ), 2 )
coef( aa5, which = "grid" )
round( coef( aa5 ), 2 )
round( coef( summary( aa5 ), which = "ols" ), 2 )
round( coef( summary( aa5 ) ), 2 )
round( vcov( aa5 ), 2 )
nobs( aa5 )
print( summary( aa5 ), digits = 1 )
lrtest( aa5 )
round( efficiencies( aa5 ), 2 )
round( efficiencies( aa5, asInData = TRUE ), 2 )
printAll( aa5 )

## cross-section data, efficiency effects frontier, no Z vars
aa9 <- sfa( log( output ) ~ log( capital ) + log( labour ) | - 1,
   data = front41Data )
print( summary( aa9 ), digits = 1 )
nobs( aa9 )
lrtest( aa9 )


## cross-section data with NAs and infinit values
naData <- front41Data
naData$output[3] <- NA
naData$capital[5] <- 0
naData$labour[9] <- 0
naData$firmNo[14] <- NA

## cross-section data with NAs, error components frontier
San1 <- sfa( log( output ) ~ log( capital ) + log( labour ), data = naData )
print( summary( San1 ), digits = 1 )
nobs( San1 )

## cross-section data with NAs, efficiency effects frontier
Saan1 <- sfa( log( output ) ~ log( capital ) + log( labour ) | firmNo - 1,
   data = naData )
print( summary( Saan1 ), digits = 1 )
nobs( Saan1 )

## data set of rice producers in the Philippines
data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

## cross-section rice data, error components frontier
sbb1 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhil )
Sbb1 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = riceProdPhil )
all.equal( Sbb1[-39], sbb1[-39], check.attributes = FALSE, tol = 1e-4 )
bb1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
all.equal( sbb1[-39], bb1[-39], tol = 1e-4 )
print( bb1, digits = 1 )
coef( bb1, which = "start" )
round( coef( bb1, which = "ols" ), 2 )
round( coef( bb1, which = "grid" ), 2 )
round( coef( bb1 ), 2 )
round( coef( summary( bb1 ), which = "ols" ), 2 )
round( coef( summary( bb1 ) ), 2 )
round( vcov( bb1 ), 2 )
nobs( bb1 )
print( summary( bb1 ), digits = 1 )
lrtest( bb1 )
round( efficiencies( bb1 ), 2 )
round( efficiencies( bb1, asInData = TRUE ), 2 )
round( residuals( bb1 ), 2 )
round( residuals( bb1, asInData = TRUE ), 2 )
printAll( bb1 )

## cross-section rice data, error components frontier, truncNorm
sbb2 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhil,
   truncNorm = TRUE )
bb2 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
all.equal( sbb2[-39], bb2[-39], tol = 1e-4 )
print( bb2, digits = 1 )
coef( bb2, which = "start" )
round( coef( bb2, which = "ols" ), 2 )
round( coef( bb2, which = "grid" ), 2 )
round( coef( bb2 ), 2 )
round( coef( summary( bb2 ), which = "ols" ), 2 )
round( coef( summary( bb2 ) ), 2 )
round( vcov( bb2 ), 2 )
nobs( bb2 )
print( summary( bb2 ), digits = 1 )
print( summary( bb2, effMinusU = FALSE ), digits = 1 )
lrtest( bb2 )
round( efficiencies( bb2 ), 2 )
round( efficiencies( bb2, asInData = TRUE ), 2 )
round( efficiencies( bb2, minusU = FALSE ), 2 )
round( efficiencies( bb2, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( bb2 ), 2 )
round( residuals( bb2, asInData = TRUE ), 2 )
printAll( bb2 )

## cross-section rice data, efficiency effects frontier
sbb5 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhil )
Sbb5 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = riceProdPhil )
all.equal( Sbb5[-39], sbb5[-39], check.attributes = FALSE, tol = 1e-4 )
bb5 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
all.equal( sbb5[-39], bb5[-39], tol = 1e-4 )
print( bb5, digits = 1 )
coef( bb5, which = "start" )
round( coef( bb5, which = "ols" ), 2 )
round( coef( bb5, which = "grid" ), 2 )
round( coef( bb5 ), 2 )
round( coef( summary( bb5 ), which = "ols" ), 2 )
round( coef( summary( bb5 ) ), 2 )
round( vcov( bb5 ), 2 )
nobs( bb5 )
print( summary( bb5 ), digits = 1 )
lrtest( bb5 )
round( efficiencies( bb5 ), 2 )
round( efficiencies( bb5, asInData = TRUE ), 2 )
round( residuals( bb5 ), 2 )
round( residuals( bb5, asInData = TRUE ), 2 )
printAll( bb5 )

## cross-section rice data, efficiency effects frontier, zIntercept
sbb6 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhil )
bb6 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
all.equal( sbb6[-39], bb6[-39], tol = 1e-4 )
print( bb6, digits = 1 )
coef( bb6, which = "start" )
round( coef( bb6, which = "ols" ), 2 )
round( coef( bb6, which = "grid" ), 2 )
round( coef( bb6 ), 2 )
round( coef( summary( bb6 ), which = "ols" ), 2 )
round( coef( summary( bb6 ) ), 2 )
round( vcov( bb6 ), 2 )
nobs( bb6 )
print( summary( bb6 ), digits = 1 )
print( summary( bb6, effMinusU = FALSE ), digits = 1 )
lrtest( bb6 )
printME( bb6eff <- efficiencies( bb6, margEff = TRUE ) )
printME( bb6effD <- efficiencies( bb6, asInData = TRUE, margEff = TRUE ) )
printME( bb6effF <- efficiencies( bb6, minusU = FALSE, margEff = TRUE ) )
printME( bb6effDF <- efficiencies( bb6, asInData = TRUE, minusU = FALSE, 
   margEff = TRUE ) )
bb6m1 <- bb6
bb6m1$dataTable[ , "EDYRS" ] <- bb6m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( bb6eff, "margEff" )[ , 1, 1 ], 
   ( efficiencies( bb6m1 ) - bb6eff )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( bb6effD, "margEff" )[ , 1 ], 
   c( efficiencies( bb6m1, asInData = TRUE ) - bb6effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( bb6effF, "margEff" )[ , 1, 1 ], 
   ( efficiencies( bb6m1, minusU = FALSE ) - bb6effF )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( bb6effDF, "margEff" )[ , 1 ],
   c( efficiencies( bb6m1, asInData = TRUE, minusU = FALSE ) - bb6effDF ) / 1e-6,
   tol = 1e-4 )
bb6m2 <- bb6
bb6m2$dataTable[ , "BANRAT" ] <- bb6m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( bb6eff, "margEff" )[ , 1, 2 ], 
   ( efficiencies( bb6m2 ) - bb6eff )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( bb6effD, "margEff" )[ , 2 ], 
   c( efficiencies( bb6m2, asInData = TRUE ) - bb6effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( bb6effF, "margEff" )[ , 1, 2 ], 
   ( efficiencies( bb6m2, minusU = FALSE ) - bb6effF )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( bb6effDF, "margEff" )[ , 2 ],
   c( efficiencies( bb6m2, asInData = TRUE, minusU = FALSE ) - bb6effDF ) / 1e-6,
   tol = 1e-4 )
round( residuals( bb6 ), 2 )
round( residuals( bb6, asInData = TRUE ), 2 )
printAll( bb6 )

## cross-section rice data, error components frontier, truncNorm, starting values
sbb7 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhil,
   truncNorm = TRUE, startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
bb7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
all.equal( sbb7[-39], bb7[-39], tol = 1e-4 )
print( bb7, digits = 1 )
coef( bb7, which = "start" )
round( coef( bb7, which = "ols" ), 2 )
coef( bb7, which = "grid" )
round( coef( bb7 ), 2 )
round( coef( summary( bb7 ), which = "ols" ), 2 )
round( coef( summary( bb7 ) ), 2 )
round( vcov( bb7 ), 2 )
nobs( bb7 )
print( summary( bb7 ), digits = 1 )
lrtest( bb7 )
round( efficiencies( bb7 ), 2 )
round( efficiencies( bb7, asInData = TRUE ), 2 )
printAll( bb7 )

## cross-section rice data, efficiency effects frontier, zIntercept, starting values
sbb8 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhil,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
bb8 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
all.equal( sbb8[-39], bb8[-39], tol = 1e-4 )
print( bb8, digits = 1 )
coef( bb8, which = "start" )
round( coef( bb8, which = "ols" ), 2 )
coef( bb8, which = "grid" )
round( coef( bb8 ), 2 )
round( coef( summary( bb8 ), which = "ols" ), 2 )
round( coef( summary( bb8 ) ), 2 )
round( vcov( bb8 ), 2 )
nobs( bb8 )
print( summary( bb8 ), digits = 1 )
lrtest( bb8 )
round( efficiencies( bb8 ), 2 )
round( efficiencies( bb8, asInData = TRUE ), 2 )
printAll( bb8 )

## cross-section rice data, efficiency effects frontier, no Z vars
bb9 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) | - 1,
   data = riceProdPhil )
print( summary( bb9 ), digits = 1 )
nobs( bb9 )
lrtest( bb9 )


## Cost Frontier (with land as quasi-fixed input)
riceProdPhil$cost <- riceProdPhil$LABOR * riceProdPhil$LABORP +
   riceProdPhil$NPK * riceProdPhil$NPKP
riceProdPhil$lCost   <- log( riceProdPhil$cost )
riceProdPhil$lLABORP <- log( riceProdPhil$LABORP )
riceProdPhil$lNPKP   <- log( riceProdPhil$NPKP )

## cross-section rice data, error components cost frontier
sdd1 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhil,
   ineffDecrease = FALSE )
Sdd1 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ), data = riceProdPhil, ineffDecrease = FALSE )
all.equal( Sdd1[-39], sdd1[-39], check.attributes = FALSE, tol = 1e-4 )
dd1 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE )
all.equal( sdd1[-39], dd1[-39], tol = 1e-4 )
print( dd1, digits = 1 )
coef( dd1, which = "start" )
round( coef( dd1, which = "ols" ), 2 )
round( coef( dd1, which = "grid" ), 2 )
round( coef( dd1 ), 2 )
round( coef( summary( dd1 ), which = "ols" ), 2 )
round( coef( summary( dd1 ) ), 2 )
round( vcov( dd1 ), 2 )
nobs( dd1 )
print( summary( dd1 ), digits = 1 )
print( summary( dd1, effMinusU = FALSE ), digits = 1 )
lrtest( dd1 )
round( efficiencies( dd1 ), 2 )
round( efficiencies( dd1, asInData = TRUE ), 2 )
round( efficiencies( dd1, minusU = FALSE ), 2 )
round( efficiencies( dd1, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( dd1 ), 2 )
round( residuals( dd1, asInData = TRUE ), 2 )
printAll( dd1 )

## cross-section rice data, error components cost frontier, truncNorm
sdd2 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhil,
   ineffDecrease = FALSE, truncNorm = TRUE )
dd2 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE, truncNorm = TRUE )
all.equal( sdd2[-39], dd2[-39], tol = 1e-4 )
print( dd2, digits = 1 )
coef( dd2, which = "start" )
round( coef( dd2, which = "ols" ), 2 )
round( coef( dd2, which = "grid" ), 2 )
round( coef( dd2 ), 2 )
round( coef( summary( dd2 ), which = "ols" ), 2 )
round( coef( summary( dd2 ) ), 2 )
round( vcov( dd2 ), 2 )
nobs( dd2 )
print( summary( dd2, effMinusU = FALSE ), digits = 1 )
lrtest( dd2 )
round( efficiencies( dd2, minusU = FALSE ), 2 )
round( efficiencies( dd2, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( dd2 ), 2 )
round( residuals( dd2, asInData = TRUE ), 2 )
printAll( dd2 )

## cross-section rice data, efficiency effects cost frontier
sdd5 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhil, ineffDecrease = FALSE )
Sdd5 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | EDYRS + BANRAT - 1, data = riceProdPhil, ineffDecrease = FALSE )
all.equal( Sdd5[-39], sdd5[-39], check.attributes = FALSE, tol = 1e-4 )
dd5 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE )
all.equal( sdd5[-39], dd5[-39], tol = 1e-4 )
print( dd5, digits = 1 )
coef( dd5, which = "start" )
round( coef( dd5, which = "ols" ), 2 )
round( coef( dd5, which = "grid" ), 2 )
round( coef( dd5 ), 2 )
round( coef( summary( dd5 ), which = "ols" ), 2 )
round( coef( summary( dd5 ) ), 2 )
round( vcov( dd5 ), 2 )
nobs( dd5 )
print( summary( dd5 ), digits = 1 )
print( summary( dd5, effMinusU = FALSE ), digits = 1 )
lrtest( dd5 )
printME( dd5eff <- efficiencies( dd5, margEff = TRUE ) )
printME( dd5effD <- efficiencies( dd5, asInData = TRUE, margEff = TRUE ) )
printME( dd5effF <- efficiencies( dd5, minusU = FALSE, margEff = TRUE ) )
printME( dd5effDF <- efficiencies( dd5, asInData = TRUE, minusU = FALSE, 
   margEff = TRUE ) )
dd5m1 <- dd5
dd5m1$dataTable[ , "EDYRS" ] <- dd5m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( dd5eff, "margEff" )[ , 1, 1 ], 
   ( efficiencies( dd5m1 ) - dd5eff )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( dd5effD, "margEff" )[ , 1 ], 
   c( efficiencies( dd5m1, asInData = TRUE ) - dd5effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( dd5effF, "margEff" )[ , 1, 1 ], 
   ( efficiencies( dd5m1, minusU = FALSE ) - dd5effF )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( dd5effDF, "margEff" )[ , 1 ],
   c( efficiencies( dd5m1, asInData = TRUE, minusU = FALSE ) - dd5effDF ) / 1e-6,
   tol = 1e-4 )
dd5m2 <- dd5
dd5m2$dataTable[ , "BANRAT" ] <- dd5m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( dd5eff, "margEff" )[ , 1, 2 ], 
   ( efficiencies( dd5m2 ) - dd5eff )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( dd5effD, "margEff" )[ , 2 ], 
   c( efficiencies( dd5m2, asInData = TRUE ) - dd5effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( dd5effF, "margEff" )[ , 1, 2 ], 
   ( efficiencies( dd5m2, minusU = FALSE ) - dd5effF )[ , 1 ] / 1e-6, tol = 1e-4 )
all.equal( attr( dd5effDF, "margEff" )[ , 2 ],
   c( efficiencies( dd5m2, asInData = TRUE, minusU = FALSE ) - dd5effDF ) / 1e-6,
   tol = 1e-4 )
round( residuals( dd5 ), 2 )
round( residuals( dd5, asInData = TRUE ), 2 )
printAll( dd5 )

## cross-section rice data, efficiency effects cost frontier, zIntercept
sdd6 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhil, ineffDecrease = FALSE )
dd6 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE, zIntercept = TRUE )
all.equal( sdd6[-39], dd6[-39], tol = 1e-4 )
print( dd6, digits = 1 )
coef( dd6, which = "start" )
round( coef( dd6, which = "ols" ), 2 )
round( coef( dd6, which = "grid" ), 2 )
round( coef( dd6 ), 2 )
round( coef( summary( dd6 ), which = "ols" ), 2 )
round( coef( summary( dd6 ) ), 2 )
round( vcov( dd6 ), 2 )
nobs( dd6 )
print( summary( dd6, effMinusU = FALSE ), digits = 1 )
lrtest( dd6 )
round( efficiencies( dd6, minusU = FALSE ), 2 )
round( efficiencies( dd6, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( dd6 ), 2 )
round( residuals( dd6, asInData = TRUE ), 2 )
printAll( dd6 )

## cross-section rice data, efficiency effects cost frontier: no Z vars
dd9 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | - 1, data = riceProdPhil, ineffDecrease = FALSE )
print( summary( dd9, effMinusU = FALSE ), digits = 1 )
nobs( dd9 )
lrtest( dd9 )


## error components frontier
## with "true" fixed individual effects and observation-specific efficiencies
riceTrue <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + 
   factor( FMERCODE ),  data = riceProdPhil )
summary( riceTrue )
lrtest( riceTrue )
lrtest( Sbb1, riceTrue )


## panel data
riceProdPhil$farm <- paste( "F_", ifelse( riceProdPhil$FMERCODE > 9, "", "0" ),
   riceProdPhil$FMERCODE, sep = "" )
riceProdPhil$year <- riceProdPhil$YEARDUM + 1998
riceProdPhilPanel <- plm.data( riceProdPhil, c( "farm", "year" ) )

## panel data, error components frontier
sb1 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   printIter = 2 )
Sb1 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = riceProdPhilPanel )
all.equal( Sb1[-c(8,39)], sb1[-c(8,39)], check.attributes = FALSE, tol = 1e-4 )
b1 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
all.equal( sb1[-c(8,39)], b1[-c(8,39)], tol = 1e-4 )
print( b1, digits = 1 )
coef( b1, which = "start" )
round( coef( b1, which = "ols" ), 2 )
round( coef( b1, which = "grid" ), 2 )
round( coef( b1 ), 2 )
round( coef( summary( b1 ), which = "ols" ), 2 )
round( coef( summary( b1 ) ), 2 )
round( vcov( b1 ), 2 )
print( logLik( b1, which = "ols" ), digits = 4 )
print( logLik( b1 ), digits = 4 )
nobs( b1 )
print( summary( b1 ), digits = 1 )
print( summary( b1, effMinusU = FALSE ), digits = 1 )
lrtest( b1 )
round( efficiencies( b1 ), 2 )
round( efficiencies( b1, asInData = TRUE ), 2 )
round( efficiencies( b1, minusU = FALSE ), 2 )
round( efficiencies( b1, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( b1 ), 2 )
round( residuals( b1, asInData = TRUE ), 2 )
printAll( b1 )

## panel data, error components frontier, truncNorm
sb2 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE )
b2 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
all.equal( sb2[-39], b2[-39], tol = 1e-4 )
print( b2, digits = 1 )
coef( b2, which = "start" )
round( coef( b2, which = "ols" ), 2 )
round( coef( b2, which = "grid" ), 2 )
round( coef( b2 ), 2 )
round( coef( summary( b2 ), which = "ols" ), 2 )
round( coef( summary( b2 ) ), 2 )
round( vcov( b2 ), 2 )
print( logLik( b2, which = "ols" ), digits = 4 )
print( logLik( b2 ), digits = 4 )
nobs( b2 )
print( summary( b2 ), digits = 1 )
lrtest( b2 )
round( efficiencies( b2 ), 2 )
round( efficiencies( b2, asInData = TRUE ), 2 )
round( residuals( b2 ), 2 )
round( residuals( b2, asInData = TRUE ), 2 )
printAll( b2 )

## panel data, error components frontier, timeEffect
sb3 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   timeEffect = TRUE )
b3 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   timeEffect = TRUE )
all.equal( sb3[-39], b3[-39], tol = 1e-4 )
print( b3, digits = 1 )
coef( b3, which = "start" )
round( coef( b3, which = "ols" ), 2 )
round( coef( b3, which = "grid" ), 2 )
round( coef( b3 ), 2 )
round( coef( summary( b3 ), which = "ols" ), 2 )
round( coef( summary( b3 ) ), 2 )
round( vcov( b3 ), 2 )
print( logLik( b3, which = "ols" ), digits = 4 )
print( logLik( b3 ), digits = 4 )
nobs( b3 )
print( summary( b3 ), digits = 1 )
lrtest( b3 )
round( efficiencies( b3 ), 2 )
round( efficiencies( b3, asInData = TRUE ), 2 )
round( residuals( b3 ), 2 )
round( residuals( b3, asInData = TRUE ), 2 )
printAll( b3 )

## panel data, error components frontier, truncNorm, timeEffect
sb4 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE, timeEffect = TRUE )
b4 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE )
all.equal( sb4[-39], b4[-39], tol = 1e-4 )
print( b4, digits = 1 )
coef( b4, which = "start" )
round( coef( b4, which = "ols" ), 2 )
round( coef( b4, which = "grid" ), 2 )
round( coef( b4 ), 2 )
round( coef( summary( b4 ), which = "ols" ), 2 )
round( coef( summary( b4 ) ), 2 )
round( vcov( b4 ), 2 )
print( logLik( b4, which = "ols" ), digits = 4 )
print( logLik( b4 ), digits = 4 )
nobs( b4 )
print( summary( b4 ), digits = 1 )
lrtest( b4 )
round( efficiencies( b4 ), 2 )
round( efficiencies( b4, asInData = TRUE ), 2 )
round( residuals( b4 ), 2 )
round( residuals( b4, asInData = TRUE ), 2 )
printAll( b4 )

## error components frontier
## with "true" fixed individual effects and time-invariant efficiencies
ricePanelTrue <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + 
      factor( FMERCODE ),  data = riceProdPhilPanel )
summary( ricePanelTrue )
lrtest( ricePanelTrue )
lrtest( Sb1, ricePanelTrue )

## error components frontier
## with "true" fixed individual effects and time-variant efficiencies
ricePanelTimeTrue <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + 
      factor( FMERCODE ), data = riceProdPhilPanel, timeEffect = TRUE )
summary( ricePanelTimeTrue )
lrtest( ricePanelTimeTrue )
lrtest( sb3, ricePanelTimeTrue )
lrtest( ricePanelTrue, ricePanelTimeTrue )


## panel data, efficiency effects frontier
sb5 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhilPanel )
Sb5 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = riceProdPhilPanel, printIter = 5 )
all.equal( Sb5[-c(8,39)], sb5[-c(8,39)], check.attributes = FALSE, tol = 1e-4 )
b5 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
all.equal( sb5[-39], b5[-39], tol = 1e-4 )
print( b5, digits = 1 )
coef( b5, which = "start" )
round( coef( b5, which = "ols" ), 2 )
round( coef( b5, which = "grid" ), 2 )
round( coef( b5 ), 2 )
round( coef( summary( b5 ), which = "ols" ), 2 )
round( coef( summary( b5 ) ), 2 )
round( vcov( b5 ), 2 )
print( logLik( b5, which = "ols" ), digits = 4 )
print( logLik( b5 ), digits = 4 )
nobs( b5 )
print( summary( b5 ), digits = 1 )
print( summary( b5, effMinusU = FALSE ), digits = 1 )
lrtest( b5 )
printME( b5eff <- efficiencies( b5, margEff = TRUE ) )
printME( b5effD <- efficiencies( b5, asInData = TRUE , margEff = TRUE) )
printME( b5effF <- efficiencies( b5, minusU = FALSE, margEff = TRUE ) )
printME( b5effDF <- efficiencies( b5, asInData = TRUE, minusU = FALSE, 
   margEff = TRUE ) )
b5m1 <- b5
b5m1$dataTable[ , "EDYRS" ] <- b5m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( b5eff, "margEff" )[ , , 1 ], 
   ( efficiencies( b5m1 ) - b5eff )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( b5effD, "margEff" )[ , 1 ], 
   c( efficiencies( b5m1, asInData = TRUE ) - b5effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( b5effF, "margEff" )[ , , 1 ], 
   ( efficiencies( b5m1, minusU = FALSE ) - b5effF )[ ,  ] / 1e-6, tol = 1e-4 )
all.equal( attr( b5effDF, "margEff" )[ , 1 ],
   c( efficiencies( b5m1, asInData = TRUE, minusU = FALSE ) - b5effDF ) / 1e-6,
   tol = 1e-4 )
b5m2 <- b5
b5m2$dataTable[ , "BANRAT" ] <- b5m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( b5eff, "margEff" )[ , , 2 ], 
   ( efficiencies( b5m2 ) - b5eff )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( b5effD, "margEff" )[ , 2 ], 
   c( efficiencies( b5m2, asInData = TRUE ) - b5effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( b5effF, "margEff" )[ , , 2 ], 
   ( efficiencies( b5m2, minusU = FALSE ) - b5effF )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( b5effDF, "margEff" )[ , 2 ],
   c( efficiencies( b5m2, asInData = TRUE, minusU = FALSE ) - b5effDF ) / 1e-6,
   tol = 1e-4 )
round( residuals( b5 ), 2 )
round( residuals( b5, asInData = TRUE ), 2 )
printAll( b5 )

## panel data, efficiency effects frontier, zIntercept
sb6 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanel )
b6 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
all.equal( sb6[-39], b6[-39], tol = 1e-4 )
print( b6, digits = 1 )
coef( b6, which = "start" )
round( coef( b6, which = "ols" ), 2 )
round( coef( b6, which = "grid" ), 2 )
round( coef( b6 ), 2 )
round( coef( summary( b6 ), which = "ols" ), 2 )
round( coef( summary( b6 ) ), 2 )
round( vcov( b6 ), 2 )
print( logLik( b6, which = "ols" ), digits = 4 )
print( logLik( b6 ), digits = 4 )
nobs( b6 )
print( summary( b6 ), digits = 1 )
lrtest( b6 )
round( efficiencies( b6 ), 2 )
round( efficiencies( b6, asInData = TRUE ), 2 )
round( residuals( b6 ), 2 )
round( residuals( b6, asInData = TRUE ), 2 )
printAll( b6 )

## panel data, error components frontier, truncNorm, timeEffect, starting values
sb7 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE, timeEffect = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
b7 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
all.equal( sb7[-39], b7[-39], tol = 1e-4 )
print( b7, digits = 1 )
coef( b7, which = "start" )
round( coef( b7, which = "ols" ), 2 )
coef( b7, which = "grid" )
round( coef( b7 ), 2 )
round( coef( summary( b7 ), which = "ols" ), 2 )
round( coef( summary( b7 ) ), 2 )
round( vcov( b7 ), 2 )
print( logLik( b7, which = "ols" ), digits = 4 )
print( logLik( b7 ), digits = 4 )
nobs( b7 )
print( summary( b7 ), digits = 1 )
lrtest( b7 )
round( efficiencies( b7 ), 2 )
round( efficiencies( b7, asInData = TRUE ), 2 )
printAll( b7 )

## panel data, efficiency effects frontier, zIntercept, starting values
sb8 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanel,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.3, -0.01, -0.4, 0.2, 0.8 ) )
b8 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.3, -0.01, -0.4, 0.2, 0.8 ) )
all.equal( sb8[-39], b8[-39], tol = 1e-4 )
print( b8, digits = 1 )
coef( b8, which = "start" )
round( coef( b8, which = "ols" ), 2 )
coef( b8, which = "grid" )
round( coef( b8 ), 2 )
round( coef( summary( b8 ), which = "ols" ), 2 )
round( coef( summary( b8 ) ), 2 )
round( vcov( b8 ), 2 )
print( logLik( b8, which = "ols" ), digits = 4 )
print( logLik( b8 ), digits = 4 )
nobs( b8 )
print( summary( b8 ), digits = 1 )
lrtest( b8 )
round( efficiencies( b8 ), 2 )
round( efficiencies( b8, asInData = TRUE ), 2 )
printAll( b8 )

## panel data, efficiency effects frontier: no Z vars
b9 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) | - 1,
   data = riceProdPhilPanel )
print( summary( b9 ), digits = 1 )
nobs( b9 )
lrtest( b9 )


## Cost Frontier (with land as quasi-fixed input)
## panel rice data, error components cost frontier
sd1 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE )
Sd1 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ), data = riceProdPhilPanel, ineffDecrease = FALSE )
all.equal( Sd1[-39], sd1[-39], check.attributes = FALSE, tol = 1e-4 )
d1 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE )
all.equal( sd1[-39], d1[-39], tol = 1e-4 )
print( d1, digits = 1 )
coef( d1, which = "start" )
round( coef( d1, which = "ols" ), 2 )
round( coef( d1, which = "grid" ), 2 )
round( coef( d1 ), 2 )
round( coef( summary( d1 ), which = "ols" ), 2 )
round( coef( summary( d1 ) ), 2 )
round( vcov( d1 ), 2 )
nobs( d1 )
print( summary( d1 ), digits = 1 )
print( summary( d1, effMinusU = FALSE ), digits = 1 )
lrtest( d1 )
round( efficiencies( d1 ), 2 )
round( efficiencies( d1, asInData = TRUE ), 2 )
round( efficiencies( d1, minusU = FALSE ), 2 )
round( efficiencies( d1, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d1 ), 2 )
round( residuals( d1, asInData = TRUE ), 2 )
printAll( d1 )

## panel rice data, error components cost frontier, truncNorm
sd2 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, truncNorm = TRUE )
d2 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, truncNorm = TRUE )
all.equal( sd2[-39], d2[-39], tol = 1e-4 )
print( d2, digits = 1 )
coef( d2, which = "start" )
round( coef( d2, which = "ols" ), 2 )
round( coef( d2, which = "grid" ), 2 )
round( coef( d2 ), 2 )
round( coef( summary( d2 ), which = "ols" ), 2 )
round( coef( summary( d2 ) ), 2 )
round( vcov( d2 ), 2 )
nobs( d2 )
print( summary( d2, effMinusU = FALSE ), digits = 1 )
lrtest( d2 )
round( efficiencies( d2, minusU = FALSE ), 2 )
round( efficiencies( d2, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d2 ), 2 )
round( residuals( d2, asInData = TRUE ), 2 )
printAll( d2 )

## panel rice data, error components cost frontier, timeEffect
sd3 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, timeEffect = TRUE )
d3 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, timeEffect = TRUE )
all.equal( sd3[-39], d3[-39], tol = 1e-4 )
print( d3, digits = 1 )
coef( d3, which = "start" )
round( coef( d3, which = "ols" ), 2 )
round( coef( d3, which = "grid" ), 2 )
round( coef( d3 ), 2 )
round( coef( summary( d3 ), which = "ols" ), 2 )
round( coef( summary( d3 ) ), 2 )
round( vcov( d3 ), 2 )
nobs( d3 )
print( summary( d3, effMinusU = FALSE ), digits = 1 )
lrtest( d3 )
round( efficiencies( d3, minusU = FALSE ), 2 )
round( efficiencies( d3, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d3 ), 2 )
round( residuals( d3, asInData = TRUE ), 2 )
printAll( d3 )

## panel rice data, error components cost frontier, truncNorm, timeEffect
sd4 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, truncNorm = TRUE, timeEffect = TRUE )
d4 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, truncNorm = TRUE,
   timeEffect = TRUE )
all.equal( sd4[-39], d4[-39], tol = 1e-4 )
print( d4, digits = 1 )
coef( d4, which = "start" )
round( coef( d4, which = "ols" ), 2 )
round( coef( d4, which = "grid" ), 2 )
round( coef( d4 ), 2 )
round( coef( summary( d4 ), which = "ols" ), 2 )
round( coef( summary( d4 ) ), 2 )
round( vcov( d4 ), 2 )
nobs( d4 )
print( summary( d4, effMinusU = FALSE ), digits = 1 )
lrtest( d4 )
round( efficiencies( d4, minusU = FALSE ), 2 )
round( efficiencies( d4, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d4 ), 2 )
round( residuals( d4, asInData = TRUE ), 2 )
printAll( d4 )

## panel rice data, efficiency effects cost frontier
sd5 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhilPanel, ineffDecrease = FALSE )
Sd5 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | EDYRS + BANRAT - 1, data = riceProdPhilPanel,
   ineffDecrease = FALSE )
all.equal( Sd5[-39], sd5[-39], check.attributes = FALSE, tol = 1e-4 )
d5 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhilPanel,
   ineffDecrease = FALSE )
all.equal( sd5[-39], d5[-39], tol = 1e-4 )
print( d5, digits = 1 )
coef( d5, which = "start" )
round( coef( d5, which = "ols" ), 2 )
round( coef( d5, which = "grid" ), 2 )
round( coef( d5 ), 2 )
round( coef( summary( d5 ), which = "ols" ), 2 )
round( coef( summary( d5 ) ), 2 )
round( vcov( d5 ), 2 )
nobs( d5 )
print( summary( d5 ), digits = 1 )
print( summary( d5, effMinusU = FALSE ), digits = 1 )
lrtest( d5 )
printME( d5eff <- efficiencies( d5, margEff = TRUE ) )
printME( d5effD <- efficiencies( d5, asInData = TRUE, margEff = TRUE ) )
printME( d5effF <- efficiencies( d5, minusU = FALSE, margEff = TRUE ) )
printME( d5effDF <- efficiencies( d5, asInData = TRUE, minusU = FALSE, 
   margEff = TRUE ) )
d5m1 <- d5
d5m1$dataTable[ , "EDYRS" ] <- d5m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( d5eff, "margEff" )[ , , 1 ], 
   ( efficiencies( d5m1 ) - d5eff )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( d5effD, "margEff" )[ , 1 ], 
   c( efficiencies( d5m1, asInData = TRUE ) - d5effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( d5effF, "margEff" )[ , , 1 ], 
   ( efficiencies( d5m1, minusU = FALSE ) - d5effF )[ ,  ] / 1e-6, tol = 1e-4 )
all.equal( attr( d5effDF, "margEff" )[ , 1 ],
   c( efficiencies( d5m1, asInData = TRUE, minusU = FALSE ) - d5effDF ) / 1e-6,
   tol = 1e-4 )
d5m2 <- d5
d5m2$dataTable[ , "BANRAT" ] <- d5m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( d5eff, "margEff" )[ , , 2 ], 
   ( efficiencies( d5m2 ) - d5eff )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( d5effD, "margEff" )[ , 2 ], 
   c( efficiencies( d5m2, asInData = TRUE ) - d5effD ) / 1e-6, tol = 1e-4 )
all.equal( attr( d5effF, "margEff" )[ , , 2 ], 
   ( efficiencies( d5m2, minusU = FALSE ) - d5effF )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( d5effDF, "margEff" )[ , 2 ],
   c( efficiencies( d5m2, asInData = TRUE, minusU = FALSE ) - d5effDF ) / 1e-6,
   tol = 1e-4 )
round( residuals( d5 ), 2 )
round( residuals( d5, asInData = TRUE ), 2 )
printAll( d5 )

## panel rice data, efficiency effects cost frontier, zIntercept
sd6 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhilPanel, ineffDecrease = FALSE )
d6 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhilPanel,
   ineffDecrease = FALSE, zIntercept = TRUE )
all.equal( sd6[-39], d6[-39], tol = 1e-4 )
print( d6, digits = 1 )
coef( d6, which = "start" )
round( coef( d6, which = "ols" ), 2 )
round( coef( d6, which = "grid" ), 2 )
round( coef( d6 ), 2 )
round( coef( summary( d6 ), which = "ols" ), 2 )
round( coef( summary( d6 ) ), 2 )
round( vcov( d6 ), 2 )
nobs( d6 )
print( summary( d6, effMinusU = FALSE ), digits = 1 )
lrtest( d6 )
round( efficiencies( d6, minusU = FALSE ), 2 )
round( efficiencies( d6, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d6 ), 2 )
round( residuals( d6, asInData = TRUE ), 2 )
printAll( d6 )

## panel rice data, efficiency effects cost frontier: no Z vars
d9 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | - 1, data = riceProdPhilPanel, ineffDecrease = FALSE )
print( summary( d9, effMinusU = FALSE ), digits = 1 )
nobs( d9 )
lrtest( d9 )


## unbalanced panel data
set.seed( 321 )
riceProdPhilPanelUnb <- riceProdPhilPanel
riceProdPhilPanelUnb[ 3, c( "PROD", "lPROD" ) ] <- NA
riceProdPhilPanelUnb[ 5, c( "AREA", "lAREA" ) ] <- NA
riceProdPhilPanelUnb[ 111, c( "LABOR", "lLABOR", "LABORP", "lLABORP" ) ] <- NA
riceProdPhilPanelUnb[ 222, c( "NPK", "lNPK", "NPKP", "lNPKP" ) ] <- NA

## unbalanced panel data, error components frontier
b1u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb )
print( b1u, digits = 1 )
print( summary( b1u ), digits = 1 )
nobs( b1u )
lrtest( b1u )
round( efficiencies( b1u ), 2 )
round( efficiencies( b1u, asInData = TRUE ), 2 )
round( residuals( b1u ), 2 )
round( residuals( b1u, asInData = TRUE ), 2 )
printAll( b1u )

## unbalanced panel data, error components frontier, truncNorm
b2u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   truncNorm = TRUE )
print( b2u, digits = 1 )
print( summary( b2u ), digits = 1 )
nobs( b2u )
lrtest( b2u )
round( efficiencies( b2u ), 2 )
round( efficiencies( b2u, asInData = TRUE ), 2 )
round( residuals( b2u ), 2 )
round( residuals( b2u, asInData = TRUE ), 2 )
printAll( b2u )

## unbalanced panel data, error components frontier, timeEffect
b3u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   timeEffect = TRUE )
print( b3u, digits = 1 )
print( summary( b3u ), digits = 1 )
nobs( b3u )
lrtest( b3u )
round( efficiencies( b3u ), 2 )
round( efficiencies( b3u, asInData = TRUE ), 2 )
round( residuals( b3u ), 2 )
round( residuals( b3u, asInData = TRUE ), 2 )
printAll( b3u )

## unbalanced panel data, error components frontier, truncNorm, timeEffect
b4u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   truncNorm = TRUE, timeEffect = TRUE )
print( b4u, digits = 1 )
print( summary( b4u ), digits = 1 )
print( summary( b4u, effMinusU = FALSE ), digits = 1 )
nobs( b4u )
lrtest( b4u )
round( efficiencies( b4u ), 2 )
round( efficiencies( b4u, asInData = TRUE ), 2 )
round( efficiencies( b4u, minusU = FALSE ), 2 )
round( efficiencies( b4u, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( b4u ), 2 )
round( residuals( b4u, asInData = TRUE ), 2 )
printAll( b4u )

## unbalanced panel data, efficiency effects frontier
b5u <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhilPanelUnb )
print( b5u, digits = 1 )
print( summary( b5u ), digits = 1 )
nobs( b5u )
lrtest( b5u )
round( efficiencies( b5u ), 2 )
round( efficiencies( b5u, asInData = TRUE ), 2 )
round( residuals( b5u ), 2 )
round( residuals( b5u, asInData = TRUE ), 2 )
printAll( b5u )

## unbalanced panel data, efficiency effects frontier, zIntercept
b6u <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanelUnb )
print( b6u, digits = 1 )
print( summary( b6u ), digits = 1 )
print( summary( b6u, effMinusU = FALSE ), digits = 1 )
nobs( b6u )
lrtest( b6u )
printME( b6ueff <- efficiencies( b6u, margEff = TRUE ) )
printME( b6ueffD <- efficiencies( b6u, asInData = TRUE, margEff = TRUE ) )
printME( b6ueffF <- efficiencies( b6u, minusU = FALSE, margEff = TRUE ) )
printME( b6ueffDF <- efficiencies( b6u, asInData = TRUE, minusU = FALSE, 
   margEff = TRUE ) )
b6um1 <- b6u
b6um1$dataTable[ , "EDYRS" ] <- b6um1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( b6ueff, "margEff" )[ , , 1 ], 
   ( efficiencies( b6um1 ) - b6ueff )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( b6ueffD, "margEff" )[ , 1 ], 
   c( efficiencies( b6um1, asInData = TRUE ) - b6ueffD ) / 1e-6, tol = 1e-4 )
all.equal( attr( b6ueffF, "margEff" )[ , , 1 ], 
   ( efficiencies( b6um1, minusU = FALSE ) - b6ueffF )[ ,  ] / 1e-6, tol = 1e-4 )
all.equal( attr( b6ueffDF, "margEff" )[ , 1 ],
   c( efficiencies( b6um1, asInData = TRUE, minusU = FALSE ) - b6ueffDF ) / 1e-6,
   tol = 1e-4 )
b6um2 <- b6u
b6um2$dataTable[ , "BANRAT" ] <- b6um2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( b6ueff, "margEff" )[ , , 2 ], 
   ( efficiencies( b6um2 ) - b6ueff )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( b6ueffD, "margEff" )[ , 2 ], 
   c( efficiencies( b6um2, asInData = TRUE ) - b6ueffD ) / 1e-6, tol = 1e-4 )
all.equal( attr( b6ueffF, "margEff" )[ , , 2 ], 
   ( efficiencies( b6um2, minusU = FALSE ) - b6ueffF )[ , ] / 1e-6, tol = 1e-4 )
all.equal( attr( b6ueffDF, "margEff" )[ , 2 ],
   c( efficiencies( b6um2, asInData = TRUE, minusU = FALSE ) - b6ueffDF ) / 1e-6,
   tol = 1e-4 )
round( residuals( b6u ), 2 )
round( residuals( b6u, asInData = TRUE ), 2 )
printAll( b6u )

## unbalanced panel rice data, error components cost frontier
d1u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE )
print( d1u, digits = 1 )
print( summary( d1u ), digits = 1 )
print( summary( d1u, effMinusU = FALSE ), digits = 1 )
nobs( d1u )
lrtest( d1u )
round( efficiencies( d1u ), 2 )
round( efficiencies( d1u, asInData = TRUE ), 2 )
round( efficiencies( d1u, minusU = FALSE ), 2 )
round( efficiencies( d1u, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d1u ), 2 )
round( residuals( d1u, asInData = TRUE ), 2 )
printAll( d1u )

## unbalanced panel rice data, error components cost frontier, truncNorm
d2u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, truncNorm = TRUE )
print( d2u, digits = 1 )
print( summary( d2u, effMinusU = FALSE ), digits = 1 )
nobs( d2u )
lrtest( d2u )
round( efficiencies( d2u, minusU = FALSE ), 2 )
round( efficiencies( d2u, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d2u ), 2 )
round( residuals( d2u, asInData = TRUE ), 2 )
printAll( d2u )

## unbalanced panel rice data, error components cost frontier, timeEffect
d3u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, timeEffect = TRUE )
print( d3u, digits = 1 )
print( summary( d3u, effMinusU = FALSE ), digits = 1 )
nobs( d3u )
lrtest( d3u )
round( efficiencies( d3u, minusU = FALSE ), 2 )
round( efficiencies( d3u, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d3u ), 2 )
round( residuals( d3u, asInData = TRUE ), 2 )
printAll( d3u )

## unbalanced panel rice data, error components cost frontier, truncNorm, timeEffect
d4u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, truncNorm = TRUE, timeEffect = TRUE )
print( d4u, digits = 1 )
print( summary( d4u, effMinusU = FALSE ), digits = 1 )
nobs( d4u )
lrtest( d4u )
round( efficiencies( d4u, minusU = FALSE ), 2 )
round( efficiencies( d4u, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d4u ), 2 )
round( residuals( d4u, asInData = TRUE ), 2 )
printAll( d4u )

## unbalanced panel rice data, efficiency effects cost frontier
d5u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhilPanelUnb, ineffDecrease = FALSE )
print( d5u, minusU = FALSE )
print( summary( d5u ), digits = 1 )
print( summary( d5u, effMinusU = FALSE ), digits = 1 )
nobs( d5u )
lrtest( d5u )
round( efficiencies( d5u ), 2 )
round( efficiencies( d5u, asInData = TRUE ), 2 )
round( efficiencies( d5u, minusU = FALSE ), 2 )
round( efficiencies( d5u, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d5u ), 2 )
round( residuals( d5u, asInData = TRUE ), 2 )
printAll( d5u )

## unbalanced panel rice data, efficiency effects cost frontier, zIntercept
d6u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhilPanelUnb, ineffDecrease = FALSE )
print( d6u, digits = 1 )
print( summary( d6u, effMinusU = FALSE ), digits = 1 )
nobs( d6u )
lrtest( d6u )
round( efficiencies( d6u, minusU = FALSE ), 2 )
round( efficiencies( d6u, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( d6u ), 2 )
round( residuals( d6u, asInData = TRUE ), 2 )
printAll( d6u )


## unbalanced panel data with firms that have NAs in all time periods
naPanelData <- riceProdPhilPanelUnb
naPanelData[ naPanelData$farm == "F_21", "PROD" ] <- NA
naPanelData[ naPanelData$farm == "F_23", "AREA" ] <- NA
naPanelData[ naPanelData$farm == "F_26", "LABOR" ] <- NA
naPanelData[ naPanelData$farm == "F_30", "NPK" ] <- NA
naPanelData[ naPanelData$farm == "F_35", "EDYRS" ] <- NA

## panel data with NA firms, error components frontier
b1n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = naPanelData )
print( b1n, digits = 1 )
print( summary( b1n ), digits = 1 )
nobs( b1n )
lrtest( b1n )
round( efficiencies( b1n ), 2 )
round( efficiencies( b1n, asInData = TRUE ), 2 )
round( residuals( b1n ), 2 )
round( residuals( b1n, asInData = TRUE ), 2 )
printAll( b1n )

## panel data with NA firms, error components frontier, truncNorm, timeEffect
b4n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = naPanelData, truncNorm = TRUE, timeEffect = TRUE )
print( b4n, digits = 1 )
print( summary( b4n ), digits = 1 )
nobs( b4n )
lrtest( b4n )
round( efficiencies( b4n ), 2 )
round( efficiencies( b4n, asInData = TRUE ), 2 )
round( residuals( b4n ), 2 )
round( residuals( b4n, asInData = TRUE ), 2 )
printAll( b4n )

## panel data with NA firms, efficiency effects frontier
b5n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = naPanelData )
print( b5n, digits = 1 )
print( summary( b5n ), digits = 1 )
nobs( b5n )
lrtest( b5n )
round( efficiencies( b5n ), 2 )
round( efficiencies( b5n, asInData = TRUE ), 2 )
round( residuals( b5n ), 2 )
round( residuals( b5n, asInData = TRUE ), 2 )
printAll( b5n )

## panel data with NA firms, efficiency effects frontier, zIntercept
b6n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT, data = naPanelData )
print( b6n, digits = 1 )
print( summary( b6n ), digits = 1 )
nobs( b6n )
lrtest( b6n )
round( efficiencies( b6n ), 2 )
round( efficiencies( b6n, asInData = TRUE ), 2 )
round( residuals( b6n ), 2 )
round( residuals( b6n, asInData = TRUE ), 2 )
printAll( b6n )


## unbalanced panel data with time periods that have NAs for all firms
naTimePanelData <- riceProdPhilPanelUnb
naTimePanelData[ naTimePanelData$year == 2001, "PROD" ] <- NA
naTimePanelData[ naTimePanelData$year == 2004, "AREA" ] <- NA
naTimePanelData[ naTimePanelData$year == 1999, "EDYRS" ] <- NA

## panel data with NA years, error components frontier
b1t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = naTimePanelData )
print( b1t, digits = 1 )
print( summary( b1t ), digits = 1 )
nobs( b1t )
lrtest( b1t )
round( efficiencies( b1t ), 2 )
round( efficiencies( b1t, asInData = TRUE ), 2 )
round( residuals( b1t ), 2 )
round( residuals( b1t, asInData = TRUE ), 2 )
printAll( b1t )

## panel data with NA years, error components frontier, truncNorm, timeEffect
b4t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = naTimePanelData, truncNorm = TRUE, timeEffect = TRUE )
print( b4t, digits = 1 )
print( summary( b4t ), digits = 1 )
nobs( b4t )
lrtest( b4t )
round( efficiencies( b4t ), 2 )
round( efficiencies( b4t, asInData = TRUE ), 2 )
round( residuals( b4t ), 2 )
round( residuals( b4t, asInData = TRUE ), 2 )
printAll( b4t )

## panel data with NA years, efficiency effects frontier
b5t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = naTimePanelData )
print( b5t, digits = 1 )
print( summary( b5t ), digits = 1 )
nobs( b5t )
lrtest( b5t )
round( efficiencies( b5t ), 2 )
round( efficiencies( b5t, asInData = TRUE ), 2 )
round( residuals( b5t ), 2 )
round( residuals( b5t, asInData = TRUE ), 2 )
printAll( b5t )

## panel data with NA years, efficiency effects frontier, zIntercept
b6t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT, data = naTimePanelData )
print( b6t, digits = 1 )
print( summary( b6t ), digits = 1 )
nobs( b6t )
lrtest( b6t )
round( efficiencies( b6t ), 2 )
round( efficiencies( b6t, asInData = TRUE ), 2 )
round( residuals( b6t ), 2 )
round( residuals( b6t, asInData = TRUE ), 2 )
printAll( b6t )


## translog frontiers
## cross-section data, error components frontier, translog
translog <- frontierQuad( data = front41Data, yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ) )
print( translog, digits = 1 )
coef( translog, which = "start" )
round( coef( translog, which = "ols" ), 2 )
round( coef( translog, which = "grid" ), 2 )
round( coef( translog ), 2 )
round( coef( summary( translog ), which = "ols" ), 2 )
round( coef( summary( translog ) ), 2 )
round( vcov( translog ), 2 )
print( logLik( translog, which = "ols" ), digits = 4 )
print( logLik( translog ), digits = 4 )
nobs( translog )
print( summary( translog ), digits = 1 )
print( summary( translog, effMinusU = FALSE ), digits = 1 )
lrtest( translog )
round( efficiencies( translog ), 2 )
round( efficiencies( translog, asInData = TRUE ), 2 )
round( efficiencies( translog, minusU = FALSE ), 2 )
round( efficiencies( translog, asInData = TRUE, minusU = FALSE ), 2 )
round( residuals( translog ), 2 )
round( residuals( translog, asInData = TRUE ), 2 )
translogEla <- elas( translog )
round( translogEla, 2 )
round( attributes( translogEla )$variance, 2 )
round( attributes( translogEla )$stdDev, 2 )
printAll( translog )

## cross-section data, error components frontier, translog, shifter
translogShift <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), shifterNames = "firmNo",
   data = front41Data )
print( translogShift, digits = 1 )
coef( translogShift, which = "start" )
round( coef( translogShift, which = "ols" ), 2 )
round( coef( translogShift, which = "grid" ), 2 )
round( coef( translogShift ), 2 )
round( coef( summary( translogShift ), which = "ols" ), 2 )
round( coef( summary( translogShift ) ), 2 )
round( vcov( translogShift ), 2 )
print( logLik( translogShift, which = "ols" ), digits = 4 )
print( logLik( translogShift ), digits = 4 )
nobs( translogShift )
print( summary( translogShift ), digits = 1 )
lrtest( translogShift )
round( efficiencies( translogShift ), 2 )
round( efficiencies( translogShift, asInData = TRUE ), 2 )
round( residuals( translogShift ), 2 )
round( residuals( translogShift, asInData = TRUE ), 2 )
translogShiftEla <- elas( translogShift )
round( translogShiftEla, 2 )
round( attributes( translogShiftEla )$variance, 2 )
round( attributes( translogShiftEla )$stdDev, 2 )
printAll( translogShift )

## cross-section data, efficiency effects frontier, translog
translogZvar <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), zNames = "firmNo",
   data = front41Data )
print( translogZvar, digits = 1 )
coef( translogZvar, which = "start" )
round( coef( translogZvar, which = "ols" ), 2 )
round( coef( translogZvar, which = "grid" ), 2 )
round( coef( translogZvar ), 2 )
round( coef( summary( translogZvar ), which = "ols" ), 2 )
round( coef( summary( translogZvar ) ), 2 )
round( vcov( translogZvar ), 2 )
print( logLik( translogZvar, which = "ols" ), digits = 4 )
print( logLik( translogZvar ), digits = 4 )
nobs( translogZvar )
print( summary( translogZvar ), digits = 1 )
print( summary( translogZvar, effMinusU = FALSE ), digits = 1 )
lrtest( translogZvar )
printME( efficiencies( translogZvar, margEff = TRUE ) )
printME( efficiencies( translogZvar, asInData = TRUE, margEff = TRUE ) )
printME( efficiencies( translogZvar, minusU = FALSE, margEff = TRUE ) )
printME( efficiencies( translogZvar, asInData = TRUE, minusU = FALSE, margEff = TRUE ) )
round( residuals( translogZvar ), 2 )
round( residuals( translogZvar, asInData = TRUE ), 2 )
translogZvarEla <- elas( translogZvar )
round( translogZvarEla, 2 )
round( attributes( translogZvarEla )$variance, 2 )
round( attributes( translogZvarEla )$stdDev, 2 )
printAll( translogZvar )


################################################
## endogenous variable (seemingly) NOT logged ##
################################################

## example data included in FRONTIER 4.1 (cross-section data)
## cross-section data, error components frontier
print( summary( Sa1, logDepVar = FALSE ), digits = 1 )
print( summary( Sa1, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( a1, logDepVar = FALSE ), 2 )
round( efficiencies( a1, asInData = TRUE, logDepVar = FALSE ), 2 )
round( efficiencies( a1, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( a1, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## cross-section data, error components frontier, truncNorm
print( summary( a2, logDepVar = FALSE ), digits = 1 )
round( efficiencies( a2, logDepVar = FALSE ), 2 )
round( efficiencies( a2, asInData = TRUE, logDepVar = FALSE ), 2 )

## cross-section data, efficiency effects frontier
print( summary( Saa1, logDepVar = FALSE ), digits = 1 )
print( summary( Saa1, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
printME( efficiencies( aa1, logDepVar = FALSE, margEff = TRUE ) )
printME( efficiencies( aa1, asInData = TRUE, logDepVar = FALSE ) )
printME( efficiencies( aa1, logDepVar = FALSE, minusU = FALSE ) )
printME( efficiencies( aa1, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ) )

## cross-section data, efficiency effects frontier, zIntercept
print( summary( aa2, logDepVar = FALSE ), digits = 1 )
round( efficiencies( aa2, logDepVar = FALSE ), 2 )
round( efficiencies( aa2, asInData = TRUE, logDepVar = FALSE ), 2 )

## cross-section rice data, error components cost frontier
print( summary( Sdd1, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( dd1, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( dd1, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## cross-section rice data, error components cost frontier, truncNorm
print( summary( dd2, logDepVar = FALSE ), digits = 1 )
print( summary( dd2, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( dd2, logDepVar = FALSE ), 2 )
round( efficiencies( dd2, asInData = TRUE, logDepVar = FALSE ), 2 )
round( efficiencies( dd2, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( dd2, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## cross-section rice data, efficiency effects cost frontier
print( summary( Sdd5, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( dd5, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( dd5, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## cross-section rice data, efficiency effects cost frontier, zIntercept
print( summary( dd6, logDepVar = FALSE ), digits = 1 )
print( summary( dd6, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( dd6, logDepVar = FALSE ), 2 )
round( efficiencies( dd6, asInData = TRUE , logDepVar = FALSE ), 2 )
round( efficiencies( dd6, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( dd6, asInData = TRUE , logDepVar = FALSE, minusU = FALSE ), 2 )

## panel data, error components frontier
print( summary( Sb1, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b1, logDepVar = FALSE ), 2 )
round( efficiencies( b1, asInData = TRUE, logDepVar = FALSE ), 2 )

## panel data, error components frontier, truncNorm
print( summary( b2, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b2, logDepVar = FALSE ), 2 )
round( efficiencies( b2, asInData = TRUE, logDepVar = FALSE ), 2 )

## panel data, error components frontier, timeEffect
print( summary( b3, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b3, logDepVar = FALSE ), 2 )
round( efficiencies( b3, asInData = TRUE, logDepVar = FALSE ), 2 )

## panel data, error components frontier, truncNorm, timeEffect
print( summary( b4, logDepVar = FALSE ), digits = 1 )
print( summary( b4, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( b4, logDepVar = FALSE ), 2 )
round( efficiencies( b4, asInData = TRUE, logDepVar = FALSE ), 2 )
round( efficiencies( b4, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( b4, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## panel data, efficiency effects frontier
print( summary( Sb5, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b5, logDepVar = FALSE ), 2 )
round( efficiencies( b5, asInData = TRUE, logDepVar = FALSE ), 2 )

## panel data, efficiency effects frontier, zIntercept
print( summary( b6, logDepVar = FALSE ), digits = 1 )
print( summary( b6, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( b6, logDepVar = FALSE ), 2 )
round( efficiencies( b6, asInData = TRUE, logDepVar = FALSE ), 2 )
round( efficiencies( b6, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( b6, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## panel rice data, error components cost frontier
print( summary( Sd1, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d1, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d1, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## panel rice data, error components cost frontier, truncNorm
print( summary( d2, logDepVar = FALSE ), digits = 1 )
print( summary( d2, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d2, logDepVar = FALSE ), 2 )
round( efficiencies( d2, asInData = TRUE, logDepVar = FALSE ), 2 )
round( efficiencies( d2, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d2, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## panel rice data, error components cost frontier, timeEffect
print( summary( d3, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d3, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d3, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## panel rice data, error components cost frontier, truncNorm, timeEffect
print( summary( d4, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d4, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d4, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## panel rice data, efficiency effects cost frontier
print( summary( Sd5, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d5, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d5, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## panel rice data, efficiency effects cost frontier, zIntercept
print( summary( d6, logDepVar = FALSE ), digits = 1 )
print( summary( d6, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d6, logDepVar = FALSE ), 2 )
round( efficiencies( d6, asInData = TRUE, logDepVar = FALSE ), 2 )
round( efficiencies( d6, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d6, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## unbalanced panel data, error components frontier
print( summary( b1u, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b1u, logDepVar = FALSE ), 2 )
round( efficiencies( b1u, asInData = TRUE, logDepVar = FALSE ), 2 )

## unbalanced panel data, error components frontier, truncNorm
print( summary( b2u, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b2u, logDepVar = FALSE ), 2 )
round( efficiencies( b2u, asInData = TRUE, logDepVar = FALSE ), 2 )

## unbalanced panel data, error components frontier, timeEffect
print( summary( b3u, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b3u, logDepVar = FALSE ), 2 )
round( efficiencies( b3u, asInData = TRUE, logDepVar = FALSE ), 2 )

## unbalanced panel data, error components frontier, truncNorm, timeEffect
print( summary( b4u, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b4u, logDepVar = FALSE ), 2 )
round( efficiencies( b4u, asInData = TRUE, logDepVar = FALSE ), 2 )

## unbalanced panel data, efficiency effects frontier
print( summary( b5u, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b5u, logDepVar = FALSE ), 2 )
round( efficiencies( b5u, asInData = TRUE, logDepVar = FALSE ), 2 )

## unbalanced panel data, efficiency effects frontier, zIntercept
print( summary( b6u, logDepVar = FALSE ), digits = 1 )
round( efficiencies( b6u, logDepVar = FALSE ), 2 )
round( efficiencies( b6u, asInData = TRUE, logDepVar = FALSE ), 2 )

## unbalanced panel rice data, error components cost frontier
print( summary( d1u, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d1u, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d1u, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## unbalanced panel rice data, error components cost frontier, truncNorm
print( summary( d2u, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d2u, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d2u, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## unbalanced panel rice data, error components cost frontier, timeEffect
print( summary( d3u, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d3u, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d3u, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## unbalanced panel rice data, error components cost frontier, truncNorm, timeEffect
print( summary( d4u, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d4u, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d4u, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## unbalanced panel rice data, efficiency effects cost frontier
print( summary( d5u, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d5u, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d5u, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )

## unbalanced panel rice data, efficiency effects cost frontier, zIntercept
print( summary( d6u, logDepVar = FALSE, effMinusU = FALSE ), digits = 1 )
round( efficiencies( d6u, logDepVar = FALSE, minusU = FALSE ), 2 )
round( efficiencies( d6u, asInData = TRUE, logDepVar = FALSE, minusU = FALSE ), 2 )


##############################################
## estimation with data NOT in a data frame ##
##############################################

## example data included in FRONTIER 4.1 (cross-section data)
y <- front41Data$output
x1 <- front41Data$capital
x2 <- front41Data$labour
z1 <- front41Data$firmNo

## cross-section data, error components frontier
a1a <- sfa( log( y ) ~ log( x1 ) + log( x2 ) )
all.equal( a1a[-39], a1[-39], check.attributes = FALSE, tol = 1e-4 )
nobs( a1a )

## cross-section data, efficiency effects frontier
aa1a <- sfa( log( y ) ~ log( x1 ) + log( x2 ) | z1 - 1 )
all.equal( aa1a[-39], aa1[-39], check.attributes = FALSE, tol = 1e-4 )

## cross-section data, efficiency effects frontier, zIntercept
aa2a <- sfa( log( y ) ~ log( x1 ) + log( x2 ) | z1 )
all.equal( aa2a[-39], aa2[-39], check.attributes = FALSE, tol = 1e-4 )


##############################################
### estimations with 0 or 1 variable only ###
##############################################

## cross-section data, error components frontier
sa10 <- sfa( logOutput ~ 1, data = front41Data )
a10 <- frontier( "logOutput", NULL, data = front41Data )
print( sa10, digits = 1 )
all.equal( sa10[-39], a10[-39], check.attributes = FALSE, tol = 1e-4 )
nobs( sa10 )

sa11 <- sfa( logOutput ~ logLabour, data = front41Data )
a11 <- frontier( "logOutput", "logLabour", data = front41Data )
print( sa11, digits = 1 )
all.equal( sa11[-39], a11[-39], check.attributes = FALSE, tol = 1e-4 )
nobs( sa11 )

## cross-section data, efficiency effects frontier
saa10 <- sfa( logOutput ~ 1 | firmNo - 1, data = front41Data )
aa10 <- frontier( data = front41Data, "logOutput", NULL,
   zNames = "firmNo" )
print( saa10, digits = 1 )
all.equal( saa10[-39], aa10[-39], tol = 1e-4 )
nobs( saa10 )

saa11 <- sfa( logOutput ~ logLabour | firmNo - 1, data = front41Data )
aa11 <- frontier( data = front41Data, "logOutput", "logLabour",
   zNames = "firmNo" )
print( saa11, digits = 1 )
all.equal( saa11[-39], aa11[-39], tol = 1e-4 )
nobs( saa11 )


##############################################
##### evaluating log likelihood values #######
##############################################
options( digits = 9 )

## cross-section data, error components frontier
print( logLik( a1 ), digits = 4 )
print( logLik( a1, newParam = coef( a1 ) ), digits = 4 )
print( logLik( sa1, newParam = coef( sa1 ) ), digits = 4 )
print( logLik( Sa1, newParam = coef( a1 ) ), digits = 4 )

## cross-section data, error components frontier, truncNorm
print( logLik( a2 ), digits = 4 )
print( logLik( a2, newParam = coef( a2 ) ), digits = 4 )
print( logLik( sa2, newParam = coef( sa2 ) ), digits = 4 )

## cross-section data, error components frontier, truncNorm, starting values
print( logLik( a5 ), digits = 4 )
print( logLik( a5, newParam = coef( a5 ) ), digits = 4 )
print( logLik( sa5, newParam = coef( sa5 ) ), digits = 4 )

## cross-section data, efficiency effects frontier
print( logLik( aa1 ), digits = 4 )
print( logLik( aa1, newParam = coef( aa1 ) ), digits = 4 )
print( logLik( saa1, newParam = coef( saa1 ) ), digits = 4 )
print( logLik( Saa1, newParam = coef( aa1 ) ), digits = 4 )

## cross-section data, efficiency effects frontier, zIntercept
print( logLik( aa2 ), digits = 4 )
print( logLik( aa2, newParam = coef( aa2 ) ), digits = 4 )
print( logLik( saa2, newParam = coef( saa2 ) ), digits = 4 )

## cross-section data, efficiency effects frontier, zIntercept, starting values
print( logLik( aa5 ), digits = 4 )
print( logLik( aa5, newParam = coef( aa5 ) ), digits = 4 )
print( logLik( saa5, newParam = coef( saa5 ) ), digits = 4 )


## data set of rice producers in the Philippines

## cross-section rice data, error components frontier
print( logLik( bb1 ), digits = 4 )
print( logLik( bb1, newParam = coef( bb1 ) ), digits = 4 )
print( logLik( sbb1, newParam = coef( sbb1 ) ), digits = 4 )
print( logLik( Sbb1, newParam = coef( bb1 ) ), digits = 4 )

## cross-section rice data, error components frontier, truncNorm
print( logLik( bb2 ), digits = 4 )
print( logLik( bb2, newParam = coef( bb2 ) ), digits = 4 )
print( logLik( sbb2, newParam = coef( sbb2 ) ), digits = 4 )

## cross-section rice data, efficiency effects frontier
print( logLik( bb5 ), digits = 4 )
print( logLik( bb5, newParam = coef( bb5 ) ), digits = 4 )
print( logLik( sbb5, newParam = coef( sbb5 ) ), digits = 4 )
print( logLik( Sbb5, newParam = coef( bb5 ) ), digits = 4 )

## cross-section rice data, efficiency effects frontier, zIntercept
print( logLik( bb6 ), digits = 4 )
print( logLik( bb6, newParam = coef( bb6 ) ), digits = 4 )
print( logLik( sbb6, newParam = coef( sbb6 ) ), digits = 4 )

## cross-section rice data, error components frontier, truncNorm, starting values
print( logLik( bb7 ), digits = 4 )
print( logLik( bb7, newParam = coef( bb7 ) ), digits = 4 )
print( logLik( sbb7, newParam = coef( sbb7 ) ), digits = 4 )

## cross-section rice data, efficiency effects frontier, zIntercept, starting values
print( logLik( bb8 ), digits = 4 )
print( logLik( bb8, newParam = coef( bb8 ) ), digits = 4 )
print( logLik( sbb8, newParam = coef( sbb8 ) ), digits = 4 )


## Cost Frontier (with land as quasi-fixed input)
## cross-section rice data, error components cost frontier
print( logLik( dd1 ), digits = 4 )
print( logLik( dd1, newParam = coef( dd1 ) ), digits = 4 )
print( logLik( sdd1, newParam = coef( sdd1 ) ), digits = 4 )
print( logLik( Sdd1, newParam = coef( dd1 ) ), digits = 4 )

## cross-section rice data, error components cost frontier, truncNorm
print( logLik( dd2 ), digits = 4 )
print( logLik( dd2, newParam = coef( dd2 ) ), digits = 4 )
print( logLik( sdd2, newParam = coef( sdd2 ) ), digits = 4 )

## cross-section rice data, efficiency effects cost frontier
print( logLik( dd5 ), digits = 4 )
print( logLik( dd5, newParam = coef( dd5 ) ), digits = 4 )
print( logLik( sdd5, newParam = coef( sdd5 ) ), digits = 4 )
print( logLik( Sdd5, newParam = coef( dd5 ) ), digits = 4 )

## cross-section rice data, efficiency effects cost frontier, zIntercept
print( logLik( dd6 ), digits = 4 )
print( logLik( dd6, newParam = coef( dd6 ) ), digits = 4 )
print( logLik( sdd6, newParam = coef( sdd6 ) ), digits = 4 )


## panel data

## panel data, error components frontier
print( logLik( b1 ), digits = 4 )
print( logLik( b1, newParam = coef( b1 ) ), digits = 4 )
print( logLik( sb1, newParam = coef( sb1 ) ), digits = 4 )
print( logLik( Sb1, newParam = coef( b1 ) ), digits = 4 )

## panel data, error components frontier, truncNorm
print( logLik( b2 ), digits = 4 )
print( logLik( b2, newParam = coef( b2 ) ), digits = 4 )
print( logLik( sb2, newParam = coef( sb2 ) ), digits = 4 )

## panel data, error components frontier, timeEffect
print( logLik( b3 ), digits = 4 )
print( logLik( b3, newParam = coef( b3 ) ), digits = 4 )
print( logLik( sb3, newParam = coef( sb3 ) ), digits = 4 )

## panel data, error components frontier, truncNorm, timeEffect
print( logLik( b4 ), digits = 4 )
print( logLik( b4, newParam = coef( b4 ) ), digits = 4 )
print( logLik( sb4, newParam = coef( sb4 ) ), digits = 4 )

## panel data, efficiency effects frontier
print( logLik( b5 ), digits = 4 )
print( logLik( b5, newParam = coef( b5 ) ), digits = 4 )
print( logLik( sb5, newParam = coef( sb5 ) ), digits = 4 )
print( logLik( Sb5, newParam = coef( b5 ) ), digits = 4 )

## panel data, efficiency effects frontier, zIntercept
print( logLik( b6 ), digits = 4 )
print( logLik( b6, newParam = coef( b6 ) ), digits = 4 )
print( logLik( sb6, newParam = coef( sb6 ) ), digits = 4 )

## panel data, error components frontier, truncNorm, timeEffect, starting values
print( logLik( b7 ), digits = 4 )
print( logLik( b7, newParam = coef( b7 ) ), digits = 4 )
print( logLik( sb7, newParam = coef( sb7 ) ), digits = 4 )

## panel data, efficiency effects frontier, zIntercept, starting values
print( logLik( b8 ), digits = 4 )
print( logLik( b8, newParam = coef( b8 ) ), digits = 4 )
print( logLik( sb8, newParam = coef( sb8 ) ), digits = 4 )


## Cost Frontier (with land as quasi-fixed input)
## panel rice data, error components cost frontier
print( logLik( d1 ), digits = 4 )
print( logLik( d1, newParam = coef( d1 ) ), digits = 4 )
print( logLik( sd1, newParam = coef( sd1 ) ), digits = 4 )
print( logLik( Sd1, newParam = coef( d1 ) ), digits = 4 )

## panel rice data, error components cost frontier, truncNorm
print( logLik( d2 ), digits = 4 )
print( logLik( d2, newParam = coef( d2 ) ), digits = 4 )
print( logLik( sd2, newParam = coef( sd2 ) ), digits = 4 )

## panel rice data, error components cost frontier, timeEffect
print( logLik( d3 ), digits = 4 )
print( logLik( d3, newParam = coef( d3 ) ), digits = 4 )
print( logLik( sd3, newParam = coef( sd3 ) ), digits = 4 )

## panel rice data, error components cost frontier, truncNorm, timeEffect
print( logLik( d4 ), digits = 4 )
print( logLik( d4, newParam = coef( d4 ) ), digits = 4 )
print( logLik( sd4, newParam = coef( sd4 ) ), digits = 4 )

## panel rice data, efficiency effects cost frontier
print( logLik( d5 ), digits = 4 )
print( logLik( d5, newParam = coef( d5 ) ), digits = 4 )
print( logLik( sd5, newParam = coef( sd5 ) ), digits = 4 )
print( logLik( Sd5, newParam = coef( d5 ) ), digits = 4 )

## panel rice data, efficiency effects cost frontier, zIntercept
print( logLik( d6 ), digits = 4 )
print( logLik( d6, newParam = coef( d6 ) ), digits = 4 )
print( logLik( sd6, newParam = coef( sd6 ) ), digits = 4 )


## translog frontiers
## cross-section data, error components frontier, translog
print( logLik( translog ), digits = 4 )
print( logLik( translog, newParam = coef( translog ) ), digits = 4 )

## cross-section data, error components frontier, translog, shifter
print( logLik( translogShift ), digits = 4 )
print( logLik( translogShift, newParam = coef( translogShift ) ), digits = 4 )

## cross-section data, efficiency effects frontier, translog
print( logLik( translogZvar ), digits = 4 )
print( logLik( translogZvar, newParam = coef( translogZvar ) ), digits = 4 )


##############################################
########   likelihood ratio tests   ##########
##############################################

## cross-section data, error components frontier
lrtest( a2, a1, a5 )
lrtest( a1, a2, a5 )

## cross-section data, efficiency effects frontier
lrtest( aa2, aa1, aa9, aa2, aa5 )
lrtest( aa9, aa1, aa2, aa5 )

## cross-section data, ECM + EEF
try( lrtest( a2, a1, aa1 ) )
try( lrtest( aa2, a1, aa1 ) )


## data set of rice producers in the Philippines
## cross-section rice data, error components frontier
lrtest( bb2, bb1, bb7 )

## cross-section rice data, efficiency effects frontier
lrtest( bb6, bb5, bb9, bb6, bb8 )


## Cost Frontier (with land as quasi-fixed input)
## cross-section rice data, error components cost frontier
lrtest( dd1, dd2 )

## cross-section rice data, efficiency effects frontier
lrtest( dd6, dd5, dd9, dd6 )


## panel data
## panel data, error components frontier
lrtest( b4, b3, b1, b7 )
lrtest( b4, b2, b1 )
lrtest( b4, b1 )

## panel data, efficiency effects frontier
lrtest( b6, b5, b9, b6, b8 )


## Cost Frontier (with land as quasi-fixed input)
## panel rice data, error components cost frontier
lrtest( d4, d3, d1 )
lrtest( d4, d2, d1 )
lrtest( d4, d1 )

## panel rice data, efficiency effects cost frontier
lrtest( d6, d5, d9, d6 )


## translog
lrtest( translogShift, translog )
