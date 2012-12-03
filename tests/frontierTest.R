library( frontier )
options( digits = 5 )

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
all.equal( Sa1[-39], sa1[-39], check.attributes = FALSE )
a1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ) )
all.equal( sa1[-39], a1[-39] )
print( a1 )
coef( a1, which = "start" )
coef( a1, which = "ols" )
coef( a1, which = "grid" )
coef( a1 )
coef( summary( a1 ), which = "ols" )
coef( summary( a1 ) )
round( vcov( a1 ), 5 )
logLik( a1, which = "ols" )
logLik( a1, which = "grid" )
logLik( a1 )
nobs( a1 )
print( summary( a1 ) )
print( summary( a1, farrell = FALSE ) )
lrtest( a1 )
round( efficiencies( a1, margEff = TRUE ), 3 )
round( efficiencies( a1, asInData = TRUE ), 3 )
round( efficiencies( a1, farrell = FALSE ), 3 )
round( efficiencies( a1, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( a1 ), 4 )
round( residuals( a1, asInData = TRUE ), 4 )
print.default( a1 )

## cross-section data, error components frontier, truncNorm
sa2 <- sfa( logOutput ~ logCapital + logLabour, data = front41Data,
   truncNorm = TRUE, printIter = 4 )
a2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE )
all.equal( sa2[-c(8,39)], a2[-c(8,39)] )
print( a2 )
coef( a2, which = "start" )
coef( a2, which = "ols" )
coef( a2, which = "grid" )
coef( a2 )
coef( summary( a2 ), which = "ols" )
coef( summary( a2 ) )
round( vcov( a2 ), 5 )
logLik( a2, which = "ols" )
logLik( a2 )
nobs( a2 )
print( summary( a2 ) )
lrtest( a2 )
round( efficiencies( a2 ), 3 )
round( efficiencies( a2, asInData = TRUE ), 3 )
round( residuals( a2 ), 4 )
round( residuals( a2, asInData = TRUE ), 4 )
print.default( a2 )

## cross-section data, error components frontier, truncNorm, starting values
sa5 <- sfa( logOutput ~ logCapital + logLabour, data = front41Data,
   truncNorm = TRUE, startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
all.equal( sa5[-39], a5[-39] )
print( a5 )
coef( a5, which = "start" )
coef( a5, which = "ols" )
coef( a5, which = "grid" )
coef( a5 )
coef( summary( a5 ), which = "ols" )
coef( summary( a5 ) )
round( vcov( a5 ), 5 )
logLik( a5, which = "ols" )
logLik( a5 )
nobs( a5 )
print( summary( a5 ) )
lrtest( a5 )
round( efficiencies( a5 ), 3 )
round( efficiencies( a5, asInData = TRUE ), 3 )
print.default( a5 )

## cross-section data, efficiency effects frontier
saa1 <- sfa( logOutput ~ logCapital + logLabour | firmNo - 1,
   data = front41Data )
Saa1 <- sfa( log( output ) ~ log( capital ) + log( labour ) | firmNo - 1,
   data = front41Data, printIter = 3 )
all.equal( Saa1[-c(8,39)], saa1[-c(8,39)], check.attributes = FALSE )
aa1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo" )
all.equal( saa1[-39], aa1[-39] )
print( aa1 )
coef( aa1, which = "start" )
coef( aa1, which = "ols" )
coef( aa1, which = "grid" )
coef( aa1 )
coef( summary( aa1 ), which = "ols" )
coef( summary( aa1 ) )
round( vcov( aa1 ), 5 )
nobs( aa1 )
print( summary( aa1 ) )
print( summary( aa1, farrell = FALSE ) )
lrtest( aa1 )
print( aa1eff <- efficiencies( aa1, margEff = TRUE ) )
print( aa1effD <- efficiencies( aa1, asInData = TRUE, margEff = TRUE ) ) 
print( aa1effF <- efficiencies( aa1, farrell = FALSE, margEff = TRUE ) )
print( aa1effDF <- efficiencies( aa1, asInData = TRUE, farrell = FALSE, 
   margEff = TRUE ) )
aa1m <- aa1
aa1m$dataTable[ , "firmNo" ] <- aa1m$dataTable[ , "firmNo" ] + 1e-6
all.equal( attr( aa1eff, "margEff" )[ , 1, 1 ], 
   ( efficiencies( aa1m ) - aa1eff )[ , 1 ] / 1e-6 )
all.equal( attr( aa1effD, "margEff" )[ , 1 ], 
   c( efficiencies( aa1m, asInData = TRUE ) - aa1effD ) / 1e-6 )
all.equal( attr( aa1effF, "margEff" )[ , 1, 1 ], 
   ( efficiencies( aa1m, farrell = FALSE ) - aa1effF )[ , 1 ] / 1e-6 )
all.equal( attr( aa1effDF, "margEff" )[ , 1 ],
   c( efficiencies( aa1m, asInData = TRUE, farrell = FALSE ) - aa1effDF ) / 1e-6 )
round( residuals( aa1 ), 4 )
round( residuals( aa1, asInData = TRUE ), 4 )
print.default( aa1 )

## cross-section data, efficiency effects frontier, zIntercept
saa2 <- sfa( logOutput ~ logCapital + logLabour | firmNo,
   data = front41Data )
aa2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE )
all.equal( saa2[-39], aa2[-39] )
print( aa2 )
coef( aa2, which = "start" )
coef( aa2, which = "ols" )
coef( aa2, which = "grid" )
coef( aa2 )
coef( summary( aa2 ), which = "ols" )
coef( summary( aa2 ) )
round( vcov( aa2 ), 5 )
nobs( aa2 )
print( summary( aa2 ) )
lrtest( aa2 )
round( efficiencies( aa2 ), 3 )
round( efficiencies( aa2, asInData = TRUE ), 3 )
round( residuals( aa2 ), 4 )
round( residuals( aa2, asInData = TRUE ), 4 )
print.default( aa2 )

## cross-section data, efficiency effects frontier, zIntercept, starting values
saa5 <- sfa( logOutput ~ logCapital + logLabour | firmNo,
   data = front41Data, startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
aa5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE,
   startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
all.equal( saa5[-39], aa5[-39] )
print( aa5 )
coef( aa5, which = "start" )
coef( aa5, which = "ols" )
coef( aa5, which = "grid" )
coef( aa5 )
coef( summary( aa5 ), which = "ols" )
coef( summary( aa5 ) )
round( vcov( aa5 ), 5 )
nobs( aa5 )
print( summary( aa5 ) )
lrtest( aa5 )
round( efficiencies( aa5 ), 3 )
round( efficiencies( aa5, asInData = TRUE ), 3 )
print.default( aa5 )

## cross-section data, efficiency effects frontier, no Z vars
aa9 <- sfa( log( output ) ~ log( capital ) + log( labour ) | - 1,
   data = front41Data )
summary( aa9 )
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
summary( San1 )
nobs( San1 )

## cross-section data with NAs, efficiency effects frontier
Saan1 <- sfa( log( output ) ~ log( capital ) + log( labour ) | firmNo - 1,
   data = naData )
summary( Saan1 )
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
all.equal( Sbb1[-39], sbb1[-39], check.attributes = FALSE )
bb1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
all.equal( sbb1[-39], bb1[-39] )
print( bb1 )
coef( bb1, which = "start" )
coef( bb1, which = "ols" )
coef( bb1, which = "grid" )
coef( bb1 )
coef( summary( bb1 ), which = "ols" )
coef( summary( bb1 ) )
round( vcov( bb1 ), 5 )
nobs( bb1 )
print( summary( bb1 ) )
lrtest( bb1 )
round( efficiencies( bb1 ), 3 )
round( efficiencies( bb1, asInData = TRUE ), 3 )
round( residuals( bb1 ), 4 )
round( residuals( bb1, asInData = TRUE ), 4 )
print.default( bb1 )

## cross-section rice data, error components frontier, truncNorm
sbb2 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhil,
   truncNorm = TRUE )
bb2 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
all.equal( sbb2[-39], bb2[-39] )
print( bb2 )
coef( bb2, which = "start" )
coef( bb2, which = "ols" )
coef( bb2, which = "grid" )
coef( bb2 )
coef( summary( bb2 ), which = "ols" )
coef( summary( bb2 ) )
round( vcov( bb2 ), 5 )
nobs( bb2 )
print( summary( bb2 ) )
print( summary( bb2, farrell = FALSE ) )
lrtest( bb2 )
round( efficiencies( bb2 ), 3 )
round( efficiencies( bb2, asInData = TRUE ), 3 )
round( efficiencies( bb2, farrell = FALSE ), 3 )
round( efficiencies( bb2, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( bb2 ), 4 )
round( residuals( bb2, asInData = TRUE ), 4 )
print.default( bb2 )

## cross-section rice data, efficiency effects frontier
sbb5 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhil )
Sbb5 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = riceProdPhil )
all.equal( Sbb5[-39], sbb5[-39], check.attributes = FALSE )
bb5 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
all.equal( sbb5[-39], bb5[-39] )
print( bb5 )
coef( bb5, which = "start" )
coef( bb5, which = "ols" )
coef( bb5, which = "grid" )
coef( bb5 )
coef( summary( bb5 ), which = "ols" )
coef( summary( bb5 ) )
round( vcov( bb5 ), 5 )
nobs( bb5 )
print( summary( bb5 ) )
lrtest( bb5 )
round( efficiencies( bb5 ), 3 )
round( efficiencies( bb5, asInData = TRUE ), 3 )
round( residuals( bb5 ), 4 )
round( residuals( bb5, asInData = TRUE ), 4 )
print.default( bb5 )

## cross-section rice data, efficiency effects frontier, zIntercept
sbb6 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhil )
bb6 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
all.equal( sbb6[-39], bb6[-39] )
print( bb6 )
coef( bb6, which = "start" )
coef( bb6, which = "ols" )
coef( bb6, which = "grid" )
coef( bb6 )
coef( summary( bb6 ), which = "ols" )
coef( summary( bb6 ) )
round( vcov( bb6 ), 5 )
nobs( bb6 )
print( summary( bb6 ) )
print( summary( bb6, farrell = FALSE ) )
lrtest( bb6 )
print( bb6eff <- efficiencies( bb6, margEff = TRUE ) )
print( bb6effD <- efficiencies( bb6, asInData = TRUE, margEff = TRUE ) )
print( bb6effF <- efficiencies( bb6, farrell = FALSE, margEff = TRUE ) )
print( bb6effDF <- efficiencies( bb6, asInData = TRUE, farrell = FALSE, 
   margEff = TRUE ) )
bb6m1 <- bb6
bb6m1$dataTable[ , "EDYRS" ] <- bb6m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( bb6eff, "margEff" )[ , 1, 1 ], 
   ( efficiencies( bb6m1 ) - bb6eff )[ , 1 ] / 1e-6 )
all.equal( attr( bb6effD, "margEff" )[ , 1 ], 
   c( efficiencies( bb6m1, asInData = TRUE ) - bb6effD ) / 1e-6 )
all.equal( attr( bb6effF, "margEff" )[ , 1, 1 ], 
   ( efficiencies( bb6m1, farrell = FALSE ) - bb6effF )[ , 1 ] / 1e-6 )
all.equal( attr( bb6effDF, "margEff" )[ , 1 ],
   c( efficiencies( bb6m1, asInData = TRUE, farrell = FALSE ) - bb6effDF ) / 1e-6 )
bb6m2 <- bb6
bb6m2$dataTable[ , "BANRAT" ] <- bb6m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( bb6eff, "margEff" )[ , 1, 2 ], 
   ( efficiencies( bb6m2 ) - bb6eff )[ , 1 ] / 1e-6 )
all.equal( attr( bb6effD, "margEff" )[ , 2 ], 
   c( efficiencies( bb6m2, asInData = TRUE ) - bb6effD ) / 1e-6 )
all.equal( attr( bb6effF, "margEff" )[ , 1, 2 ], 
   ( efficiencies( bb6m2, farrell = FALSE ) - bb6effF )[ , 1 ] / 1e-6 )
all.equal( attr( bb6effDF, "margEff" )[ , 2 ],
   c( efficiencies( bb6m2, asInData = TRUE, farrell = FALSE ) - bb6effDF ) / 1e-6 )
round( residuals( bb6 ), 4 )
round( residuals( bb6, asInData = TRUE ), 4 )
print.default( bb6 )

## cross-section rice data, error components frontier, truncNorm, starting values
sbb7 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhil,
   truncNorm = TRUE, startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
bb7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
all.equal( sbb7[-39], bb7[-39] )
print( bb7 )
coef( bb7, which = "start" )
coef( bb7, which = "ols" )
coef( bb7, which = "grid" )
coef( bb7 )
coef( summary( bb7 ), which = "ols" )
coef( summary( bb7 ) )
round( vcov( bb7 ), 5 )
nobs( bb7 )
print( summary( bb7 ) )
lrtest( bb7 )
round( efficiencies( bb7 ), 3 )
round( efficiencies( bb7, asInData = TRUE ), 3 )
print.default( bb7 )

## cross-section rice data, efficiency effects frontier, zIntercept, starting values
sbb8 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhil,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
bb8 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
all.equal( sbb8[-39], bb8[-39] )
print( bb8 )
coef( bb8, which = "start" )
coef( bb8, which = "ols" )
coef( bb8, which = "grid" )
coef( bb8 )
coef( summary( bb8 ), which = "ols" )
coef( summary( bb8 ) )
round( vcov( bb8 ), 5 )
nobs( bb8 )
print( summary( bb8 ) )
lrtest( bb8 )
round( efficiencies( bb8 ), 3 )
round( efficiencies( bb8, asInData = TRUE ), 3 )
print.default( bb8 )

## cross-section rice data, efficiency effects frontier, no Z vars
bb9 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) | - 1,
   data = riceProdPhil )
summary( bb9 )
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
all.equal( Sdd1[-39], sdd1[-39], check.attributes = FALSE )
dd1 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE )
all.equal( sdd1[-39], dd1[-39] )
print( dd1 )
coef( dd1, which = "start" )
coef( dd1, which = "ols" )
coef( dd1, which = "grid" )
coef( dd1 )
coef( summary( dd1 ), which = "ols" )
coef( summary( dd1 ) )
round( vcov( dd1 ), 5 )
nobs( dd1 )
print( summary( dd1 ) )
print( summary( dd1, farrell = FALSE ) )
lrtest( dd1 )
round( efficiencies( dd1 ), 3 )
round( efficiencies( dd1, asInData = TRUE ), 3 )
round( efficiencies( dd1, farrell = FALSE ), 3 )
round( efficiencies( dd1, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( dd1 ), 4 )
round( residuals( dd1, asInData = TRUE ), 4 )
print.default( dd1 )

## cross-section rice data, error components cost frontier, truncNorm
sdd2 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhil,
   ineffDecrease = FALSE, truncNorm = TRUE )
dd2 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE, truncNorm = TRUE )
all.equal( sdd2[-39], dd2[-39] )
print( dd2 )
coef( dd2, which = "start" )
coef( dd2, which = "ols" )
coef( dd2, which = "grid" )
coef( dd2 )
coef( summary( dd2 ), which = "ols" )
coef( summary( dd2 ) )
round( vcov( dd2 ), 5 )
nobs( dd2 )
print( summary( dd2, farrell = FALSE ) )
lrtest( dd2 )
round( efficiencies( dd2, farrell = FALSE ), 3 )
round( efficiencies( dd2, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( dd2 ), 4 )
round( residuals( dd2, asInData = TRUE ), 4 )
print.default( dd2 )

## cross-section rice data, efficiency effects cost frontier
sdd5 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhil, ineffDecrease = FALSE )
Sdd5 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | EDYRS + BANRAT - 1, data = riceProdPhil, ineffDecrease = FALSE )
all.equal( Sdd5[-39], sdd5[-39], check.attributes = FALSE )
dd5 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE )
all.equal( sdd5[-39], dd5[-39] )
print( dd5 )
coef( dd5, which = "start" )
coef( dd5, which = "ols" )
coef( dd5, which = "grid" )
coef( dd5 )
coef( summary( dd5 ), which = "ols" )
coef( summary( dd5 ) )
round( vcov( dd5 ), 5 )
nobs( dd5 )
print( summary( dd5 ) )
print( summary( dd5, farrell = FALSE ) )
lrtest( dd5 )
print( dd5eff <- efficiencies( dd5, margEff = TRUE ) )
print( dd5effD <- efficiencies( dd5, asInData = TRUE, margEff = TRUE ) )
print( dd5effF <- efficiencies( dd5, farrell = FALSE, margEff = TRUE ) )
print( dd5effDF <- efficiencies( dd5, asInData = TRUE, farrell = FALSE, 
   margEff = TRUE ) )
dd5m1 <- dd5
dd5m1$dataTable[ , "EDYRS" ] <- dd5m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( dd5eff, "margEff" )[ , 1, 1 ], 
   ( efficiencies( dd5m1 ) - dd5eff )[ , 1 ] / 1e-6 )
all.equal( attr( dd5effD, "margEff" )[ , 1 ], 
   c( efficiencies( dd5m1, asInData = TRUE ) - dd5effD ) / 1e-6 )
all.equal( attr( dd5effF, "margEff" )[ , 1, 1 ], 
   ( efficiencies( dd5m1, farrell = FALSE ) - dd5effF )[ , 1 ] / 1e-6 )
all.equal( attr( dd5effDF, "margEff" )[ , 1 ],
   c( efficiencies( dd5m1, asInData = TRUE, farrell = FALSE ) - dd5effDF ) / 1e-6 )
dd5m2 <- dd5
dd5m2$dataTable[ , "BANRAT" ] <- dd5m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( dd5eff, "margEff" )[ , 1, 2 ], 
   ( efficiencies( dd5m2 ) - dd5eff )[ , 1 ] / 1e-6 )
all.equal( attr( dd5effD, "margEff" )[ , 2 ], 
   c( efficiencies( dd5m2, asInData = TRUE ) - dd5effD ) / 1e-6 )
all.equal( attr( dd5effF, "margEff" )[ , 1, 2 ], 
   ( efficiencies( dd5m2, farrell = FALSE ) - dd5effF )[ , 1 ] / 1e-6 )
all.equal( attr( dd5effDF, "margEff" )[ , 2 ],
   c( efficiencies( dd5m2, asInData = TRUE, farrell = FALSE ) - dd5effDF ) / 1e-6 )
round( residuals( dd5 ), 4 )
round( residuals( dd5, asInData = TRUE ), 4 )
print.default( dd5 )

## cross-section rice data, efficiency effects cost frontier, zIntercept
sdd6 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhil, ineffDecrease = FALSE )
dd6 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE, zIntercept = TRUE )
all.equal( sdd6[-39], dd6[-39] )
print( dd6 )
coef( dd6, which = "start" )
coef( dd6, which = "ols" )
coef( dd6, which = "grid" )
coef( dd6 )
coef( summary( dd6 ), which = "ols" )
coef( summary( dd6 ) )
round( vcov( dd6 ), 5 )
nobs( dd6 )
print( summary( dd6, farrell = FALSE ) )
lrtest( dd6 )
round( efficiencies( dd6, farrell = FALSE ), 3 )
round( efficiencies( dd6, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( dd6 ), 4 )
round( residuals( dd6, asInData = TRUE ), 4 )
print.default( dd6 )

## cross-section rice data, efficiency effects cost frontier: no Z vars
dd9 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | - 1, data = riceProdPhil, ineffDecrease = FALSE )
summary( dd9, farrell = FALSE )
nobs( dd9 )
lrtest( dd9 )


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
all.equal( Sb1[-c(8,39)], sb1[-c(8,39)], check.attributes = FALSE )
b1 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
all.equal( sb1[-c(8,39)], b1[-c(8,39)] )
print( b1 )
coef( b1, which = "start" )
coef( b1, which = "ols" )
coef( b1, which = "grid" )
coef( b1 )
coef( summary( b1 ), which = "ols" )
coef( summary( b1 ) )
round( vcov( b1 ), 5 )
logLik( b1, which = "ols" )
logLik( b1 )
nobs( b1 )
print( summary( b1 ) )
print( summary( b1, farrell = FALSE ) )
lrtest( b1 )
round( efficiencies( b1 ), 3 )
round( efficiencies( b1, asInData = TRUE ), 3 )
round( efficiencies( b1, farrell = FALSE ), 3 )
round( efficiencies( b1, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( b1 ), 4 )
round( residuals( b1, asInData = TRUE ), 4 )
print.default( b1 )

## panel data, error components frontier, truncNorm
sb2 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE )
b2 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
all.equal( sb2[-39], b2[-39] )
print( b2 )
coef( b2, which = "start" )
coef( b2, which = "ols" )
coef( b2, which = "grid" )
coef( b2 )
coef( summary( b2 ), which = "ols" )
coef( summary( b2 ) )
round( vcov( b2 ), 5 )
logLik( b2, which = "ols" )
logLik( b2 )
nobs( b2 )
print( summary( b2 ) )
lrtest( b2 )
round( efficiencies( b2 ), 3 )
round( efficiencies( b2, asInData = TRUE ), 3 )
round( residuals( b2 ), 4 )
round( residuals( b2, asInData = TRUE ), 4 )
print.default( b2 )

## panel data, error components frontier, timeEffect
sb3 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   timeEffect = TRUE )
b3 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   timeEffect = TRUE )
all.equal( sb3[-39], b3[-39] )
print( b3 )
coef( b3, which = "start" )
coef( b3, which = "ols" )
coef( b3, which = "grid" )
coef( b3 )
coef( summary( b3 ), which = "ols" )
coef( summary( b3 ) )
round( vcov( b3 ), 5 )
logLik( b3, which = "ols" )
logLik( b3 )
nobs( b3 )
print( summary( b3 ) )
lrtest( b3 )
round( efficiencies( b3 ), 3 )
round( efficiencies( b3, asInData = TRUE ), 3 )
round( residuals( b3 ), 4 )
round( residuals( b3, asInData = TRUE ), 4 )
print.default( b3 )

## panel data, error components frontier, truncNorm, timeEffect
sb4 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE, timeEffect = TRUE )
b4 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE )
all.equal( sb4[-39], b4[-39] )
print( b4 )
coef( b4, which = "start" )
coef( b4, which = "ols" )
coef( b4, which = "grid" )
coef( b4 )
coef( summary( b4 ), which = "ols" )
coef( summary( b4 ) )
round( vcov( b4 ), 5 )
logLik( b4, which = "ols" )
logLik( b4 )
nobs( b4 )
print( summary( b4 ) )
lrtest( b4 )
round( efficiencies( b4 ), 3 )
round( efficiencies( b4, asInData = TRUE ), 3 )
round( residuals( b4 ), 4 )
round( residuals( b4, asInData = TRUE ), 4 )
print.default( b4 )

## panel data, efficiency effects frontier
sb5 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhilPanel )
Sb5 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = riceProdPhilPanel, printIter = 5 )
all.equal( Sb5[-c(8,39)], sb5[-c(8,39)], check.attributes = FALSE )
b5 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
all.equal( sb5[-39], b5[-39] )
print( b5 )
coef( b5, which = "start" )
coef( b5, which = "ols" )
coef( b5, which = "grid" )
coef( b5 )
coef( summary( b5 ), which = "ols" )
coef( summary( b5 ) )
round( vcov( b5 ), 5 )
logLik( b5, which = "ols" )
logLik( b5 )
nobs( b5 )
print( summary( b5 ) )
print( summary( b5, farrell = FALSE ) )
lrtest( b5 )
print( b5eff <- efficiencies( b5, margEff = TRUE ) )
print( b5effD <- efficiencies( b5, asInData = TRUE , margEff = TRUE) )
print( b5effF <- efficiencies( b5, farrell = FALSE, margEff = TRUE ) )
print( b5effDF <- efficiencies( b5, asInData = TRUE, farrell = FALSE, 
   margEff = TRUE ) )
b5m1 <- b5
b5m1$dataTable[ , "EDYRS" ] <- b5m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( b5eff, "margEff" )[ , , 1 ], 
   ( efficiencies( b5m1 ) - b5eff )[ , ] / 1e-6 )
all.equal( attr( b5effD, "margEff" )[ , 1 ], 
   c( efficiencies( b5m1, asInData = TRUE ) - b5effD ) / 1e-6 )
all.equal( attr( b5effF, "margEff" )[ , , 1 ], 
   ( efficiencies( b5m1, farrell = FALSE ) - b5effF )[ ,  ] / 1e-6 )
all.equal( attr( b5effDF, "margEff" )[ , 1 ],
   c( efficiencies( b5m1, asInData = TRUE, farrell = FALSE ) - b5effDF ) / 1e-6 )
b5m2 <- b5
b5m2$dataTable[ , "BANRAT" ] <- b5m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( b5eff, "margEff" )[ , , 2 ], 
   ( efficiencies( b5m2 ) - b5eff )[ , ] / 1e-6 )
all.equal( attr( b5effD, "margEff" )[ , 2 ], 
   c( efficiencies( b5m2, asInData = TRUE ) - b5effD ) / 1e-6 )
all.equal( attr( b5effF, "margEff" )[ , , 2 ], 
   ( efficiencies( b5m2, farrell = FALSE ) - b5effF )[ , ] / 1e-6 )
all.equal( attr( b5effDF, "margEff" )[ , 2 ],
   c( efficiencies( b5m2, asInData = TRUE, farrell = FALSE ) - b5effDF ) / 1e-6 )
round( residuals( b5 ), 4 )
round( residuals( b5, asInData = TRUE ), 4 )
print.default( b5 )

## panel data, efficiency effects frontier, zIntercept
sb6 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanel )
b6 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
all.equal( sb6[-39], b6[-39] )
print( b6 )
coef( b6, which = "start" )
coef( b6, which = "ols" )
coef( b6, which = "grid" )
coef( b6 )
coef( summary( b6 ), which = "ols" )
coef( summary( b6 ) )
round( vcov( b6 ), 5 )
logLik( b6, which = "ols" )
logLik( b6 )
nobs( b6 )
print( summary( b6 ) )
lrtest( b6 )
round( efficiencies( b6 ), 3 )
round( efficiencies( b6, asInData = TRUE ), 3 )
round( residuals( b6 ), 4 )
round( residuals( b6, asInData = TRUE ), 4 )
print.default( b6 )

## panel data, error components frontier, truncNorm, timeEffect, starting values
sb7 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE, timeEffect = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
b7 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
all.equal( sb7[-39], b7[-39] )
print( b7 )
coef( b7, which = "start" )
coef( b7, which = "ols" )
coef( b7, which = "grid" )
coef( b7 )
coef( summary( b7 ), which = "ols" )
coef( summary( b7 ) )
round( vcov( b7 ), 5 )
logLik( b7, which = "ols" )
logLik( b7 )
nobs( b7 )
print( summary( b7 ) )
lrtest( b7 )
round( efficiencies( b7 ), 3 )
round( efficiencies( b7, asInData = TRUE ), 3 )
print.default( b7 )

## panel data, efficiency effects frontier, zIntercept, starting values
sb8 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanel,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.3, -0.01, -0.4, 0.2, 0.8 ) )
b8 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.3, -0.01, -0.4, 0.2, 0.8 ) )
all.equal( sb8[-39], b8[-39] )
print( b8 )
coef( b8, which = "start" )
coef( b8, which = "ols" )
coef( b8, which = "grid" )
coef( b8 )
coef( summary( b8 ), which = "ols" )
coef( summary( b8 ) )
round( vcov( b8 ), 5 )
logLik( b8, which = "ols" )
logLik( b8 )
nobs( b8 )
print( summary( b8 ) )
lrtest( b8 )
round( efficiencies( b8 ), 3 )
round( efficiencies( b8, asInData = TRUE ), 3 )
print.default( b8 )

## panel data, efficiency effects frontier: no Z vars
b9 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) | - 1,
   data = riceProdPhilPanel )
summary( b9 )
nobs( b9 )
lrtest( b9 )


## Cost Frontier (with land as quasi-fixed input)
## panel rice data, error components cost frontier
sd1 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE )
Sd1 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ), data = riceProdPhilPanel, ineffDecrease = FALSE )
all.equal( Sd1[-39], sd1[-39], check.attributes = FALSE )
d1 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE )
all.equal( sd1[-39], d1[-39] )
print( d1 )
coef( d1, which = "start" )
coef( d1, which = "ols" )
coef( d1, which = "grid" )
coef( d1 )
coef( summary( d1 ), which = "ols" )
coef( summary( d1 ) )
round( vcov( d1 ), 5 )
nobs( d1 )
print( summary( d1 ) )
print( summary( d1, farrell = FALSE ) )
lrtest( d1 )
round( efficiencies( d1 ), 3 )
round( efficiencies( d1, asInData = TRUE ), 3 )
round( efficiencies( d1, farrell = FALSE ), 3 )
round( efficiencies( d1, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d1 ), 4 )
round( residuals( d1, asInData = TRUE ), 4 )
print.default( d1 )

## panel rice data, error components cost frontier, truncNorm
sd2 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, truncNorm = TRUE )
d2 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, truncNorm = TRUE )
all.equal( sd2[-39], d2[-39] )
print( d2 )
coef( d2, which = "start" )
coef( d2, which = "ols" )
coef( d2, which = "grid" )
coef( d2 )
coef( summary( d2 ), which = "ols" )
coef( summary( d2 ) )
round( vcov( d2 ), 5 )
nobs( d2 )
print( summary( d2, farrell = FALSE ) )
lrtest( d2 )
round( efficiencies( d2, farrell = FALSE ), 3 )
round( efficiencies( d2, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d2 ), 4 )
round( residuals( d2, asInData = TRUE ), 4 )
print.default( d2 )

## panel rice data, error components cost frontier, timeEffect
sd3 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, timeEffect = TRUE )
d3 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, timeEffect = TRUE )
all.equal( sd3[-39], d3[-39] )
print( d3 )
coef( d3, which = "start" )
coef( d3, which = "ols" )
coef( d3, which = "grid" )
coef( d3 )
coef( summary( d3 ), which = "ols" )
coef( summary( d3 ) )
round( vcov( d3 ), 5 )
nobs( d3 )
print( summary( d3, farrell = FALSE ) )
lrtest( d3 )
round( efficiencies( d3, farrell = FALSE ), 3 )
round( efficiencies( d3, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d3 ), 4 )
round( residuals( d3, asInData = TRUE ), 4 )
print.default( d3 )

## panel rice data, error components cost frontier, truncNorm, timeEffect
sd4 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, truncNorm = TRUE, timeEffect = TRUE )
d4 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, truncNorm = TRUE,
   timeEffect = TRUE )
all.equal( sd4[-39], d4[-39] )
print( d4 )
coef( d4, which = "start" )
coef( d4, which = "ols" )
coef( d4, which = "grid" )
coef( d4 )
coef( summary( d4 ), which = "ols" )
coef( summary( d4 ) )
round( vcov( d4 ), 5 )
nobs( d4 )
print( summary( d4, farrell = FALSE ) )
lrtest( d4 )
round( efficiencies( d4, farrell = FALSE ), 3 )
round( efficiencies( d4, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d4 ), 4 )
round( residuals( d4, asInData = TRUE ), 4 )
print.default( d4 )

## panel rice data, efficiency effects cost frontier
sd5 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhilPanel, ineffDecrease = FALSE )
Sd5 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | EDYRS + BANRAT - 1, data = riceProdPhilPanel,
   ineffDecrease = FALSE )
all.equal( Sd5[-39], sd5[-39], check.attributes = FALSE )
d5 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhilPanel,
   ineffDecrease = FALSE )
all.equal( sd5[-39], d5[-39] )
print( d5 )
coef( d5, which = "start" )
coef( d5, which = "ols" )
coef( d5, which = "grid" )
coef( d5 )
coef( summary( d5 ), which = "ols" )
coef( summary( d5 ) )
round( vcov( d5 ), 5 )
nobs( d5 )
print( summary( d5 ) )
print( summary( d5, farrell = FALSE ) )
lrtest( d5 )
print( d5eff <- efficiencies( d5, margEff = TRUE ) )
print( d5effD <- efficiencies( d5, asInData = TRUE, margEff = TRUE ) )
print( d5effF <- efficiencies( d5, farrell = FALSE, margEff = TRUE ) )
print( d5effDF <- efficiencies( d5, asInData = TRUE, farrell = FALSE, 
   margEff = TRUE ) )
d5m1 <- d5
d5m1$dataTable[ , "EDYRS" ] <- d5m1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( d5eff, "margEff" )[ , , 1 ], 
   ( efficiencies( d5m1 ) - d5eff )[ , ] / 1e-6 )
all.equal( attr( d5effD, "margEff" )[ , 1 ], 
   c( efficiencies( d5m1, asInData = TRUE ) - d5effD ) / 1e-6 )
all.equal( attr( d5effF, "margEff" )[ , , 1 ], 
   ( efficiencies( d5m1, farrell = FALSE ) - d5effF )[ ,  ] / 1e-6 )
all.equal( attr( d5effDF, "margEff" )[ , 1 ],
   c( efficiencies( d5m1, asInData = TRUE, farrell = FALSE ) - d5effDF ) / 1e-6 )
d5m2 <- d5
d5m2$dataTable[ , "BANRAT" ] <- d5m2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( d5eff, "margEff" )[ , , 2 ], 
   ( efficiencies( d5m2 ) - d5eff )[ , ] / 1e-6 )
all.equal( attr( d5effD, "margEff" )[ , 2 ], 
   c( efficiencies( d5m2, asInData = TRUE ) - d5effD ) / 1e-6 )
all.equal( attr( d5effF, "margEff" )[ , , 2 ], 
   ( efficiencies( d5m2, farrell = FALSE ) - d5effF )[ , ] / 1e-6 )
all.equal( attr( d5effDF, "margEff" )[ , 2 ],
   c( efficiencies( d5m2, asInData = TRUE, farrell = FALSE ) - d5effDF ) / 1e-6 )
round( residuals( d5 ), 4 )
round( residuals( d5, asInData = TRUE ), 4 )
print.default( d5 )

## panel rice data, efficiency effects cost frontier, zIntercept
sd6 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhilPanel, ineffDecrease = FALSE )
d6 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhilPanel,
   ineffDecrease = FALSE, zIntercept = TRUE )
all.equal( sd6[-39], d6[-39] )
print( d6 )
coef( d6, which = "start" )
coef( d6, which = "ols" )
coef( d6, which = "grid" )
coef( d6 )
coef( summary( d6 ), which = "ols" )
coef( summary( d6 ) )
round( vcov( d6 ), 5 )
nobs( d6 )
print( summary( d6, farrell = FALSE ) )
lrtest( d6 )
round( efficiencies( d6, farrell = FALSE ), 3 )
round( efficiencies( d6, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d6 ), 4 )
round( residuals( d6, asInData = TRUE ), 4 )
print.default( d6 )

## panel rice data, efficiency effects cost frontier: no Z vars
d9 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | - 1, data = riceProdPhilPanel, ineffDecrease = FALSE )
summary( d9, farrell = FALSE )
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
print( b1u )
print( summary( b1u ) )
nobs( b1u )
lrtest( b1u )
round( efficiencies( b1u ), 3 )
round( efficiencies( b1u, asInData = TRUE ), 3 )
round( residuals( b1u ), 4 )
round( residuals( b1u, asInData = TRUE ), 4 )
print.default( b1u )

## unbalanced panel data, error components frontier, truncNorm
b2u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   truncNorm = TRUE )
print( b2u )
print( summary( b2u ) )
nobs( b2u )
lrtest( b2u )
round( efficiencies( b2u ), 3 )
round( efficiencies( b2u, asInData = TRUE ), 3 )
round( residuals( b2u ), 4 )
round( residuals( b2u, asInData = TRUE ), 4 )
print.default( b2u )

## unbalanced panel data, error components frontier, timeEffect
b3u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   timeEffect = TRUE )
print( b3u )
print( summary( b3u ) )
nobs( b3u )
lrtest( b3u )
round( efficiencies( b3u ), 3 )
round( efficiencies( b3u, asInData = TRUE ), 3 )
round( residuals( b3u ), 4 )
round( residuals( b3u, asInData = TRUE ), 4 )
print.default( b3u )

## unbalanced panel data, error components frontier, truncNorm, timeEffect
b4u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   truncNorm = TRUE, timeEffect = TRUE )
print( b4u )
print( summary( b4u ) )
print( summary( b4u, farrell = FALSE ) )
nobs( b4u )
lrtest( b4u )
round( efficiencies( b4u ), 3 )
round( efficiencies( b4u, asInData = TRUE ), 3 )
round( efficiencies( b4u, farrell = FALSE ), 3 )
round( efficiencies( b4u, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( b4u ), 4 )
round( residuals( b4u, asInData = TRUE ), 4 )
print.default( b4u )

## unbalanced panel data, efficiency effects frontier
b5u <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhilPanelUnb )
print( b5u )
print( summary( b5u ) )
nobs( b5u )
lrtest( b5u )
round( efficiencies( b5u ), 3 )
round( efficiencies( b5u, asInData = TRUE ), 3 )
round( residuals( b5u ), 4 )
round( residuals( b5u, asInData = TRUE ), 4 )
print.default( b5u )

## unbalanced panel data, efficiency effects frontier, zIntercept
b6u <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanelUnb )
print( b6u )
print( summary( b6u ) )
print( summary( b6u, farrell = FALSE ) )
nobs( b6u )
lrtest( b6u )
print( b6ueff <- efficiencies( b6u, margEff = TRUE ) )
print( b6ueffD <- efficiencies( b6u, asInData = TRUE, margEff = TRUE ) )
print( b6ueffF <- efficiencies( b6u, farrell = FALSE, margEff = TRUE ) )
print( b6ueffDF <- efficiencies( b6u, asInData = TRUE, farrell = FALSE, 
   margEff = TRUE ) )
b6um1 <- b6u
b6um1$dataTable[ , "EDYRS" ] <- b6um1$dataTable[ , "EDYRS" ] + 1e-6
all.equal( attr( b6ueff, "margEff" )[ , , 1 ], 
   ( efficiencies( b6um1 ) - b6ueff )[ , ] / 1e-6 )
all.equal( attr( b6ueffD, "margEff" )[ , 1 ], 
   c( efficiencies( b6um1, asInData = TRUE ) - b6ueffD ) / 1e-6 )
all.equal( attr( b6ueffF, "margEff" )[ , , 1 ], 
   ( efficiencies( b6um1, farrell = FALSE ) - b6ueffF )[ ,  ] / 1e-6 )
all.equal( attr( b6ueffDF, "margEff" )[ , 1 ],
   c( efficiencies( b6um1, asInData = TRUE, farrell = FALSE ) - b6ueffDF ) / 1e-6 )
b6um2 <- b6u
b6um2$dataTable[ , "BANRAT" ] <- b6um2$dataTable[ , "BANRAT" ] + 1e-6
all.equal( attr( b6ueff, "margEff" )[ , , 2 ], 
   ( efficiencies( b6um2 ) - b6ueff )[ , ] / 1e-6 )
all.equal( attr( b6ueffD, "margEff" )[ , 2 ], 
   c( efficiencies( b6um2, asInData = TRUE ) - b6ueffD ) / 1e-6 )
all.equal( attr( b6ueffF, "margEff" )[ , , 2 ], 
   ( efficiencies( b6um2, farrell = FALSE ) - b6ueffF )[ , ] / 1e-6 )
all.equal( attr( b6ueffDF, "margEff" )[ , 2 ],
   c( efficiencies( b6um2, asInData = TRUE, farrell = FALSE ) - b6ueffDF ) / 1e-6 )
round( residuals( b6u ), 4 )
round( residuals( b6u, asInData = TRUE ), 4 )
print.default( b6u )

## unbalanced panel rice data, error components cost frontier
d1u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE )
print( d1u )
print( summary( d1u ) )
print( summary( d1u, farrell = FALSE ) )
nobs( d1u )
lrtest( d1u )
round( efficiencies( d1u ), 3 )
round( efficiencies( d1u, asInData = TRUE ), 3 )
round( efficiencies( d1u, farrell = FALSE ), 3 )
round( efficiencies( d1u, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d1u ), 4 )
round( residuals( d1u, asInData = TRUE ), 4 )
print.default( d1u )

## unbalanced panel rice data, error components cost frontier, truncNorm
d2u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, truncNorm = TRUE )
print( d2u )
print( summary( d2u, farrell = FALSE ) )
nobs( d2u )
lrtest( d2u )
round( efficiencies( d2u, farrell = FALSE ), 3 )
round( efficiencies( d2u, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d2u ), 4 )
round( residuals( d2u, asInData = TRUE ), 4 )
print.default( d2u )

## unbalanced panel rice data, error components cost frontier, timeEffect
d3u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, timeEffect = TRUE )
print( d3u )
print( summary( d3u, farrell = FALSE ) )
nobs( d3u )
lrtest( d3u )
round( efficiencies( d3u, farrell = FALSE ), 3 )
round( efficiencies( d3u, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d3u ), 4 )
round( residuals( d3u, asInData = TRUE ), 4 )
print.default( d3u )

## unbalanced panel rice data, error components cost frontier, truncNorm, timeEffect
d4u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, truncNorm = TRUE, timeEffect = TRUE )
print( d4u )
print( summary( d4u, farrell = FALSE ) )
nobs( d4u )
lrtest( d4u )
round( efficiencies( d4u, farrell = FALSE ), 3 )
round( efficiencies( d4u, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d4u ), 4 )
round( residuals( d4u, asInData = TRUE ), 4 )
print.default( d4u )

## unbalanced panel rice data, efficiency effects cost frontier
d5u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhilPanelUnb, ineffDecrease = FALSE )
print( d5u, farrell = FALSE )
print( summary( d5u ) )
print( summary( d5u, farrell = FALSE ) )
nobs( d5u )
lrtest( d5u )
round( efficiencies( d5u ), 3 )
round( efficiencies( d5u, asInData = TRUE ), 3 )
round( efficiencies( d5u, farrell = FALSE ), 3 )
round( efficiencies( d5u, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d5u ), 4 )
round( residuals( d5u, asInData = TRUE ), 4 )
print.default( d5u )

## unbalanced panel rice data, efficiency effects cost frontier, zIntercept
d6u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhilPanelUnb, ineffDecrease = FALSE )
print( d6u )
print( summary( d6u, farrell = FALSE ) )
nobs( d6u )
lrtest( d6u )
round( efficiencies( d6u, farrell = FALSE ), 3 )
round( efficiencies( d6u, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( d6u ), 4 )
round( residuals( d6u, asInData = TRUE ), 4 )
print.default( d6u )


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
print( b1n )
summary( b1n )
nobs( b1n )
lrtest( b1n )
round( efficiencies( b1n ), 3 )
round( efficiencies( b1n, asInData = TRUE ), 3 )
round( residuals( b1n ), 4 )
round( residuals( b1n, asInData = TRUE ), 4 )
print.default( b1n )

## panel data with NA firms, error components frontier, truncNorm, timeEffect
b4n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = naPanelData, truncNorm = TRUE, timeEffect = TRUE )
print( b4n )
summary( b4n )
nobs( b4n )
lrtest( b4n )
round( efficiencies( b4n ), 3 )
round( efficiencies( b4n, asInData = TRUE ), 3 )
round( residuals( b4n ), 4 )
round( residuals( b4n, asInData = TRUE ), 4 )
print.default( b4n )

## panel data with NA firms, efficiency effects frontier
b5n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = naPanelData )
print( b5n )
summary( b5n )
nobs( b5n )
lrtest( b5n )
round( efficiencies( b5n ), 3 )
round( efficiencies( b5n, asInData = TRUE ), 3 )
round( residuals( b5n ), 4 )
round( residuals( b5n, asInData = TRUE ), 4 )
print.default( b5n )

## panel data with NA firms, efficiency effects frontier, zIntercept
b6n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT, data = naPanelData )
print( b6n )
summary( b6n )
nobs( b6n )
lrtest( b6n )
round( efficiencies( b6n ), 3 )
round( efficiencies( b6n, asInData = TRUE ), 3 )
round( residuals( b6n ), 4 )
round( residuals( b6n, asInData = TRUE ), 4 )
print.default( b6n )


## unbalanced panel data with time periods that have NAs for all firms
naTimePanelData <- riceProdPhilPanelUnb
naTimePanelData[ naTimePanelData$year == 2001, "PROD" ] <- NA
naTimePanelData[ naTimePanelData$year == 2004, "AREA" ] <- NA
naTimePanelData[ naTimePanelData$year == 1999, "EDYRS" ] <- NA

## panel data with NA years, error components frontier
b1t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = naTimePanelData )
print( b1t )
summary( b1t )
nobs( b1t )
lrtest( b1t )
round( efficiencies( b1t ), 3 )
round( efficiencies( b1t, asInData = TRUE ), 3 )
round( residuals( b1t ), 4 )
round( residuals( b1t, asInData = TRUE ), 4 )
print.default( b1t )

## panel data with NA years, error components frontier, truncNorm, timeEffect
b4t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = naTimePanelData, truncNorm = TRUE, timeEffect = TRUE )
print( b4t )
summary( b4t )
nobs( b4t )
lrtest( b4t )
round( efficiencies( b4t ), 3 )
round( efficiencies( b4t, asInData = TRUE ), 3 )
round( residuals( b4t ), 4 )
round( residuals( b4t, asInData = TRUE ), 4 )
print.default( b4t )

## panel data with NA years, efficiency effects frontier
b5t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = naTimePanelData )
print( b5t )
summary( b5t )
nobs( b5t )
lrtest( b5t )
round( efficiencies( b5t ), 3 )
round( efficiencies( b5t, asInData = TRUE ), 3 )
round( residuals( b5t ), 4 )
round( residuals( b5t, asInData = TRUE ), 4 )
print.default( b5t )

## panel data with NA years, efficiency effects frontier, zIntercept
b6t <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT, data = naTimePanelData )
print( b6t )
summary( b6t )
nobs( b6t )
lrtest( b6t )
round( efficiencies( b6t ), 3 )
round( efficiencies( b6t, asInData = TRUE ), 3 )
round( residuals( b6t ), 4 )
round( residuals( b6t, asInData = TRUE ), 4 )
print.default( b6t )


## translog frontiers
## cross-section data, error components frontier, translog
translog <- frontierQuad( data = front41Data, yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ) )
print( translog )
coef( translog, which = "start" )
coef( translog, which = "ols" )
coef( translog, which = "grid" )
coef( translog )
coef( summary( translog ), which = "ols" )
coef( summary( translog ) )
round( vcov( translog ), 5 )
logLik( translog, which = "ols" )
logLik( translog )
nobs( translog )
print( summary( translog ) )
print( summary( translog, farrell = FALSE ) )
lrtest( translog )
round( efficiencies( translog ), 3 )
round( efficiencies( translog, asInData = TRUE ), 3 )
round( efficiencies( translog, farrell = FALSE ), 3 )
round( efficiencies( translog, asInData = TRUE, farrell = FALSE ), 3 )
round( residuals( translog ), 4 )
round( residuals( translog, asInData = TRUE ), 4 )
translogEla <- elas( translog )
print( translogEla )
attributes( translogEla )$variance
attributes( translogEla )$stdDev
print.default( translog )

## cross-section data, error components frontier, translog, shifter
translogShift <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), shifterNames = "firmNo",
   data = front41Data )
print( translogShift )
coef( translogShift, which = "start" )
coef( translogShift, which = "ols" )
coef( translogShift, which = "grid" )
coef( translogShift )
coef( summary( translogShift ), which = "ols" )
coef( summary( translogShift ) )
round( vcov( translogShift ), 5 )
logLik( translogShift, which = "ols" )
logLik( translogShift )
nobs( translogShift )
print( summary( translogShift ) )
lrtest( translogShift )
round( efficiencies( translogShift ), 3 )
round( efficiencies( translogShift, asInData = TRUE ), 3 )
round( residuals( translogShift ), 4 )
round( residuals( translogShift, asInData = TRUE ), 4 )
translogShiftEla <- elas( translogShift )
print( translogShiftEla )
attributes( translogShiftEla )$variance
attributes( translogShiftEla )$stdDev
print.default( translogShift )

## cross-section data, efficiency effects frontier, translog
translogZvar <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), zNames = "firmNo",
   data = front41Data )
print( translogZvar )
coef( translogZvar, which = "start" )
coef( translogZvar, which = "ols" )
coef( translogZvar, which = "grid" )
coef( translogZvar )
coef( summary( translogZvar ), which = "ols" )
coef( summary( translogZvar ) )
round( vcov( translogZvar ), 5 )
logLik( translogZvar, which = "ols" )
logLik( translogZvar )
nobs( translogZvar )
print( summary( translogZvar ) )
print( summary( translogZvar, farrell = FALSE ) )
lrtest( translogZvar )
round( efficiencies( translogZvar, margEff = TRUE ), 3 )
round( efficiencies( translogZvar, asInData = TRUE, margEff = TRUE ), 3 )
round( efficiencies( translogZvar, farrell = FALSE, margEff = TRUE ), 3 )
round( efficiencies( translogZvar, asInData = TRUE, farrell = FALSE, margEff = TRUE ), 3 )
round( residuals( translogZvar ), 4 )
round( residuals( translogZvar, asInData = TRUE ), 4 )
translogZvarEla <- elas( translogZvar )
print( translogZvarEla ) 
attributes( translogZvarEla )$variance
attributes( translogZvarEla )$stdDev
print.default( translogZvar )


################################################
## endogenous variable (seemingly) NOT logged ##
################################################

## example data included in FRONTIER 4.1 (cross-section data)
## cross-section data, error components frontier
summary( Sa1, logDepVar = FALSE )
summary( Sa1, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( a1, logDepVar = FALSE ), 3 )
round( efficiencies( a1, asInData = TRUE, logDepVar = FALSE ), 3 )
round( efficiencies( a1, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( a1, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## cross-section data, error components frontier, truncNorm
summary( a2, logDepVar = FALSE )
round( efficiencies( a2, logDepVar = FALSE ), 3 )
round( efficiencies( a2, asInData = TRUE, logDepVar = FALSE ), 3 )

## cross-section data, efficiency effects frontier
summary( Saa1, logDepVar = FALSE )
summary( Saa1, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( aa1, logDepVar = FALSE, margEff = TRUE ), 3 )
round( efficiencies( aa1, asInData = TRUE, logDepVar = FALSE ), 3 )
round( efficiencies( aa1, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( aa1, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## cross-section data, efficiency effects frontier, zIntercept
summary( aa2, logDepVar = FALSE )
round( efficiencies( aa2, logDepVar = FALSE ), 3 )
round( efficiencies( aa2, asInData = TRUE, logDepVar = FALSE ), 3 )

## cross-section rice data, error components cost frontier
summary( Sdd1, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( dd1, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( dd1, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## cross-section rice data, error components cost frontier, truncNorm
summary( dd2, logDepVar = FALSE )
summary( dd2, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( dd2, logDepVar = FALSE ), 3 )
round( efficiencies( dd2, asInData = TRUE, logDepVar = FALSE ), 3 )
round( efficiencies( dd2, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( dd2, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## cross-section rice data, efficiency effects cost frontier
summary( Sdd5, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( dd5, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( dd5, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## cross-section rice data, efficiency effects cost frontier, zIntercept
summary( dd6, logDepVar = FALSE )
summary( dd6, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( dd6, logDepVar = FALSE ), 3 )
round( efficiencies( dd6, asInData = TRUE , logDepVar = FALSE ), 3 )
round( efficiencies( dd6, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( dd6, asInData = TRUE , logDepVar = FALSE, farrell = FALSE ), 3 )

## panel data, error components frontier
summary( Sb1, logDepVar = FALSE )
round( efficiencies( b1, logDepVar = FALSE ), 3 )
round( efficiencies( b1, asInData = TRUE, logDepVar = FALSE ), 3 )

## panel data, error components frontier, truncNorm
summary( b2, logDepVar = FALSE )
round( efficiencies( b2, logDepVar = FALSE ), 3 )
round( efficiencies( b2, asInData = TRUE, logDepVar = FALSE ), 3 )

## panel data, error components frontier, timeEffect
summary( b3, logDepVar = FALSE )
round( efficiencies( b3, logDepVar = FALSE ), 3 )
round( efficiencies( b3, asInData = TRUE, logDepVar = FALSE ), 3 )

## panel data, error components frontier, truncNorm, timeEffect
summary( b4, logDepVar = FALSE )
summary( b4, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( b4, logDepVar = FALSE ), 3 )
round( efficiencies( b4, asInData = TRUE, logDepVar = FALSE ), 3 )
round( efficiencies( b4, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( b4, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## panel data, efficiency effects frontier
summary( Sb5, logDepVar = FALSE )
round( efficiencies( b5, logDepVar = FALSE ), 3 )
round( efficiencies( b5, asInData = TRUE, logDepVar = FALSE ), 3 )

## panel data, efficiency effects frontier, zIntercept
summary( b6, logDepVar = FALSE )
summary( b6, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( b6, logDepVar = FALSE ), 3 )
round( efficiencies( b6, asInData = TRUE, logDepVar = FALSE ), 3 )
round( efficiencies( b6, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( b6, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## panel rice data, error components cost frontier
summary( Sd1, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d1, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d1, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## panel rice data, error components cost frontier, truncNorm
summary( d2, logDepVar = FALSE )
summary( d2, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d2, logDepVar = FALSE ), 3 )
round( efficiencies( d2, asInData = TRUE, logDepVar = FALSE ), 3 )
round( efficiencies( d2, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d2, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## panel rice data, error components cost frontier, timeEffect
summary( d3, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d3, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d3, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## panel rice data, error components cost frontier, truncNorm, timeEffect
summary( d4, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d4, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d4, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## panel rice data, efficiency effects cost frontier
summary( Sd5, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d5, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d5, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## panel rice data, efficiency effects cost frontier, zIntercept
summary( d6, logDepVar = FALSE )
summary( d6, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d6, logDepVar = FALSE ), 3 )
round( efficiencies( d6, asInData = TRUE, logDepVar = FALSE ), 3 )
round( efficiencies( d6, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d6, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## unbalanced panel data, error components frontier
summary( b1u, logDepVar = FALSE )
round( efficiencies( b1u, logDepVar = FALSE ), 3 )
round( efficiencies( b1u, asInData = TRUE, logDepVar = FALSE ), 3 )

## unbalanced panel data, error components frontier, truncNorm
summary( b2u, logDepVar = FALSE )
round( efficiencies( b2u, logDepVar = FALSE ), 3 )
round( efficiencies( b2u, asInData = TRUE, logDepVar = FALSE ), 3 )

## unbalanced panel data, error components frontier, timeEffect
summary( b3u, logDepVar = FALSE )
round( efficiencies( b3u, logDepVar = FALSE ), 3 )
round( efficiencies( b3u, asInData = TRUE, logDepVar = FALSE ), 3 )

## unbalanced panel data, error components frontier, truncNorm, timeEffect
summary( b4u, logDepVar = FALSE )
round( efficiencies( b4u, logDepVar = FALSE ), 3 )
round( efficiencies( b4u, asInData = TRUE, logDepVar = FALSE ), 3 )

## unbalanced panel data, efficiency effects frontier
summary( b5u, logDepVar = FALSE )
round( efficiencies( b5u, logDepVar = FALSE ), 3 )
round( efficiencies( b5u, asInData = TRUE, logDepVar = FALSE ), 3 )

## unbalanced panel data, efficiency effects frontier, zIntercept
summary( b6u, logDepVar = FALSE )
round( efficiencies( b6u, logDepVar = FALSE ), 3 )
round( efficiencies( b6u, asInData = TRUE, logDepVar = FALSE ), 3 )

## unbalanced panel rice data, error components cost frontier
summary( d1u, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d1u, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d1u, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## unbalanced panel rice data, error components cost frontier, truncNorm
summary( d2u, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d2u, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d2u, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## unbalanced panel rice data, error components cost frontier, timeEffect
summary( d3u, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d3u, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d3u, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## unbalanced panel rice data, error components cost frontier, truncNorm, timeEffect
summary( d4u, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d4u, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d4u, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## unbalanced panel rice data, efficiency effects cost frontier
summary( d5u, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d5u, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d5u, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )

## unbalanced panel rice data, efficiency effects cost frontier, zIntercept
summary( d6u, logDepVar = FALSE, farrell = FALSE )
round( efficiencies( d6u, logDepVar = FALSE, farrell = FALSE ), 3 )
round( efficiencies( d6u, asInData = TRUE, logDepVar = FALSE, farrell = FALSE ), 3 )


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
all.equal( a1a[-39], a1[-39], check.attributes = FALSE )
nobs( a1a )

## cross-section data, efficiency effects frontier
aa1a <- sfa( log( y ) ~ log( x1 ) + log( x2 ) | z1 - 1 )
all.equal( aa1a[-39], aa1[-39], check.attributes = FALSE )

## cross-section data, efficiency effects frontier, zIntercept
aa2a <- sfa( log( y ) ~ log( x1 ) + log( x2 ) | z1 )
all.equal( aa2a[-39], aa2[-39], check.attributes = FALSE )


##############################################
### estimations with 0 or 1 variable only ###
##############################################

## cross-section data, error components frontier
sa10 <- sfa( logOutput ~ 1, data = front41Data )
a10 <- frontier( "logOutput", NULL, data = front41Data )
print( sa10 )
all.equal( sa10[-39], a10[-39], check.attributes = FALSE )
nobs( sa10 )

sa11 <- sfa( logOutput ~ logLabour, data = front41Data )
a11 <- frontier( "logOutput", "logLabour", data = front41Data )
print( sa11 )
all.equal( sa11[-39], a11[-39], check.attributes = FALSE )
nobs( sa11 )

## cross-section data, efficiency effects frontier
saa10 <- sfa( logOutput ~ 1 | firmNo - 1, data = front41Data )
aa10 <- frontier( data = front41Data, "logOutput", NULL,
   zNames = "firmNo" )
print( saa10 )
all.equal( saa10[-39], aa10[-39] )
nobs( saa10 )

saa11 <- sfa( logOutput ~ logLabour | firmNo - 1, data = front41Data )
aa11 <- frontier( data = front41Data, "logOutput", "logLabour",
   zNames = "firmNo" )
print( saa11 )
all.equal( saa11[-39], aa11[-39] )
nobs( saa11 )


##############################################
##### evaluating log likelihood values #######
##############################################
options( digits = 9 )

## cross-section data, error components frontier
logLik( a1 )
logLik( a1, newParam = coef( a1 ) )
logLik( sa1, newParam = coef( sa1 ) )
logLik( Sa1, newParam = coef( a1 ) )

## cross-section data, error components frontier, truncNorm
logLik( a2 )
logLik( a2, newParam = coef( a2 ) )
logLik( sa2, newParam = coef( sa2 ) )

## cross-section data, error components frontier, truncNorm, starting values
logLik( a5 )
logLik( a5, newParam = coef( a5 ) )
logLik( sa5, newParam = coef( sa5 ) )

## cross-section data, efficiency effects frontier
logLik( aa1 )
logLik( aa1, newParam = coef( aa1 ) )
logLik( saa1, newParam = coef( saa1 ) )
logLik( Saa1, newParam = coef( aa1 ) )

## cross-section data, efficiency effects frontier, zIntercept
logLik( aa2 )
logLik( aa2, newParam = coef( aa2 ) )
logLik( saa2, newParam = coef( saa2 ) )

## cross-section data, efficiency effects frontier, zIntercept, starting values
logLik( aa5 )
logLik( aa5, newParam = coef( aa5 ) )
logLik( saa5, newParam = coef( saa5 ) )


## data set of rice producers in the Philippines

## cross-section rice data, error components frontier
logLik( bb1 )
logLik( bb1, newParam = coef( bb1 ) )
logLik( sbb1, newParam = coef( sbb1 ) )
logLik( Sbb1, newParam = coef( bb1 ) )

## cross-section rice data, error components frontier, truncNorm
logLik( bb2 )
logLik( bb2, newParam = coef( bb2 ) )
logLik( sbb2, newParam = coef( sbb2 ) )

## cross-section rice data, efficiency effects frontier
logLik( bb5 )
logLik( bb5, newParam = coef( bb5 ) )
logLik( sbb5, newParam = coef( sbb5 ) )
logLik( Sbb5, newParam = coef( bb5 ) )

## cross-section rice data, efficiency effects frontier, zIntercept
logLik( bb6 )
logLik( bb6, newParam = coef( bb6 ) )
logLik( sbb6, newParam = coef( sbb6 ) )

## cross-section rice data, error components frontier, truncNorm, starting values
logLik( bb7 )
logLik( bb7, newParam = coef( bb7 ) )
logLik( sbb7, newParam = coef( sbb7 ) )

## cross-section rice data, efficiency effects frontier, zIntercept, starting values
logLik( bb8 )
logLik( bb8, newParam = coef( bb8 ) )
logLik( sbb8, newParam = coef( sbb8 ) )


## Cost Frontier (with land as quasi-fixed input)
## cross-section rice data, error components cost frontier
logLik( dd1 )
logLik( dd1, newParam = coef( dd1 ) )
logLik( sdd1, newParam = coef( sdd1 ) )
logLik( Sdd1, newParam = coef( dd1 ) )

## cross-section rice data, error components cost frontier, truncNorm
logLik( dd2 )
logLik( dd2, newParam = coef( dd2 ) )
logLik( sdd2, newParam = coef( sdd2 ) )

## cross-section rice data, efficiency effects cost frontier
logLik( dd5 )
logLik( dd5, newParam = coef( dd5 ) )
logLik( sdd5, newParam = coef( sdd5 ) )
logLik( Sdd5, newParam = coef( dd5 ) )

## cross-section rice data, efficiency effects cost frontier, zIntercept
logLik( dd6 )
logLik( dd6, newParam = coef( dd6 ) )
logLik( sdd6, newParam = coef( sdd6 ) )


## panel data

## panel data, error components frontier
logLik( b1 )
logLik( b1, newParam = coef( b1 ) )
logLik( sb1, newParam = coef( sb1 ) )
logLik( Sb1, newParam = coef( b1 ) )

## panel data, error components frontier, truncNorm
logLik( b2 )
logLik( b2, newParam = coef( b2 ) )
logLik( sb2, newParam = coef( sb2 ) )

## panel data, error components frontier, timeEffect
logLik( b3 )
logLik( b3, newParam = coef( b3 ) )
logLik( sb3, newParam = coef( sb3 ) )

## panel data, error components frontier, truncNorm, timeEffect
logLik( b4 )
logLik( b4, newParam = coef( b4 ) )
logLik( sb4, newParam = coef( sb4 ) )

## panel data, efficiency effects frontier
logLik( b5 )
logLik( b5, newParam = coef( b5 ) )
logLik( sb5, newParam = coef( sb5 ) )
logLik( Sb5, newParam = coef( b5 ) )

## panel data, efficiency effects frontier, zIntercept
logLik( b6 )
logLik( b6, newParam = coef( b6 ) )
logLik( sb6, newParam = coef( sb6 ) )

## panel data, error components frontier, truncNorm, timeEffect, starting values
logLik( b7 )
logLik( b7, newParam = coef( b7 ) )
logLik( sb7, newParam = coef( sb7 ) )

## panel data, efficiency effects frontier, zIntercept, starting values
logLik( b8 )
logLik( b8, newParam = coef( b8 ) )
logLik( sb8, newParam = coef( sb8 ) )


## Cost Frontier (with land as quasi-fixed input)
## panel rice data, error components cost frontier
logLik( d1 )
logLik( d1, newParam = coef( d1 ) )
logLik( sd1, newParam = coef( sd1 ) )
logLik( Sd1, newParam = coef( d1 ) )

## panel rice data, error components cost frontier, truncNorm
logLik( d2 )
logLik( d2, newParam = coef( d2 ) )
logLik( sd2, newParam = coef( sd2 ) )

## panel rice data, error components cost frontier, timeEffect
logLik( d3 )
logLik( d3, newParam = coef( d3 ) )
logLik( sd3, newParam = coef( sd3 ) )

## panel rice data, error components cost frontier, truncNorm, timeEffect
logLik( d4 )
logLik( d4, newParam = coef( d4 ) )
logLik( sd4, newParam = coef( sd4 ) )

## panel rice data, efficiency effects cost frontier
logLik( d5 )
logLik( d5, newParam = coef( d5 ) )
logLik( sd5, newParam = coef( sd5 ) )
logLik( Sd5, newParam = coef( d5 ) )

## panel rice data, efficiency effects cost frontier, zIntercept
logLik( d6 )
logLik( d6, newParam = coef( d6 ) )
logLik( sd6, newParam = coef( sd6 ) )


## translog frontiers
## cross-section data, error components frontier, translog
logLik( translog )
logLik( translog, newParam = coef( translog ) )

## cross-section data, error components frontier, translog, shifter
logLik( translogShift )
logLik( translogShift, newParam = coef( translogShift ) )

## cross-section data, efficiency effects frontier, translog
logLik( translogZvar )
logLik( translogZvar, newParam = coef( translogZvar ) )


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
