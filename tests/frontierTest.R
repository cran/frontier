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
all.equal( Sa1[-33], sa1[-33], check.attributes = FALSE )
a1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ) )
all.equal( sa1[-33], a1[-33] )
print( a1 )
coef( a1, which = "start" )
coef( a1, which = "ols" )
coef( a1, which = "grid" )
coef( a1 )
coef( summary( a1 ), which = "ols" )
coef( summary( a1 ) )
vcov( a1 )
logLik( a1, which = "ols" )
logLik( a1, which = "grid" )
logLik( a1 )
print( summary( a1 ) )
efficiencies( a1 )
efficiencies( a1, asInData = TRUE )
residuals( a1 )
residuals( a1, asInData = TRUE )
print.default( a1 )

## cross-section data, error components frontier, truncNorm
sa2 <- sfa( logOutput ~ logCapital + logLabour, data = front41Data,
   truncNorm = TRUE )
a2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE )
all.equal( sa2[-33], a2[-33] )
print( a2 )
coef( a2, which = "start" )
coef( a2, which = "ols" )
coef( a2, which = "grid" )
coef( a2 )
coef( summary( a2 ), which = "ols" )
coef( summary( a2 ) )
vcov( a2 )
logLik( a2, which = "ols" )
logLik( a2 )
print( summary( a2 ) )
efficiencies( a2 )
efficiencies( a2, asInData = TRUE )
residuals( a2 )
residuals( a2, asInData = TRUE )
print.default( a2 )

## cross-section data, error components frontier, truncNorm, starting values
sa5 <- sfa( logOutput ~ logCapital + logLabour, data = front41Data,
   truncNorm = TRUE, startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
all.equal( sa5[-33], a5[-33] )
print( a5 )
coef( a5, which = "start" )
coef( a5, which = "ols" )
coef( a5, which = "grid" )
coef( a5 )
coef( summary( a5 ), which = "ols" )
coef( summary( a5 ) )
vcov( a5 )
logLik( a5, which = "ols" )
logLik( a5 )
print( summary( a5 ) )
efficiencies( a5 )
efficiencies( a5, asInData = TRUE )
print.default( a5 )

## cross-section data, efficiency effects frontier
saa1 <- sfa( logOutput ~ logCapital + logLabour | firmNo - 1,
   data = front41Data )
Saa1 <- sfa( log( output ) ~ log( capital ) + log( labour ) | firmNo - 1,
   data = front41Data )
all.equal( Saa1[-33], saa1[-33], check.attributes = FALSE )
aa1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo" )
all.equal( saa1[-33], aa1[-33] )
print( aa1 )
coef( aa1, which = "start" )
coef( aa1, which = "ols" )
coef( aa1, which = "grid" )
coef( aa1 )
coef( summary( aa1 ), which = "ols" )
coef( summary( aa1 ) )
vcov( aa1 )
print( summary( aa1 ) )
efficiencies( aa1 )
efficiencies( aa1, asInData = TRUE )
residuals( aa1 )
residuals( aa1, asInData = TRUE )
print.default( aa1 )

## cross-section data, efficiency effects frontier, zIntercept
saa2 <- sfa( logOutput ~ logCapital + logLabour | firmNo,
   data = front41Data )
aa2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE )
all.equal( saa2[-33], aa2[-33] )
print( aa2 )
coef( aa2, which = "start" )
coef( aa2, which = "ols" )
coef( aa2, which = "grid" )
coef( aa2 )
coef( summary( aa2 ), which = "ols" )
coef( summary( aa2 ) )
vcov( aa2 )
print( summary( aa2 ) )
efficiencies( aa2 )
efficiencies( aa2, asInData = TRUE )
residuals( aa2 )
residuals( aa2, asInData = TRUE )
print.default( aa2 )

## cross-section data, efficiency effects frontier, zIntercept, starting values
saa5 <- sfa( logOutput ~ logCapital + logLabour | firmNo,
   data = front41Data, startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
aa5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE,
   startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
all.equal( saa5[-33], aa5[-33] )
print( aa5 )
coef( aa5, which = "start" )
coef( aa5, which = "ols" )
coef( aa5, which = "grid" )
coef( aa5 )
coef( summary( aa5 ), which = "ols" )
coef( summary( aa5 ) )
vcov( aa5 )
print( summary( aa5 ) )
efficiencies( aa5 )
efficiencies( aa5, asInData = TRUE )
print.default( aa5 )


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
all.equal( Sbb1[-33], sbb1[-33], check.attributes = FALSE )
bb1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
all.equal( sbb1[-33], bb1[-33] )
print( bb1 )
coef( bb1, which = "start" )
coef( bb1, which = "ols" )
coef( bb1, which = "grid" )
coef( bb1 )
coef( summary( bb1 ), which = "ols" )
coef( summary( bb1 ) )
vcov( bb1 )
print( summary( bb1 ) )
efficiencies( bb1 )
efficiencies( bb1, asInData = TRUE )
residuals( bb1 )
residuals( bb1, asInData = TRUE )
print.default( bb1 )

## cross-section rice data, error components frontier, truncNorm
sbb2 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhil,
   truncNorm = TRUE )
bb2 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
all.equal( sbb2[-33], bb2[-33] )
print( bb2 )
coef( bb2, which = "start" )
coef( bb2, which = "ols" )
coef( bb2, which = "grid" )
coef( bb2 )
coef( summary( bb2 ), which = "ols" )
coef( summary( bb2 ) )
vcov( bb2 )
print( summary( bb2 ) )
efficiencies( bb2 )
efficiencies( bb2, asInData = TRUE )
residuals( bb2 )
residuals( bb2, asInData = TRUE )
print.default( bb2 )

## cross-section rice data, efficiency effects frontier
sbb5 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhil )
Sbb5 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = riceProdPhil )
all.equal( Sbb5[-33], sbb5[-33], check.attributes = FALSE )
bb5 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
all.equal( sbb5[-33], bb5[-33] )
print( bb5 )
coef( bb5, which = "start" )
coef( bb5, which = "ols" )
coef( bb5, which = "grid" )
coef( bb5 )
coef( summary( bb5 ), which = "ols" )
coef( summary( bb5 ) )
vcov( bb5 )
print( summary( bb5 ) )
efficiencies( bb5 )
efficiencies( bb5, asInData = TRUE )
residuals( bb5 )
residuals( bb5, asInData = TRUE )
print.default( bb5 )

## cross-section rice data, efficiency effects frontier, zIntercept
sbb6 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhil )
bb6 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
all.equal( sbb6[-33], bb6[-33] )
print( bb6 )
coef( bb6, which = "start" )
coef( bb6, which = "ols" )
coef( bb6, which = "grid" )
coef( bb6 )
coef( summary( bb6 ), which = "ols" )
coef( summary( bb6 ) )
vcov( bb6 )
print( summary( bb6 ) )
efficiencies( bb6 )
efficiencies( bb6, asInData = TRUE )
residuals( bb6 )
residuals( bb6, asInData = TRUE )
print.default( bb6 )

## cross-section rice data, error components frontier, truncNorm, starting values
sbb7 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhil,
   truncNorm = TRUE, startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
bb7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
all.equal( sbb7[-33], bb7[-33] )
print( bb7 )
coef( bb7, which = "start" )
coef( bb7, which = "ols" )
coef( bb7, which = "grid" )
coef( bb7 )
coef( summary( bb7 ), which = "ols" )
coef( summary( bb7 ) )
vcov( bb7 )
print( summary( bb7 ) )
efficiencies( bb7 )
efficiencies( bb7, asInData = TRUE )
print.default( bb7 )

## cross-section rice data, efficiency effects frontier, zIntercept, starting values
sbb8 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhil,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
bb8 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
all.equal( sbb8[-33], bb8[-33] )
print( bb8 )
coef( bb8, which = "start" )
coef( bb8, which = "ols" )
coef( bb8, which = "grid" )
coef( bb8 )
coef( summary( bb8 ), which = "ols" )
coef( summary( bb8 ) )
vcov( bb8 )
print( summary( bb8 ) )
efficiencies( bb8 )
efficiencies( bb8, asInData = TRUE )
print.default( bb8 )


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
all.equal( Sdd1[-33], sdd1[-33], check.attributes = FALSE )
dd1 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE )
all.equal( sdd1[-33], dd1[-33] )
print( dd1 )
coef( dd1, which = "start" )
coef( dd1, which = "ols" )
coef( dd1, which = "grid" )
coef( dd1 )
coef( summary( dd1 ), which = "ols" )
coef( summary( dd1 ) )
vcov( dd1 )
print( summary( dd1 ) )
efficiencies( dd1 )
efficiencies( dd1, asInData = TRUE )
residuals( dd1 )
residuals( dd1, asInData = TRUE )
print.default( dd1 )

## cross-section rice data, error components cost frontier, truncNorm
sdd2 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhil,
   ineffDecrease = FALSE, truncNorm = TRUE )
dd2 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE, truncNorm = TRUE )
all.equal( sdd2[-33], dd2[-33] )
print( dd2 )
coef( dd2, which = "start" )
coef( dd2, which = "ols" )
coef( dd2, which = "grid" )
coef( dd2 )
coef( summary( dd2 ), which = "ols" )
coef( summary( dd2 ) )
vcov( dd2 )
print( summary( dd2 ) )
efficiencies( dd2 )
efficiencies( dd2, asInData = TRUE )
residuals( dd2 )
residuals( dd2, asInData = TRUE )
print.default( dd2 )

## cross-section rice data, efficiency effects cost frontier
sdd5 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhil, ineffDecrease = FALSE )
Sdd5 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | EDYRS + BANRAT - 1, data = riceProdPhil, ineffDecrease = FALSE )
all.equal( Sdd5[-33], sdd5[-33], check.attributes = FALSE )
dd5 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE )
all.equal( sdd5[-33], dd5[-33] )
print( dd5 )
coef( dd5, which = "start" )
coef( dd5, which = "ols" )
coef( dd5, which = "grid" )
coef( dd5 )
coef( summary( dd5 ), which = "ols" )
coef( summary( dd5 ) )
vcov( dd5 )
print( summary( dd5 ) )
efficiencies( dd5 )
efficiencies( dd5, asInData = TRUE )
residuals( dd5 )
residuals( dd5, asInData = TRUE )
print.default( dd5 )

## cross-section rice data, efficiency effects cost frontier, zIntercept
sdd6 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhil, ineffDecrease = FALSE )
dd6 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE, zIntercept = TRUE )
all.equal( sdd6[-33], dd6[-33] )
print( dd6 )
coef( dd6, which = "start" )
coef( dd6, which = "ols" )
coef( dd6, which = "grid" )
coef( dd6 )
coef( summary( dd6 ), which = "ols" )
coef( summary( dd6 ) )
vcov( dd6 )
print( summary( dd6 ) )
efficiencies( dd6 )
efficiencies( dd6, asInData = TRUE )
residuals( dd6 )
residuals( dd6, asInData = TRUE )
print.default( dd6 )


## panel data
riceProdPhil$farm <- paste( "F_", ifelse( riceProdPhil$FMERCODE > 9, "", "0" ),
   riceProdPhil$FMERCODE, sep = "" )
riceProdPhil$year <- riceProdPhil$YEARDUM + 1998
riceProdPhilPanel <- plm.data( riceProdPhil, c( "farm", "year" ) )

## panel data, error components frontier
sb1 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel )
Sb1 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = riceProdPhilPanel )
all.equal( Sb1[-33], sb1[-33], check.attributes = FALSE )
b1 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
all.equal( sb1[-33], b1[-33] )
print( b1 )
coef( b1, which = "start" )
coef( b1, which = "ols" )
coef( b1, which = "grid" )
coef( b1 )
coef( summary( b1 ), which = "ols" )
coef( summary( b1 ) )
vcov( b1 )
logLik( b1, which = "ols" )
logLik( b1 )
print( summary( b1 ) )
efficiencies( b1 )
efficiencies( b1, asInData = TRUE )
residuals( b1 )
residuals( b1, asInData = TRUE )
print.default( b1 )

## panel data, error components frontier, truncNorm
sb2 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE )
b2 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
all.equal( sb2[-33], b2[-33] )
print( b2 )
coef( b2, which = "start" )
coef( b2, which = "ols" )
coef( b2, which = "grid" )
coef( b2 )
coef( summary( b2 ), which = "ols" )
coef( summary( b2 ) )
vcov( b2 )
logLik( b2, which = "ols" )
logLik( b2 )
print( summary( b2 ) )
efficiencies( b2 )
efficiencies( b2, asInData = TRUE )
residuals( b2 )
residuals( b2, asInData = TRUE )
print.default( b2 )

## panel data, error components frontier, timeEffect
sb3 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   timeEffect = TRUE )
b3 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   timeEffect = TRUE )
all.equal( sb3[-33], b3[-33] )
print( b3 )
coef( b3, which = "start" )
coef( b3, which = "ols" )
coef( b3, which = "grid" )
coef( b3 )
coef( summary( b3 ), which = "ols" )
coef( summary( b3 ) )
vcov( b3 )
logLik( b3, which = "ols" )
logLik( b3 )
print( summary( b3 ) )
efficiencies( b3 )
efficiencies( b3, asInData = TRUE )
residuals( b3 )
residuals( b3, asInData = TRUE )
print.default( b3 )

## panel data, error components frontier, truncNorm, timeEffect
sb4 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE, timeEffect = TRUE )
b4 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE )
all.equal( sb4[-33], b4[-33] )
print( b4 )
coef( b4, which = "start" )
coef( b4, which = "ols" )
coef( b4, which = "grid" )
coef( b4 )
coef( summary( b4 ), which = "ols" )
coef( summary( b4 ) )
vcov( b4 )
logLik( b4, which = "ols" )
logLik( b4 )
print( summary( b4 ) )
efficiencies( b4 )
efficiencies( b4, asInData = TRUE )
residuals( b4 )
residuals( b4, asInData = TRUE )
print.default( b4 )

## panel data, efficiency effects frontier
sb5 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhilPanel )
Sb5 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = riceProdPhilPanel )
all.equal( Sb5[-33], sb5[-33], check.attributes = FALSE )
b5 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
all.equal( sb5[-33], b5[-33] )
print( b5 )
coef( b5, which = "start" )
coef( b5, which = "ols" )
coef( b5, which = "grid" )
coef( b5 )
coef( summary( b5 ), which = "ols" )
coef( summary( b5 ) )
vcov( b5 )
logLik( b5, which = "ols" )
logLik( b5 )
print( summary( b5 ) )
efficiencies( b5 )
efficiencies( b5, asInData = TRUE )
residuals( b5 )
residuals( b5, asInData = TRUE )
print.default( b5 )

## panel data, efficiency effects frontier, zIntercept
sb6 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanel )
b6 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
all.equal( sb6[-33], b6[-33] )
print( b6 )
coef( b6, which = "start" )
coef( b6, which = "ols" )
coef( b6, which = "grid" )
coef( b6 )
coef( summary( b6 ), which = "ols" )
coef( summary( b6 ) )
vcov( b6 )
logLik( b6, which = "ols" )
logLik( b6 )
print( summary( b6 ) )
efficiencies( b6 )
efficiencies( b6, asInData = TRUE )
residuals( b6 )
residuals( b6, asInData = TRUE )
print.default( b6 )

## panel data, error components frontier, truncNorm, timeEffect, starting values
sb7 <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanel,
   truncNorm = TRUE, timeEffect = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
b7 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
all.equal( sb7[-33], b7[-33] )
print( b7 )
coef( b7, which = "start" )
coef( b7, which = "ols" )
coef( b7, which = "grid" )
coef( b7 )
coef( summary( b7 ), which = "ols" )
coef( summary( b7 ) )
vcov( b7 )
logLik( b7, which = "ols" )
logLik( b7 )
print( summary( b7 ) )
efficiencies( b7 )
efficiencies( b7, asInData = TRUE )
print.default( b7 )

## panel data, efficiency effects frontier, zIntercept, starting values
sb8 <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanel,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.3, -0.01, -0.4, 0.2, 0.8 ) )
b8 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.3, -0.01, -0.4, 0.2, 0.8 ) )
all.equal( sb8[-33], b8[-33] )
print( b8 )
coef( b8, which = "start" )
coef( b8, which = "ols" )
coef( b8, which = "grid" )
coef( b8 )
coef( summary( b8 ), which = "ols" )
coef( summary( b8 ) )
vcov( b8 )
logLik( b8, which = "ols" )
logLik( b8 )
print( summary( b8 ) )
efficiencies( b8 )
efficiencies( b8, asInData = TRUE )
print.default( b8 )


## Cost Frontier (with land as quasi-fixed input)
## panel rice data, error components cost frontier
sd1 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE )
Sd1 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ), data = riceProdPhilPanel, ineffDecrease = FALSE )
all.equal( Sd1[-33], sd1[-33], check.attributes = FALSE )
d1 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE )
all.equal( sd1[-33], d1[-33] )
print( d1 )
coef( d1, which = "start" )
coef( d1, which = "ols" )
coef( d1, which = "grid" )
coef( d1 )
coef( summary( d1 ), which = "ols" )
coef( summary( d1 ) )
vcov( d1 )
print( summary( d1 ) )
efficiencies( d1 )
efficiencies( d1, asInData = TRUE )
residuals( d1 )
residuals( d1, asInData = TRUE )
print.default( d1 )

## panel rice data, error components cost frontier, truncNorm
sd2 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, truncNorm = TRUE )
d2 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, truncNorm = TRUE )
all.equal( sd2[-33], d2[-33] )
print( d2 )
coef( d2, which = "start" )
coef( d2, which = "ols" )
coef( d2, which = "grid" )
coef( d2 )
coef( summary( d2 ), which = "ols" )
coef( summary( d2 ) )
vcov( d2 )
print( summary( d2 ) )
efficiencies( d2 )
efficiencies( d2, asInData = TRUE )
residuals( d2 )
residuals( d2, asInData = TRUE )
print.default( d2 )

## panel rice data, error components cost frontier, timeEffect
sd3 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, timeEffect = TRUE )
d3 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, timeEffect = TRUE )
all.equal( sd3[-33], d3[-33] )
print( d3 )
coef( d3, which = "start" )
coef( d3, which = "ols" )
coef( d3, which = "grid" )
coef( d3 )
coef( summary( d3 ), which = "ols" )
coef( summary( d3 ) )
vcov( d3 )
print( summary( d3 ) )
efficiencies( d3 )
efficiencies( d3, asInData = TRUE )
residuals( d3 )
residuals( d3, asInData = TRUE )
print.default( d3 )

## panel rice data, error components cost frontier, truncNorm, timeEffect
sd4 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanel,
   ineffDecrease = FALSE, truncNorm = TRUE, timeEffect = TRUE )
d4 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhilPanel, ineffDecrease = FALSE, truncNorm = TRUE,
   timeEffect = TRUE )
all.equal( sd4[-33], d4[-33] )
print( d4 )
coef( d4, which = "start" )
coef( d4, which = "ols" )
coef( d4, which = "grid" )
coef( d4 )
coef( summary( d4 ), which = "ols" )
coef( summary( d4 ) )
vcov( d4 )
print( summary( d4 ) )
efficiencies( d4 )
efficiencies( d4, asInData = TRUE )
residuals( d4 )
residuals( d4, asInData = TRUE )
print.default( d4 )

## panel rice data, efficiency effects cost frontier
sd5 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhilPanel, ineffDecrease = FALSE )
Sd5 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ) | EDYRS + BANRAT - 1, data = riceProdPhilPanel,
   ineffDecrease = FALSE )
all.equal( Sd5[-33], sd5[-33], check.attributes = FALSE )
d5 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhilPanel,
   ineffDecrease = FALSE )
all.equal( sd5[-33], d5[-33] )
print( d5 )
coef( d5, which = "start" )
coef( d5, which = "ols" )
coef( d5, which = "grid" )
coef( d5 )
coef( summary( d5 ), which = "ols" )
coef( summary( d5 ) )
vcov( d5 )
print( summary( d5 ) )
efficiencies( d5 )
efficiencies( d5, asInData = TRUE )
residuals( d5 )
residuals( d5, asInData = TRUE )
print.default( d5 )

## panel rice data, efficiency effects cost frontier, zIntercept
sd6 <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhilPanel, ineffDecrease = FALSE )
d6 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhilPanel,
   ineffDecrease = FALSE, zIntercept = TRUE )
all.equal( sd6[-33], d6[-33] )
print( d6 )
coef( d6, which = "start" )
coef( d6, which = "ols" )
coef( d6, which = "grid" )
coef( d6 )
coef( summary( d6 ), which = "ols" )
coef( summary( d6 ) )
vcov( d6 )
print( summary( d6 ) )
efficiencies( d6 )
efficiencies( d6, asInData = TRUE )
residuals( d6 )
residuals( d6, asInData = TRUE )
print.default( d6 )


## unbalanced panel data
set.seed( 321 )
riceProdPhilPanelUnb <- riceProdPhilPanel[ -c(3,5,111,222), ]

## unbalanced panel data, error components frontier
b1u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb )
print( b1u )
print( summary( b1u ) )
efficiencies( b1u )
efficiencies( b1u, asInData = TRUE )
residuals( b1u )
residuals( b1u, asInData = TRUE )
print.default( b1u )

## unbalanced panel data, error components frontier, truncNorm
b2u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   truncNorm = TRUE )
print( b2u )
print( summary( b2u ) )
efficiencies( b2u )
efficiencies( b2u, asInData = TRUE )
residuals( b2u )
residuals( b2u, asInData = TRUE )
print.default( b2u )

## unbalanced panel data, error components frontier, timeEffect
b3u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   timeEffect = TRUE )
print( b3u )
print( summary( b3u ) )
efficiencies( b3u )
efficiencies( b3u, asInData = TRUE )
residuals( b3u )
residuals( b3u, asInData = TRUE )
print.default( b3u )

## unbalanced panel data, error components frontier, truncNorm, timeEffect
b4u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   truncNorm = TRUE, timeEffect = TRUE )
print( b4u )
print( summary( b4u ) )
efficiencies( b4u )
efficiencies( b4u, asInData = TRUE )
residuals( b4u )
residuals( b4u, asInData = TRUE )
print.default( b4u )

## unbalanced panel data, efficiency effects frontier
b5u <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT - 1,
   data = riceProdPhilPanelUnb )
print( b5u )
print( summary( b5u ) )
efficiencies( b5u )
efficiencies( b5u, asInData = TRUE )
residuals( b5u )
residuals( b5u, asInData = TRUE )
print.default( b5u )

## unbalanced panel data, efficiency effects frontier, zIntercept
b6u <- sfa( lPROD ~ lAREA + lLABOR + lNPK | EDYRS + BANRAT,
   data = riceProdPhilPanelUnb )
print( b6u )
print( summary( b6u ) )
efficiencies( b6u )
efficiencies( b6u, asInData = TRUE )
residuals( b6u )
residuals( b6u, asInData = TRUE )
print.default( b6u )

## unbalanced panel rice data, error components cost frontier
d1u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE )
print( d1u )
print( summary( d1u ) )
efficiencies( d1u )
efficiencies( d1u, asInData = TRUE )
residuals( d1u )
residuals( d1u, asInData = TRUE )
print.default( d1u )

## unbalanced panel rice data, error components cost frontier, truncNorm
d2u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, truncNorm = TRUE )
print( d2u )
print( summary( d2u ) )
efficiencies( d2u )
efficiencies( d2u, asInData = TRUE )
residuals( d2u )
residuals( d2u, asInData = TRUE )
print.default( d2u )

## unbalanced panel rice data, error components cost frontier, timeEffect
d3u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, timeEffect = TRUE )
print( d3u )
print( summary( d3u ) )
efficiencies( d3u )
efficiencies( d3u, asInData = TRUE )
residuals( d3u )
residuals( d3u, asInData = TRUE )
print.default( d3u )

## unbalanced panel rice data, error components cost frontier, truncNorm, timeEffect
d4u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP, data = riceProdPhilPanelUnb,
   ineffDecrease = FALSE, truncNorm = TRUE, timeEffect = TRUE )
print( d4u )
print( summary( d4u ) )
efficiencies( d4u )
efficiencies( d4u, asInData = TRUE )
residuals( d4u )
residuals( d4u, asInData = TRUE )
print.default( d4u )

## unbalanced panel rice data, efficiency effects cost frontier
d5u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT - 1,
   data = riceProdPhilPanelUnb, ineffDecrease = FALSE )
print( d5u )
print( summary( d5u ) )
efficiencies( d5u )
efficiencies( d5u, asInData = TRUE )
residuals( d5u )
residuals( d5u, asInData = TRUE )
print.default( d5u )

## unbalanced panel rice data, efficiency effects cost frontier, zIntercept
d6u <- sfa( lCost ~ lPROD + lAREA + lLABORP + lNPKP | EDYRS + BANRAT,
   data = riceProdPhilPanelUnb, ineffDecrease = FALSE )
print( d6u )
print( summary( d6u ) )
efficiencies( d6u )
efficiencies( d6u, asInData = TRUE )
residuals( d6u )
residuals( d6u, asInData = TRUE )
print.default( d6u )


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
vcov( translog )
logLik( translog, which = "ols" )
logLik( translog )
print( summary( translog ) )
efficiencies( translog )
efficiencies( translog, asInData = TRUE )
residuals( translog )
residuals( translog, asInData = TRUE )
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
vcov( translogShift )
logLik( translogShift, which = "ols" )
logLik( translogShift )
print( summary( translogShift ) )
efficiencies( translogShift )
efficiencies( translogShift, asInData = TRUE )
residuals( translogShift )
residuals( translogShift, asInData = TRUE )
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
vcov( translogZvar )
logLik( translogZvar, which = "ols" )
logLik( translogZvar )
print( summary( translogZvar ) )
efficiencies( translogZvar )
efficiencies( translogZvar, asInData = TRUE )
residuals( translogZvar )
residuals( translogZvar, asInData = TRUE )
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
efficiencies( a1, logDepVar = FALSE )
efficiencies( a1, asInData = TRUE, logDepVar = FALSE )

## cross-section data, error components frontier, truncNorm
summary( a2, logDepVar = FALSE )
efficiencies( a2, logDepVar = FALSE )
efficiencies( a2, asInData = TRUE, logDepVar = FALSE )

## cross-section data, efficiency effects frontier
summary( Saa1, logDepVar = FALSE )
efficiencies( aa1, logDepVar = FALSE )
efficiencies( aa1, asInData = TRUE, logDepVar = FALSE )

## cross-section data, efficiency effects frontier, zIntercept
summary( aa2, logDepVar = FALSE )
efficiencies( aa2, logDepVar = FALSE )
efficiencies( aa2, asInData = TRUE, logDepVar = FALSE )

## cross-section rice data, error components cost frontier
summary( Sdd1, logDepVar = FALSE )
efficiencies( dd1, logDepVar = FALSE )
efficiencies( dd1, asInData = TRUE, logDepVar = FALSE )

## cross-section rice data, error components cost frontier, truncNorm
summary( dd2, logDepVar = FALSE )
efficiencies( dd2, logDepVar = FALSE )
efficiencies( dd2, asInData = TRUE, logDepVar = FALSE )

## cross-section rice data, efficiency effects cost frontier
summary( Sdd5, logDepVar = FALSE )
efficiencies( dd5, logDepVar = FALSE )
efficiencies( dd5, asInData = TRUE, logDepVar = FALSE )

## cross-section rice data, efficiency effects cost frontier, zIntercept
summary( dd6, logDepVar = FALSE )
efficiencies( dd6, logDepVar = FALSE )
efficiencies( dd6, asInData = TRUE , logDepVar = FALSE)

## panel data, error components frontier
summary( Sb1, logDepVar = FALSE )
efficiencies( b1, logDepVar = FALSE )
efficiencies( b1, asInData = TRUE, logDepVar = FALSE )

## panel data, error components frontier, truncNorm
summary( b2, logDepVar = FALSE )
efficiencies( b2, logDepVar = FALSE )
efficiencies( b2, asInData = TRUE, logDepVar = FALSE )

## panel data, error components frontier, timeEffect
summary( b3, logDepVar = FALSE )
efficiencies( b3, logDepVar = FALSE )
efficiencies( b3, asInData = TRUE, logDepVar = FALSE )

## panel data, error components frontier, truncNorm, timeEffect
summary( b4, logDepVar = FALSE )
efficiencies( b4, logDepVar = FALSE )
efficiencies( b4, asInData = TRUE, logDepVar = FALSE )

## panel data, efficiency effects frontier
summary( Sb5, logDepVar = FALSE )
efficiencies( b5, logDepVar = FALSE )
efficiencies( b5, asInData = TRUE, logDepVar = FALSE )

## panel data, efficiency effects frontier, zIntercept
summary( b6, logDepVar = FALSE )
efficiencies( b6, logDepVar = FALSE )
efficiencies( b6, asInData = TRUE, logDepVar = FALSE )

## panel rice data, error components cost frontier
summary( Sd1, logDepVar = FALSE )
efficiencies( d1, logDepVar = FALSE )
efficiencies( d1, asInData = TRUE, logDepVar = FALSE )

## panel rice data, error components cost frontier, truncNorm
summary( d2, logDepVar = FALSE )
efficiencies( d2, logDepVar = FALSE )
efficiencies( d2, asInData = TRUE, logDepVar = FALSE )

## panel rice data, error components cost frontier, timeEffect
summary( d3, logDepVar = FALSE )
efficiencies( d3, logDepVar = FALSE )
efficiencies( d3, asInData = TRUE, logDepVar = FALSE )

## panel rice data, error components cost frontier, truncNorm, timeEffect
summary( d4, logDepVar = FALSE )
efficiencies( d4, logDepVar = FALSE )
efficiencies( d4, asInData = TRUE, logDepVar = FALSE )

## panel rice data, efficiency effects cost frontier
summary( Sd5, logDepVar = FALSE )
efficiencies( d5, logDepVar = FALSE )
efficiencies( d5, asInData = TRUE, logDepVar = FALSE )

## panel rice data, efficiency effects cost frontier, zIntercept
summary( d6, logDepVar = FALSE )
efficiencies( d6, logDepVar = FALSE )
efficiencies( d6, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel data, error components frontier
summary( b1u, logDepVar = FALSE )
efficiencies( b1u, logDepVar = FALSE )
efficiencies( b1u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel data, error components frontier, truncNorm
summary( b2u, logDepVar = FALSE )
efficiencies( b2u, logDepVar = FALSE )
efficiencies( b2u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel data, error components frontier, timeEffect
summary( b3u, logDepVar = FALSE )
efficiencies( b3u, logDepVar = FALSE )
efficiencies( b3u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel data, error components frontier, truncNorm, timeEffect
summary( b4u, logDepVar = FALSE )
efficiencies( b4u, logDepVar = FALSE )
efficiencies( b4u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel data, efficiency effects frontier
summary( b5u, logDepVar = FALSE )
efficiencies( b5u, logDepVar = FALSE )
efficiencies( b5u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel data, efficiency effects frontier, zIntercept
summary( b6u, logDepVar = FALSE )
efficiencies( b6u, logDepVar = FALSE )
efficiencies( b6u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel rice data, error components cost frontier
summary( d1u, logDepVar = FALSE )
efficiencies( d1u, logDepVar = FALSE )
efficiencies( d1u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel rice data, error components cost frontier, truncNorm
summary( d2u, logDepVar = FALSE )
efficiencies( d2u, logDepVar = FALSE )
efficiencies( d2u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel rice data, error components cost frontier, timeEffect
summary( d3u, logDepVar = FALSE )
efficiencies( d3u, logDepVar = FALSE )
efficiencies( d3u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel rice data, error components cost frontier, truncNorm, timeEffect
summary( d4u, logDepVar = FALSE )
efficiencies( d4u, logDepVar = FALSE )
efficiencies( d4u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel rice data, efficiency effects cost frontier
summary( d5u, logDepVar = FALSE )
efficiencies( d5u, logDepVar = FALSE )
efficiencies( d5u, asInData = TRUE, logDepVar = FALSE )

## unbalanced panel rice data, efficiency effects cost frontier, zIntercept
summary( d6u, logDepVar = FALSE )
efficiencies( d6u, logDepVar = FALSE )
efficiencies( d6u, asInData = TRUE, logDepVar = FALSE )


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
all.equal( a1a[-33], a1[-33], check.attributes = FALSE )

## cross-section data, efficiency effects frontier
aa1a <- sfa( log( y ) ~ log( x1 ) + log( x2 ) | z1 - 1 )
all.equal( aa1a[-33], aa1[-33], check.attributes = FALSE )

## cross-section data, efficiency effects frontier, zIntercept
aa2a <- sfa( log( y ) ~ log( x1 ) + log( x2 ) | z1 )
all.equal( aa2a[-33], aa2[-33], check.attributes = FALSE )


##############################################
### estimations with 0 or 1 variable only ###
##############################################

## cross-section data, error components frontier
sa10 <- sfa( logOutput ~ 1, data = front41Data )
a10 <- frontier( "logOutput", NULL, data = front41Data )
print( sa10 )
all.equal( sa10[-33], a10[-33], check.attributes = FALSE )

sa11 <- sfa( logOutput ~ logLabour, data = front41Data )
a11 <- frontier( "logOutput", "logLabour", data = front41Data )
print( sa11 )
all.equal( sa11[-33], a11[-33], check.attributes = FALSE )

## cross-section data, efficiency effects frontier
saa10 <- sfa( logOutput ~ 1 | firmNo - 1, data = front41Data )
aa10 <- frontier( data = front41Data, "logOutput", NULL,
   zNames = "firmNo" )
print( saa10 )
all.equal( saa10[-33], aa10[-33] )

saa11 <- sfa( logOutput ~ logLabour | firmNo - 1, data = front41Data )
aa11 <- frontier( data = front41Data, "logOutput", "logLabour",
   zNames = "firmNo" )
print( saa11 )
all.equal( saa11[-33], aa11[-33] )


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
