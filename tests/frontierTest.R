library( frontier )
options( digits = 5 )

data( front41Data )
row.names( front41Data ) <- paste( "F", row.names( front41Data ), sep = "_" )
front41Data$logOutput  <- log( front41Data$output )
front41Data$logCapital <- log( front41Data$capital )
front41Data$logLabour  <- log( front41Data$labour )
front41Data$firmNo     <- c( 1:nrow( front41Data ) )

## cross-section data, error components frontier
a1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ) )
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
print.default( a1 )

## cross-section data, error components frontier, truncNorm
a2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE )
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
print.default( a2 )

## cross-section data, error components frontier, truncNorm, starting values
a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
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
aa1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo" )
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
print.default( aa1 )

## cross-section data, efficiency effects frontier, zIntercept
aa2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE )
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
print.default( aa2 )

## cross-section data, efficiency effects frontier, zIntercept, starting values
aa5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE,
   startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
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
bb1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
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
print.default( bb1 )

## cross-section rice data, error components frontier, truncNorm
bb2 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
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
print.default( bb2 )

## cross-section rice data, efficiency effects frontier
bb5 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
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
print.default( bb5 )

## cross-section rice data, efficiency effects frontier, zIntercept
bb6 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
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
print.default( bb6 )

## cross-section rice data, error components frontier, truncNorm, starting values
bb7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
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
bb8 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
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
dd1 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE )
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
print.default( dd1 )

## cross-section rice data, error components cost frontier, truncNorm
dd2 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE, truncNorm = TRUE )
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
print.default( dd2 )

## cross-section rice data, efficiency effects cost frontier
dd5 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS" ), data = riceProdPhil, #, "BANRAT"
   ineffDecrease = FALSE )
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
print.default( dd5 )

## cross-section rice data, efficiency effects cost frontier, zIntercept
dd6 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE, zIntercept = TRUE )
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
print.default( dd6 )


## panel data
riceProdPhil$farm <- paste( "F_", ifelse( riceProdPhil$FMERCODE > 9, "", "0" ),
   riceProdPhil$FMERCODE, sep = "" )
riceProdPhil$year <- riceProdPhil$YEARDUM + 1998
riceProdPhil <- plm.data( riceProdPhil, c( "farm", "year" ) )

## panel data, error components frontier
b1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
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
print.default( b1 )

## panel data, error components frontier, truncNorm
b2 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE )
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
print.default( b2 )

## panel data, error components frontier, timeEffect
b3 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   timeEffect = TRUE )
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
print.default( b3 )

## panel data, error components frontier, truncNorm, timeEffect
b4 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE )
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
print.default( b4 )

## panel data, efficiency effects frontier
b5 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
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
print.default( b5 )

## panel data, efficiency effects frontier, zIntercept
b6 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE )
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
print.default( b6 )

## panel data, error components frontier, truncNorm, timeEffect, starting values
b7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, timeEffect = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
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
b8 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, -0.3, -0.01, -0.4, 0.2, 0.8 ) )
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
d1 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE )
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
print.default( d1 )

## panel rice data, error components cost frontier, truncNorm
d2 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE, truncNorm = TRUE )
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
print.default( d2 )

## panel rice data, error components cost frontier, timeEffect
d3 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE, timeEffect = TRUE )
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
print.default( d3 )

## panel rice data, error components cost frontier, truncNorm, timeEffect
d4 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE, truncNorm = TRUE,
   timeEffect = TRUE )
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
print.default( d4 )

## panel rice data, efficiency effects cost frontier
d5 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS" ), data = riceProdPhil, #, "BANRAT"
   ineffDecrease = FALSE )
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
print.default( d5 )

## panel rice data, efficiency effects cost frontier, zIntercept
d6 <- frontier( "lCost", xNames = c( "lAREA", "lLABORP", "lNPKP" ),
   zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil,
   ineffDecrease = FALSE, zIntercept = TRUE )
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
print.default( d6 )


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
translogZvarEla <- elas( translogZvar )
print( translogZvarEla ) 
attributes( translogZvarEla )$variance
attributes( translogZvarEla )$stdDev
print.default( translogZvar )


##############################################
##### evaluating log likelihood values #######
##############################################
options( digits = 9 )

## cross-section data, error components frontier
logLik( a1 )
logLik( a1, newParam = coef( a1 ) )

## cross-section data, error components frontier, truncNorm
logLik( a2 )
logLik( a2, newParam = coef( a2 ) )

## cross-section data, error components frontier, truncNorm, starting values
logLik( a5 )
logLik( a5, newParam = coef( a5 ) )

## cross-section data, efficiency effects frontier
logLik( aa1 )
logLik( aa1, newParam = coef( aa1 ) )

## cross-section data, efficiency effects frontier, zIntercept
logLik( aa2 )
logLik( aa2, newParam = coef( aa2 ) )

## cross-section data, efficiency effects frontier, zIntercept, starting values
logLik( aa5 )
logLik( aa5, newParam = coef( aa5 ) )


## data set of rice producers in the Philippines
riceProdPhil <- as.data.frame( riceProdPhil )

## cross-section rice data, error components frontier
logLik( bb1 )
logLik( bb1, newParam = coef( bb1 ) )

## cross-section rice data, error components frontier, truncNorm
logLik( bb2 )
logLik( bb2, newParam = coef( bb2 ) )

## cross-section rice data, efficiency effects frontier
logLik( bb5 )
logLik( bb5, newParam = coef( bb5 ) )

## cross-section rice data, efficiency effects frontier, zIntercept
logLik( bb6 )
logLik( bb6, newParam = coef( bb6 ) )

## cross-section rice data, error components frontier, truncNorm, starting values
logLik( bb7 )
logLik( bb7, newParam = coef( bb7 ) )

## cross-section rice data, efficiency effects frontier, zIntercept, starting values
logLik( bb8 )
logLik( bb8, newParam = coef( bb8 ) )


## panel data
riceProdPhil <- plm.data( riceProdPhil, c( "farm", "year" ) )

## panel data, error components frontier
logLik( b1 )
logLik( b1, newParam = coef( b1 ) )

## panel data, error components frontier, truncNorm
logLik( b2 )
logLik( b2, newParam = coef( b2 ) )

## panel data, error components frontier, timeEffect
logLik( b3 )
logLik( b3, newParam = coef( b3 ) )

## panel data, error components frontier, truncNorm, timeEffect
logLik( b4 )
logLik( b4, newParam = coef( b4 ) )

## panel data, efficiency effects frontier
logLik( b5 )
logLik( b5, newParam = coef( b5 ) )

## panel data, efficiency effects frontier, zIntercept
logLik( b6 )
logLik( b6, newParam = coef( b6 ) )

## panel data, error components frontier, truncNorm, timeEffect, starting values
logLik( b7 )
logLik( b7, newParam = coef( b7 ) )

## panel data, efficiency effects frontier, zIntercept, starting values
logLik( b8 )
logLik( b8, newParam = coef( b8 ) )


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
