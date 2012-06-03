library( frontier )
options( digits = 5 )

## example data included in FRONTIER 4.1 (cross-section data)
data( front41Data )
data( riceProdPhil )
riceProdPhil$cost <- riceProdPhil$LABOR * riceProdPhil$LABORP +
   riceProdPhil$NPK * riceProdPhil$NPKP

###### left-skewed residuals but ineffDecrease = FALSE
## front41Data
a1 <- sfa( log( output ) ~ log( capital ) + log( labour ),
   ineffDecrease = FALSE, data = front41Data )
summary( a1, farrell = FALSE )
lrtest( a1 )

## front41Data, truncNorm
a2 <- sfa( log( output ) ~ log( capital ) + log( labour ),
   ineffDecrease = FALSE, truncNorm = TRUE, data = front41Data )
summary( a2, farrell = FALSE )
lrtest( a2 )

## riceProdPhil
b1 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   ineffDecrease = FALSE, data = riceProdPhil )
summary( b1, farrell = FALSE )
lrtest( b1 )

## riceProdPhil, truncNorm
b2 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   ineffDecrease = FALSE, truncNorm = TRUE, data = riceProdPhil )
summary( b2, farrell = FALSE )
lrtest( b2 )


###### right-skewed residuals but ineffDecrease = TRUE
## riceProdPhil
d1 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ), data = riceProdPhil )
summary( d1 )
lrtest( d1 )

## riceProdPhil, truncNorm
d2 <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP ) +
   log( NPKP ), truncNorm = TRUE, data = riceProdPhil )
summary( d2 )
lrtest( d2 )
