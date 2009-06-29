library( frontier )

## data set of rice producers in the Philippines
data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )
riceProdPhil$farm <-
   paste( "F_", ifelse( riceProdPhil$FMERCODE > 9, "", "0" ),
   riceProdPhil$FMERCODE, sep = "" )
riceProdPhil$year <- riceProdPhil$YEARDUM + 1998
riceProdPhil <- plm.data( riceProdPhil, c( "farm", "year" ) )


########## cross-section data #############

## without mu / zIntercept
# Error Components Frontier (ECF)
bb5ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )

# Efficiency Effects Frontier (EEF)
bb5eef <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = NA )

# Comparisons
rbind( coef( bb5ecf ), coef( bb5eef ) )
all.equal( coef( bb5ecf ), coef( bb5eef ) )
all.equal( vcov( bb5ecf ), vcov( bb5eef ) )
all.equal( efficiencies( bb5ecf ), efficiencies( bb5eef ) )


## with mu / zIntercept
# Error Components Frontier (ECF)
bb6ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, muBound = 0 )

# Efficiency Effects Frontier (EEF)
bb6eef <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zIntercept = TRUE, zNames = NA )

# Comparisons
rbind( coef( bb6ecf ), coef( bb6eef )[ c( 1:4, 6:7, 5 ) ] )
all.equal( efficiencies( bb6ecf ), efficiencies( bb6eef ) )


############ panel data ###############

## without mu / zIntercept
# Error Components Frontier (ECF)
b5ecf <- bb5ecf

# Efficiency Effects Frontier (EEF)
b5eef <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = NA )
all.equal( b5eef[ -c( 4, 5, 17, 27, 34 ) ], bb5eef[ -c( 4, 5, 17, 27, 34 ) ] )

# Comparisons
rbind( coef( b5ecf ), coef( b5eef ) )
all.equal( coef( b5ecf ), coef( b5eef ) )
all.equal( vcov( b5ecf ), vcov( b5eef ) )
all.equal( c( efficiencies( b5ecf ) ), c( t( efficiencies( b5eef ) ) ) )


## without mu / zIntercept
# Error Components Frontier (ECF)
b6ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, muBound = Inf )
all.equal( b6ecf[ -34 ], bb6ecf[ -34 ] )

# Efficiency Effects Frontier (EEF)
b6eef <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zIntercept = TRUE, zNames = NA )
all.equal( b6eef[ -c( 4, 5, 17, 27, 34 ) ], bb6eef[ -c( 4, 5, 17, 27, 34 ) ] )
all.equal( c( efficiencies( b6ecf ) ), c( efficiencies( bb6eef ) ) )

# Comparisons
rbind( coef( b6ecf ), coef( b6eef )[ c( 1:4, 6:7, 5 ) ] )
all.equal( c( efficiencies( b6ecf ) ), c( t( efficiencies( b6eef ) ) ) )

