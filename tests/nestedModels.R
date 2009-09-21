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
sbb5ecf <- sfa( lPROD ~ lAREA + lLABOR + lNPK,
   data = as.data.frame( riceProdPhil ) )
bb5ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
all.equal( sbb5ecf[-33], bb5ecf[-33] )

# Efficiency Effects Frontier (EEF)
sbb5eef <- sfa( lPROD ~ lAREA + lLABOR + lNPK | - 1,
   data = as.data.frame( riceProdPhil ) )
bb5eef <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = NA )
all.equal( sbb5eef[-33], bb5eef[-33] )

# Comparisons
rbind( coef( bb5ecf ), coef( bb5eef ) )
all.equal( coef( bb5ecf ), coef( bb5eef ) )
all.equal( vcov( bb5ecf ), vcov( bb5eef ) )
all.equal( efficiencies( bb5ecf ), efficiencies( bb5eef ) )


## with mu / zIntercept
# Error Components Frontier (ECF)
sbb6ecf <- sfa( lPROD ~ lAREA + lLABOR + lNPK,
   data = as.data.frame( riceProdPhil ), truncNorm = TRUE, muBound = 0 )
bb6ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, muBound = 0 )
all.equal( sbb6ecf[-33], bb6ecf[-33] )

# Efficiency Effects Frontier (EEF)
sbb6eef <- sfa( lPROD ~ lAREA + lLABOR + lNPK | 1,
   data = as.data.frame( riceProdPhil ) )
bb6eef <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zIntercept = TRUE, zNames = NA )
all.equal( sbb6eef[-33], bb6eef[-33] )

# Comparisons
rbind( coef( bb6ecf ), coef( bb6eef )[ c( 1:4, 6:7, 5 ) ] )
all.equal( efficiencies( bb6ecf ), efficiencies( bb6eef ) )


############ panel data ###############

## without mu / zIntercept
# Error Components Frontier (ECF)
b5ecf <- bb5ecf

# Efficiency Effects Frontier (EEF)
sb5eef <- sfa( lPROD ~ lAREA + lLABOR + lNPK | - 1,
   data = riceProdPhil )
b5eef <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = NA )
all.equal( sb5eef[-33], b5eef[-33] )
all.equal( b5eef[ -c( 3, 4, 16, 26, 33 ) ], bb5eef[ -c( 3, 4, 16, 26, 33 ) ] )
all.equal( c( t( residuals( b5eef ) ) ), c( residuals( bb5eef ) ) )

# Comparisons
rbind( coef( b5ecf ), coef( b5eef ) )
all.equal( coef( b5ecf ), coef( b5eef ) )
all.equal( vcov( b5ecf ), vcov( b5eef ) )
all.equal( c( efficiencies( b5ecf ) ), c( t( efficiencies( b5eef ) ) ) )


## without mu / zIntercept
# Error Components Frontier (ECF)
sb6ecf <- sfa( lPROD ~ lAREA + lLABOR + lNPK,
   data = as.data.frame( riceProdPhil ), truncNorm = TRUE, muBound = Inf )
b6ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, muBound = Inf )
all.equal( sb6ecf[-33], b6ecf[-33] )
all.equal( b6ecf[-33], bb6ecf[-33] )

# Efficiency Effects Frontier (EEF)
sb6eef <- sfa( lPROD ~ lAREA + lLABOR + lNPK | 1,
   data = riceProdPhil )
b6eef <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zIntercept = TRUE, zNames = NA )
all.equal( sb6eef[-33], b6eef[-33] )
all.equal( b6eef[ -c( 3, 4, 16, 26, 33 ) ], bb6eef[ -c( 3, 4, 16, 26, 33 ) ] )
all.equal( c( efficiencies( b6ecf ) ), c( efficiencies( bb6eef ) ) )
all.equal( c( residuals( b6ecf ) ), c( residuals( bb6eef ) ) )

# Comparisons
rbind( coef( b6ecf ), coef( b6eef )[ c( 1:4, 6:7, 5 ) ] )
all.equal( c( efficiencies( b6ecf ) ), c( t( efficiencies( b6eef ) ) ) )
all.equal( c( residuals( b6ecf ) ), c( t( residuals( b6eef ) ) ) )

