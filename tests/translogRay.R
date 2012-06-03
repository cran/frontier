library( frontier )
options( digits = 5 )

## preparing data
data( germanFarms )
# quantity of crop outputs
germanFarms$qCrop <- germanFarms$vCrop / germanFarms$pOutput
# quantity of animal outputs
germanFarms$qAnimal <- germanFarms$vAnimal / germanFarms$pOutput
# quantity of variable inputs
germanFarms$qVarInput <- germanFarms$vVarInput / germanFarms$pVarInput
# a time trend to account for technical progress:
germanFarms$time <- c(1:20)

# estimate a translog ray production function
estResultRay <- frontierTranslogRay( yNames = c( "qCrop", "qAnimal" ),
   xNames = c( "qLabor", "land", "qVarInput" ),
   data = germanFarms )
print( estResultRay )
summary( estResultRay )
lrtest( estResultRay )
efficiencies( estResultRay )
efficiencies( estResultRay, asInData = TRUE )
residuals( estResultRay )
residuals( estResultRay, asInData = TRUE )
print.default( estResultRay )
