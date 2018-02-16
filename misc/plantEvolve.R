#===================#
#					#
#	PLANT WORLD		#
#					#
#===================#

WIDTH <- 1000
GROUND_DEPTH <- 200


createWorld <- function(width=WIDTH, groundDepth=GROUND_DEPTH, skyHeight=800, nNutrient=20) {
	par(mar=rep(0, 4))
	plot(0, 0, type='n', xlim=c(0, width), ylim=c(-groundDepth, skyHeight))
	polygon(c(0, width, width, 0), c(0, 0, -groundDepth, -groundDepth), col='brown')
	nutrientX <- runif(nNutrient, 0, width)
	nutrientY <- runif(nNutrient, -groundDepth, 0)
	nutrientData <- cbind(nutID=1:nNutrient, nutrientX, nutrientY)
	points(nutrientX, nutrientY)
}

createWorld()

createPlant <- function(id) {
	return (list(ID=id, seed=round(runif(1, 0, WIDTH)), height=0, baseWidth=0, energy=500, 
			nutrition=500))
}

sow <- function(nPlants) {
	plantList <- list()
	for (p in 1:nPlants) {
		plant <- createPlant(p)
		plantList[[p]] <- plant
	}
	
	return (plantList)
}

sun <- function(center=c(WIDTH / 2, ), time) {
	
}

