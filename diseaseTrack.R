rm(list=ls())

# Area
X_DIM <- 100
Y_DIM <- 100

# Population
M_PROP0 <- 0.5
F_PROP0 <- 0.5
N0 <- 200

# Home ranges
M_RADIUS_MEAN <- 0.2
M_RADIUS_SD <- 0.06
F_RADIUS_MEAN <- 0.12
F_RADIUS_SD <- 0.08
MIN_RADIUS <- 0.025

# Dispersal
M_DISPERSE_DIST_MEAN <- 0.2
M_DISPERSE_DIST_SD <- 0.1
F_DISPERSE_DIST_MEAN <- 0.5
F_DISPERSE_DIST_SD <- 0.2

# Reproduction and survival
CHILD_SURVIVAL <- 0.2	# Expected fraction that will survive
TEEN_SURVIVAL <- 0.6
ADULT_SURVIVAL <- 0.8
LIFE_EXPECTANCY <- 10
TEEN_FECUNDITY <- 0.2	# Probablity of reproducing
ADULT_FECUNDITY <- 0.9
OLD_AGE_LAMBDA <- 0.8	# mulitplier for survival odds if age > LIFE_EXP

# Habitat quality
TYPICAL_HABITAT <- 10

# Carrying Capacity Coefficient:
K <- 0.5		# K * habitat.val = capacity

# Disease
VIRULENCE <- 0.3		# Probability of commuting if contact is made




# Create a map, with 10 x 10 patches randomly assigned habitat quality values
cell.x <- X_DIM / 10
cell.y <- Y_DIM / 10


n.cells <- cell.x * cell.y
habitat.vals <- rchisq(n.cells, TYPICAL_HABITAT)
habitat.map <- matrix(habitat.vals, nrow=cell.x, ncol=cell.y)

par(mar=c(0, 0, 0, 0))
image(habitat.map, col=terrain.colors(50))  # red = poor, white = optimal




# Initialize population
sex <- sample(c('male', 'female'), N0, replace=T, prob=c(M_PROP0, F_PROP0))
x <- runif(N0, 0, 1)
y <- runif(N0, 0, 1)


animals <- data.frame(sex, x, y)
rm(sex, x, y)
animals$age <- rep(1, N0)

plot.animals <- function() {
	points(animals$x, animals$y, col=animals$sex, cex=animals$age, pch=16)
}

plot.animals()

# initalize homerange radii
animals$radius <- numeric(N0)
for (r in 1:N0) {
	if (animals$sex[r] == 'male') {
		animals$radius[r] <- max( rnorm(1, M_RADIUS_MEAN, M_RADIUS_SD), 
								  MIN_RADIUS )
	} else {
		animals$radius[r] <- max( rnorm(1, F_RADIUS_MEAN, F_RADIUS_SD),
								  MIN_RADIUS )
	}
}

plot.radii <- function() {
	points(animals$x, animals$y, col=animals$sex, cex=animals$radius * 100)
}

plot.radii()

animals$dispersed <- rep(F, N0)

disperse <- function(sex, x, y) {
	if (sex == 'male') {
		d <- max(0, rnorm(1, M_DISPERSE_DIST_MEAN, M_DISPERSE_DIST_SD))
	} else {
		d <- max(0, rnorm(1, F_DISPERSE_DIST_MEAN, F_DISPERSE_DIST_SD))
	}
		
	if (d > 0) {
		rad <- runif(1, 0, 2*pi)
		x <- x + d * cos(rad)
		y <- y + d * sin(rad)
	}
	
	return(list(x=x, y=y))
}

do.disperse <- function(animals) {
	indices <- which(animals$dispersed == F)
	remove <- c()
	
	for (a in indices) {
		coords <- disperse(animals$sex[a], animals$x[a], animals$y[a])
		animals$x[a] <- coords$x
		animals$y[a] <- coords$y
		animals$dispersed <- T

		# remove animals out of range from the data set			
		if ( animals$x[a] < 0 | animals$x[a] > 1 | animals$y[a] < 0 | 
			 animals$y[a] > 1 ) {
			remove <- c(remove, a)	 	
		}
	}
	if (length(remove) > 0) {
		animals <- animals[-remove,]
	}
	return(animals)
}

animals <- do.disperse(animals)
image(habitat.map, col=terrain.colors(50)); plot.animals(); plot.radii()


# Define a transition matrix:
#		child			teen				adult
# child	0				TEEN_FECUNDITY	ADULT_FECUNDITY
# teen	CHILD_SURVIVAL	0				0
# adult	0				TEEN_SURVIVAL	ADULT_SURVIVAL

get.habitat <- function(x, y) {
	x.cell <- ceiling(10 * x)
	y.cell <- ceiling(10 * y)
	return(habitat.map[x.cell, y.cell])
}

reproduce <- function(age, radius, habitat) {
	area.coef <- radius / F_RADIUS_MEAN
	hab.coef <- 2 * pchisq(habitat, TYPICAL_HABITAT)
	# teens
	if (age == 2) {
		repro <- sample( c(T, F), 1, 
						 prob=area.coef * hab.coef * 
						 		c( TEEN_FECUNDITY, 1 - TEEN_FECUNDITY ) )
	} else if (age > 2) {  
		# adults
		repro <- sample( c(T, F), 1, 
						 prob=area.coef * hab.coef * 
						 		c( ADULT_FECUNDITY, 1 - ADULT_FECUNDITY ) )
		
	}
	
	return (repro)
}

do.reproduce <- function(animals) {
	indices <- which(animals$sex == 'female' & animals$age > 1)
	for (a in indices) {
		repro <- reproduce( animals$age[a], animals$radius[a],
							get.habitat(animals$x[a], animals$y[a]) )
		if (repro) {
			# generate a new child at same x, y
			sx <- sample( c('male', 'female'), 1, replace=T, 
						   prob=c(M_PROP0, F_PROP0) )
			r <-  ifelse( sx == 'male' , max( rnorm( 1, M_RADIUS_MEAN, 
													M_RADIUS_SD ), 
								  			  MIN_RADIUS ),
								  		 max( rnorm( 1, F_RADIUS_MEAN, 
													F_RADIUS_SD ), 
								  			  MIN_RADIUS ))

			newb <- data.frame( sex=sx, x=animals$x[a], y=animals$y[a], 
								age=1, radius=r, dispersed=F )
			animals <- rbind(animals, newb)
		}
	}
	return (animals)
}

animals <- do.reproduce(animals)
image(habitat.map, col=terrain.colors(50)); plot.animals(); plot.radii()


die <- function(age, habitat) {
	hab.coef <- 2 * pchisq(habitat, TYPICAL_HABITAT)
	
	if (age == 1) {
		# child
		surv.coef <- CHILD_SURVIVAL		
	} else if (age == 2) {
		# teen
		surv.coef <- TEEN_SURVIVAL
	} else {
		# adult
		surv.coef <- ADULT_SURVIVAL
	}
	
	survive.prob <- min(hab.coef * surv.coef, 1)

	if (age > LIFE_EXPECTANCY) {
		years.beyond <- age - LIFE_EXPECTANCY
		survive.prob <- survive.prob * OLD_AGE_LAMBDA^years.beyond
	}
	
	return (sample(c(T, F), 1, prob=c(survive.prob, 1 - survive.prob)))
} 

do.die <- function(animals) {
	dead <- c()
	for (a in 1:nrow(animals)) {
		survive <- die( animals$age[a], 
						get.habitat(animals$x[a], animals$y[a]) )
		if (survive == F) {
			dead <- c(dead, a)
		}
		
	}
	
	if (length(dead) > 0) {
		animals <- animals[-dead, ]
	}
	
	return (animals)
}

animals <- do.die(animals)
image(habitat.map, col=terrain.colors(50)); plot.animals(); plot.radii()

grow <- function(animals) {
	animals$age <- animals$age + 1
	return (animals)
}

animals <- grow(animals)

# Repeat sequence
animals <- do.disperse(animals)
image(habitat.map, col=terrain.colors(50)); plot.animals(); plot.radii()

animals <- do.reproduce(animals)
image(habitat.map, col=terrain.colors(50)); plot.animals(); plot.radii()

animals <- do.die(animals)
image(habitat.map, col=terrain.colors(50)); plot.animals(); plot.radii()

animals <- grow(animals)
