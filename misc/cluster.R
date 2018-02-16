clusterM <- function(x=100, y=100, n.clust=3, points=0.1) {
	M <- matrix(0, nrow=x, ncol=y)
	clust.x <- sample(1:x, n.clust)
	clust.y <- sample(1:y, n.clust)
	cluster.centers <- cbind(clust.x, clust.y)

	# Distance of each cell to the nearest cluster center
	d.matrix <- M
	for (i in 1:x) {
		for (j in 1:y) {
			d.matrix[i, j] <- min( sqrt((i - clust.x[1])^2 + (j - clust.y[1])^2),
					  			   sqrt((i - clust.x[2])^2 + (j - clust.y[2])^2),
					  			   sqrt((i - clust.x[3])^2 + (j - clust.y[3])^2) 
					  			 )	
		}
	}
	
	d.matrix <- 1 / d.matrix
	d.matrix[d.matrix > 1] <- 1
	d.matrix <- d.matrix / sum(d.matrix)

	ps <- as.integer(points*x*y)
	areas <- sample(1:(x*y), ps, replace=T, prob=d.matrix)
	
	for (a in areas) {
		M[a] <- M[a] + 1
	}
	
	return (list('Inverse Distance Matrix'=d.matrix, areas=M))
}

run <- clusterM()
d.matrix <- run$'Inverse Distance Matrix'
M <- run$areas


#par(mar=c(0, 0, 0, 0))
image(M^0.1, col=heat.colors(50))
image(log(d.matrix), col=heat.colors(50))






# Another approach
cluster <- function(x=100, y=100, sd=7, p=0.1, center=NA) {
	ps <- as.integer(p*x*y)
	M <- matrix(0, x, y)
	if (is.na(center[1])) {
		center <- cbind(sample(1:x, 1), sample(1:y, 1))
	}

	M[center] <- 1

	dx <- as.integer(rnorm(ps, mean=center[1], sd=sd))
	dx[dx < 0 | dx > x] <- center[1]
	dy <- as.integer(rnorm(ps, mean=center[2], sd=sd))
	dy[dy < 0 | dy > y] <- center[2]
	scatter <- cbind(dx, dy)
	M[scatter] <- 1

#	image(M)
	return(M)
}

par(mfrow=c(4, 4))
par(mar=c(0,0,0,0))


M1 <- cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M1)
image(M1 > 0)
M2 <- M1 + cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M2)
image(M2 > 0)
M3 <- M2 + cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M3)
image(M3 > 0)
M4 <- M3 + cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M4)
image(M4 > 0)
M5 <- M4 + cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M5)
image(M5 > 0)
M6 <- M5 + cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M6)
image(M6 > 0)
M7 <- M6 + cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M7)
image(M7 > 0)
M8 <- M7 + cluster(100, 100, 15, 0.1, center=cbind(50, 50))
image(M8)
image(M8 > 0)

new.area <- numeric(8)
new.area[1] <- sum(M1 > 0)
new.area[2] <- sum(M2 > 0) - sum(M1 > 0)
new.area[3] <- sum(M3 > 0) - sum(M2 > 0)
new.area[4] <- sum(M4 > 0) - sum(M3 > 0)
new.area[5] <- sum(M5 > 0) - sum(M4 > 0)
new.area[6] <- sum(M6 > 0) - sum(M5 > 0)
new.area[7] <- sum(M7 > 0) - sum(M6 > 0)
new.area[8] <- sum(M8 > 0) - sum(M7 > 0)

quartz()
plot(new.area, type='l')