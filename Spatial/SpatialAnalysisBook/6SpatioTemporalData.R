#=======================================================#
#                                                       #
#  Examples and Exercises from Bivand et al. 			#
#  "Applied Spatial Data Analysis with R"               #
#  Chapter 6: Spatio-Temporal Data				 		#
#  Damian Satterthwaite-Phillips <damiansp@gmail.com>   #
#  Updated: 29Mar2014                                   #
#                                                       #
#=======================================================#

rm(list=ls())
library(maps)
library(maptools)
library(plm)
library(RColorBrewer)
library(sp)
library(spacetime)
library(xts)

load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')

# 6.5 Construction of ST Objects
ecd.ll <- as.matrix(read.table(
			  '~/Desktop/R/Spatial/SpatialAnalysisBook/ECDovelatlon.dat', 
			  header=F
		  ))
ecd.ll <- SpatialPoints(ecd.ll[, c(2,1)])
proj4string(ecd.ll) <- CRS('+proj=longlat +datum=WGS84')

ecd.years <- 1986:2003
ecd.y <- as.Date(paste(ecd.years, '-01-01', sep=''), '%Y-%m-%d')

ecd <- read.table(
		  '~/Desktop/R/Spatial/SpatialAnalysisBook/ECDoveBBS1986_2003.dat', 
		  header=F
	   )
ecd[ecd == -1] <- NA
ecd.st <- STFDF(ecd.ll, ecd.y, data.frame(counts=as.vector(as.matrix(ecd))))
dim(ecd.st)

# Could also be constructed using:
ecd.st2 <- stConstruct( ecd, ecd.ll, list(counts=names(ecd)), TimeObj=ecd.y, 
						interval=T )
all.equal(ecd.st, ecd.st2)



# 6.6 Selection, Addition, and Replacement of Attributes
m <- map('state', 'florida', fill=T)
FL <- map2SpatialPolygons(m, 'FL')
proj4string(FL) <- proj4string(ecd.st)
plot(FL)

# Use FL object to get subset of ecd.st data
dim(ecd.st[FL, ])
dim(ecd.st[, '1998::2003'])
dim(ecd.st[,, 'counts'])
dim(ecd.st[FL, '1998::2003', 'counts'])

mode(ecd.st[[1]])	# = ecd.st$counts # = ecd.st[['counts']]
ecd.st$sqrtcounts <- sqrt(ecd.st$counts)



# 6.7 Overlay and Aggregation
bb <- STF(FL, ecd.y[c(4, 6, 8, 10, 12)])
bb <- STF(FL, ecd.y)

# NB:
over(bb, ecd.st) # returns only the first point, instead:
over(bb, ecd.st, returnList=T) # or aggregate with a function:
over(bb, ecd.st, fn=sum, na.rm=T)

# Use to create a new STFDF:
bb.counts <- new('STFDF', bb, data=over(bb, ecd.st, fn=sum, na.rm=T))

# This can be done more simply with aggregate():
aggregate(ecd.st, bb, sum, na.rm=T)
plot(aggregate(ecd.st, bb, sum, na.rm=T)$counts)
lines(aggregate(ecd.st, bb, sum, na.rm=T)$sqrtcounts, col=2)
matplot(aggregate(ecd.st, bb, sum, na.rm=T), type='l')

# Aggregate over time instead of space:
ecd.5y <- aggregate(ecd.st, '5 years', mean, na.rm=T)

#vignette('sto')
#?aggregate.zoo



# 6.8 Visualization
	# 6.8.1 Multi-Panel Plots
	print(stplot( ecd.5y[FL,], c( "1986-1990", "1991-1995", "1996-2000", 
								  "2001-2003" ),
		   		  col.regions = brewer.pal(6, "Reds"), cex=1,
		   		  cuts=c(0, 5, 10, 20, 40, 80, 131), 
		   		  sp.layout = list("sp.polygons", FL, col = "grey"),
		   		  ylim = bbox(FL)[2,], scales = list(draw=FALSE), 
		   		  colorkey=TRUE ))

	# 6.8.2 Space-Time Plots
	ecd.FL <- ecd.st[FL, , 'sqrtcounts']
	x <- as(ecd.FL, 'xts')
	x[is.na(x)] <- 0
	o <- order(as.vector(1:18 %*% x) / apply(x, 2, sum))
	
	pal <- brewer.pal(6, 'Blues')
	cuts <- seq(0, 12, 2)
	ck <- list(at=cuts, label=as.character(cuts^2))
	stplot( ecd.FL[o,], mode='xt', col.regions=pal, cuts=6, asp=0.4, 
		    xlab='Sites ordered by time', colorkey=ck )
		    
	# 6.8.3 Animated Plots
	states.m <- map("state", plot=FALSE, fill=TRUE)
	IDs <- sapply(strsplit(states.m$names, ":"), function(x) { x[1] })
	states <- map2SpatialPolygons(states.m, IDs=IDs)
	yrs <- 1970:1986
	time <- as.POSIXct(paste(yrs, "-01-01", sep=""), tz = "GMT")
	#data("Produc")
	Produc.st <- STFDF(states[-8], time, Produc[order(Produc[2], Produc[1]),])
	print( stplot(Produc.st[1:2,,5:8], mode = "tp", key.space = "bottom"),
		   more = TRUE, split = c(1,1,2,1) )
	print( stplot(Produc.st[c(1:3,5),,5:6], mode="ts", key.space="bottom"),
		   more = FALSE, split = c(2,1,2,1) )
		   
	

	
	
	

save.image('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')