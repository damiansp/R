#---------#---------#---------#---------#---------#---------#---------#---------
setwd('~/Learning/R/spatial/spatialAnalysisForMapping')
rm(list=ls())

library(GISTools)
library(OpenStreetMap)
library(PBSmapping)
library(RgoogleMaps)

data(newhaven)



# 2 Introduction: GISTools
# 2.2 Spatial tools in GISTools
class(roads)

par(mar=rep(4, 0))
plot(blocks, lwd=0.5, col='grey50')
plot(roads, add=T, col=2)
plot(breach, col=4, pch=1, add=T)
colors()

# 2.3 Embellishing the map
map.scale(534750, 152000, miles2ft(2), 'Miles', 4, 0.5)
#map.scale(xc, yc, length, 'text', n.grad, grad.len)
north.arrow(534750, 154000, miles2ft(0.25), col='lightblue')
#north.arrow(xc.base, yc.base, base.len, ...)
title('New Haven, CT')

# 2.4 Saving maps
#pdf(file='./images/newHaven.pdf')
#png(file='./images/newHaven.png')
#dev.off()



# 3 Mapping Spatial Objects
# 3.3 Plotting options
georgia.outline <- gUnaryUnion(georgia, id=NULL)
plot(georgia, col=2, bg='wheat', border=4)
plot(georgia.outline, lwd=3, add=T)
title(main='The State of Georgia', 
      font.main=2, 
      cex.main=1.5, 
      sub='and its Counties', 
      font.sub=3, 
      col.sub=4)

lat <- data.frame(georgia)[, 1]
lon <- data.frame(georgia)[, 2]
names <- data.frame(georgia)[, 13]
par(mar=rep(0, 4))
plot(georgia, col=NA)
point.label <- pointLabel(lon, lat, names, offset=0, cex=0.5)
county.tmp <- c(16, 17, 21, 53, 62, 81:83, 121, 124, 150)
georgia.sub <- georgia[county.tmp]
par(mar=c(0, 0, 3, 0))
plot(georgia.sub, col='gold1', border='grey')
plot(georgia.outline, add=T, lwd=2)
title('Some Georgia Counties', cex.main=2, font.main=1)
point.label <- pointLabel(
  lon[county.tmp], lat[county.tmp], names[county.tmp], offset=3, cex=1.5)

plot(georgia, border='grey', lwd=0.5)
plot(georgia.sub, add=T, col='lightblue')
plot(georgia.outline, lwd=2, add=T)
title('Georgia: subset of counties')

# 3.4 Adding context
upper.left <- as.vector(cbind(bbox(georgia.sub)[2, 2], 
                              bbox(georgia.sub)[1, 1]))
lower.right <- as.vector(cbind(bbox(georgia.sub)[2, 1],
                               bbox(georgia.sub)[1, 2]))
georgia.osmap <- openmap(upper.left, lower.right, 9, 'mapquest')
par(mar=rep(0, 4))
plot(georgia.osmap, removeMargin=F)
plot(spTransform(georgia.sub, osm(), add=T, lwd=2))

shp <- SpatialPolygons2PolySet(georgia.sub)
bb <- qbbox(lat=shp[, 'Y'], lon=shp[, 'X'])
georgia.gmap <- GetMap.bbox(bb$lonR, bb$latR, destfile='./images/DC.jpg')
par(mar=rep(0, 4))
PlotPolysOnStaticMap(georgia.gmap, shp, lwd=2, col=rep(0.25, 4), add=F)
par(mar=c(5, 4, 4, 2))



# 4 Mapping Spatial Data Attributes
