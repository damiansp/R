#!/usr/bin/env Rscript
#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/spatial/spatialAnalysisForMapping')
Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre')

library(GISTools)
library(maps)
library(OpenStreetMap)
library(PBSmapping)
library(rJava)
library(RgoogleMaps)

data(georgia)
data(meuse.grid)
data(newhaven)
data(quakes)


# 2 Introduction: GISTools
# 2.2 Spatial tools in GISTools
class(roads)

par(mar=rep(0, 4))
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
georgia.sub <- georgia[county.tmp,]
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
# 4.2 Attributes and data frames
ls()
summary(blocks)
summary(breach)
head(data.frame(blocks))
blocks$OCCUPIED
hist(blocks$P_VACANT)

breach.dens <- kde.points(breach, lims=tracts) # kernel density
summary(breach.dens)
class(breach.dens)
head(data.frame(breach.dens))
plot(breach.dens)
breach.dens.grid <- as(breach.dens, 'SpatialGridDataFrame')
summary(breach.dens.grid)

# 4.3 Mapping polygons and attributes
par(mfrow=c(1, 1))
display.brewer.all()

head(data.frame(blocks))
par(mfrow=c(2, 2))
par(mar=rep(0, 4))
choropleth(blocks, blocks$P_WHITE)
white.shades <- auto.shading(blocks$P_WHITE)
choro.legend(533000, 161000, white.shades)
black.shades <- auto.shading(blocks$P_BLACK, n=7)
choropleth(blocks, blocks$P_BLACK, shading=black.shades)
choro.legend(533000, 165000, black.shades)
es.shades <- auto.shading(blocks$P_AMERI_ES, cols=brewer.pal(7, 'BuGn'))
choropleth(blocks, blocks$P_AMERI_ES, shading=es.shades)
as.shades <- auto.shading(
  blocks$P_ASIAN_PI, n=5, cols=brewer.pal(5, 'Blues'), cutter=rangeCuts)
choropleth(blocks, blocks$P_ASIAN_PI, shading=as.shades)
choro.legend(533000, 161000, as.shades)

white.shades

# 4.4 Mapping points and attributes
par(mfrow=c(1, 1))
plot(blocks)
plot(breach, pch=16, col='#DE2D2680', add=T)

head(quakes)
quake.coords <- cbind(quakes$long, quakes$lat)
quake.spdf <- SpatialPointsDataFrame(quake.coords, data=data.frame(quakes))
plot(quake.spdf, pch=16, col='#FB6A4A80')
map('world2', fill=F, add=T)

tmp <- georgia.polys[c(1, 3, 151, 113)]
t1 <- Polygon(tmp[1])
t1 <- Polygons(list(t1), '1')
t2 <- Polygon(tmp[2])
t2 <- Polygons(list(t2), '2')
t3 <- Polygon(tmp[3])
t3 <- Polygons(list(t3), '3')
t4 <- Polygon(tmp[4])
t4 <- Polygons(list(t4), '4')
tmp.sp <- SpatialPolygons(list(t1, t2, t3, t4), 1:4)
names <- c('Appling', 'Bacon', 'Wayne', 'Pierce')
tmp.spdf <- SpatialPolygonsDataFrame(tmp.sp, data=data.frame(names))
data.frame(tmp.spdf)
plot(tmp.spdf, col=2:5)

par(mfrow=c(2, 2))
par(mar=rep(0, 4))
choropleth(quake.spdf, quakes$mag, pch=16)
shades <- auto.shading(quakes$mag, n=6, cols=brewer.pal(6, 'Greens'))
choropleth(quake.spdf, quakes$mag, shades, pch=16)
shades$cols <- add.alpha(shades$cols, 0.5)
choropleth(quake.spdf, quakes$mag, shades, pch=16)
tmp <- quakes$mag
tmp <- tmp - min(tmp)
tmp <- tmp / max(tmp)
plot(quake.spdf, cex=3*tmp, pch=16, col='#FB6A4A88')

par(mfrow=c(1, 2))
par(mar=rep(0, 4))
tmp2 <- cut(quakes$mag, fivenum(quakes$mag), include.lowest=T)
class <- match(tmp2, levels(tmp2)) # convert factors to numeric representation
plot(quake.spdf, pch=16, cex=c(1, 2, 3, 4)[class], col='#25252566')

idx1 <- 1 * (quakes$mag >= 4 & quakes$mag < 5)
idx2 <- 2 * (quakes$mag >= 5 & quakes$mag < 5.5)
idx3 <- 3 * (quakes$mag >= 5.5)
class <- idx1 + idx2 + idx3
col.var <- brewer.pal(3, 'Blues')
plot(quake.spdf, col=col.var[class], cex=class, pch=16)

par(mfrow=c(1, 1))
lat <- as.vector(quakes$lat)
lon <- as.vector(quakes$long)
g.map <- MapBackground(lat, lon, zoom=10)
PlotOnStaticMap(g.map, lat, lon, cex=class, pch=16, col=rgb(0, 0, 0, 0.4))

g.map <- MapBackground(lat, lon, zoom=10, maptype='satellite')
PlotOnStaticMap(g.map, lat, lon, cex=class, pch=16, col=rgb(1, 0.5, 0.5, 0.3))


# 4.5 Mapping lines and attributes
xmin <- bbox(roads)[1, 1]
ymin <- bbox(roads)[2, 1]
xmax <- xmin + diff(bbox(roads)[1, ]) / 2
ymax <- ymin + diff(bbox(roads)[2, ]) / 2
xx <- as.vector(c(xmin, xmin, xmax, xmax, xmin))
yy <- as.vector(c(ymin, ymax, ymax, ymin, ymin))
crds <- cbind(xx, yy)
pl <- Polygon(crds)
id <- 'clip'
pls <- Polygons(list(pl), ID=id)
spls <- SpatialPolygons(list(pls))
df <- data.frame(value=1, row.names=id)
clip.bb <- SpatialPolygonsDataFrame(spls, df)

roads.tmp <- gIntersection(clip.bb, roads, byid=T)
tmp <- as.numeric(gsub('clip', '', names(roads.tmp)))
tmp <- data.frame(roads)[tmp, ]
roads.tmp <- SpatialLinesDataFrame(roads.tmp, data=tmp, match.ID=F)

par(mfrow=c(1, 3))
par(mar=rep(0, 4))
plot(roads.tmp)
road.class <- unique(roads.tmp$AV_LEGEND)
shades <- rev(brewer.pal(length(road.class), 'Spectral'))
tmp <- roads.tmp$AV_LEGEND
index <- match(tmp, as.vector(road.class))
plot(roads.tmp, col=shades[index], lwd=3)
plot(roads.tmp, lwd=10*roads.tmp$LENGTH_MI)
par(mfrow=c(1, 1))


# 4.6 Mapping raster attributes
class(meuse.grid)
head(meuse.grid)
plot(meuse.grid$x, meuse.grid$y, asp=1, col=meuse.grid$soil, pch=16)

meuse.grid.spdf <- SpatialPixelsDataFrame(points=meuse.grid[c('x', 'y')], 
                                          dat=meuse.grid)
par(mfrow=c(1, 2))
par(mar=rep(0, 4))
image(meuse.grid.spdf, 'dist', col=rainbow(50))
image(meuse.grid.spdf, 'dist', col=heat.colors(50))

p1 <- spplot(meuse.grid.spdf, 'dist', col.regions=terrain.colors(50))
p1
spplot(meuse.grid.spdf, 
       c('part.a', 'part.b', 'soil', 'ffreq'), 
       col.regions=topo.colors(50))



# 5 Simple Descriptive Statistical Analyses
# 5.1 Histograms and boxplots
par(mfrow=c(1, 1))
par(mar=c(4, 5, 3, 1))
hist(blocks$P_VACANT, breaks=20, col='cyan', border='salmon')
index <- blocks$P_VACANT > 10
high.vac <- blocks[index,]
low.vac <- blocks[-index,]
cols <- rev(brewer.pal(3, 'Blues'))
par(mfrow=c(1, 2))
par(mar=c(2.5, 2, 3, 1))
boxplot(high.vac$P_OWNEROCC, 
        high.vac$P_WHITE, 
        high.vac$P_BLACK, 
        names=c('Owner\nOccupied', 'White', 'Black'),
        col=cols,
        cex.axis=0.7,
        main='High Vacancy')
boxplot(low.vac$P_OWNEROCC, 
        low.vac$P_WHITE, 
        low.vac$P_BLACK, 
        names=c('Owner\nOccupied', 'White', 'Black'),
        col=cols,
        cex.axis=0.7,
        main='Low Vacancy')
par(mfrow=c(1, 1))
par(mar=c(5, 4, 4, 2))