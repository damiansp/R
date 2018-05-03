#---------#---------#---------#---------#---------#---------#---------#---------
library(GISTools)
library(RColorBrewer)

data(georgia)
data(meuse.grid)



# 4 Plots
# 4.1 Basic plot tools
appling <- georgia.polys[[1]]
plot(appling, asp=1, type='n', xlab='Easting', ylab='Northing')
#polygon(appling, density=14, angle=145)
polygon(appling, col=rgb(0, 0.5, 0.7, 0.4))

mat <- SpatialPixelsDataFrame(points=meuse.grid[c('x', 'y')], data=meuse.grid)

par(mfrow=c(1, 2))
par(mar=rep(0, 4))
image(mat, 'dist')
greens <- brewer.pal(14, 'Greens')
image(mat, 'dist', col=greens)
par(mfrow=c(1, 1))



# 5 Reading, Writing, Loading, Saving
# 5.2 R data files
# save(list=c('obj1', 'obj2, '...'), file='file/path.RData')
# load('filepath.RData')

# 5.3 Spatial data files
#writePolyShape(georgia, 'path/georgia.shp')
#georgia <- readShapePoly('path/georgia.shp')


