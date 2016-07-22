install.packages("jpeg", dependencies=TRUE)
library(jpeg)
library(grid)
require(raster)
mask1 <- readJPEG("C:/Users/Tom/Dropbox/Kittiwake remote camera/masknest1.jpg")

grid.raster(mask1)

ma <- mask1[, , 1]
#Plot 
grid.raster(ma)


#grid.raster(mask1)
ma <- as.numeric(ma > 0)
# make non-
#ma[ma == 0] = NA


test_image <- readJPEG("/Users/freeman.r/Dropbox/Kittiwake remote camera/02-07-2013/IMAG1385.JPG")

grid.raster(test_image)

test_image <- test_image*ma

grid.raster(test_image)

#I <- as.raster(test_image)
#mask <- raster(ma)
#pol_mask <- rasterToPolygons(mask)
#I_masked <- mask(I, mask)