# install necessary packages
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

#access necessary libraries
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in files
g1966 <- readOGR("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
g1998 <- readOGR("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

#investigate data about plots
str(g2015)
str(g1966)

head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

g1966@proj4string

#plot data
spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#read in rgb imagery from landsat
redL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_blue.tif")


#check coordinate system
redL@crs




