# install necessary packages
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

#access necessary libraries
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in files
f1966=c("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_1966.shp","/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/GNPglaciers/GNPglaciers_1966.shp")
f1998=c("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_1998.shp", "/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/GNPglaciers/GNPglaciers_1998.shp")
f2005=c("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_2005.shp", "/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/GNPglaciers/GNPglaciers_2005.shp")
f2015=c("Y:\\Students\\klogar\\a06\\GNPglaciers\\GNPglaciers_2015.shp", "/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/GNPglaciers/GNPglaciers_2015.shp")


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
red.files <- c("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_red.tif", "/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/glacier_09_05_14/l08_red.tif")
green.files <- c("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_green.tif", "/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/glacier_09_05_14/l08_green.tif")
blue.files <- c("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_blue.tif", "/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/glacier_09_05_14/l08_blue.tif")

redL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_blue.tif")


#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity6/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

#plot NDVI data 
#higher dvi --> more vegetation on ground
plot(NDVIraster[[1]])

#########################################
##            Question 3               ##
#########################################

#xaxs="i"; yaxs="i" get rid of gaps in plot axes - nevermind
#note - no overlap in x/y grid values
#two different projection systems - even if on same graph,
#zoomed out and only see two blips

#plot side by side
par(mai=c(1,1,1,1))
par(mfrow=c(1,2))
plot(NDVIraster[[1]],axes=TRUE)
plot(g1966, axes=TRUE, add=FALSE) 

########################################

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)
par(mai=c(1,1,1,1))
plot(NDVIraster[[1]])

#########################################
##            Question 4               ##
#########################################

par(mai=c(1,1,1,1))
plot(NDVIraster[[13]], axes = FALSE, box=FALSE)
plot(g2015p, col=NA, border="black",add = TRUE)


#############################################

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   


#########################################
##            Question 5               ##
#########################################

#create column % change in area
g2015p@data$percent.change <- ((g2015p@data$a2015m.sq - g1966p@data$a1966m.sq)/g1966p@data$a1966m.sq)*100

#make spplot
spplot(g2015p, "percent.change")

#########################################


diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#########################################
##            Question 6               ##
#########################################

#identify highest percentage loss
most.loss <- min(g2015p@data$percent.change)

#identify corresponding glacier
max.loss.2015 <- subset(g2015p, g2015p$percent.change==most.loss)
max.loss.name <- max.loss.2015$GLACNAME


#subset remaining years to just boulder glacier
max.loss.1966 <- subset(g1966p, g1966p$GLACNAME == max.loss.name)
max.loss.1998 <- subset(g1998p, g1998p$GLACNAME == max.loss.name)
max.loss.2005 <- subset(g2005p, g2005p$GLACNAME == max.loss.name)

# note: glacial extent in 1966:
#[-80123:-78850, 106841:107222]
par(mai = c(1,1,1,1))
plot(NDVIraster[[13]], axes = FALSE, box=FALSE, xlim = c(-80250,-78700), ylim=c(106300,107600))
plot(max.loss.2015, col=NA, border="black",add = TRUE)
plot(max.loss.2005, col=NA, border="slateblue3",add = TRUE)
plot(max.loss.1998, col=NA, border="steelblue2",add = TRUE)
plot(max.loss.1966, col=NA, border="coral2",add = TRUE)
title("Boulder Glacier (84.72% Loss)")
legend(x = -80200,
       y= 107000,
       box.lty = 0,
       lty = 1,
       legend = c("1966", "1998", "2005", "2015"), 
       col = c("coral2", "steelblue2", "slateblue3", "black"))


#########################################


#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)


#########################################
##            Question 7               ##
#########################################

# lower around white areas (water)
# green areas are far from where glaciers are

#########################################


#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#########################################
##            Question 8               ##
#########################################

#open ended response...
#gdifference - use on datasets
#raster math - directly using on rasterized data

#########################################


meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)


#########################################
##            Question 9               ##
#########################################

#remove zone w/o glaciers
meanChange.noz <- meanChange[-1,]

#add mean change col to 2015 data
g2015p$meanChange <- meanChange.noz[,2]

#plot data color coded by mean change
spplot(g2015p,"meanChange")

#most negative; very low --> decrease in veg over time

#########################################


#########################################
##            Question 10              ##
#########################################

#somewhat inconclusive - analyze map before question 7 ; maybe tie in some info from line 
# graph too --> generally, no trend; BUT some areas have significant inc/dec...
# tie in some sort of discussion related to glaciers

#########################################


#########################################
##            Question 11              ##
#########################################

# average maximum NDVI across all years
NDVImean <- calc(NDVIstack, mean)
plot(NDVImean, axes = FALSE, box = FALSE)

area.sum <- summary(g2015p@data$a2015m.sq)
#add color-coded glacier polygons
g2015p@data$NDVIcol <- ifelse(g2015p@data$a2015m.sq<80653,"black",
                              ifelse(g2015p@data$a2015m.sq<218317, "red",
                                     ifelse(g2015p@data$a2015m.sq<503914, "olivedrab",
                                            "deepskyblue4")))


plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)
legend(x="bottomleft",
       lty = 1,
       lw = 4,
       title = "Glacier Area (in square meters)",
       legend = c("<80,653", "80,653-218,316", "218,317-503913", ">503,914"),
       col = c("black", "red", "olivedrab", "deepskyblue4"))