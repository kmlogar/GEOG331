#load any necessary libraries
library(ggplot2)
library(gridExtra)

# create a function. the names of the arguments for your function will be in parentheses.
# Everything in curly brackets will be run each time the function is run.

assert <- function(statement, err.message) {
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE) {
    print(err.message)
  }
}

# check how statement works
# evaluate a false statement
assert(1==2, "error: unequal values")

# evaluate a true statement
assert(2 == 2, "error: unequal values")

#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")


#read in the data file
#skip the first 3 rows since there is additional column info
#specify the NA is designated differently
#y:\\Students\\klogar\\a03\\bewkes_weather.csv
#/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity3/bewkes_weather.csv
datW <- read.csv("y:\\Students\\klogar\\a03\\bewkes_weather.csv",
                 na.strings = c("#N/A"),
                 skip = 3,
                 header = FALSE)

#preview data
print(datW[1,])

#get sensor info from file
#this data table will contain all relevant units
#y:\\Students\\hkropp\\a03\\bewkes_weather.csv
sensorInfo <- read.csv("y:\\Students\\klogar\\a03\\bewkes_weather.csv",
                       na.strings=c("#N/A"), 
                       nrows = 2)

print(sensorInfo)

#get column names from sensorInfo table
#and set weather station colnames to be the same
colnames(datW) <- colnames(sensorInfo)
#preview data
print(datW[1,])


#use install.packages to install lubridate
#install.packages(c("lubridate"))
#it is helpful to comment this line after running it on the computer
# and the package installs so that it's not unnecessarily repeated

library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz = "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed 
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year", 
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", 
     xlab = "Day of Year", 
     ylab = "Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusion.
#It can be particularly confusing when you are just learning R
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement is true.
#the last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value.
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

############################################
##    Question 4                          ##
############################################

#look at days with really low air temperature
low.temp <- datW[datW$air.tempQ1 < 8,]
low.temp

low.temp.Q1.F <- (low.temp$air.tempQ1 * 9 / 5) + 32

#look at days with really high air temperature
high.temp <- datW[datW$air.tempQ1 > 33,]
high.temp

high.temp.Q1.F <- (high.temp$air.tempQ1 * 9 / 5) + 32






#plot precipitation and lightning strikes on the same plot
#normalize lightning strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked 
#make it empty to start and add in features
plot(datW$DD, datW$precipitation,
     xlab = "Day of Year",
     ylab = "Precipitation & lightning",
     type = "n")
#plot precipitation points only when there is precipitation
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col = rgb(95/255, 158/255, 160/255, .5), pch=15)

#plot lightning points only when there is lightning
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col = "tomato3", pch = 19)

#######################################
##           Question 5              ##
#######################################

assert(length(lightscale) == nrow(datW), "error: unequal length")

#filter out storms in wind and air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5mm.
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#######################################
##           Question 6              ##
#######################################
#create a new wind speed column
datW$wind.speedQ1 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                           ifelse(datW$precipitation > 5, NA, datW$wind.speed))
#plot the new windspeed data
ggplot(datW, aes(datW$DD, datW$wind.speedQ1))+
  geom_line()+
  geom_point(alpha = 0.5,
             colour = "steelblue")+
  theme_bw()+
  xlab("Day of Year")+
  geom_hline(yintercept = 0)+
  ylab("Wind Speed")

#describe difference between overall windspeed data and filtered data
summary(datW$wind.speed)
summary(datW$wind.speedQ1)

#test with assert
for (i in 1:nrow(datW)) {
  if ((datW$precipitation[i] >= 2 & datW$lightning.acvitivy[i] > 0) || datW$precipitation > 5) {
    assert(is.na(datW$wind.speedQ1[i]), "error: not properly filtered")
  }
}

#######################################
##           Question 7              ##
#######################################

#normalize soil temperature to match soil moisture
datW$norm.soil.temp <- (max(datW$soil.moisture, na.rm=TRUE)/max(datW$soil.temp, na.rm = TRUE)) * datW$soil.temp

#plot normalized soil temperature with soil moisture
#note that, while there are many dramatic changes in normalized soil temperature,
#soil moisture is only changing moderately in the days leading up to the outage
g1 <- ggplot(datW, aes(DD))+
  geom_line(y=datW$norm.soil.temp,
            colour = "steelblue")+
  geom_line(y=datW$soil.moisture,
            colour = "mediumpurple")+
  theme_bw()+
  xlab("Day of Year")+
  ylab("Soil Moisture & Normalized Soil Temperature")


#for further context, plot of soil temperature and air temperature
g2 <- ggplot(datW, aes(DD))+
  geom_point(y = datW$precipitation,
             colour = "steelblue")+
  theme_bw()+
  xlab("Day of Year")+
  ylab("Precipitation")

grid.arrange(g1, g2, nrow = 2)

#######################################
##           Question 8              ##
#######################################
#compute desired averages and total
mean(datW$air.tempQ2, na.rm = TRUE)
mean(datW$wind.speedQ1, na.rm = TRUE)
mean(datW$soil.moisture, na.rm = TRUE)
mean(datW$soil.temp, na.rm = TRUE)
sum(datW$precipitation, na.rm = TRUE)

#find date ranges
head(datW)
tail(datW)

datW.soil <- subset(datW,!is.na(datW$soil.moisture))
head(datW.soil)
tail(datW.soil)

#determine number of observations used in each calculation
nrow(subset(datW, !is.na(datW$air.tempQ2)))
nrow(subset(datW, !is.na(datW$wind.speedQ1)))
nrow(subset(datW, !is.na(datW$soil.moisture)))
nrow(subset(datW, !is.na(datW$soil.temp)))
nrow(subset(datW, !is.na(datW$precipitation)))


#######################################
##           Question 9              ##
#######################################

######## Soil Moisture ################
p1 <- ggplot(datW, aes(DD, soil.moisture))+
  #geom_line()+
  geom_point(colour = "steelblue",
             alpha = 0.5)+
  theme_bw()+
  xlab("Day of Year")+
  ylab("Soil Moisture (meters cubed per meter cubed)")

####### Air Temperature ###############
p2 <- ggplot(datW, aes(DD, datW$air.tempQ2))+
  #geom_line()+
  geom_point(colour = "darkolivegreen4",
             alpha = 0.5)+
  theme_bw()+
  xlab("Day of Year")+
  ylab("Air Temperature (degrees C)")

####### Soil Temperature ##############
p3 <- ggplot(datW, aes(DD, datW$soil.temp))+
  #geom_line()+
  geom_point(colour = "mediumpurple",
             alpha = 0.5)+
  theme_bw()+
  xlab("Day of Year")+
  ylab("Soil Temperature (degrees C)")

####### Precipitation #################
p4 <- ggplot(datW, aes(DD, datW$precipitation))+
  #geom_line()+
  geom_point(colour = "lightpink4",
             alpha = 0.5)+
  theme_bw()+
  xlab("Day of Year")+
  ylab("Precipitation (mm)")

grid.arrange(p1, p3, p4, p2, nrow = 2, ncol = 2)
