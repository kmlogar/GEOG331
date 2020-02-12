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
datW <- read.csv("y:\\Students\\klogar\\a03\\bewkes_weather.csv",
                 na.strings = c("#N/A"),
                 skip = 3,
                 header = FALSE)

#preview data
print(datW[1,])

#get sensor info from file
#this data table will contain all relevant units
sensorInfo <- read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
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