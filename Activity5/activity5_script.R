#Kayla Logar
#Activity 5

#load in lubridate
library(lubridate)

#read in streamflow data
#/Users/kaylal26/Documents/ColgateAcademics/Spring2020/EnvironmentalDataScience/GitHub/GEOG331/Activity3/bewkes_weather.csv
#y:\\Students\\klogar\\a05\\stream_flow_data.csv
datH <- read.csv("y:\\Students\\klogar\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("y:\\Students\\klogar\\a05\\2049867.csv")
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow ####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation ####
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year
datP$year <- year(dateP)

#### get decimal formats ####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD) + (minute(timesD)/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year), datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP
datP$hour <- hour(dateP) + (minute(dateP)/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year), datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

##########################################
##              Question 3              ##
##########################################
#number of observations of streamflow data
nrow(datH)
#number of most reliable observations of streamflow data
nrow(datD)
#number of observations of precipitation data
nrow(datP)

#Basic Plot Formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN = "mean")
colnames(aveF) <- c("doy", "dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy", "dailySD")

#start new plot
dev.new(width=8, height=8)

#bigger margins
par(mai = c(1,1,1,1))
#make plot
plot(aveF$doy, aveF$dailyAve,
     type = "l",
     xlab = "Year",
     ylab = expression(paste("Discharge ft"^"3 ", "sec"^"-1")),
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy, aveF$dailyAve,
     type = "l",
     xlab = "Year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs = "i") #remove gaps from axes
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)), #x coordinates
        c(aveF$dailyAve - sdF$dailySD, rev(aveF$dailyAve+sdF$dailySD)), #ycoord
        col = rgb(0.392, 0.584, 0.929, 0.2), #color that is semi-transparent
        border = NA) #no border

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy, aveF$dailyAve,
     type="l",
     xlab = "Year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd = 2, 
     ylim = c(0,90),
     xaxs = "i", yaxs="i", #remove gaps from axes
     axes = FALSE) #no axes
polygon(c(aveF$doy, rev(aveF$doy)), #x coordinates
        c(aveF$dailyAve-sdF$dailySD, rev(aveF$dailyAve+sdF$dailySD)), #ycoord
        col = rgb(0.392, 0.584, 0.929, 0.2), #color that is semi-transparent
        border = NA) #no border
axis(1, seq(0,360,by=40), #tick intervals
     lab = seq(0,360,by=40)) #tick labels
axis(2,seq(0,80,by=20),
     seq(0,80,by=20),
     las = 2) #show ticks at 90 degree angle


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#############################
##      Question 5         ##
#############################
start.of.months <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month.labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
dat2017<- subset(datD,datD$year == 2017)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,200),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
lines(dat2017$doy, dat2017$discharge,
      col = "plum4")
axis(1, start.of.months, #tick intervals
     lab=month.labels) #tick labels
axis(2, seq(0,160, by=20),
     seq(0,160, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 observations"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "plum4"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

####  Making a Hydrograph ####

##############################
##      Question 7          ##
##############################

#create a new data frame that has the number of measurements taken each day
datM <- aggregate(datP$doy,by=list(datP$doy, datP$year), FUN = "length")

#create a new data frame with only the days where measurements were taken every hour of the day
datM.full <- subset(datM, datM$x == 24)
datM.full$decYear <- ifelse(leap_year(datM.full$Group.2), datM.full$Group.2 + ((datM.full$Group.1-1)/366),
                            datM.full$Group.2 + ((datM.full$Group.1-1)/365))
num.full <- nrow(datM.full)
yvals <- rep(390,num.full)

#plot data
par(mai=c(1,1,1,1))
plot(datD$decYear,datD$discharge, 
     type="l", 
     xlab="Decimal Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,460),
     col = "skyblue4",
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
axis(1, seq(2007, 2020, by=1), #tick intervals
     lab=seq(2007, 2020, by=1)) #tick labels
axis(2, seq(0,400, by=20),
     seq(0,400, by=20),
     las = 2)#show ticks at 90 degree angle
points(datM.full$decYear, 
       yvals,
       pch=1,
       lwd=2,
       col = "skyblue3")
legend("topright", c("Streamflow Measurements","Dates with full precipitation observations"), #legend items
       lwd=c(2,NA),#lines
       col=c("skyblue4","skyblue3"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#subset discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#check minimum flow for period
min(hydroD$discharge)


#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge)) - 1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
p1 <- 0
pm <- ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit 
hydroP$pscale <- (((yh - yl)/(pm - p1)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge,
     type = "l",
     ylim = c(y1, yh),
     lwd = 2,
     xlab = "Day of year",
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929, 0.2), border = NA)
}

#################################
##         Question 8          ##
#################################

#subset discharge and and precipitation within range of interest
hydro2D <- subset(datD, datD$year == 2012 & datD$doy >= 59 & datD$doy < 61)
hydro2P <- subset(datP, datP$year == 2012 & datP$doy >= 59 & datP$doy < 61)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydro2D$discharge))-1
#celing rounds up to the integer
yh2 <- ceiling(max(hydro2D$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydro2P$HPCP))+.5
#scale precipitation to fit on the 
hydro2P$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydro2P$HPCP) + yl2

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydro2D$decDay,
     hydro2D$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydro2P)){
        polygon(c(hydro2P$decDay[i]-0.017,hydro2P$decDay[i]-0.017,
                  hydro2P$decDay[i]+0.017,hydro2P$decDay[i]+0.017),
                c(yl2,hydro2P$pscale[i],hydro2P$pscale[i],yl2),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


####### Making Box Plots and Violin Plots ######
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data = datD, aes(yearPlot, discharge)) +
        geom_boxplot()

#make a violin plot
ggplot(data = datD, aes(yearPlot, discharge)) +
        geom_violin()


#########################################
##              Question 10            ##
#########################################
library(gridExtra)

#create dataframe of 2016 data
dat2016 <- subset(datD, datD$year == 2016)

#create columns of data for seasons
dat2016$season <- ifelse(dat2016$doy>= 61 & dat2016$doy <= 152, "Spring",
                         ifelse(dat2016$doy >= 153 & dat2016$doy <= 244, "Summer",
                                ifelse(dat2016$doy >= 245 & dat2016$doy <= 335, "Fall", "Winter")))

dat2017$season <- ifelse(dat2017$doy>= 60 & dat2017$doy <= 151, "Spring",
                         ifelse(dat2017$doy >= 152 & dat2017$doy <= 243, "Summer",
                                ifelse(dat2017$doy >= 244 & dat2017$doy <= 334, "Fall", "Winter")))

#store seasons as factors
dat2016$season <- factor(dat2016$season, levels = c("Winter", "Spring", "Summer", "Fall"))
dat2017$season <- factor(dat2017$season, levels = c("Winter", "Spring", "Summer", "Fall"))


#plot 2016 data
p16 <- ggplot(dat2016, aes(x=season, y=discharge))+
        geom_violin(col="lightblue4",
                    fill="lightblue")+
        theme_light()+
        xlab("Season")+
        ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
        geom_hline(yintercept = 0)+
        ggtitle("Discharge in 2016 by Season")

#plot 2017 data
p17 <- ggplot(dat2017, aes(x=season, y=discharge))+
        geom_violin(col = "mediumpurple",
                    fill = "lavender")+
        theme_light()+
        xlab("Season")+
        ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
        geom_hline(yintercept = 0)+
        ggtitle("Discharge in 2017 by Season")

grid.arrange(p16, p17, nrow = 2)
