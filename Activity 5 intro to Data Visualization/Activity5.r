#Q1 
#load in lubridate
library(lubridate)
library(tidyverse)
library(patchwork)
library(dplyr)

#read in streamflow data
datH <- read.csv("/Volumes/GEOG331_S22/data/streamflow/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("/Volumes/GEOG331_S22/data/streamflow/2049867.csv")                            
head(datP)

#only use most reliable measurements using the USGS data flag A
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)
#Q2 
#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))   

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#Q3 - code for getting no. of observations
dim(datH) # <-number of observations for Streamflow data   
dim(datD) #streamflow observations approved for publication
dim(datP) # <-number of observations for Precipitation data    


#Q4 question about paste function and plot resizing answered in doc (screenshots from old plot included)
# Q5- code for adding a line for the 2017 observations
#to avoid redundant code, only the final version of the plot with the 2017 line is included
#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean") #mean
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd") #standard deviation
colnames(sdF) <- c("doy","dailySD")
Discharge2017 <- datD[datD$year==2017,]# 2017 data 
months<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept",
           "Oct","Nov","Dec")


#start new plot
dev.new(width=14,height=8)
# making the plot
par(mai=c(1,1,1,1))

plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month of Year
      DoY", #altered for Q5
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,180),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes

polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

lines(Discharge2017 $doy, Discharge2017 $discharge, type = "l",
      lwd=1, col="green")

axis(1, seq(0,366, by=33), #tick intervals
     lab=months) #tick labels
axis(2, seq(0,180, by=20),
     seq(0,180, by=20),
     las = 2)#show ticks at 90 degree angle

legend("topright", c("mean","1 standard deviation", "2017 Streamflow"), #legend items
       lwd=c(2,NA,1),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"green"),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border

#Q6  code for querying apropriate statistical calaculuations
range(Discharge2017 $discharge) 
mean(Discharge2017 $discharge)
sd(Discharge2017 $discharge)
median(Discharge2017 $discharge)
quantile(Discharge2017 $discharge)

#Q7

datPLengths <- aggregate(x = datP, by = list(datP$year, datP$doy), FUN=length) # group datP by year and doy, then summarize it by legnth ( number of occurences of this datP, doy combination , which should be equal to the number of hourly observations)
datP <- left_join(datP, datPLengths, by =c("year" = "Group.1","doy" = "Group.2")) #joining length data 
datPFullDay= filter(datP,hour.y==24) #filtered dataframe containing fulldays

# Creating the plot
par(mai=c(1,1,1,1))
plot(datD$decYear, 
     datD$discharge,
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     ylim=c(0,500))

polygon(datPFullDay$decYear,#x coordinates
        0,#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
points(datPFullDay$decYear.x, 
       datPFullDay$HPCP.x, 
       col = 'red', pch=15, cex=0.5)

legend("topright", 
       c("Discharge","Days with all prcp measures"), #legend items
       lwd=c(2,NA),#lines
       col=c("black","red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

# Hydrographs

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP.x))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP.x) + yl


par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay.x[i]-0.017,hydroP$decDay.x[i]-0.017,
            hydroP$decDay.x[i]+0.017,hydroP$decDay.x[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}



#Q8

# Looking at  the fullday precipitation dataframe from the previous question , I picked DOY 7 and 8 in 2009  as my winter days

#subsest discharge and precipitation within range of interest
hydroDQ8 <- datD[datD$doy >= 7 & datD$doy < 8 & datD$year == 2009,]
hydroPQ8 <- datP[datP$doy >= 7 & datP$doy < 8 & datP$year == 2009,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
ylQ8 <- floor(min(hydroDQ8$discharge))-1
#ceiling rounds up to the integer
yhQ8 <- ceiling(max(hydroDQ8$discharge))+1
#minimum and maximum range of precipitation to plot
plQ8 <- 0
pmQ8 <-  ceiling(max(hydroPQ8$HPCP.x))+.5
#scale precipitation to fit on the 
hydroPQ8$pscale <- (((yhQ8-ylQ8)/(pmQ8-plQ8)) * hydroPQ8$HPCP.x) + ylQ8

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroDQ8$decDay,
     hydroDQ8$discharge, 
     type="l", 
     ylim=c(ylQ8,yhQ8), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroPQ8)){
  polygon(c(hydroPQ8$decDay.x[i]-0.017,hydroPQ8$decDay.x[i]-0.017,
            hydroPQ8$decDay.x[i]+0.017,hydroPQ8$decDay.x[i]+0.017),
          c(ylQ8,hydroPQ8$pscale[i],hydroPQ8$pscale[i],ylQ8),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#violin plots
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#Q9
# creating a dataframe for 2016 and 2017 (each) with a column for labelling seasons
Discharge2016 <- datD[datD$year == 2016,]
Discharge2016$season <- ifelse(Discharge2016$doy < 32, "Winter",
                  ifelse(Discharge2016$doy < 153, "Spring",
                         ifelse(Discharge2016$doy < 245, "Summer",
                                ifelse(Discharge2016$doy < 336, "Fall", "Winter"))))
Discharge2017 <- datD[datD$year == 2017,]
Discharge2017$season <- ifelse(Discharge2017$doy < 32, "Winter",
                               ifelse(Discharge2017$doy < 153, "Spring",
                                      ifelse(Discharge2017$doy < 245, "Summer",
                                             ifelse(Discharge2017$doy < 336, "Fall", "Winter"))))

# making the violin plots

violin2016 <- ggplot(data= Discharge2016, aes(season,discharge,fill=season)) + #fill the violin plot by season for aesthetics 
  geom_violin()+
  labs(y=expression(paste("Discharge (ft"^"3 ","sec"^"-1",")")),
       x = "Season",
       title="Discharge in 2016 by season")+theme_minimal()


violin2017 <- ggplot(data= Discharge2017, aes(season,discharge,fill=season)) + 
  geom_violin()+
  labs(y=expression(paste("Discharge (ft"^"3 ","sec"^"-1",")")),
       x = "Season",
       title="Discharge in 2017 by season")+
  theme_minimal()

violin2016
violin2017
