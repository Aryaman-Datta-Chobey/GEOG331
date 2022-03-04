library(lubridate)
#creating assert function for Q5
assert <- function(statement,err.message){
if(statement == FALSE){
    print(err.message)
  }
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")
#Question 3
datW <- read.csv("C:\\Users\\HP\\Desktop\\comp sci\\GEOG331\\GEOG331\\Activty 3 Intro to data cleaning and QAQC\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
sensorInfo <-   read.csv("C:\\Users\\HP\\Desktop\\comp sci\\GEOG331\\GEOG331\\Activty 3 Intro to data cleaning and QAQC\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
colnames(datW) <-   colnames(sensorInfo)
#Question 4
#date calculuations
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
datW$doy <- yday(dates) #calculate day of year
datW$hour <- hour(dates) + (minute(dates)/60) #calculate hour in the day
datW$DD <- datW$doy + (datW$hour/24) #calculate decimal day of year
#identifying missing data
length(which(is.na(datW$soil.moisture)))#707
length(which(is.na(datW$soil.temp)))#707
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature) #QA/QC for temp
quantile(datW$air.tempQ1)

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)
# QUESTION 5
assert(length(datW$precipitation) == length(datW$lightning.acvitivy),
       "error: unequal length")

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
#QUESTION 6- QA/QC for windspeed, filtering out data collected during storms , defined by lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))
print(length(is.na(datW$wind.speedQ1)==FALSE))
assert(length(is.na(datW$wind.speedQ1)==FALSE) == length(datW$wind.speedQ1),
       "error: unequal length")
assert(length(length((is.na(datW$wind.speedQ1)[which(is.na(datW$wind.speedQ1==TRUE))]))) == length(datW$wind.speedQ1),
       "error: unequal length")
assert(length(is.na(datW$wind.speedQ1)==FALSE) == length(datW$wind.speedQ1),
       "error: unequal length")
print(length((is.na(datW$wind.speedQ1)[which(is.na(datW$wind.speedQ1==TRUE))])))
print(length(ifelse(datW$precipitation  < 2 & datW$lightning.acvitivy ==0, NA,
                    ifelse(datW$precipitation < 5, NA, datW$wind.speed))))
plot(datW$DD , datW$wind.speedQ1, xlab = "Day of Year", ylab = "Windspeed",
     type="n")
points(datW$DD[datW$wind.speedQ1 > 0], datW$wind.speedQ1[datW$wind.speedQ1 > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15) 
lines(datW$DD[datW$wind.speedQ1 > 0], datW$wind.speedQ1[datW$wind.speedQ1 > 0],
       col= "tomato3", pch=15) 
#Question 7
datW$soil.moistureQ1<-  ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                               ifelse(datW$precipitation > 5, NA, datW$soil.moisture))
datW$soil.tempQ1<-  ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                               ifelse(datW$precipitation > 5, NA, datW$soil.temp))

plot(datW$DD , datW$soil.moistureQ1, xlab = "Day of Year", ylab = "Soil Moisture",
     type="n")
points(datW$DD[datW$soil.moistureQ1!= NA], datW$soil.moistureQ1[datW$soil.moistureQ1!= NA],
       col= rgb(95/255,158/255,160/255,.5), pch=15) 

