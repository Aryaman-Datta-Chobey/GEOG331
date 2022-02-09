#question 1
datW <- read.csv("C:\\Users\\HP\\Desktop\\comp sci\\GEOG331\\GEOG331\\Activity2 Intro to Statistics and Weather\\Noaa weather\\2011124.csv",
                 stringsAsFactors = T)
str(datW)
#checking number of rows and columns
nrow(datW)
ncol(datW)
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))
# Question 2
char_vector <-c("a","bc","def","hijk","lmnop")
num_vector<-c(1,-2,44,3.3,0)
int_vector<- c(1,-2,3,4,5)
factor_vector<- factor(char_vector) bruh
#Question 3
datW$siteN <- as.numeric(datW$NAME)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
#Histogram for Aberdeen
hist(datW$TAVE[datW$siteN==1],
     freq=FALSE,  
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50", 
     border="white")
# mean line
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 1,
       lwd = 3)
#1 standard deviation above
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#1 standard deviation below
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
help(hist)
help(paste)
#Question 4
#function for getting histograms with mean and standard deviation lines for different stations 
SiteSummary<-function(SiteNumber, graphcol,linecol){
  #intialize variables
  datW$siteN <- as.numeric(datW$NAME)
  datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
  hist(datW$TAVE[datW$siteN==SiteNumber],
       freq=FALSE,  
       main = paste(levels(datW$NAME)[SiteNumber]),
       xlab = "Average daily temperature (degrees C)", 
       ylab="Relative frequency",
       col=graphcol, 
       border="white")
  # mean line
  abline(v = mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE), 
         col = linecol, 
         lty = 1,
         lwd = 3)
  #1 standard deviation above
  abline(v = mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE), 
         col = linecol, 
         lty = 3,
         lwd = 3)
  #1 standard deviation below
  abline(v = mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE), 
         col = linecol, 
         lty = 3,
         lwd = 3)
}
#Histogram for Livemore
hist(datW$TAVE[datW$siteN==2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="purple",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#Histogram for Mandan Experiment Station
hist(datW$TAVE[datW$siteN==3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="green",
     border="white")
#Histogram for Mormon flat
hist(datW$TAVE[datW$siteN==4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="orange",
     border="white")



print(datW$NAME)
print(datW$TAVE[datW$siteN])
print(datW$TAVE[datW$siteN==1])
print(datW$TAVE[datW$siteN==2])
