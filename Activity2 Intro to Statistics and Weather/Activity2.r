#question 1
datW <- read.csv("C:\\Users\\HP\\Desktop\\comp sci\\GEOG331\\GEOG331\\Activity2 Intro to Statistics and Weather\\Noaa weather\\2011124.csv",
                 stringsAsFactors = T)
#checking number of rows and columns
nrow(datW)
ncol(datW)
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))
# Question 2
char_vector <-c("a","bc","def","hijk","lmnop")
num_vector<-c(1,-2,44,3.3,0)
int_vector<- c(1,-2,3,4,5)
factor_vector<- factor(char_vector) 
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

#Question 4
#function for getting histograms with mean and standard deviation lines for different stations 
SiteSummary_AvgTemp<-function(SiteNumber, graphcol,linecol){
  #intialize variables
  datW$siteN <- as.numeric(datW$NAME)
  datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
  h1<-hist(datW$TAVE[datW$siteN==SiteNumber],
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
  #Normal distrubtion dotted graph for q5
  #Sequences have been set between mean-3standard deviations and mean+3 stard deviations for the X ab=nd Y axis to create a more universersal probability density function plot than -10,30 which is mainly apropriate for aberdeen
  x.plot <- seq(mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE) - 3*sd(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE),mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE) + 3*sd(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE), length.out = 100)
  y.plot <-  dnorm(seq(mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE) - 3*sd(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE),mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE) + 3*sd(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE), length.out = 100),
                   mean(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE),
                   sd(datW$TAVE[datW$siteN == SiteNumber],na.rm=TRUE))
  y.scaled <- (max(h1$density)/max(y.plot)) * y.plot
  points(x.plot,
         y.scaled, 
         type = "l", 
         col = "royalblue3",
         lwd = 4, 
         lty = 2)
}
par(mfrow=c(2,2))
SiteSummary_AvgTemp(1,"grey50","tomato3") #Histogram for Aberdeen
SiteSummary_AvgTemp(2,"purple","tomato3") #Histogram for Livemore
SiteSummary_AvgTemp(3,"green","tomato3") #Histogram for Mandan Experiment Station
SiteSummary_AvgTemp(4,"yellow","tomato3") #Histogram for Mormon flat

#Question 6
qnorm(0.95,
      mean(4+datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
help(qnorm)
#Question 7

