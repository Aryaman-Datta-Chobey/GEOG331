#question 1
datW <- read.csv("C:\\Users\\HP\\Desktop\\comp sci\\GEOG331\\GEOG331\\Activity2 Intro to Statistics and Weather\\Noaa weather\\2011124.csv",
                 stringsAsFactors = T)
str(datW)
#checking number of rows and columns
nrow(datW)
ncol(datW)
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
datW$year <- as.numeric(format(datW$dateF,"%Y"))
print(datw$year)
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
#generating 4 histograms in one window
par(mfrow=c(2,2))
SiteSummary_AvgTemp(1,"grey50","tomato3") #Histogram for Aberdeen
SiteSummary_AvgTemp(2,"purple","tomato3") #Histogram for Livemore
SiteSummary_AvgTemp(3,"green","tomato3") #Histogram for Mandan Experiment Station
SiteSummary_AvgTemp(4,"yellow","tomato3") #Histogram for Mormon flat

#Question 6
1-pnorm(qnorm(0.95, #using qnorm to get the temperature at which extreme temperatures start for aberdeen
            mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
            sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)),
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4, # shifting mean by 4
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))  #retaining orignal standard deviation


#Question 7
h2<-hist(datW$PRCP[datW$siteN==1],
         freq=FALSE,  
         main = paste(levels(datW$NAME)[1]),
         xlab = "Average daily precipitation ", 
         ylab="Relative frequency",
         col="purple", 
         border="white")
#Question 8
#creating a dataframe with aberdeen values 
datWAb<- datW[which(datW$siteN==1), , drop = FALSE]
#Creating a data frame for annual precipitation for aberdeen
year<-seq(1930,2019)
AnnualPRCPAberdeen<-data.frame(year,AnnualPRCP=rep(0,length(year)))
#For loop to add annual precipitation values to data frame
 for(i in datW$year){
   AnnualPRCPAberdeen$AnnualPRCP[AnnualPRCPAberdeen$year==i]<-sum(datWAb$PRCP[datWAb$DATE[datWAb$year==i]],na.rm = TRUE)
 }
h3<-hist(AnnualPRCPAberdeen$AnnualPRCP,
         freq=FALSE,  
         main = paste(levels(datWAb$NAME)[1]),
         xlab = "yearly precipitation (mm) ", 
         ylab="Relative frequency",
         col="purple", 
         border="white")

#Question 9
#Mean temps
mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) #Aberdeen
mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) #LIVERMORE
mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) #MANDAN EXPERIMENT STATION
mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) #MORMON FLAT
mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) #MORRISVILLE

#Mean precipitation
#Aberdeen
mean(AnnualPRCPAberdeen$AnnualPRCP,na.rm=TRUE)
#LIVERMORE
#creating a dataframe with site values 
datWLiv<- datW[which(datW$siteN==2), , drop = FALSE]
#Creating a data frame for annual precipitation 
AnnualPRCPLivermore<-data.frame(year,AnnualPRCP=rep(0,length(year)))
#For loop to add annual precipitation values to data frame
for(i in datW$year){
  AnnualPRCPLivermore$AnnualPRCP[AnnualPRCPLivermore$year==i]<-sum(datWLiv$PRCP[datWLiv$DATE[datWLiv$year==i]],na.rm = TRUE)
}
mean(AnnualPRCPLivermore$AnnualPRCP,na.rm=TRUE) #mean calculation

#MANDAN EXPERIMENT STATION
#creating a dataframe with site values 
datWMan<- datW[which(datW$siteN==3), , drop = FALSE]
#Creating a data frame for annual precipitation 
AnnualPRCPMandan<-data.frame(year,AnnualPRCP=rep(0,length(year)))
#For loop to add annual precipitation values to data frame
for(i in datW$year){
  AnnualPRCPMandan$AnnualPRCP[AnnualPRCPMandan$year==i]<-sum(datWMan$PRCP[datWMan$DATE[datWMan$year==i]],na.rm = TRUE)
}
mean(AnnualPRCPMandan$AnnualPRCP,na.rm=TRUE) #mean calculation

#MORMON FLAT
#creating a dataframe with site values 
datWMorm<- datW[which(datW$siteN==4), , drop = FALSE]
#Creating a data frame for annual precipitation 
AnnualPRCPMorm<-data.frame(year,AnnualPRCP=rep(0,length(year)))
#For loop to add annual precipitation values to data frame
for(i in datW$year){
  AnnualPRCPMorm$AnnualPRCP[AnnualPRCPMorm$year==i]<-sum(datWMorm$PRCP[datWMorm$DATE[datWMorm$year==i]],na.rm = TRUE)
}
mean(AnnualPRCPMorm$AnnualPRCP,na.rm=TRUE) #mean calculation
#MORRISVILLE
#creating a dataframe with site values 
datWMorris<- datW[which(datW$siteN==5), , drop = FALSE]
#Creating a data frame for annual precipitation 
AnnualPRCPMorris<-data.frame(year,AnnualPRCP=rep(0,length(year)))
#For loop to add annual precipitation values to data frame
for(i in datW$year){
  AnnualPRCPMorris$AnnualPRCP[AnnualPRCPMorris$year==i]<-sum(datWMorris$PRCP[datWMorris$DATE[datWMorris$year==i]],na.rm = TRUE)
}
mean(AnnualPRCPMorris$AnnualPRCP,na.rm=TRUE) #mean calculation

