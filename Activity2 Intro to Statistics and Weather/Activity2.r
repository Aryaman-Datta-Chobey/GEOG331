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
factor_vector<- factor(char_vector)
#Question 3
help(hist)
datW$siteN <- as.numeric(datW$NAME)
hist(datW$siteN,
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
