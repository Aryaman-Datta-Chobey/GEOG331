library(terra)
library(sf)
library(caret)
library(randomForest)

#WORKING DIRECTORY
setwd("/Volumes/class/GEOG331_S22/students/achobey/Geog331 class project aryaman") #setwd("/Users/aryaman/Desktop/Geog331 class project aryaman/") working directory on my local device
#RGB Files
#CYNFL017_RGB_file<-file.path("resource_map_doi_10_18739_A2ZC7RV6H/data/CYN/TR2/RU_CYN_TR2_FL017_RGB.tif")
#CYNFL017_RGB<-rast(CYNFL017_RGB_files)
#plot(CYNFL017_RGB)
#plot(CYNFL016_)

#CYNFL016_RGB_file<-file.path("resource_map_doi_10_18739_A2ZC7RV6H/data/CYN/TR1/FL016M/RU_CYN_TR1_FL016_RGB.tif")
#CYNFL016_RGB<-rast(CYNFL016_RGB_files)
#non rgb files
CYNFL016M_files <- list.files(path = "resource_map_doi_10_18739_A2ZC7RV6H/data/CYN/TR1/FL016M", pattern = "FL016M", full.names = T)
CYNFL016M<-rast(CYNFL016M_files)
#plot(CYNFL016M)
band<- c("green","ndvi","nir","red_edge","red")
names(CYNFL016M)<-band
#ground truth data
SHP_FILE<-file.path("ground_truth/cyn_landover_pts.shp")
GroundTruth<-as.data.frame(read_sf(SHP_FILE))
GroundTruth$coordinate<-crds(vect(GroundTruth$geometry))
#ggplot()+geom_sf(data=shp,color="black",fill=CYNFL016M)
#plot(CYNFL016M)#plot(CYNFL016M[[1]])
#plot(GroundTruth,add=TRUE)
#plot(GroundTruth,CYNFL016M)


#setting up training and validation data
#set seed so samples always the same
set.seed(12153)
#randomly select the data in each dataset to be  used
sampleType <- rep("train",404)
#samples to randomly convert to validation data
sampleSamp <- sample(seq(1,404),202)
#convert these random samples from training to validation
sampleType[sampleSamp] <- "valid"
GroundTruth$sampleType<-sampleType
#Assign numerical id to each ladncover type
GroundTruth$LandcoverID<-ifelse(GroundTruth$Landcover== "ground", 1,
                                ifelse(GroundTruth$Landcover== "larch", 2,
                                       ifelse(GroundTruth$Landcover== "lichen", 3,
                                              ifelse(GroundTruth$Landcover== "shrub", 4,
                                                     ifelse(GroundTruth$Landcover== "water", 5,0)))))
#create df with only numerical id for landcover type , coordinates and sample type
GroundTruthID<- subset(GroundTruth,select=-c(Landcover,geometry))


#extract reflectance values for our sample points
reflectance <- data.frame(extract(CYNFL016M,GroundTruthID[,0:1]))[,-1]

#combine point information with raster information
GTReflectanceNA <- cbind(GroundTruthID,reflectance)
GTReflectance<-na.omit(GTReflectanceNA)
#subset into two different data frames
trainGT <- GTReflectance[GroundTruth$sampleType == "train",]
validGT<- GTReflectance[GroundTruth$sampleType == "valid",]
#######RANDOM FORRESTS#######
#Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:sqrt(4)) # number of variables available for splitting at each tree node

# Train the random forest model to the UAV data
#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainGT[,c(4:7)], #digital number data
                         y = as.factor(trainGT$LandcoverID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model

# Apply the random forest model to the UAV data
rf_prediction <- terra::predict(CYNFL016M, rf_model, na.rm = T)
#view prediction
plot(rf_prediction)
