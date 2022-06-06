################Loading Libraries and Setting Working Directory##########################
library(terra)
library(sf)
library(caret)
library(randomForest)
setwd("/Users/aryaman/Desktop/Geog331 class project aryaman/") #working directory on my local device#setwd("/Volumes/GEOG331_S22/students/achobey/Geog331 class project aryaman") 
################File Querying and Rasterization##########################
#CYNFL016M <-data pertaining to flight that for CYN transect 1
CYNFL016M_files <- list.files(path = "resource_map_doi_10_18739_A2ZC7RV6H/data/CYN/TR1/FL016M", pattern = "FL016M", full.names = T)
CYNFL016M<-rast(CYNFL016M_files)
band<- c("green","ndvi","nir","red_edge","red")
names(CYNFL016M)<-band
plot(CYNFL016M)
#ground truth data
SHP_FILE<-file.path("ground_truth/cyn_landover_pts.shp")
GroundTruth<-as.data.frame(read_sf(SHP_FILE))
GroundTruth$coordinate<-crds(vect(GroundTruth$geometry))

#CYNFL020 
CYNFL020M_files <- list.files(path = "resource_map_doi_10_18739_A2ZC7RV6H/data/CYN/TR1/FL020M", pattern = "FL020M", full.names = T)
CYNFL020M<-rast(CYNFL020M_files)
band<- c("green","ndvi","nir","red_edge","red")
names(CYNFL020M)<-band
#plot(CYNFL020M)

#CYNFL017 UAV data pertaining to CYN transect2 
CYNFL017M_files <- list.files(path = "resource_map_doi_10_18739_A2ZC7RV6H/data/CYN/TR2", pattern = "FL017M", full.names = T)
CYNFL017M<-rast(CYNFL017M_files)
band<- c("green","ndvi","nir","red_edge","red")
names(CYNFL017M)<-band
#plot(CYNFL017M)

#ALNFL008 UAV data pertaining to alnus TR1
ALNFL008M_files <- list.files(path = "resource_map_doi_10_18739_A2ZC7RV6H/data/ALN", pattern = "FL008M", full.names = T)
ALNFL008M<-rast(ALNFL008M_files)
band<- c("green","ndvi","nir","red_edge","red")
names(ALNFL008M)<-band
#plot(ALNFL008M)

#ANSFL005 UAV data pertaining to ANS TR1
ANSFL005M_files <- list.files(path = "resource_map_doi_10_18739_A2ZC7RV6H/data/ANS/FL05M", pattern = "FL005M", full.names = T)
ANSFL005M<-rast(ANSFL005M_files)
band<- c("green","ndvi","nir","red_edge","red")
names(ANSFL005M)<-band
#plot(ANSFL005M)

#BPFL013 UAV data pertaining to ANS TR2
BPFL013M_files <- list.files(path = "resource_map_doi_10_18739_A2ZC7RV6H/data/BP/FL013M", pattern = "FL013M", full.names = T)
BPFL013M<-rast(BPFL013M_files)
band<- c("green","ndvi","nir","red_edge","red")
names(BPFL013M)<-band
#plot(BPFL013M)
#ggplot()+geom_sf(data=shp,color="black",fill=CYNFL016M)
#plot(CYNFL016M[[1]]) #plot(CYNFL016M)
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
#create df with only numerical id for landcover type , coordinates and sample type and create a df with landcover name and id
GroundTruthID<- subset(GroundTruth,select=-c(Landcover,geometry))
landclass<-landclass <- data.frame(landcID= seq(1,5),
                                   landcover = c("ground", "larch","lichen","shrub","water"))


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
rf.grid <- expand.grid(mtry=1:sqrt(5)) # number of variables available for splitting at each tree node

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
#plot(rf_prediction)

#set up categorical colors
landclass$cols <-c("#964B00","#9F2B68","#66c2a5",
                   "#fc8d62","#8da0cb")
#make plot and legend
plot(rf_prediction,
     type = "classes",
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
legend("top", paste(landclass$landcover),
       fill=landclass$cols ,bty="n",horiz = T) 

#get validation data from raster by extracting 
#cell values at the cell coordinates
rf_Eval <- extract(rf_prediction, validGT[,0:1])

#make the confusion matrix
rf_errorM <- confusionMatrix(as.factor(rf_Eval[,1]),as.factor(validGT$LandcoverID))
#add landcover names
colnames(rf_errorM$table) <- landclass$landcover
rownames(rf_errorM$table) <- landclass$landcover
#view the matrix
rf_errorM$table
rf_errorM

rf_VarImp<- varImp(rf_model,scale=FALSE)
plot(rf_VarImp)


