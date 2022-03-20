#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables
RegressionTables <- list() 

versicolorData <- iris[iris$Species=='versicolor',] 

for (i in 1:3) { #each i from 1 to 3 represents a relationship for which a regression table is needed
  if (i == 1) {
    RegressionTables[[i]] <- summary(lm(versicolorData$Sepal.Length ~ versicolorData$Sepal.Width))$coefficients
  }
  if (i == 2) {
    RegressionTables[[i]] <- summary(lm(versicolorData$Petal.Length ~ versicolorData$Petal.Width))$coefficients
  }
  if (i == 3) {
    RegressionTables[[i]] <-  summary(lm(versicolorData$Sepal.Length ~ versicolorData$Petal.Length))$coefficients
  }
}
RegressionTables


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
dfIris <- data.frame(iris) #new data frame for iris data set
full_join(dfIris, height)            # joins maximum height


#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
GGplot = ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()
GGplot

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
# blank arugment for theme_classic() removes gridlines
GGClassicPlot = ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point() + theme_classic()
GGClassicPlot

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

GGColorPlot = ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) + #aes stands for aesthetic mappings , here the aesthetic mappings allows the plot to assign a different color for each species
  geom_point(aes(size = Petal.Length), show.legend = TRUE)+  #here the aesthetic mapping makes the point size propotional to petal length, show.legend allows display of a legend key for point sizez and colors
  theme_classic() + 
  ggtitle("Scatter plot for Sepal length and Width for Iris Setosa, Versicolor and Virginica") + #title 
  xlab("Sepal Length") + ylab("Sepal Width") #axis labels
GGColorPlot

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
#ggplot allows for more aesthetics specifc arguments (e.g. the different arguments for aes , show.legend) which make it more intuitvie to create a scatter plot that can display more information using graphics 
#the base plot requires that the orignal data frame be specified when referencing a vector belonging to the same using the df$vector cconvention, which makes it hard to read for a plot function with many arugments
#ggplot allows the user to specify the dataframe as the first arguement and then simply reference the vector names for subsequent arugments for the function and any functions within the ggplot function
