#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
#install.packages(c("dplyr", "ggplot2"))
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
versicolor <- iris[iris$Species == "versicolor", ]

# y ~ x
# dependent variable "is a function of" independent variable
x <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y <- c("Sepal.Width", "Petal.Width", "Petal.Width")

lm.out <- list()

#calculate regression on 3 pairs of variables
#store regression information in list lm.out
#in some cases, may need to use paste() function within lm function
#i.e. paste(y[i])
for(i in 1:3) {
  lm.out[[i]] <- lm(versicolor[,y[i]] ~ versicolor[,x[i]]) 
}


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))



#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot


#3b. make a scatter plot with ggplot and get rid of  busy grid lines


#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################