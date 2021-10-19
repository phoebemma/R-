getwd()
setwd("C:/Users/Chidimma E/Downloads")
#loading and taking a look at the data
wine_quality_data <- read.csv("winequality-red.csv")

#Data looks really messy and on closer observation  it is a semi-colon-separated file.Analysis cannont be run on it like this, thus the need for data parsing
wine_quality_data_parsed <- read.csv("winequality-red.csv", sep = ";")
#Now, taking a look at the data reveals 1599 observations and 12 variables
dim(wine_quality_data_parsed)
summary(wine_quality_data_parsed)

#creating a subset of the first 100 rows as demanded by the task
subset_wine_data <- wine_quality_data_parsed [c(1:100), ]

#loading libraries I may need
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library (RColorBrewer)

dev.off()
#checking the distribution of the variable "quality" across the dataset
qplot(data = subset_wine_data, x = quality, alpha = 0.2, bins = 10)

#More of the samples fell within quality variable of 5
#confirming this using summary statistics
summary(subset_wine_data$quality)

#summary statistics of all the data in the subset
summary(subset_wine_data)

#checking for correlation between the variable "quality" and the others
corrr <- round(cor(subset_wine_data), 1)
ggcorrplot(corrr,  hc.order = TRUE, tl.cex = 12, tl.col = "black", tl.srt = 90)

#The highest correlation of quality seemes to be with alcohol

#Taking a look at the correlation coefficents of the variables to each other in the subset
ggcorrplot(corrr,  hc.order = TRUE, tl.cex = 12, tl.col = "black", tl.srt = 90, lab = TRUE)

#relationship between all the variables to "quality"
subset_wine_data %>% 
  
  mutate(quality = as.factor(quality)) %>% 
  
  
  ggpairs(aes(color = quality, alpha=0.4),
          
          
          lower=list(continuous="points"),
          
          upper=list(continuous="blank"),
          
          axisLabels="none", switch="both")


#For personal reasons, I am curious to see if the information from the data subset is representative of the entire dataset
qplot(data = wine_quality_data_parsed, x = quality, bins = 12)
#The data generated shows that most of the wines sampled fell in the range 5 and 6
#checking the statistical summary of the variable "quality" in the dataset

summary(wine_quality_data_parsed$quality)

#checking for correlation between the variable "quality" and the others
corr <- round(cor(wine_quality_data_parsed), 1)

ggcorrplot(corr,  hc.order = TRUE, tl.cex = 12, tl.col = "black", tl.srt = 90)

#apparently, "quality" has the highest positive correlation with the alcohol content of wines, while there was basically no correlation between "quality" and "residual sugar"

#looking at the correlation coefficient of the variables with each other
ggcorrplot(corr,  hc.order = TRUE, tl.cex = 12, tl.col = "black", tl.srt = 90, lab = TRUE)


wine_quality_data_parsed %>% 
  
  mutate(quality = as.factor(quality)) %>% 
  
  
  ggpairs(aes(color = quality, alpha=0.4),
          
          
          lower=list(continuous="points"),
          
          upper=list(continuous="blank"),
          
          axisLabels="none", switch="both")
#the result from the subset differed from the full dataset. 

