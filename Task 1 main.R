getwd()
setwd("C:/Users/Chidimma E/Downloads")
#loading and taking a look at the data
wine_quality_data <- read.csv("winequality-red.csv")

#Data looks really messy and on closer observation  it is a semi-colon-separated file.Analysis cannont be run on it like this, thus the need for data parsing
wine_quality_data_parsed <- read.csv("winequality-red.csv", sep = ";")

#Now, taking a look at the data reveals 1599 observations and 12 variables

#sneek peak of the data
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
library(factoextra)

dev.off()
#checking the distribution of the variable "quality" across the dataset
qplot(data = subset_wine_data, x = quality, alpha = 3, bins = 8)

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


#running a principle component anaylses (PCA)
subset_data_pcr <- prcomp(subset_wine_data, center = TRUE, scale = TRUE)
summary(subset_data_pcr)


#plotting a screeplot of the Eigenvalues of the 12 variables in the data
screeplot(subset_data_pcr, type = "l", npcs = 12, main = "Screeplot of the principal components")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#checking the variation explained
variation_explained = subset_data_pcr$sdev^2 / sum(subset_data_pcr$sdev^2)
#to see the exact percentage variance of each principle component
print(variation_explained)
 
#presenting this in a plot
qplot(c(1:12), variation_explained)+
  geom_line()+
  xlab("Principal component")+
  ylab("Variation explained")

#both graph and figures reveal that only the first 4 principle components have Eigenvalues > 1 (accounting for 67%of the variance), the next three components have Eigenvalues that can be rounded off to one (ie PC 5:7) and account for 21% of the variance. So i think it is say to say that the first 7 components explain the variation
#basically, 7 variables explain 81% of the variance



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

wine_quality_data_parsed_pcr <- prcomp(wine_quality_data_parsed, center = TRUE, scale = TRUE)
summary(wine_quality_data_parsed_pcr)

#the PCA analyses on the subset data agrees with that from the full dataset
variation_explained_full_dataset = wine_quality_data_parsed_pcr$sdev^2 / sum(wine_quality_data_parsed_pcr$sdev^2)
#to see the exact percentage variance of each principle component
print(variation_explained_full_dataset)

#presenting this in a plot
qplot(c(1:12), variation_explained_full_dataset)+
  geom_line()+
  xlab("Principal component")+
  ylab("Variation explained")