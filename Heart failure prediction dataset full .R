#data source https://www.kaggle.com/fedesoriano/heart-failure-prediction
getwd()
setwd("C:/Users/Chidimma E/Downloads")
install.packages("tidyverse")
heart_failure_prediction_dataset <-read.csv("heart.csv")
#check data size
dim(heart_failure_prediction_dataset)
# 918 rows and 12 columns
library(tidyverse)
#Inspecting the data
str(heart_failure_prediction_dataset)

#preview
view(heart_failure_prediction_dataset)
#check some descriptive
qplot(data = heart_failure_prediction_dataset, x= Sex)
#there are much more men than women
#
#extract negative cases ie, non-heart failure cases
negative_results <- subset(heart_failure_prediction_dataset,HeartDisease == 0 )
dim(negative_results)

#410 results returned negative
#what is the sex distribution in the negative cases
qplot(data = negative_results, x= Sex)

#extract positive results
positive_results <- subset(heart_failure_prediction_dataset, HeartDisease == 1)


  
dim(positive_results)
#508 persons returned positive to heart disease
#sex distribution in positive cases
qplot(data = positive_results, x= Sex) 

#dataset not suitable for model training. However I will continue EDA
summary(heart_failure_prediction_dataset$Age)
summary(heart_failure_prediction_dataset$Cholesterol)

#checking the r/ship between age and cholesterol values
ggplot(heart_failure_prediction_dataset, aes(y = Age,x = Cholesterol, color = HeartDisease ))+
  geom_point()+ 
  ggtitle("Age by cholesterol values")+
  ylim(28, 78)+
  xlim(170, 610)

#chart a bit ambigous so I would run same analyses in negative and positive subgroups as there doesnt seem to be a difference in the cholesterol values of those diagnosed with heart disease and those without

ggplot(negative_results, aes(y = Age,x = Cholesterol, color = ChestPainType ))+
  geom_point()+ 
  ggtitle("Age by cholesterol values non-heart disease patients")+
  ylim(28, 78)+
  xlim(170, 610)+
  facet_wrap(~ChestPainType)

ggplot(positive_results, aes(y = Age,x = Cholesterol, color = ChestPainType ))+
  geom_point()+ 
  ggtitle("Age by cholesterol values in positive patients")+
  ylim(28, 78)+
  xlim(170, 610)+
  facet_wrap(~ChestPainType)
#there appears to be a lot of ASY in the positive cases

#analysing based on chest pain type
ggplot(heart_failure_prediction_dataset, aes(y = Age,x = Cholesterol, color = ChestPainType ))+
  geom_point()+ 
  facet_wrap(~ChestPainType)+
  ggtitle("Age by cholesterol values by Chest pain type")+
  ylim(28, 78)+
  xlim(170, 610)

#checking the relationship between the numeric variables
heart_dataset_subset <- heart_failure_prediction_dataset
heart_dataset_subset$Sex <-NULL
heart_dataset_subset$ChestPainType <- NULL
heart_dataset_subset$ST_Slope <- NULL
heart_dataset_subset$RestingECG <- NULL
heart_dataset_subset$ExerciseAngina <- NULL
ggcorr(heart_dataset_subset, nbreaks = 5)

#While a couple of deductions can be made from this dataset, it is not good enough for prediction and model-building purposes
