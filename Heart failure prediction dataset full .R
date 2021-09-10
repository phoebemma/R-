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
#extract negative cases ie, non-heart failure cases
negative_results <- subset(heart_failure_prediction_dataset,HeartDisease == 0 )
dim(negative_results)

#410 results returned negative

#extract positive results
positive_results <- subset(heart_failure_prediction_dataset, HeartDisease == 1)

dim(positive_results)
#508 persons returned positive to heart disease


