getwd()
setwd("C:/Users/Chidimma E/Downloads")
#file is an xlsx file so the readxl package is needed
library(readxl)
taxi_data <-read_xlsx("Taxi_Data.xlsx")
#data contains 263166 observations of 19 variables
#sneak peek at data
head(taxi_data)
summary(taxi_data)

# loading libraries  I will need
library(tidyverse)
library (caret)
library(ggplot2)
library(lattice)
library(httr)
library(dplyr)
library (GGally)

str(taxi_data)
# extracting the duration of the trip from the pickup and drop-off times . The units is in minutes
taxi_data$trip_duration <- as.numeric (difftime(taxi_data$tpep_dropoff_datetime, taxi_data$tpep_pickup_datetime, units = c("mins")))

#traffic experts will likely be interested in knowing the average speed
#creating a column called "average speed"
taxi_data$average_speed <- taxi_data$trip_distance/ taxi_data$trip_duration                                    

#To create a ML model that can be used to predict the distance of the trip, I will use simple linear regression method 
#First I will create a subset ot the dataframe which would contain only the variables I believe will be of use in the prediction. Variables like Tip, cost etc would likely be of no effect to how long a journey would last

subset_taxi_data <- taxi_data %>% select(trip_distance, pickup_longitude, pickup_latitude, dropoff_latitude, dropoff_longitude, trip_duration, average_speed)

#checking if there are any missing values in the subsetted data
complete.cases(subset_taxi_data)

#removing the rows with missing data
subset_taxi_data_complete <- subset_taxi_data[complete.cases(subset_taxi_data), ]

#the subset_taxi_data_complete dataset has 263166 observations and 7 variables which means there were 333 rows with missing variables

#Would be creating a model using Predictive modelling (linear regression)
#Would divide my data into the set to be used for training and that for testing.
#70% of the dataset will be training set and the rest for testing
df = sort(sample(nrow(subset_taxi_data_complete), nrow(subset_taxi_data_complete)*.7))
train <- subset_taxi_data_complete[df,]
test <- subset_taxi_data_complete[-df,]

#now to the training proper using simple linear regression method using trip_duration as the dependent variable and distance as independent variable
MLmodel <- train (trip_duration ~.,
                 data = train,
                 method = "lm")

#using K-fold cross-validation to resample the model
model_control <-trainControl(method= "repeatedCV",
number = 10,
repeats = 10)

model_resampling <- train(as.numeric(trip_duration) ~ trip_distance,
                          data = train,
                          method = "lm",
                          trControl = model_control,
                          preProcess = c("center"))
model_resampling
#Rsquared value is so low and tends to suggest that the other variables cannot be used to reliably predict the trip duration
#Root Mean Square Error (RMSE) is way too high, this shows the prediction to be produced fromthe model would be bad and highly unrelaible. reliable RMSE values are normally below 1
#This suggests there are several variables that should not have been part of the prediction. 

#To find out why I consistently got bad model values (I tried 4 different approaches), I decided to do perhaps what i should have done before model building- check the correlation between the variables to see if one could be used to predict the other
#Used ggpairs to build a scatterplot matrix
ggpairs(subset_taxi_data_complete, title = "Taxi data for prediction")
#horrible correlation/relationship. This implies those variables cannot be used to build a reliable model.

#However, I decide to test the model
testing_model <- predict(model_resampling, test, level = .95)
testing_model


