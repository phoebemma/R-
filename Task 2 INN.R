getwd()
setwd("C:/Users/Chidimma E/Downloads")
#file is an xlsx file so the readxl package is needed
library(readxl)
taxi_data <-read_xlsx("Taxi_Data.xlsx")
#data contains 263166 observations of 19 variables
#sneak peek at data
head(taxi_data)
summary(taxi_data)

# loading libraries I believe I will need
library(tidyverse)
library (caret)
library(ggplot2)
library(lattice)
library(httr)

str(taxi_data)
# extracting the duration of the trip from the pickup and drop-off times
taxi_data$trip_duration <- difftime(taxi_data$tpep_dropoff_datetime, taxi_data$tpep_pickup_datetime, units = c("mins"))

#traffic experts will likely be interested in knowing the average speed
#creating a column called "average speed"
taxi_data$average_speed <- taxi_data$trip_distance/ as.double(taxi_data$trip_duration)                                    

#checking if there are any missing values
complete.cases(taxi_data)

#removing the rows with missing data
taxi_data_complete <- taxi_data[complete.cases(taxi_data), ]

#the taxi_data_complete dataset has 262833 observations and 21 variables which means there were 333 rows with missing variables

#Would be creating a model using Predictive modelling (linear regression)
#Now to divide my dataset into training and testing sets

#70% of the dataset will be training set and the rest for testing
df = sort(sample(nrow(taxi_data_complete), nrow(taxi_data_complete)*.7))
train <- taxi_data_complete[df,]
test <- taxi_data_complete[-df,]

#now to the training proper using simple linear regression method using trip_duration as the dependent variable and distance as independent variable
MLmodel <- train( as.numeric(trip_duration) ~trip_distance,
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
#This suggests there are several variables that should not have been part of the prediction. Probably a Principle component anaylses should be run before attempting to build a model. 
#testing the model
testing_model <- predict(model_resampling, test, level = .95)
testing_model
