getwd()
info()
setwd("downloads")
setwd("C:/Users/Chidimma E/Downloads")
#Extracting the data in the stateData file
trial_info <-read.csv("stateData.csv")
#states in regions designated "2"
subset(trial_info, state.region ==2)
#or
trial_info[trial_info$state.region ==2, ]
#creating a subset
stateSubset <- subset(trial_info, state.region = 2)
dim(stateSubset)
#creating a subset of states with murder rate less than 10
Subset_based_on_murder_rate <- subset(trial_info, murder < 10)
dim(Subset_based_on_murder_rate)
#33 states have murder rate less than 10

#subset of states with illiteracy rate more than 1.5
subset_based_on_literacy_rate <- subset(trial_info, illiteracy >1.5)
