#EU Covid_19 data sourced from https://www.kaggle.com/prajittr/latest-covid19-cases-eu-countries-data
getwd()
setwd("C:/Users/Chidimma E/Downloads")
EU_Covid_data <-read.csv("EU_COVID_data_15July2021.csv")
library(dplyr)
library(tidyverse)
library(ggcorrplot)
library(ggplot2)
#Checking the content of the data
glimpse(EU_Covid_data)

#visualizing the relationship between confirmed cases and deaths across countries
ggplot(data = EU_Covid_data, aes(x = Confirmed/10000, y = Deaths/ 1000, color = Country))+
  geom_jitter(alpha = 2)+
  labs(x = "Confirmed cases x 10000", y ="Deaths x 1000" )+
  theme_bw()
#The relationship between confirmed cases and deaths are significant (as expected)
  
#Making visualization easier by splitting into multiple plots
ggplot(data = EU_Covid_data, aes(x = Confirmed/10000, y = Deaths/ 1000, color = Country))+
  geom_jitter(alpha = 2)+
  labs(x = "Confirmed cases x 10000", y ="Deaths x 1000" )+
  facet_wrap(~Country)+
  theme_bw()

#Comparing the death rates across all EU countries
EU_Covid_data <- read.csv("EU_COVID_data_15July2021.csv")
ggplot(EU_Covid_data, aes(Deaths/1000, Country, color = Country))+
  geom_col(alpha = 5)+
  ggtitle("deaths by country")+
  theme_bw()
#Italy, followed by France and Germany had the highest death rates.

#Checking if the number of confirmed cases and the test rates follwed the same trend
ggplot(EU_Covid_data, aes(Confirmed/10000, Country, color = Country))+
  geom_col(alpha = 5)+
  ggtitle("confirmed cases by country")+
  theme_bw()
#France recorded the highest number of cases, followed by Italy and Spain

ggplot(EU_Covid_data, aes(Tests/100000, Country, color = Country))+
  geom_col(alpha = 5)+
  ggtitle("Tests by country")+
  theme_bw()
#France, Denmark and Italy tested the highest number of people.

#Visualizing if these had anything to do with the population of the countries
ggplot(EU_Covid_data, aes(Population/100000, Country, color = Country))+
  geom_col(alpha = 5)+
  ggtitle("Population by country")+
  theme_bw()
#Germany has the highest population. Italy has the third largest population, second highest number of confirmed cases and the highest number of deaths

#visualising the relationship between population and cases across all countries
ggplot(EU_Covid_data, aes(Confirmed/10000, Population/100000, color = Country))+
  geom_point(alpha = 5)+
  ggtitle("Population vs confirmed cases")+
  theme_bw()

#relationship between number of tests and confirmed cases
ggplot(EU_Covid_data, aes(Confirmed/10000, Tests/10000, color = Country))+
  geom_point(alpha = 5)+
  ggtitle("Population vs confirmed cases")+
  theme_bw()
#the correlation between  tests and confirmed cases was not exactly significant

#checking the relationship per country
ggplot(EU_Covid_data, aes(Confirmed/10000, Tests/10000, color = Country))+
  geom_point(alpha = 5)+
  ggtitle("Tests vs confirmed cases")+
  theme_bw()+ 
  facet_wrap(~Country)

cor.test(EU_Covid_data$Population, EU_Covid_data$Confirmed)
#Highly significant correlation between population and confirmed cases

#checking same for population and tests
cor.test(EU_Covid_data$Population, EU_Covid_data$Tests)
#Relatively significant relationship

#checking for population and Death
cor.test(EU_Covid_data$Population, EU_Covid_data$Deaths)
#Apparently, the larger the population, the more the recorded cases and the more the deaths

#checking a correlation of all the factors
library(PerformanceAnalytics)
update_EU_Covid_data <- EU_Covid_data
update_EU_Covid_data$Country <- NULL
chart.Correlation(update_EU_Covid_data, histogram = TRUE, method = "pearson")
