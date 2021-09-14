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
  
#Making visualization easier by splitting into multiple plots
ggplot(data = EU_Covid_data, aes(x = Confirmed/10000, y = Deaths/ 1000, color = Country))+
  geom_jitter(alpha = 2)+
  labs(x = "Confirmed cases x 10000", y ="Deaths x 1000" )+
  facet_wrap(~Country)+
  theme_bw()

#checking correlation of the various variables
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

library(PerformanceAnalytics)
chart.Correlation(updated_EU_Covid_data, histogram = TRUE, method = "pearson")

qplot(data = EU_Covid_data, x= Deaths/1000, y = Confirmed/ 10000, color = "blue", alpha = 5)+
  facet_wrap(~Country)+
  labs(y = "Confirmed Covid-19 cases", x = "Deaths")
