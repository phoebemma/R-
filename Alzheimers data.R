getwd()
mri_alzheimers <- read.csv("oasis_longitudinal.csv")
# Exploring the nature of the data
dim(mri_alzheimers)
colnames(mri_alzheimers)
summary(mri_alzheimers)

#installing packages I think I may need based on the data 
library(tidyverse)
library(caret)
library(rpart)
library(pROC)
library(randomForest)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(GGally)

#EDA of the data based on the several parameters
ggplot(mri_alzheimers, aes(x=Age))+geom_histogram()
qplot(data= mri_alzheimers, x = Group, )
qplot(data= mri_alzheimers, x= Age)
qplot(data= mri_alzheimers, x= CDR)
qplot(data= mri_alzheimers, x= Visit)
qplot(data= mri_alzheimers, x= EDUC )
qplot(data= mri_alzheimers, x= ASF, bins = 50 )

#checking relationship between some variables
ggplot(mri_alzheimers, aes(Group, EDUC))+
  geom_col()+
  ggtitle("Group by Education")+
  theme(plot.title = element_text(hjust = .5))

ggplot(mri_alzheimers, aes(CDR, EDUC))+
  geom_col()+
  ggtitle("CDR by Education")+
  theme(plot.title = element_text(hjust = .5))


#Checking the correlation of the various variables but first dropping the non_numeric data
mri_alzheimers$Group<-NULL
mri_alzheimers$M.F<-NULL
mri_alzheimers$Subject.ID<-NULL
mri_alzheimers$MRI.ID<-NULL
mri_alzheimers$Hand<-NULL
ggcorr(mri_alzheimers, nbreaks = 5)

#There appears to be significant correlation between ASF and eTIV,  nWBV, and Educ; between nWBV, eTIV and MMSE.
#investigating the correlations
ggplot(mri_alzheimers, aes(ASF,eTIV))+ geom_point(col = "red")+ ggtitle("ASF by eTIV")+
  theme(plot.title = element_text(hjust = .5))

ggplot(mri_alzheimers, aes(ASF,nWBV))+
  geom_point(col = "red")+
  ggtitle("ASF by nWBV")+
  theme(plot.title = element_text(hjust = .5))

ggplot(mri_alzheimers, aes(nWBV,eTIV))+
  geom_point(col = "red")+
  ggtitle("nWBV by eTIV")+
  theme(plot.title = element_text(hjust = .5))

