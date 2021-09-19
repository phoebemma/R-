getwd()
mri_alzheimers <- read.csv("oasis_longitudinal.csv")
# Exploring the nature of the data
dim(mri_alzheimers)
colnames(mri_alzheimers)
summary(mri_alzheimers)

#loading packages I think I may need based on the data 
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
#First check it based on age
summary(mri_alzheimers$Age)
#minimum age is 60 and maximum is 98. I will use those to set the limits on the histogram
summary(mri_alzheimers$eTIV)

#checking the relationship between age and eTIV using scatter plots
ggplot(mri_alzheimers, aes(x=Age,y=eTIV ))+
  geom_point()+
  xlim(60, 98)+
  ylim(1106, 2004)

#checking the same relationship using line plot
ggplot(mri_alzheimers, aes(x=Age,y=eTIV ))+
  geom_line()+
  xlim(60, 98)+
  ylim(1106, 2004)
#apparently, there is no clear relationship between Age and eTIV values

#checking the correlation between age and ASF values
summary(mri_alzheimers$ASF)
ggplot(mri_alzheimers, aes(x=Age,y=ASF ))+
  geom_point()+
  xlim(60, 98)+ 
  ylim(0.876, 1.587)+
  coord_trans(x = "sqrt")

#checking that between ASF and eTIV
ggplot(mri_alzheimers, aes(ASF,eTIV))+ 
  geom_point(col = "red")+
  ggtitle("ASF by eTIV")+
  theme(plot.title = element_text(hjust = .5))
#There is significant negative correlation between ASF and eTIV
#checking the correlation values
cor.test(mri_alzheimers$ASF, mri_alzheimers$eTIV)
#-09888765 correlation values are quite significant

#checking if that correlation has anything to do with the group
ggplot(mri_alzheimers, aes(ASF,eTIV, color = Group))+ 
  geom_point()+
  ggtitle("ASF by eTIV")+
  theme(plot.title = element_text(hjust = .5))
#Apparently those two variables correlate no matter the group

#generating charts of the various variables
qplot(data= mri_alzheimers, x = Group, )
qplot(data= mri_alzheimers, x= Age, bin = 30)
qplot(data= mri_alzheimers, x= CDR, bin = 10)
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
  facet_wrap(~Age)


#Checking the correlation of the various variables but first dropping the non_numeric data
mri_alzheimers_subgroup  <- mri_alzheimers
mri_alzheimers_subgroup$Group<-NULL
mri_alzheimers_subgroup$M.F<-NULL
mri_alzheimers_subgroup$Subject.ID<-NULL
mri_alzheimers_subgroup$MRI.ID<-NULL
mri_alzheimers_subgroup$Hand<-NULL
ggcorr(mri_alzheimers_subgroup, nbreaks = 5)

#There appears to be correlation between ASF and eTIV,  nWBV, and Educ; between nWBV, eTIV and MMSE.

#checking that between ASF and nWBV
ggplot(mri_alzheimers, aes(ASF,nWBV))+
  geom_point(col = "red")+
  ggtitle("ASF by nWBV")+
  theme(plot.title = element_text(hjust = .5))
#No obvious pattern. But lets check the values
cor.test(mri_alzheimers$ASF, mri_alzheimers$nWBV)
#correlation value of 0.213 




cor.test(mri_alzheimers$nWBV, mri_alzheimers$eTIV)

group_by(mri_alzheimers_subgroup, Age, EDUC, ASF)
