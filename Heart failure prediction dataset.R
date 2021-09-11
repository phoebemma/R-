install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)
library(gridExtra)
library(corrplot)
#Retrieving the column names/titles
colnames(heart_failure_prediction_dataset)
str(heart_failure_prediction_dataset)
#converting alll character variables to factors
heart_failure_prediction_dataset <- heart_failure_prediction_dataset %>% mutate(across(where(is.character), as.factor))
#checking for na values
colSums(is.na(heart_failure_prediction_dataset))

#Data visualisation based on the Resting ECG
ggplot(heart_failure_prediction_dataset) + 
  geom_bar(aes(Age, fill = RestingECG), position = "fill") + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8))

#Visualisation based on sex
ggplot(heart_failure_prediction_dataset) + 
  geom_bar(aes(Age, fill = Sex), position = "fill") + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8))

#Visualization based on chest pain type
ggplot(heart_failure_prediction_dataset) + 
  geom_bar(aes(Age, fill = ChestPainType), position = "fill") + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 10))
#based on Exercise angina
ggplot(heart_failure_prediction_dataset) + 
  geom_bar(aes(Age, fill = ExerciseAngina), position = "fill") + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8))

heart_failure_prediction_dataset %>%
  select(ST_Slope, ends_with("RestingBP")) %>%
  group_by(ST_Slope) %>% 
  summarise(across(everything(), list(mean = mean, median = median,  sd = sd)))

heart_failure_prediction_dataset %>%
  select(ChestPainType, ends_with("RestingBP")) %>%
  group_by(ChestPainType) %>% 
  summarise(across(everything(), list(mean = mean, median = median,  sd = sd)))
#visualisation using boxplots
options(repr.plot.width=12, repr.plot.height=12)
fig1 <- ggplot(heart_failure_prediction_dataset, aes(y=Age))+geom_boxplot()
fig2 <- ggplot(heart_failure_prediction_dataset, aes(y=ChestPainType))+geom_boxplot()
fig3 <- ggplot(heart_failure_prediction_dataset, aes(y=Sex))+geom_boxplot()
fig4 <- ggplot(heart_failure_prediction_dataset, aes(y=RestingECG))+geom_boxplot()
fig5 <- ggplot(heart_failure_prediction_dataset, aes(y=FastingBS))+geom_boxplot()
fig6 <- ggplot(heart_failure_prediction_dataset, aes(y=MaxHR))+geom_boxplot()
fig7 <- ggplot(heart_failure_prediction_dataset, aes(y=ExerciseAngina))+geom_boxplot()
fig8 <- ggplot(heart_failure_prediction_dataset, aes(y=ST_Slope))+geom_boxplot()
fig9 <- ggplot(heart_failure_prediction_dataset, aes(y=HeartDisease))+geom_boxplot()
fig10 <- ggplot(heart_failure_prediction_dataset, aes(y=Oldpeak))+geom_boxplot()
fig11 <- ggplot(heart_failure_prediction_dataset, aes(y=Cholesterol))+geom_boxplot()
fig12 <- ggplot(heart_failure_prediction_dataset, aes(y=RestingBP))+geom_boxplot()

grid.arrange(fig1,fig2,fig3,fig4,fig5,fig6,fig7,fig8,fig9,fig10,fig11,fig12, ncol=4, nrow=4)

m<-cor(heart_failure_prediction_dataset, x  = Cholesterol, y = RestingBP)
corrplot(m, method="number")

#visualization using histogram
options()
stat_bin(bins = 12)
fig1 <- ggplot(heart_failure_prediction_dataset, aes(y=Age))+geom_histogram()
fig2 <- ggplot(heart_failure_prediction_dataset, aes(y=ChestPainType))+geom_histogram()
fig3 <- ggplot(heart_failure_prediction_dataset, aes(y=Sex))+geom_histogram()
fig4 <- ggplot(heart_failure_prediction_dataset, aes(y=RestingECG))+geom_histogram()
fig5 <- ggplot(heart_failure_prediction_dataset, aes(y=FastingBS))+geom_histogram()
fig6 <- ggplot(heart_failure_prediction_dataset, aes(y=MaxHR))+geom_histogram()
fig7 <- ggplot(heart_failure_prediction_dataset, aes(y=ExerciseAngina))+geom_histogram()
fig8 <- ggplot(heart_failure_prediction_dataset, aes(y=ST_Slope))+geom_histogram()
fig9 <- ggplot(heart_failure_prediction_dataset, aes(y=HeartDisease))+geom_histogram()
fig10 <- ggplot(heart_failure_prediction_dataset, aes(y=Oldpeak))+geom_histogram()
fig11 <- ggplot(heart_failure_prediction_dataset, aes(y=Cholesterol))+geom_histogram()
fig12 <- ggplot(heart_failure_prediction_dataset, aes(y=RestingBP))+geom_histogram()

grid.arrange(fig1,fig2,fig3,fig4,fig5,fig6,fig7,fig8,fig9,fig10,fig11,fig12, ncol=4, nrow=4)


