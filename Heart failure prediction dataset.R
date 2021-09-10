install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)
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