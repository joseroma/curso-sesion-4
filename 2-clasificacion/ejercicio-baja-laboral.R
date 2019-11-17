###################################################
#
#   ¿QUÉ DETERMINA QUE UN EMPLEADO SE QUIERA IR?
#
###################################################



### Cargamos las librerías
library(ranger)
library(caret)
library(data.table)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(precrec)
library(gridExtra)
library(neuralnet)
library(dplyr)
library(reshape2)
library(ggplot2)
library("GGally")

##    Leemos los datos
df <- read.csv("Employee-Attrition.csv")

##    Echamos un vistazo a los datos
df %>% glimpse()

##    Codificamos la variable Business travel
df$BusinessTravel = factor(df$BusinessTravel, levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'), labels = c(1, 2, 3))
df$Attrition = factor(df$Attrition, levels = c('Yes', 'No'), labels = c(1, 0))
df$OverTime = factor(df$OverTime, levels = c('Yes', 'No'), labels = c(1, 0))
df$Gender = factor(df$Gender, levels = c('Female', 'Male'), labels = c(1, 0))
df$MaritalStatus = factor(df$MaritalStatus, levels = c('Divorced', 'Married', 'Single'), labels = c(0, 1, 2))
df$JobRole = factor(df$JobRole, levels = c('Healthcare Representative', 'Human Resources', 'Laboratory Technician', "Manager", "Manufacturing Director","Research Director", "Research Scientist" ,"Sales Executive", "Sales Representative"), labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8))
df$Department = factor(df$Department, levels = c("Human Resources","Research & Development", "Sales"  ), labels = c(0, 1, 2))    
df$EducationField = factor(df$EducationField, levels = c("Human Resources", "Life Sciences", "Marketing", "Medical", "Other", "Technical Degree"), labels = c(0, 1, 2, 3, 4, 5))





##     Convertimos a factor todas aquellas variables que lo necesiten
cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel",
          "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", 
          "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance")
df[cols] <- lapply(df[cols], factor)

##    Eliminamos variables innecesarias
cols <- c("Over18", "EmployeeNumber", "EmployeeCount", "Generation", "Educational_Levels", "CatYearsManager")
df[cols] <- NULL


##    Dividimos conjuntos de datos 
indice_datos_train <- createDataPartition(df$Attrition, p=0.8,list=FALSE, times=1)

train <- df[indice_datos_train,]
test <- df[-indice_datos_train,]

levels(train$Attrition) <- c(0,1)
levels(test$Attrition) <- c(0,1)


##    Comprobamos que se mantengan las proporciones entre bajas
prop_train <- train %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>% mutate(pct=round(prop.table(n), 2))
prop_test <- test %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>% mutate(pct=round(prop.table(n), 2))

prop_train
prop_test


#################################################################
#
#   DESCRIPCIÓN DEL EJERCICIO
#
#   La idea es aplicar todo lo aprendido hasta ahora
#   para predecir la columna Attrition que encontramos en el 
#   conjunto de datos que acabamos de leer.
#
#   Se espera hacer uso de todos los modelos vistos anteriormente
#   para el caso de fraude, y quedarnos finalmente con el que
#   arroje mejores resultados.
#
##################################################################
