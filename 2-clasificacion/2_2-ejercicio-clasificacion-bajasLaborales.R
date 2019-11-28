###################################################
#
#   ¿QUÉ DETERMINA QUE UN EMPLEADO SE QUIERA IR?
#
###################################################



# Load and install packages
source("../install_load_pkgs.R")
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
packages <- c("ranger","caret", "data.table", "caTools", "pROC", "rpart", "rpart.plot", "randomForest", "xgboost", "precrec", "gridExtra", "neuralnet", "dplyr", "reshape2", "ggplot2", "GGally", "h2o")
ipak(packages)

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

##    Eliminamos variables innecesarias
cols <- c("Over18", "EmployeeNumber", "EmployeeCount", "Generation", "Educational_Levels", "CatYearsManager")
df[cols] <- NULL


##    Dividimos conjuntos de datos 
indice_datos_train <- createDataPartition(df$Attrition, p=0.8,list=FALSE, times=1)

train <- df[indice_datos_train,]
test <- df[-indice_datos_train,]


##    Comprobamos que se mantengan las proporciones entre bajas
prop_train <- train %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>% mutate(pct=round(prop.table(n), 2))
prop_test <- test %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>% mutate(pct=round(prop.table(n), 2))

prop_train
prop_test

## Esta transformación de datos es necesaria para usar XGBoost
##    Transform data in matrix
xgb_train <- train %>% 
  as.matrix() %>% 
  mapply(FUN=as.numeric) %>% 
  matrix(ncol=ncol(train), nrow=nrow(train))
xgb_test <- test %>% 
  as.matrix() %>% 
  mapply(FUN=as.numeric) %>% 
  matrix(ncol=ncol(test), nrow=nrow(test))

colnames(xgb_train) <- colnames(train)
colnames(xgb_test) <- colnames(test)


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
