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
df$BusinessTravel = factor(df$BusinessTravel,
                           levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'),
                           labels = c(1, 2, 3))

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


# Correlation panel
ggpairs(as.data.frame(test_tmp))


###############################################
##
##         Modelo de regresión logística
##
###############################################

##   Entrenamos el modelo
glm_model = glm(Attrition~.,train,family=binomial())

##   Resumen del modelo
summary(glm_model)


##   Predecimos resultados
lr.predict <- predict(glm_model, newdata = test, type="response", levelsOnly=True)

#    Comprobamos resultados
confusionMatrix(as.factor(round(lr.predict)),as.factor(test$Attrition), positive = "1")


###############################################
##
##         Árboles de decisión
##
###############################################

##   Entrenamos el modelo
dt_model <- rpart(Attrition ~ . , train , method = 'class')

##   Extraemos predicciones (CLASE)
predicted_val <- predict(dt_model, test , type = 'class')

##   Mostramos gráfico con el arbol generado
rpart.plot(rpart.tree)

##   Comprobamos resultados
confusionMatrix(predicted_val,test$Attrition, positive = "1")


###############################################
##
##          Random Forests
##
###############################################

##    Definimos la fórmula
n <- names(train)
rf.form <- as.formula(paste("Attrition ~", paste(n[!n %in% "Attrition"], collapse = " + ")))

##    Entrenamos el modelo con 100 árboles
start.time <- Sys.time()
rf_model <- randomForest(rf.form,train,ntree=5,importance=T)
end.time <- Sys.time()

##    Extraemos el tiempo que ha tardado en entrenarse
time.taken <- end.time - start.time
time.taken

##   Extraemos la importancia de variables
varimp <- data.frame(rf_model$importance)

##   Plot importancia de variables
vi1 <- ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")

##    Calculamos predicciones
predicted <- predict(rf_model ,test)

##    Comprobamos funcionamiento del modelo
confusionMatrix(as.factor(predicted),as.factor(test$Attrition), positive = "1")


###############################################
##
##      EXTREME GRADIENT BOOSTING MACHINE
##
###############################################

##    Entrenamos el modelo
xgb_model <- xgboost(data = as.matrix(train[,-2]),
                     label = as.numeric(as.character(train$Attrition)),
                     nrounds = 100,
                     print_every_n = 101L #we dont want it to show messages
)

##    Extraemos predicciones
prediccion <- predict(xgb_model,newdata = as.matrix(test[-30]))

##    Comprobamos funcionamiento del modelo
confusionMatrix(as.factor(round(prediccion)),as.factor(test$Class), positive = "1")

###############################################
##
##      Redes neuronales artificiales básica
##
###############################################

##    Entrenamos el modelo
ANN_model <- neuralnet(Class == "1" ~ ., train, linear.output = FALSE)

##    Mostramos arquitectura de la red
plot(ANN_model)

##    Extraemos predicciones
predANN=predict(ANN_model,test)

##    Obtenemos clase en base a prob
resultANN=round(predANN)

##    Comprobamos resultados
confusionMatrix(as.factor(resultANN),as.factor(test$Class), positive = "1")




###############################################
##
##      Redes neuronales profundas
##
###############################################


##    Iniciamos cluster
h2o.init()

y <- "Attrition"  #response column: digits 0-9
x <- setdiff(names(train), y)  #vector of predictor column names

##    Guardamos los datos y los importamos en formato h2o
write.table(x = train, file = "Attrition.h2o", row.names = F, col.names = T)
write.table(x = test, file = "Attrition.h2o", row.names = F, col.names = T)
train_h2o <- h2o.importFile(path = "Attrition.h2o", destination_frame = "train_h2o")
test_h2o <- h2o.importFile(path = "Attrition.h2o", destination_frame = "test_h2o")

# Ha empezado a las 15:41

train_h2o[,y] <- as.factor(train_h2o[,y])
test_h2o[,y] <- as.factor(test_h2o[,y])

hiperparametros <- list(activation = c("Rectifier","RectifierWithDropout", "MaxoutWithDropout"),
                        hidden = list(c(20), c(40), c(50), c(80),c(100),c(120),c(200),c(300),
                                      c(80, 40), c(80, 60), c(80, 20),c(80, 90),
                                      c(90, 40), c(90, 60), c(90, 20),c(90, 90),
                                      c(80,60,30), c(80,50,40), c(80,40,10), c(80,30,10), c(80,20,5)),
                        epochs = c(70,150, 300, 500),
                        rate = c(0.5, 0.1, 0.001),
                        input_dropout_ratio = c(0, 0.5))

search_criteria <- list(
  strategy = "RandomDiscrete", max_models = 10, seed = 1234
)

grid_dl <- h2o.grid(
  # Algoritmo.
  algorithm = "deeplearning",
  # Variable respuesta y predictores.
  y = y,
  x = x,
  # Datos de entrenamiento.
  training_frame = train_h2o,
  # Preprocesado.
  standardize = TRUE,
  #Regularization
  l1 = 0.00001, 
  l2 = 0.00001,
  # Detención temprana.
  stopping_metric = "misclassification",
  # Hiperparámetros optimizados.
  hyper_params = hiperparametros,
  # Tipo de búsqueda.
  search_criteria = search_criteria,
  seed = 1234,
  grid_id = "grid_dl"
)

# Ordenamos los modelos según su recall
dl_grid <- h2o.getGrid(grid_id = "grid_dl", sort_by = "auc", decreasing = TRUE)

##    Extraemos el mejor modelo
dl_model <- h2o.getModel(dl_grid@model_ids[[2]])


##    Extraemos predicciones
predictions <- predict(dl_model,newdata = test_h2o)

##    Comprobamos resultados
confusionMatrix(as.factor(as.vector(predictions["predict"])),as.factor(test$Attrition), positive = "1")

##    Apagamos cluster
h2o.shutdown()

