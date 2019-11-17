##########################################################
##          Instalar paquete  
##          ----------------
##
## Ejemplo para instalar "caret" (2 opciones)
##    1. Recuadro abajo derecha -> 
##    | 
##    |-> Packages (pestaña) 
##    |-> Install (botón)
##    |-> Escribir/Seleccionar "caret"
##    |-> Instalar(botón)
##                                        
##    2. install.packages('caret')
##
##########################################################

# Cargamos las librerías
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
# The following two commands remove any previously installed H2O packages for R.
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
library(h2o)

set.seed(1234)

#####################   Cargamos datos de tarjetas de crédito ##########################
#
#               Estos datos se han obtenido mediante una PCA.
#
# ¿Qué es la PCA?
#
#   Una de las aplicaciones de PCA es la reducción de dimensionalidad (variables). 
#
#   Cuando contamos con un gran número de variables cuantitativas posiblemente 
#   correlacionadas (información redundante), la PCA permite reducirlas a un número 
#   menor de variables transformadas que explican la variabilidad en los datos.
#
########################################################################################

##    Leemos archivo
df <- read.csv("creditcard.csv")
dim(df)
head(df,6)

##    Transformación en los datos

df$Amount <- scale(df$Amount, center = TRUE, scale = TRUE)     # Normalizamos amount para que se corresponda al resto de variables.
df <- df[, -1]
df$Class <- as.factor(df$Class)
head(df,6)



# Pequeña transformación para el plot
#
#   Transformamos para tener en una fila:
#     - Clase
#     - Nombre de variable
#     - Valor de variable

df_reshaped <- reshape2::melt(df,id.vars = "Class", measure.vars = colnames(df)[c(-29,-30)])


# Mostramos las distribuciones para cada variable 
ggplot(df_reshaped, aes(x = value, fill = Class) ) + 
  geom_density(alpha = 0.5,  col = "black") +
  facet_wrap("variable", ncol = 4, nrow = 7, scales = "free_y") +
  xlim(-5,5) +
  scale_fill_discrete(labels = c("Normal", "Fraude")) +
  scale_color_discrete(breaks = NULL) +
  labs(title = "Distribución para cada variable de la PCA")+
  theme ( axis.title.y = element_blank() )


# A simple vista ya podemos identificar una serie de variables que parecen poco
# informativas, concretamente:
#
#  - V13, V15, V22, V23, V24, V25 and V26
#
#   Esto quiere decir que tenemos 22 variables potencialmente informativas


##   Dividimos conjuntos de datos

data_sample = sample.split(df$Class, SplitRatio=0.80)
train = subset(df,data_sample==TRUE)
test = subset(df,data_sample==FALSE)
dim(train)

# Correlation panel
test_tmp <- sapply(test, as.numeric)
cor(test_tmp)


###############################################
##
##         Modelo de regresión logística
##
###############################################

##   Entrenamos el modelo
glm_model = glm(Class~.,train,family=binomial())

##   Resumen del modelo
summary(modelo_glm)


##   Predecimos resultados
lr.predict <- predict(modelo_glm, newdata = test, type="response")

#    Comprobamos resultados
confusionMatrix(as.factor(round(lr.predict)),as.factor(test$Class), positive = "1")


###############################################
##
##         Árboles de decisión
##
###############################################

##   Entrenamos el modelo
dt_model <- rpart(Class ~ . , train, method = 'class')

##   Extraemos predicciones (CLASE)
predicted_val <- predict(dt_model, test, type = 'class')

##   Extraemos predicciones (PROBABILIDAD)
probability <- predict(dt_model, test, type = 'prob')

##   Mostramos gráfico con el arbol generado
rpart.plot(dt_model)

##   Comprobamos resultados
confusionMatrix(as.factor(round(probability[,"1"])),as.factor(test$Class), positive = "1")


###############################################
##
##          Random Forests
##
###############################################

##    Definimos la fórmula
n <- names(train)
rf.form <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))

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
confusionMatrix(as.factor(predicted),as.factor(test$Class), positive = "1")


###############################################
##
##      EXTREME GRADIENT BOOSTING MACHINE
##
###############################################

##    Entrenamos el modelo
xgb_model <- xgboost(data = as.matrix(train[-30]),
                               label = as.numeric(as.character(train$Class)),
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

y <- "Class"  #response column: digits 0-9
x <- setdiff(names(train), y)  #vector of predictor column names

##    Guardamos los datos y los importamos en formato h2o
write.table(x = train, file = "train_creditcard.h2o", row.names = F, col.names = T)
write.table(x = test, file = "test_creditcard.h2o", row.names = F, col.names = T)
train_h2o <- h2o.importFile(path = "train_creditcard.h2o", destination_frame = "train_h2o")
test_h2o <- h2o.importFile(path = "test_creditcard.h2o", destination_frame = "test_h2o")

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
  strategy = "RandomDiscrete", max_models = 5, seed = 1234
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
dl_grid <- h2o.getGrid(grid_id = "grid_dl", sort_by = "accuracy", decreasing = TRUE)

##    Extraemos el mejor modelo
dl_model <- h2o.getModel(dl_grid@model_ids[[1]])


##    Extraemos predicciones
predictions <- predict(dl_model,newdata = test_h2o)

##    Comprobamos resultados
confusionMatrix(as.factor(as.vector(predictions["predict"])),as.factor(test$Class), positive = "1")

##    Apagamos cluster
h2o.shutdown()
