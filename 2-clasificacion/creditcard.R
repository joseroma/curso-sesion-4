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
library(gridExtra)
library(neuralnet)
set.seed(1234)

#######   Cargamos datos de tarjetas de créditos - Estos datos se han obtenido mediante una PCA.

# ¿Qué es la PCA?
#
#   Una de las aplicaciones de PCA es la reducción de dimensionalidad (variables), perdiendo la 
# menor cantidad de información (varianza) posible: cuando contamos con un gran número de variables 
# cuantitativas posiblemente correlacionadas (indicativo de existencia de información redundante),
# PCA permite reducirlas a un número menor de variables transformadas (componentes principales) que 
# expliquen gran parte de la variabilidad en los datos.

# Una característica interesante de la PCA es que las columnas generadas no estan correlacionadas entre sí

df <- read.csv("creditcard.csv")
dim(df)
head(df,6)

##    Transformación en los datos

df$Amount <- scale(df$Amount)     # Normalizamos amount para que se corresponda al resto de variables.
head(df,6)

##   Dividimos conjuntos de datos

data_sample = sample.split(df$Class, SplitRatio=0.80)
train = subset(df,data_sample==TRUE)
test = subset(df,data_sample==FALSE)
dim(train)

###############################################
##
##         Modelo de regresión binomial
##
###############################################

##   Entrenamos el modelo
modelo_glm=glm(Class~.,train,family=binomial())
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

##   Extraemos predicciones
predicted_val <- predict(dt_model, test, type = 'class')
probability <- predict(dt_model, test, type = 'prob')
rpart.plot(dt_model)

#    Comprobamos resultados
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
rf.model <- randomForest(rf.form,train,ntree=5,importance=T)
end.time <- Sys.time()

##    Extraemos el tiempo que ha tardado en entrenarse
time.taken <- end.time - start.time
time.taken

##   Extraemos la importancia de variables
varimp <- data.frame(rf.model$importance)

##   Plot importancia de variables
vi1 <- ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")

##    Calculamos predicciones
verset$predicted <- predict(trainset.rf ,verset)

##    Comprobamos funcionamiento del modelo
confusionMatrix(as.factor(resultANN),as.factor(test$Class), positive = "1")


###############################################
##
##      Redes neuronales artificiales
##
###############################################

##    Entrenamos modelo
ANN_model =neuralnet(Class~.,train )

##    Mostramos arquitectura de la red
plot(ANN_model)

##    Extraemos predicciones
predANN=compute(ANN_model,test)

##    Obtenemos clase en base a prob
resultANN=predANN$net.result
resultANN=round(resultANN)

#    Comprobamos resultados
confusionMatrix(as.factor(resultANN),as.factor(test$Class), positive = "1")
