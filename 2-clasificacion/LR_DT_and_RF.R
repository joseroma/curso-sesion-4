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
library(neuralnet)
set.seed(1234)

#######   Cargamos datos de tarjetas de créditos  
#  Estos datos se han obtenido mediante una PCA.

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

dt_model <- rpart(Class ~ . , train, method = 'class')
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

# Definimos la fórmula
n <- names(train)
rf.form <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))

# Entrenamos el modelo con 100 árboles
rf.model <- randomForest(rf.form,train,ntree=100,importance=T)

varimp <- data.frame(trainset.rf$importance)

vi1 <- ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")

vi2 <- ggplot(varimp, aes(x=reorder(rownames(varimp),X.IncMSE), y=X.IncMSE)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (%IncMSE)", x="Variable", y="Variable importance (%IncMSE)")

grid.arrange(vi1, vi2, ncol=2)

verset$predicted <- predict(trainset.rf ,verset)

confusionMatrix(as.factor(resultANN),as.factor(test$Class), positive = "1")


###############################################
##
##      Redes neuronales artificiales
##
###############################################


ANN_model =neuralnet(Class~.,train ,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test)
resultANN=predANN$net.result
resultANN=round(resultANN)

#    Comprobamos resultados
confusionMatrix(as.factor(resultANN),as.factor(test$Class), positive = "1")
