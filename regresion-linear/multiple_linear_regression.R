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
library(caret)
library(dplyr)
library(ggplot2)

# Cargamos los datos
dataset = read.csv('50_Startups.csv')

# Revisamos los datos
View(dataset)

# Dividimos nuestro conjunto de datos en datos de entrenamiento y de testeo (train y test)
# Esta es una práctica muy común que sirve para comprobar el funcionamiento del modelo de IA.

indice_train<-createDataPartition(y=dataset$beneficio,p=0.8,list=FALSE)
data<-dataset[indice_train,]
test<-dataset[-indice_train,]


# Datos para la regresión linear

data %>% select(beneficio, inversion_marketing) %>%
  ggplot(aes(x=inversion_marketing, y=beneficio)) +
  geom_point()

##############################################################################
##
##                     REGRESIÓN LINEAR SIMPLE
##                     -----------------------
##
##          La regresión linear simple tiene la forma 
##
##                           Y≈β0+β1X
##
##   "Y"  es la respuesta, o variable dependiente, y "X" es el predictor.
##   Por lo que la regresión linear podría tener esta forma para nuestro caso:
##
##                beneficio≈β0+β1inversion_marketing
##
##############################################################################

# Definimos fórmula
regresion = lm(formula = beneficio ~  inversion_marketing, data = data)
# regressor = lm(formula = beneficio ~  I(inversion_marketing^3) + I(inversion_marketing^2) + inversion_marketing, data = data)

# Pasamos a ver el resultado de este modelo
summary(regresion)


######################################################################
##                  RESUMEN DEL MODELO
## Call:
##    lm(formula = beneficio ~ inversion_marketing, data = data)
##
## Residuals:
##          Min   1Q     Median  3Q    Max 
##        -83961 -21988   3744  16103  63437 
##
## La idea es dar un vistazo rápido a la distribución de los residuos.
## Los residuos es la distancia desde los puntos reales al que se corresponde en modelo.
##
##  Lo ideal sería que 1Q y 3Q fuesen iguales (absoluto), y que la mediana rondase cero.
##
## Residual standard error: 
##        28710 on 40 degrees of freedom
##
## Multiple R-squared:  
##      0.5287,	Adjusted R-squared:  0.5169 
##
## F-statistic: 
##      44.87 on 1 and 40 DF,  p-value: 4.947e-08
##
##
#######################################################################

##         Curva generada por el modelo

# Creamos un set de datos con los valores en la curva de los puntos
predicted_df <- data.frame(beneficio = predict(regresion, data), inversion_marketing=data$inversion_marketing)

# Mostramos resultado
ggplot(data = data, aes(x = inversion_marketing, y = beneficio)) +                       # Definimos los ejes
  geom_point() +                                                                         # Añadimoslos puntos
  geom_line(color='red',data = predicted_df, aes(x=inversion_marketing, y=beneficio)) +  # Añadimos la curva del modelo
  labs(title = paste("Adj R2 = ",signif(summary(regresion)$adj.r.squared, 5),            # Añadimos R^2
                     " P =",signif(summary(regresion)$coef[2,4], 5)))                    # Añadimos el p-valor de F-statistic

##         Entendiendo los residuos

# Calculamos los residuos
data$predicted <- predict(regresion)  # Obtenemos los valores en la recta para los puntos
data$residuos <- residuals(regresion) # Calculamos el residuo por punto

# Hacemos un plot con los resultados
ggplot(data, aes(x = inversion_marketing, y = beneficio)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +                     # Linea de la regresión  
  geom_segment(aes(xend = inversion_marketing, yend = predicted), alpha = .2) +     # Dibujamos línea punto - linea (residuo)
  geom_point(aes(color = abs(residuos), size = abs(residuos))) +                    # Ajustamos el tamaño de los puntos
  scale_color_continuous(low = "green", high = "red") +                             # Coloreamos en función del tamaño - verde pequeño, rojo grande
  geom_point(aes(y = predicted), shape = 1) +                                       # Colocamos un punto en la recta que coincida con el residuo
  theme_bw()


# Vemos cómo se distribuye los residuos
par(mfrow = c(2, 2))  # Avisamos de que queremos un panel de 2 x 2
plot(regresion)             # Plot de la regresion




# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)







# Codificamos la variable estado
dataset$estado = factor(dataset$estado,
                        levels = c('New York', 'California', 'Florida'), # Cambiamos cada nivel diferente de la columna
                        labels = c(1, 2, 3))                             # por un valor numérico
