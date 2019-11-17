###################################################
#
#   ¿QUÉ DETERMINA QUE UN EMPLEADO SE QUIERA IR?
#
###################################################


### Cargamos las librerías
library(dplyr)

##    Leemos los datos
df <- read.csv("Employee-Attrition.csv")

##    Echamos un vistazo a los datos
df %>% glimpse()


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
