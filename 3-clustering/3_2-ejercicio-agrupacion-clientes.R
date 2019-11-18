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
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)
library(dplyr)
library(factoextra)
library(cluster)
set.seed(1234)

##    Leemos los datos
df <- read.csv("wholesalescustomers.csv")

##    Echamos un vistazo a los datos
df %>% glimpse()
