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
# Load and install packages
source("../install_load_pkgs.R")
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
packages <- c("tidyverse","corrplot", "gridExtra", "GGally", "knitr", "dplyr", "factoextra", "cluster")
ipak(packages)
set.seed(1234)

##    Leemos los datos
df <- read.csv("wholesalescustomers.csv")

##    Echamos un vistazo a los datos
df %>% glimpse()
