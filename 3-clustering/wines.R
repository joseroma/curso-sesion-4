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
##    Leemos archivo
wines <- read.csv("Wine.csv")

##    No necesitamos el segmento del cliente
wines <- wines[, -14]

##    Mostramos un histograma para cada columna
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Datos vino - Histograma") +
  theme_bw()

##    Diagrama de cajas y bigotes para cada columan 
wines %>%
  gather(Attributes, values, c(1:4, 6:12)) %>%
  ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Datos vino - Boxplot") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ylim(0, 35) +
  coord_flip()

##    Vemos la distribución de datos
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Wines Attributes - Density plots") +
  theme_bw()

##    Correlación entre los datos
ggpairs(wines)

##    Normalizamos los datos
winesNorm <- as.data.frame(scale(wines))

##    Distribución de datos originales
p1 <- ggplot(wines, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Original data") +
  theme_bw()

##    Distribución de datos normalizados
p2 <- ggplot(winesNorm, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Normalized data") +
  theme_bw()

# Comparamos
grid.arrange(p1, p2, ncol=2)

##    Mostramos al relación entre total de fenoles y flavanoids
ggplot(wines, aes(x=Total_Phenols, y=Flavanoids)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Wines Attributes",
       subtitle="Relationship between Phenols and Flavanoids") +
  theme_bw()

##############      AGRUPACIÓN    ####################
#
#   Vamos a probar a agrupar los vinos de 2 formas:
#
#       1. Hierarchical Clustering
#       2. K-Means
#
######################################################

df <- winesNorm


###     Hierarchical Clustering
res <- hcut(df, k = 4, stand = TRUE)

fviz_dend(res, rect = TRUE)

fviz_silhouette(res)

fviz_cluster(res)

###     K-Means

##    Diagrama de codo
fviz_nbclust(df , kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method ")
#ggsave("WSS.png", plot = last_plot())

##    Índice de Silhouette
fviz_nbclust(df, kmeans, method = "silhouette")+ 
  labs(subtitle = "Silhouette method") 
#ggsave("silhouette.png", plot = last_plot())

##    Gap statistic
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+ 
  labs(subtitle = "Gap statistic method")
#ggsave(paste("metrics/best_k_", i, ".png", sep=""), plot = last_plot())



# Execution of k-means with k=2
set.seed(1234)
wines_k2 <- kmeans(winesNorm, centers=2)



# Execution of k-means with k=3
set.seed(1234)

wines_k3 <- kmeans(winesNorm, centers=3)

# Mean values of each cluster
aggregate(wines, by=list(wines_k3$cluster), mean)

ggpairs(cbind(wines, Cluster=as.factor(wines_k3$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()
