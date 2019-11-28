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

# Load and install packages
source("../install_load_pkgs.R")
packages <- c("tidyverse","corrplot", "gridExtra", "GGally", "knitr", "dplyr", "factoextra", "cluster")
ipak(packages)
set.seed(1234)
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


###     Hierarchical Clustering   ####
#
#   1. Comprobamos la k que queremos
#
######################################

##    Diagrama de codo
fviz_nbclust(df , hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method ")
#ggsave("WSS.png", plot = last_plot())

##    Índice de Silhouette
fviz_nbclust(df, hcut, method = "silhouette")+ 
  labs(subtitle = "Silhouette method") 
#ggsave("silhouette.png", plot = last_plot())



# Elegimos 3 grupos, puesto que:
#
#   - Es el mejor resultado de silhouette
#   - Buen resultado según wcc
#


###     Hierarchical Clustering   ####
#
#   2. Hacemos las agrupaciones
#
######################################

##    Hacemos agrupación
grupos <- hcut(df, k = 3, stand = TRUE)

##    Podemos representar el índice de silhouette 
##    para cada punto, teniendo ya el número de k
fviz_silhouette(grupos)

##    Visualizamos los grupos creados en el árbol
fviz_dend(grupos, rect = TRUE)

##    Finalmente vemos las agrupaciones en el gráfico

fviz_cluster(grupos)

##    Extraemos perfiles para los grupos
hmodel_n4<-eclust(df, FUNcluster="hclust", k=3, hc_metric="euclidean", hc_method="complete")
mean_df <- aggregate(df, by=list(hmodel_n4$cluster), mean) %>% mutate(metric = "mean")
median_df <- aggregate(df, by=list(hmodel_n4$cluster), median)%>% mutate(metric = "median")
max_df <- aggregate(df, by=list(hmodel_n4$cluster), max)%>% mutate(metric = "max")
min_df <- aggregate(df, by=list(hmodel_n4$cluster), min)%>% mutate(metric = "min")
result <- rbind(mean_df, median_df, max_df, min_df)
result

##    Vemos como se distribuyen las clases para cada columna
ggpairs(cbind(df, Cluster=as.factor(grupos$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="cor"),
        axisLabels="none", switch="both") +
        theme_bw()


# Podemos concluir que hay 3 grupos claramente
# diferenciados.

###            K-means            ####
#
#   1. Comprobamos la k que queremos
#
######################################

##    Diagrama de codo
fviz_nbclust(df , kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method ")
#ggsave("WSS.png", plot = last_plot())

##    Índice de Silhouette
fviz_nbclust(df, kmeans, method = "silhouette")+ 
  labs(subtitle = "Silhouette method") 
#ggsave("silhouette.png", plot = last_plot())


# Elegimos 3 grupos, puesto que:
#
#   - Es el mejor resultado de silhouette
#   - Buen resultado según wcc
#


###             K-Means           ####
#
#   2. Hacemos las agrupaciones
#
######################################

##    Hacemos agrupación
grupos <- kmeans(df, centers=3)

##    Podemos representar el índice de silhouette 
##    para cada punto, teniendo ya el número de k
sil <- silhouette(grupos$cluster, dist(df))
fviz_silhouette(sil)

##    Finalmente vemos las agrupaciones en el gráfico
fviz_cluster(grupos, df)

##    Extraemos perfiles para los grupos
hmodel_n4<-eclust(df, FUNcluster="hclust", k=3, hc_metric="euclidean", hc_method="complete")
mean_df <- aggregate(df, by=list(hmodel_n4$cluster), mean) %>% mutate(metric = "mean")
median_df <- aggregate(df, by=list(hmodel_n4$cluster), median)%>% mutate(metric = "median")
max_df <- aggregate(df, by=list(hmodel_n4$cluster), max)%>% mutate(metric = "max")
min_df <- aggregate(df, by=list(hmodel_n4$cluster), min)%>% mutate(metric = "min")
result <- rbind(mean_df, median_df, max_df, min_df)
result


##    Vemos como se distribuyen las clases para cada columna
ggpairs(cbind(df, Cluster=as.factor(grupos$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="cor"),
        axisLabels="none", switch="both") +
  theme_bw()


# Al igual que antes, distinguimos claramente 3 grupos,
# notamos ciertas mejoras en la agrupación, especialmente
# en la representación gráfica de los puntos/clusters al igual
# que en la gráfica final, en la distribución de grupos por columna








