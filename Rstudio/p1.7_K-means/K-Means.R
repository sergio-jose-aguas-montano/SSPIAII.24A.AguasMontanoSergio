#Creado por: Aguas Montaño Sergio José - 217815601

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio/p1.7_K-means")

options(scipen = 999)
set.seed(2000)

install.packages("cluster")
install.packages("factoextra")
library(factoextra)
library(cluster)
library(ggplot2)
library(caTools)
library(mltools)
library(data.table)
library(cowplot)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gridExtra)

#Preprocesamiento
datos <- read.csv("Credit Card_Kaggle.csv",
                  header = T,
                  stringsAsFactors = T)

datos$CUST_ID <- NULL
datos$CREDIT_LIMIT <- NULL
datos$PRC_FULL_PAYMENT <-NULL
summary(datos)

#Suplantación de NA en el dataset
datos$MINIMUM_PAYMENTS <- ifelse(is.na(datos$MINIMUM_PAYMENTS), 
                      ave(datos$MINIMUM_PAYMENTS, 
                          FUN = function(x) mean(x,na.rm = T)),
                      datos$MINIMUM_PAYMENTS)

#Visualización de los datos
boxplot(datos)
datos.cor <- cor(datos)
View(datos.cor)

#Modelo de K-means
mdl.NSup <- kmeans(datos, 3, trace = T)

#Elbow (Saber cantidad de clusters)
n.obs <- length(datos$PAYMENTS)
#Y - wcss
#X - N.clusters
wcss <- vector()
for (i in 1:15) {
  wcss[i] <- kmeans(datos, i)$tot.withinss
}

wcss <- as.data.frame(wcss)
wcss$k <- seq(1,15,1)

ggplot()+
  geom_line(aes(x = wcss$k,
                y = wcss$wcss))+
  geom_point(aes(x = wcss$k,
                 y = wcss$wcss,
                 color = wcss$k))+
  ggtitle("Método del codo")+
  xlab("Iteración")+
  ylab("WCSS")+
  theme_light()

#Uso de ggplot base para representar los cluster, básico solo mandar la información de x y y en el plano junto con el modelo para los puntos
ggplot(datos, aes(x = PURCHASES, y = PAYMENTS)) +
  geom_point(aes(color = as.factor(mdl.NSup$cluster))) +
  labs(title = "Clusters con ggplot2", x = "Compras", y = "Pagos", color = "Cluster") +
  theme_minimal()

#Clusplot: Ayuda a hacer gráficos 2D, la versión genérica tiene por defecto método de partición
#se muestra tanto valores acertados como los fuera de rango, marcadospor colores y formas para cada cluster
clusplot(datos, mdl.NSup$cluster, main = 'Clusters con clusplot', color = TRUE, shade = TRUE)

#Fviz_cluster: Una manera más elegante de mostrar los clusters, donde una elipse más clara es dibujada de manera automática 
#para representar el alcance de los clusters y la información, denotando con formas y colores más legibles cada uno
#donde podemos elegir incluso el tipo de elipse y la paleta de colores
fviz_cluster(object = mdl.NSup, data = datos, geom = "point", ellipse.type = "norm", palette = "jco", main = "Clusters con factoextra", ggtheme = theme_minimal())
