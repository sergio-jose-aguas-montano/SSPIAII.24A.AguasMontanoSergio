#Creado por: Aguas Montaño Sergio José - 217815601

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio")

options(scipen = 999)
set.seed(2000)

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
datos <- read.csv("Datasets/Mall_Customers.csv",
                  header = T,
                  stringsAsFactors = T)
datos$CustomerID <- NULL

summary(datos)
boxplot(datos)

datos$Genre <- as.numeric(datos$Genre)

#Matriz de correlación
datos.cor <- cor(datos)
View(datos.cor)

datos.nuevos <- NULL
datos.nuevos$Age <- datos$Age
datos.nuevos$SScore <- datos$Spending.Score..1.100.
#Re_hacer la tabla o juntar los datos que pasamos individualmente
datos.nuevos <- as.data.frame(datos.nuevos)

mdl.NSup <- kmeans(datos.nuevos, 3, trace = T)

#Elbow (Saber cantidad de clusters)
n.obs <- length(datos.nuevos$SScore)
#Y - wcss
#X - N.clusters
wcss <- vector()
for (i in 1:15) {
  wcss[i] <- kmeans(datos.nuevos, i)$tot.withinss
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