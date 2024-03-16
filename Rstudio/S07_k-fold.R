#Creado por: Aguas Montaño Sergio José - 217815601

#Entorno de trabajo
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio")

options(scipen = 999)
set.seed(2000)

#Instalar librerías
install.packages()

#Librerías
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
library(e1071)

#Data
df.Social <- read.csv("Datasets/Social_Network_Ads.csv",
                      stringsAsFactors = T)
df.Social$User.ID <- NULL

df.Social$Purchased <- as.factor(df.Social$Purchased)

#Separación
Split <- sample.split(df.Social$Purchased, SplitRatio = 0.8)
df.Social.Train <- subset(df.Social, Split == T)
df.Social.Test <- subset(df.Social, Split == F)

#Modelo
mdl.SVM <- svm(formula = Purchased ~ .,
               data = df.Social.Train,
               type = "C-classification")

summary(mdl.SVM)

#Evaluación
Y.pred <- predict(mdl.SVM, newdata = df.Social.Test)

#Matriz de confusión
cm <- confusionMatrix(Y.pred, df.Social.Test$Purchased,
                      mode = "everything")
cm[["overall"]][["Accuracy"]]
cm.Table <- table(Y.pred, df.Social.Test$Purchased)
Accu <- sum(diag(cm.Table))/length(df.Social.Test$Purchased)

#K-fold
k.val <- createFolds(df.Social.Train$Purchased, k = 10)
f.cross <- lapply(k.val, function(x){
  cross.train <- df.Social.Train[-x,] #Quitar las primeras filas
  cross.test <- df.Social.Train[x,] 
  cross.mdl <- svm(formula = Purchased ~ .,
                   data = cross.train,
                   type = "C-classification")
  cross.pred <- predict(cross.mdl, newdata = cross.test)
  #summary(mdl.Regresor)$adj.r.squared
  cross.cm <- confusionMatrix(cross.pred, cross.test$Purchased,
                              mode = "everything")
  return (cross.cm[["overall"]][["Accuracy"]])
})


Accuracy.prom <- mean(as.numeric(f.cross))
#Desviasión estandar
Accuracy.sd <- sd(as.numeric(f.cross))