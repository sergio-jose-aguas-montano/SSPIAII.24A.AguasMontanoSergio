#Creado por: Sergio José Aguas Montaño - 217815601

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio/p1.3_Polinomial_SVR")

options(scipen = 999)
set.seed(2000)

library(ggplot2)
library(caTools)
library(mltools)
library(data.table)
library(cowplot)

#Datos
df.Social <- read.csv("Social_Network_Ads.csv",
                      header = T,
                      stringsAsFactors = T)

df.Social$User.ID <- NULL
#Factores
df.Social$Gender <- as.numeric(df.Social$Gender)

#Exploracion
summary(df.Social)
boxplot(df.Social$Gender)
boxplot(df.Social$Age)
boxplot(df.Social$EstimatedSalary)
boxplot(df.Social$Purchased)

#Matriz de correlacion y Seleccion de variables
cor.Social <- cor(df.Social)
View(cor.Social)

df.Social$Gender <- NULL

#Escalado
df.Social$Age <- scale(df.Social$Age)
df.Social$EstimatedSalary <- scale(df.Social$EstimatedSalary)

#División
Split <- sample.split(df.Social$Purchased, SplitRatio = 0.8)
df.Social.Train <- subset(df.Social, Split == T)
df.Social.Test <- subset(df.Social, Split == F)

#Modelo
mdl.RLog <- glm(formula = Purchased ~ .,
                data = df.Social.Train,
                family = binomial)
summary(mdl.RLog)

#Pedicción
predict.Social <- predict(mdl.RLog, type = "response",
                          newdata = df.Social.Test)
predict.Social

Y.pred <- ifelse(predict.Social >= 0.5, 1, 0)
Y.pred

plt.Social <- ggplot(df.Social.Test, aes(x = Age, y = Purchased)) +
  geom_point(aes(color = Purchased)) +
  geom_smooth(aes(y = predict.Social), color = "magenta") +
  ggtitle("Regresión Logística") +
  xlab("Edad") +
  ylab("Compra")
plt.Social
