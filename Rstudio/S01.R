#Vectores
vector1 <- c(1,2,3)

reg <- seq(from = -10, to = 10, by = 0.5)

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Programas")

#Instalar y activar librerias
install.packages("ggplot2", dependencies = T)
library(ggplot2)

#Datasets
df.Iris <- iris
df.AirQ <- airquality
View(df.AirQ)

#Importar los datos
df.Salary <- read.csv(file = "Datasets/Salary_Data.csv", header= T)

#Acomoda todas las medidas de dependencia central
sum.AirQ <- summary(df.AirQ)
View(sum.AirQ)

#Graficos
#Dispersion
plot(df.Iris)
#Selecciona cual elegir separando las clases
plot(df.Iris$Petal.Length)
#Histograma
hist(df.Iris$Sepal.Length)
#Caja
boxplot(df.AirQ)
#Pares
pairs(df.Iris)

#Externo (extrae info de otros archivos o los importa)
source("S01.1.R")

#Datos estadisticos
sum.Reg <- summary(reg)
mean.Reg <- mean(reg)
median.Reg <- median(reg)
min.reg <- min(reg)
mode.Reg <- mode(reg)

sum.Reg[4]

plot(reg)
