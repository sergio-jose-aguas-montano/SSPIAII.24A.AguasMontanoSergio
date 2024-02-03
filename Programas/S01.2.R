source("S01.Librerias.R")
options(scipen = 999)
set.seed(1991) #Usar la misma en todos los scripts

#Importar datos
df.Data <- read.csv(file = "Datasets/Data.csv", header = T, stringsAsFactors = F)

View(df.Data)
summary(df.Data)
plot(df.Data)
boxplot(df.Data$Age)
boxplot(df.Data$Salary)

#NA
df.Data$Age <- ifelse(is.na(df.Data$Age), 
                      ave(df.Data$Age, 
                          FUN = function(x) mean(x,na.rm = T)
                          ),
                      df.Data$Age)
#Factores
df.Data$Country <- factor(df.Data$Country,
                          levels = c("France","Spain","Germany"),
                          labels = c(1,2,3))
#Escalado
#df.Data$Age
#df.Data$Salary
#, todas las filas y columnas de 2 a 3 (rango)
df.Data[,2:3] <- scale(x = df.Data[,2:3])
df.Data$Country <- NULL
df.Data$Age1 <- df.Data$Age
df.Data$Age2 <- df.Data$Age
df.Data <- df.Data[,-c(1,4)]

#Dropping de filas
df.Data <- df.Data[-c(5,7),]

#HACER COPIAR SIEMPRE A LOS DATASETS