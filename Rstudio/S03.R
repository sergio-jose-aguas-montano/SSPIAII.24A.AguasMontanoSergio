#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio")

#Librerías
source("S01.Librerias.R")

options(scipen = 999)
set.seed(2000)

#Preprocesamiento
df.Startups <- read.csv(file = "Datasets/50_Startups.csv",
                       header = T,
                       stringsAsFactors = T)

summary(df.Startups)

#OHE
install.packages("data.table")
install.packages("mltools")
install.packages("corrplot")
install.packages("viridis")

library(data.table)
library(mltools)
library(corrplot)
library(viridis)

df.Startups.ohe <- one_hot(as.data.table(df.Startups))
df.Startups.ohe$`State_New York` <- NULL
df.Startups.new <- as.data.frame(df.Startups.ohe)

#Correlacion
mt.Correlation <- cor(df.Startups.new)
View(mt.Correlation)

#Gráfico
corrplot(mt.Correlation,
         addCoef.col = "black",
         insig = "label_sig",
         method = "color",
         type = "lower",
         diag = F,
         col = viridis(n=7, direction=1),
         title = "Correlation: Startups")

#Modelo
split <- sample.split(Y = df.Startups.new$Profit,
                      SplitRatio = 0.8)
df.Startups.Train <- subset(df.Startups.new, split == T)
df.Startups.Test <- subset(df.Startups.new, split == F)

#summary(mdl.All) #Uso de modelo exhaustivo
mdl.All <- lm(formula = Profit ~ .,
              data = df.Startups.Train)
mdl.All.sum <- summary(mdl.All)

mdl.R2 <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
             data = df.Startups.Train)
mdl.R2.sum <- summary(mdl.R2)

mdl.R1 <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
             data = df.Startups.Train)
mdl.R1.sum <- summary(mdl.R1)

mdl.All.sum
mdl.R2.sum
mdl.R1.sum

#--------------------------------------------------------------------------------------

# Predicciones en los datos de entrenamiento
df.Startups.Train$Predictions_All <- predict(mdl.All, newdata = df.Startups.Train)
df.Startups.Train$Predictions_R2 <- predict(mdl.R2, newdata = df.Startups.Train)
df.Startups.Train$Predictions_R1 <- predict(mdl.R1, newdata = df.Startups.Train)

# Predicciones en los datos de prueba
df.Startups.Test$Predictions_All <- predict(mdl.All, newdata = df.Startups.Test)
df.Startups.Test$Predictions_R2 <- predict(mdl.R2, newdata = df.Startups.Test)
df.Startups.Test$Predictions_R1 <- predict(mdl.R1, newdata = df.Startups.Test)

# Gráfico para datos de entrenamiento
ggplot(df.Startups.Train, aes(x = Profit, y = Predictions_All)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Datos de Entrenamiento - Modelo Completo",
       x = "Profit",
       y = "Predicciones")

ggplot(df.Startups.Train, aes(x = Profit, y = Predictions_R2)) +
  geom_point(color = "green") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Datos de Entrenamiento - Modelo R2",
       x = "Profit",
       y = "Predicciones")

ggplot(df.Startups.Train, aes(x = Profit, y = Predictions_R1)) +
  geom_point(color = "orange") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Datos de Entrenamiento - Modelo R1",
       x = "Profit",
       y = "Predicciones")

# Gráfico para datos de prueba
ggplot(df.Startups.Test, aes(x = Profit, y = Predictions_All)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Datos de Prueba - Modelo Completo",
       x = "Profit",
       y = "Predicciones")

ggplot(df.Startups.Test, aes(x = Profit, y = Predictions_R2)) +
  geom_point(color = "green") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Datos de Prueba - Modelo R2",
       x = "Profit",
       y = "Predicciones")

ggplot(df.Startups.Test, aes(x = Profit, y = Predictions_R1)) +
  geom_point(color = "orange") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Datos de Prueba - Modelo R1",
       x = "Profit",
       y = "Predicciones")

#--------------------------------------------------------------------------------------


#Librerías
library(MASS)
mdl.Lib <- stepAIC(mdl.All,
                   direction = "backward",
                   trace = 1)
mdl.Lib
summary(mdl.Lib)

mdl.All.sum$adj.r.squared
coef(summary(mdl.All.sum)[c(2:length(df.Startups.Train)),"Pr(>|t|)"])

#eliminacion hacia atrás rudimentaria
P <- 0.05
back <- function(datos, pv){
  k <- length(datos)
  for(i in c(1:n)){
    reg <- lm(formula = Profit ~., data = datos)
    maxP <- max(coef(summary(reg))[c(2:n),"Pr(>|t|)"])
    if(maxP > pv){
      x <- which(coef(summary(reg))[c(2:n),"Pr(>|t|)"] == maxP)
      datos <- datos[, -x]
    }
    n <- n-1
    print(summary(reg))
  }
  return(reg)
}

mdl.Rudi <- back(df.Startups.Train, P)
mdl.Rudi
back(df.Startups.Train, P)

#Gráfica
ggplot() +
  geom_point(aes(x = df.Startups.Train$R.D.Spend,
                 y = df.Startups.Train$Profit),
             colour = "gray") +
  geom_line(aes(x = df.Startups.Train$R.D.Spend,
                y = predict(mdl.R2,
                            newdata = df.Startups.Train)),
            colour = "red") +
  geom_line(aes(x = df.Startups.Train$R.D.Spend,
                y = predict(mdl.R1,
                            newdata = df.Startups.Train)),
            colour = "green") +
  geom_line(aes(x = df.Startups.Train$R.D.Spend,
                y = predict(mdl.All,
                            newdata = df.Startups.Train)),
            colour = "blue")

#Comparacion de los modelos lineales
anova(mdl.All, mdl.R2, mdl.R1)
