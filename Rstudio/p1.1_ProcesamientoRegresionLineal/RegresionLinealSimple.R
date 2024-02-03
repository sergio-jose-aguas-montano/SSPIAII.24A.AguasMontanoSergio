#Autor: Sergio Jose Aguas Montaño

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio/p1.1_ProcesamientoRegresionLineal")

options(scipen = 999)
set.seed(2000)

#Librería de gráficos
library(ggplot2)

#Dataset 100 ~ 200 registros
#Procesado de datos a forma de variables
dato.Iris <- iris
dato.Aire <- airquality

#Reemplazo de datos NA en campo Numérico
dato.Aire$Ozone <- ifelse(is.na(dato.Aire$Ozone), 
                      ave(dato.Aire$Ozone, 
                          FUN = function(x) mean(x,na.rm = T)
                      ),
                      dato.Aire$Ozone)

#Creación de Factores para datos categóricos
dato.Iris$Species <- factor(dato.Iris$Species,
                          levels = c("setosa","versicolor","virginica"),
                          labels = c("Echeverria","Hongo","perenne"))

#División de los conjuntos de datos en Training y Testing
Split <- sample.split(Y = dato.Aire$Ozone, SplitRatio = 45)
dato.Aire.Train <- subset(dato.Aire, Split == T)
dato.Aire.Test <- subset(dato.Aire, Split == F)

#Creación del modelo de datos lineal para el Dataset (Reg. Lineal simple)
mdl.Regresor <- lm(formula = Ozone ~ Day,
                   data = dato.Aire.Train)
summary(mdl.Regresor)

#Prediccion
mdl.Predict <- predict(object = mdl.Regresor,
                       newdata = dato.Aire.Test)

#Representa los datos obtenidos del modelo en una gráfica para los datos de entrenamiento y prueba
plt.Aire <- ggplot() + 
  theme_dark() +
  ggtitle("Regresión: Cantidad de Ozono por día") + 
  xlab("Día") + 
  ylab("Ozono")

plt.Aire.Grafico <- plt.Aire + 
  geom_point(aes(x = dato.Aire.Train$Day, 
                 y = dato.Aire.Train$Ozone),
             colour = "deepskyblue1",
             shape = 14,
             size = 5) +
  geom_line(aes(x = dato.Aire.Train$Day,
                y = predict(mdl.Regresor, newdata = dato.Aire.Train)),
            colour = "green",
  )
plt.Aire.Grafico

#Formas de validacion de confianza en Modelo Lineal
#1.- Dividir los datos en conjuntos de entrenamiento y prueba (vistos en clase)
#2.- Validación cruzada, k-fold para evaluar el rendimiento del modelo en múltiples divisiones de datos
#if (!requireNamespace("caret", quietly = TRUE)) {
#install.packages("caret")
#}
#library(caret)
#data <- read.csv("tu_data.csv")
#formula <- y ~ x1 + x2 + x3
#control <- trainControl(method = "cv", number = 5)
#model <- train(formula, data = data, method = "lm", trControl = control)
#print(model)
#3.- Regularizacion 
#if (!requireNamespace("glmnet", quietly = TRUE)) {
#install.packages("glmnet")
#}
#library(glmnet)
#data <- read.csv("tu_data.csv")
#X <- as.matrix(data[, c("x1", "x2", "x3")])
#y <- data$y
#alphas <- seq(0.1, 1, by = 0.1)
#ridge_model <- cv.glmnet(X, y, alpha = 0, lambda = alphas)
#print(ridge_model)
#best_alpha <- ridge_model$lambda.min
#cat("Mejor valor de alfa:", best_alpha, "\n")
#coefficients <- coef(ridge_model, s = best_alpha)
#print(coefficients)
