#Creado por: Aguas Montaño Sergio José - 217815601

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio/p1.4_RegresiónLogística")

options(scipen = 999)
set.seed(2000)

library(ggplot2)
library(caTools)
library(mltools)
library(data.table)
library(cowplot)
#install.packages("caret")
library(caret)

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
colnames(df.Social) <- c('Age', 'EstimatedSalary', 'Purchased')

df.Social$Age <- as.numeric(df.Social$Age)
df.Social$EstimatedSalary <- as.numeric(df.Social$EstimatedSalary)

#Realiza la separación en los subset de Training y Testing
#División
Split <- sample.split(df.Social$Purchased, SplitRatio = 0.8)
df.Social.Train <- subset(df.Social, Split == T)
df.Social.Test <- subset(df.Social, Split == F)

#Creación del modelo de datos de regresión logística para el dataset de la plataforma (Social Networks Ads)
#Modelo
mdl.RLog <- glm(formula = Purchased ~ .,
                data = df.Social.Train,
                family = binomial)
summary(mdl.RLog)

#Pedicción
predict.Social <- predict(mdl.RLog, type = "response",
                          newdata = df.Social.Test)
predict.Social

#Representa los datos obtenidos del modelo en una gráfica (ggplot2) para los datos de entrenamiento y prueba
# Predicciones de Entrenamiento y prueba
df.Social.Train$Predicted <- predict(mdl.RLog, type = "response", newdata = df.Social.Train)
df.Social.Test$Predicted <- predict.Social

# Gráfico entrenamiento
plot_train <- ggplot(data = df.Social.Train, aes(x = Predicted, y = Purchased)) +
  geom_point(color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(title = "Datos de Entrenamiento", x = "Predicción", y = "Real")

# Gráfico prueba
plot_test <- ggplot(data = df.Social.Test, aes(x = Predicted, y = Purchased)) +
  geom_point(color = "red") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
  labs(title = "Datos de Prueba", x = "Predicción", y = "Real")

# Mostrar los gráficos
plot_train
ggsave(file = "plot_train.jpg",
       plot = plot_train,
       units = "in",
       height = 7,
       width = 14)

plot_test
ggsave(file = "plot_test.jpg",
       plot = plot_test,
       units = "in",
       height = 7,
       width = 14)

Y.pred <- ifelse(predict.Social >= 0.5, 1, 0)
Y.pred

plt.Social <- ggplot(df.Social.Test, aes(x = Age, y = Purchased)) +
  geom_point(aes(color = Purchased)) +
  geom_smooth(aes(y = predict.Social), color = "magenta") +
  ggtitle("Regresión Logística") +
  xlab("Edad") +
  ylab("Compra")
plt.Social


ggsave(file = "graf_relog.jpg",
       plot = plt.Social,
       units = "in",
       height = 7,
       width = 14)


summary(mdl.RLog)

#Colorcitos
set <- df.Social.Test
X1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1)
X2 <- seq(min(set$EstimatedSalary) - 1,
          max(set$EstimatedSalary) + 1,
          by = 0.1)

#Juntarlas en un solo dataset
grid.set <- expand.grid(X1, X2)

#Cambiar nombre de las variables de la tabla
colnames(grid.set) <- c('Age', 'EstimatedSalary')

prob.set <- predict(mdl.RLog, type = 'response',
                    newdata = grid.set)

Y.grid <- ifelse(prob.set > 0.5, 1, 0)

#No querer la columna 3 de la tabla
#plot(set[,-3],
#     main = "Grafico de Clasificacion",
#     xlab = "Edad",
#     ylab = "Sueldo estimado",
#     xlim = range(X1),
#     ylim = range(X2))
#Recibe una matriz, no dataframe
#contour(X1, X2, matrix(as.numeric(Y.grid),
#                       length(X1),
#                       length(X2)),
#        add = T)
#points(grid.set, pch = "|",
#       col = ifelse(Y.grid == 1, "darkgreen", "red"))
#points(set, pch = 21, bg = ifelse(
#  set[,3] == 1, "darkgreen", "red"
#))

#Evaluacion de modelos de clasificacion
#Revisar el balanceo
summary(as.factor(df.Social.Train$Purchased))

#Graficar la tendencia de compra
ggplot(df.Social.Train,
       aes(x=as.factor(df.Social.Train$Purchased)))+
  geom_bar(stat = "count",)+
  theme_minimal()

#Crea la matriz de confusión
#Matriz de confusión
matriz <- table(df.Social.Test$Purchased, Y.pred)
matriz
#View(matriz)
#confusionMatrix(matriz)$byClass

#A partir de la matriz, obtén de manera manual, es decir extrayendo los coeficientes, de los valores para Accuracy, Precision, Recall y F1 Score; según sea la clase de interés.
#   0  1
#0 40 11
#1 16 13

#A= TP + TN / TP + TN + FP + FN
#P= TP / TP + FP
#R= TP / TP + FN
#F1=(2 X P X R)/(P+R)

#Valores de la matriz de confusión
TP <- 13
TN <- 40
FP <- 11
FN <- 16

#Accuracy (A)
A <- (TP + TN) / (TP + TN + FP + FN)

#Precision (P)
P <- TP / (TP + FP)

#Recall (R)
R <- TP / (TP + FN)

#F1 Score (F1)
F1 <- (2 * P * R) / (P + R)

#Resultados
cat("Accuracy (A):", A, "\n")
cat("Precision (P):", P, "\n")
cat("Recall (R):", R, "\n")
cat("F1 Score (F1):", F1, "\n")

#Mediante alguna librería determina los mismos factores de evaluación.
confusionMatrix(as.factor(Y.pred),
                as.factor(df.Social.Test$Purchased),
                mode = "everything",
                positive = "1")

#Compara los resultados obtenidos en ambos cálculos y coloca las conclusiones obtenidas en comentarios.
#Los resultados fueron casi exactos, obviamente la librería tiene mayor facilidad en el manejo de los valores
#que ingresarlos manualmente para los cáluclos con lo cual son más exactos, pero se hablan de centecimas y que
#a fin de cuentas para prácticas o estudio los resultados con esas fallas de exactitudes de calculos no afectan,
#además de que la librería ayuda mucho al ordenar y mostrar más informacion de una manera mas completa.