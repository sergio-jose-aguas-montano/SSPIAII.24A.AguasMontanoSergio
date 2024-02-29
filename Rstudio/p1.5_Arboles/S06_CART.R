#Creado por: Aguas Montaño Sergio José - 217815601

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio/p1.5_Arboles")

options(scipen = 999)
set.seed(2000)

#PARTE 1
#Dataset "Position Salaries" y librería rpart para modelo de regresión
#Representar los datos en una gráfica con ggplot2

#Intalar nuevas librerías
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("gridExtra")

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
PoSal <- read.csv("Position_Salaries.csv",
                        header = T,
                        stringsAsFactors = T)
summary(PoSal)
PoSal$Position <- NULL

#Modelo lineal
mdl.lineal.Position <- lm(formula = Salary ~ Level,
                          data = PoSal)
summary(mdl.lineal.Position)

#Modelo con rpart
PoSal.rpart <- rpart(formula = Salary ~ Level, data = PoSal, minsplit = 5)

#Gráfico con ayuda del modelo rpart
rpart.plot(PoSal.rpart)

x <- seq(min(PoSal[,1]) - 1,
         max(PoSal[,1]) + 1,
         by = 0.1)
new.Data <- as.data.frame(x)
colnames(new.Data) <- "Level"

#Gráfico
plt.PoSal <- ggplot() +
  theme_light() +
  geom_point(aes(x = PoSal$Level,
                 y = PoSal$Salary)) +
  xlab("Nivel") +
  ylab("Salario") +
  geom_line(aes(x = PoSal$Level,
                y = predict(PoSal.rpart,
                            newdata = PoSal)),
            colour = "green")+
  geom_line(aes(x = new.Data$Level,
                y = predict(PoSal.rpart,
                            newdata = new.Data)),
            colour = "blue")

#Llamar al gráfico
plt.PoSal

ggsave(file = "parte1.jpg",
       plot = plt.PoSal,
       units = "in",
       height = 7,
       width = 14)

#PARTE 2
#Utiliza el dataset de "Social Network Ads" para construir dos modelos de clasificación uno de árboles de decisión (rpart) y el otro con bosques aleatorios (randomForest)
#Construye la matriz de confusión para evaluar la efectividad de cada modelo
#Representa los datos en una gráfica de clasificación para ambos casos

# Leer el conjunto de datos
SoNetAds <- read.csv("Social_Network_Ads.csv",
                     header = TRUE,
                     stringsAsFactors = T)

# Eliminar la columna User.ID
SoNetAds$User.ID <- NULL

# Convertir la variable Purchased a factor
SoNetAds$Purchased <- as.factor(SoNetAds$Purchased)

# Dividir los datos en conjuntos de entrenamiento y prueba, esta es otra manera de separar los datos en lugar del split
indice <- sample(2, nrow(SoNetAds), replace = TRUE, prob = c(0.7, 0.3))
train_data <- SoNetAds[indice == 1, ]
test_data <- SoNetAds[indice == 2, ]

# Modelo con rpart a partir del conjunto de datos y una fórmula de predicción, un árbol de decisión que puede usarse para pronosticar con la funcion de predict
SoNetAds.rpart <- rpart(formula = Purchased ~ Age + EstimatedSalary, data = train_data, minsplit = 5)

# Realizar predicciones con el modelo rpart
predic_rpart <- predict(SoNetAds.rpart, test_data, type = "class")

# Construir la matriz de confusión de los datos
matriz_rpart <- confusionMatrix(data = predic_rpart, reference = test_data$Purchased)
print(matriz_rpart)

# Gráfico de clasificación con el modelo rpart
# Creamos un conjunto de datos para la visualización
grid <- expand.grid(Age = seq(min(SoNetAds$Age), max(SoNetAds$Age), length.out = 100),
                    EstimatedSalary = seq(min(SoNetAds$EstimatedSalary), max(SoNetAds$EstimatedSalary), length.out = 100))

# Hacemos predicciones sobre el conjunto de datos
grid$Purchased <- predict(SoNetAds.rpart, newdata = grid, type = "class")

# Graficamos los datos con ggplot2
plot_rpart <- ggplot() +
  geom_point(data = SoNetAds, aes(x = Age, y = EstimatedSalary, color = Purchased), alpha = 0.5) +
  geom_point(data = grid, aes(x = Age, y = EstimatedSalary, color = Purchased), size = 1) +
  scale_color_manual(values = c("red", "green")) +
  labs(title = "Clasificación con modelo rpart",
       x = "Age", y = "Estimated Salary")

ggsave(file = "pt2_grp1.jpg",
       plot = plot_rpart,
       units = "in",
       height = 7,
       width = 14)

# Modelo con Random Forest utilizando una serie de árboles de desición y así mejorar la tasa de clasificación
SoNetAds_rf <- randomForest(formula = Purchased ~ Age + EstimatedSalary, data = train_data, ntree = 500)

# Realizar predicciones con el modelo Random Forest
predictions_rf <- predict(SoNetAds_rf, test_data)

# Construir la matriz de confusión
matriz_rf <- confusionMatrix(data = predictions_rf, reference = test_data$Purchased)
print(matriz_rf)

# Gráfico de clasificación con el modelo Random Forest
# Generar datos para la visualización
grid_rf <- expand.grid(Age = seq(min(SoNetAds$Age), max(SoNetAds$Age), length.out = 100),
                       EstimatedSalary = seq(min(SoNetAds$EstimatedSalary), max(SoNetAds$EstimatedSalary), length.out = 100))

# Hacer predicciones sobre el conjunto de datos
grid_rf$Purchased <- predict(SoNetAds_rf, newdata = grid_rf)

# Crear gráfico con ggplot2 para Random Forest
plot_rf <- ggplot() +
  geom_point(data = SoNetAds, aes(x = Age, y = EstimatedSalary, color = Purchased), alpha = 0.5) +
  geom_point(data = grid_rf, aes(x = Age, y = EstimatedSalary, color = Purchased), size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Clasificación con Random Forest",
       x = "Age", y = "Estimated Salary")

ggsave(file = "pt2_grp2.jpg",
       plot = plot_rf,
       units = "in",
       height = 7,
       width = 14)

# Construir una sola gráfica que incluya ambas visualizaciones
comparacion <- grid.arrange(plot_rpart, plot_rf, nrow = 1)

ggsave(file = "pt2_comparacion.jpg",
       plot = comparacion,
       units = "in",
       height = 7,
       width = 14)

#Especificación de exactitud de ambos algoritmos:
#En el caso de rpart hace un solo aproximado de la información, con lo cual el resultado
#no es el más preciso pero ayuda a mantener una idea del comportamiento de los datos
#En el caso de Random Forest, al crear múltiples de árboles de tomas de decisiones puede
#crear varios escenarios de manera simultánea con diferentes resultados entre ellos,
#permitiendo que así encontrar uno en el que los valores son más exactos y tener un resultado
#más real 

# Imprimir la gráfica combinada
print(comparacion)