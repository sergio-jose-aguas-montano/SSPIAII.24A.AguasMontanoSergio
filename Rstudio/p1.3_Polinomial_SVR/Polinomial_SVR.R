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
library(e1071)

#Preprocesamiento
datos <- read.csv("50_Startups.csv",
                        header = T,
                        stringsAsFactors = T)

datos$State <- NULL

#División
Split <- sample.split(datos$Profit, SplitRatio = 0.8)
datos.Train <- subset(datos, Split == T)
datos.Test <- subset(datos, Split == F)

# ***** POLINOMIAL *****
#Grado 1
modelo.lin <- lm(formula = Profit ~ (R.D.Spend + Administration + Marketing.Spend),
                 data = datos)
summary(modelo.lin)
grfico.1.modlin <- geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                                 y = predict(modelo.lin,
                                             newdata = datos)),
                             colour = "red")

#Grado 5
modelo.lin5 <- lm(formula = Profit ~ poly((R.D.Spend + Administration + Marketing.Spend), 5),
                 data = datos)
summary(modelo.lin5)
grfico.5.modlin <- geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                                 y = predict(modelo.lin5,
                                             newdata = datos)),
                             colour = "blue")

#Grado 10
modelo.lin10 <- lm(formula = Profit ~ poly((R.D.Spend + Administration + Marketing.Spend), 10),
                  data = datos)
summary(modelo.lin10)
grfico.10.modlin <- geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                                 y = predict(modelo.lin10,
                                             newdata = datos)),
                             colour = "green")

#Grado 14
modelo.lin14 <- lm(formula = Profit ~ poly((R.D.Spend + Administration + Marketing.Spend), 14),
                  data = datos)
summary(modelo.lin14)
grfico.14.modlin <- geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                                 y = predict(modelo.lin14,
                                             newdata = datos)),
                             colour = "pink")

#Grado 17
modelo.lin17 <- lm(formula = Profit ~ poly((R.D.Spend + Administration + Marketing.Spend), 17),
                  data = datos)
summary(modelo.lin17)
grfico.17.modlin <- geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                                 y = predict(modelo.lin17,
                                             newdata = datos)),
                             colour = "gray")

#Grado 20
modelo.lin20 <- lm(formula = Profit ~ poly((R.D.Spend + Administration + Marketing.Spend), 20),
                  data = datos)
summary(modelo.lin20)
grfico.20.modlin <- geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                                 y = predict(modelo.lin20,
                                             newdata = datos)),
                             colour = "violet")

#Grafico
grafico.completo <- ggplot() +
  theme_light() +
  geom_point(aes(x = datos$Marketing.Spend+datos$Administration+datos$R.D.Spend,
                 y = datos$Profit)) +
  xlab("Gastos") +
  ylab("Ingresos")

grafico1 <- grafico.completo + grfico.1.modlin + ggtitle("Grado 1")
grafico5 <- grafico.completo + grfico.5.modlin + ggtitle("Grado 5")
grafico10 <- grafico.completo + grfico.10.modlin + ggtitle("Grado 10")
grafico14 <- grafico.completo + grfico.14.modlin + ggtitle("Grado 14")
grafico17 <- grafico.completo + grfico.17.modlin + ggtitle("Grado 17")
grafico20 <- grafico.completo + grfico.20.modlin + ggtitle("Grado 20")

graficos <- plot_grid(grafico1, grafico5, grafico10, grafico14, grafico17, grafico20, ncol = 3)

graficos

ggsave(file = "Poli_SVR.jpg",
       plot = graficos,
       units = "in",
       height = 7,
       width = 14)

#Modelo raro
modelo.raro.datos <- lm(formula = Profit ~ (R.D.Spend + Administration + Marketing.Spend) +
                          I((datos$R.D.Spend + datos$Administration + datos$Marketing.Spend)^2),
                        data = datos)
summary(modelo.raro.datos)

grafico.raro.datos <- 
  grafico.completo + geom_line(aes(x = datos$Marketing.Spend+datos$Administration+datos$R.D.Spend,
                                   y = predict(modelo.raro.datos,
                                               newdata = datos)),
                               colour = "dodgerblue")
grafico.raro.datos

ggsave(file = "mod_raro.jpg",
       plot = grafico.raro.datos,
       units = "in",
       height = 7,
       width = 14)

# El modelo fue basado en la actividad anterior en donde cada uno de los grados muestra un reultado diferente,
# en este caso el grado 1 es el que muestra mejores o más cercanos resultados a el resto en cuanto a la regreción
# de gastos contra ingresos se refiere, el modelo 1 y el modelo 20 tiene sus diferencias visualmente, pero en
# cuanto a exactitud y cercanía el modelo 1 ronda por más puntos dentro de la gráfica aunque sean roses cercanos
# a diferencia del grado 20 en el cual la línea pasa por en medio de los puntos, mostrando más exactitud pero con
# entre menos puntos.

# Al ejecutar "summary(modelo.raro.datos)" se muestra que el R-cuadrado ajustado es un total de 0.9471, demasiado
# cerca de uno, lo cual muestra que es la mejor opcion o la más viable para el modelo



# ***** SUPPORT VECTOR REGRESSION (SVR) *****

svr.lin <- svm(formula = Profit ~ .,
               data = datos,
               kernel = "linear",
               type = "eps-regression")
summary(svr.lin)

svr.poly <- svm(formula = Profit ~ .,
                data = datos,
                kernel = "polynomial",
                type = "eps-regression")
summary(svr.poly)

svr.rad <- svm(formula = Profit ~ .,
               data = datos,
               kernel = "radial",
               type = "eps-regression")
summary(svr.rad)

svr.sig <- svm(formula = Profit ~ .,
               data = datos,
               kernel = "sigmoid",
               type = "eps-regression")
summary(svr.sig)


#Modelo al cuadrado
Y.pred <- predict(svr.poly, newdata = datos)
SSr <- sum((datos$Profit - Y.pred)^2)

Y.mean <- mean(datos$Profit)
SSt <- sum((datos$Profit - Y.mean)^2)
R.squared <- 1 - (SSr / SSt)

n <- length(datos$Profit)
p <- ncol(datos) -1 #Variables independientes o predecitoras
Adj.E.squared <- 1 - ( 1 - R.squared) * ((n - 1) / (n - p - 1))

#Grafica
grafico.svr <- ggplot() +
  theme_light()+
  geom_point(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                 y = datos$Profit)) +
  xlab("Gasto") +
  ylab("Ingreso")+
  geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                y = predict(svr.lin,
                            newdata = datos)),
            colour = "blue",
            linewidth = 1.2) +
  geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                y = predict(svr.poly,
                            newdata = datos)),
            colour = "green",
            linewidth = 1.2)+
  geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                y = predict(svr.rad,
                            newdata = datos)),
            colour = "red",
            linewidth = 1.2)+
  geom_line(aes(x = (datos$Marketing.Spend+datos$Administration+datos$R.D.Spend),
                y = predict(svr.sig,
                            newdata = datos)),
            colour = "violet",
            linewidth = 1.2)
grafico.svr

# El mejor Kernel es el de polinomial debido a que el grado de trabajo para este tiene que ser alto para que
# pase por la mayor cantidad de puntos, con eso indica que el modelo está más cercano a ser acertado y no tener
# tantos errores

ggsave(file = "graf_svr.jpg",
       plot = grafico.svr,
       units = "in",
       height = 7,
       width = 14)