#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Programas")

source("S01.Librerias.R")
options(scipen = 999)
set.seed(2002)

#Importar datos
df.Salary <- read.csv(file ="Datasets/Salary_Data.csv",
                    header = T,
                    stringsAsFactors = F)

#preprocesamiento
summary(df.Salary)
boxplot(df.Salary)
plot(df.Salary)

#Dividir
Split <- sample.split(Y = df.Salary$Salary, SplitRatio = 0.8)
df.Salary.Train <- subset(df.Salary, Split == T)
df.Salary.Test <- subset(df.Salary, Split == F)



#modelo Reg. Lineal simple
mdl.Regresor <- lm(formula = Salary ~ YearsExperience,
                   data = df.Salary.Train)

summary(mdl.Regresor)

#Prediccion
mdl.Predict <- predict(object = mdl.Regresor,
                       newdata = df.Salary.Test)

#Gráfica
plt.Salary <- ggplot() + 
  theme_dark() +
  ggtitle("Regresión: Sueldo Vs. Experiencia") + 
  xlab("Años de experiencia") + 
  ylab("Salario")


plt.Salary.Data <- plt.Salary + 
  geom_point(aes(x = df.Salary.Train$YearsExperience, 
                 y = df.Salary.Train$Salary),
             colour = "deepskyblue1",
             shape = 25,
             size = 5) +
  geom_line(aes(x = df.Salary.Train$YearsExperience,
                y = predict(mdl.Regresor, newdata = df.Salary.Train)),
            colour = "darkorchid4",
            )
plt.Salary.Data

#Gráficos visual
install.packages("esquisse")
library(esquisse)

esquisser(df.Salary.Train)

library(ggplot2)

ggplot(df.Salary.Train) +
 aes(x = YearsExperience, y = Salary, colour = Salary) +
 geom_point(shape = "circle plus", 
 size = 2.5) +
 geom_smooth(span = 0.75) +
 scale_color_viridis_c(option = "viridis", direction = 1) +
 theme_minimal()




