#Autor: Sergio Jose Aguas Montaño

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio/p1.1_ProcesamientoRegresionLineal")

options(scipen = 999)
set.seed(2000)

#Asignacion de variables
variable1 <- "1"
variable2 <- "2"
variable3 <- "3"

#Dataset simple
datos <- data.frame(
  Nombre = c("Juan", "María", "Pedro", "Ana"),
  Edad = c(25, 30, 22, 28),
  Puntuacion = c(85, 92, 78, 89)
)

#Dataset de R
datos_autos <- mtcars
head(datos_autos)

#Importa datos desde un csv y almacenarlo en una variable
datos_csv <- read.csv("Datasets/Data.csv", header=T)

#Emplea la función "source" para llamar otro script de R
source("VariablesVector.R")