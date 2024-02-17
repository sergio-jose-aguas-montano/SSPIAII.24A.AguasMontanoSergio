#Librerias

libs <- c("ggplot2","caTools","mltools","data.table","cowplot")

if(!require(libs)){
  install.packages(libs, dependencies = T)}

#Habilitar
library(ggplot2)
library(caTools)
library(mltools)
library(data.table)
library(cowplot)