# -*- coding: utf-8 -*-
"""
Created on Sat Apr 13 04:30:39 2024

@author: Sergio José Aguas Montaño
"""

# Librerías
import random  as rd
import math 

# Creación del dataset
dataset = [
    {"entrada":[0,0,1],"salida":0},
    {"entrada":[1,1,1],"salida":1},
    {"entrada":[1,0,1],"salida":1},
    {"entrada":[0,1,1],"salida":0}
    ]

dataset

# Creación de calse para el perceptrón
class ANN:
    # Constructor
    def __init__(self):
        rd.seed(2002)
        # Definir pesos
        # -1, 1 es una concenvión para rango de los pesos
        # una asignación de peso por cada variable        
        self.__pesos = [
            rd.uniform(-1, 1),
            rd.uniform(-1, 1),
            rd.uniform(-1, 1)
            ]
    
    # Núcleo del perceptrón
    def __nucleo(self, entradas):
        suma = 0
        for i, entradas in enumerate(entradas):
            # Suma de ponderaciones
            suma += self.__pesos[i]*entradas
        return suma
    
    # Función de activación - sigmoide = 1 / (1+e^(-x))
    # Hecha en clase
    def __fnActSigmoide(self, score):
        return 1/(1 + math.exp(-score))
    
    # Función de activación - binary step 0 if x<0 ; 1 if x>=0
    def __fnActBinaryStep(self, score):
        if score <= 0:
            return 0
        elif score > 0:
            return 1

    # Función de activación - softplus = ln(1 + e**x)
    def __fnActSoftPlus(self, score):
        return math.log(1 + math.exp(score))

    # Predicción - salida
    def predict(self, datos):
        score = self.__nucleo(datos)
        y_pred = self.__fnActSigmoide(score)
        #y_pred = self.__fnActBinaryStep(score)
        #y_pred = self.__fnActSoftPlus(score)
        return y_pred
    
    # Función de coste
    def __coste(self, y_pred, y_actual):
        return ((y_pred-y_actual)**2)/2    

    # Obtener pesos
    def getPesos(self):
        return self.__pesos  

    # Entrenamiento
    def training(self, datos, epochs):
        for epoch in range(epochs):
            for obs in datos:
                y_pred = self.predict(obs["entrada"])
                coste = self.__coste(y_pred, obs["salida"])
                
                error = obs["salida"] - y_pred
                # Ajuste de pesos
                for index in range(len(self.__pesos)):
                    entrada = obs["entrada"][index]
                    adj = entrada * coste * error
                    print("Pesos Adj. ", index, "-[",epoch, "]: ",adj)
                    self.__pesos[index] += adj

# Modelo    
neurona = ANN()
print("Pesos iniciales: ", neurona.getPesos())

# Predicción
prediccion = neurona.predict(dataset[3]['entrada'])
print(prediccion)

# Entrenamiento
neurona.training(dataset, 5)
print("Pesos nuevos: ", neurona.getPesos())

# Predicción
prediccion = neurona.predict(dataset[3]['entrada'])
print(prediccion)