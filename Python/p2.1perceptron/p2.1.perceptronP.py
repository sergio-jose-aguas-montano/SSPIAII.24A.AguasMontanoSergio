# -*- coding: utf-8 -*-
"""
Created on Sat Apr 13 14:53:08 2024

@author: Acer
"""

# Librerías
import random  as rd
import math 
import numpy as np

# Creación del dataset
dataset = [
    {"entrada":[0,0,1],"salida":0},
    {"entrada":[1,1,1],"salida":1},
    {"entrada":[1,0,1],"salida":1},
    {"entrada":[0,1,1],"salida":0}
    ]

# Creación de clase para el perceptrón
class ANN:
    # Constructor
    def __init__(self):
        rd.seed(2002)
        # Definir pesos
        # -1, 1 es una convención para rango de los pesos
        # una asignación de peso por cada variable        
        self.__pesos = [
            rd.uniform(-1, 1),
            rd.uniform(-1, 1),
            rd.uniform(-1, 1)
            ]
    
    # Núcleo del perceptrón
    def __nucleo(self, entradas):
        suma = 0
        for i, entrada in enumerate(entradas):
            # Suma de ponderaciones
            suma += self.__pesos[i]*entrada
        return suma
    
    # Función de activación - sigmoide = 1 / (1+e^(-x))
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
    def predict(self, datos, activation_fn):
        score = self.__nucleo(datos)
        if activation_fn == 'sigmoide':
            return self.__fnActSigmoide(score)
        elif activation_fn == 'binary_step':
            return self.__fnActBinaryStep(score)
        elif activation_fn == 'softplus':
            return self.__fnActSoftPlus(score)
        else:
            raise ValueError("Función de activación no válida")

    # Función de coste (existen muchas formulas para esto)
    def __coste(self, y_pred, y_actual):
        return ((y_pred-y_actual)**2)/2    

    # Obtener pesos
    def getPesos(self):
        return self.__pesos  

    # Entrenamiento
    def training(self, datos, epochs, activation_fn):
        for epoch in range(epochs):
            for obs in datos:
                y_pred = self.predict(obs["entrada"], activation_fn)
                coste = self.__coste(y_pred, obs["salida"])
                
                error = obs["salida"] - y_pred
                # Ajuste de pesos
                for index in range(len(self.__pesos)):
                    entrada = obs["entrada"][index]
                    adj = entrada * coste * error
                    self.__pesos[index] += adj

# Función para construir matriz de confusión
def confusion_matrix(actual, predicted):
    tp, fp, tn, fn = 0, 0, 0, 0
    for i in range(len(actual)):
        if actual[i] == 1 and predicted[i] == 1:
            tp += 1
        elif actual[i] == 0 and predicted[i] == 1:
            fp += 1
        elif actual[i] == 1 and predicted[i] == 0:
            fn += 1
        elif actual[i] == 0 and predicted[i] == 0:
            tn += 1
    return np.array([[tp, fp], [fn, tn]])

# Modelo    
neurona = ANN()
print("Pesos iniciales: ", neurona.getPesos())

# Predicción
prediccion = neurona.predict(dataset[3]['entrada'], 'sigmoide')
print("Predicción antes del entrenamiento: ", prediccion)

# Entrenamiento
neurona.training(dataset, 100, 'sigmoide')
print("Pesos nuevos: ", neurona.getPesos())

# Predicción
prediccion = neurona.predict(dataset[3]['entrada'], 'sigmoide')
print("Predicción después del entrenamiento: ", prediccion)

# Predicciones sobre observaciones no utilizadas en el entrenamiento
print("\nPredicciones sobre observaciones no utilizadas en el entrenamiento:")
for i in range(len(dataset)):
    if i != 3:  # Evitar el dato utilizado en el entrenamiento
        prediccion = neurona.predict(dataset[i]['entrada'], 'sigmoide')
        print(f"Predicción para observación {i+1}: {prediccion}")

# Construcción de matriz de confusión
actual_values = [obs["salida"] for obs in dataset]
predicted_values = [neurona.predict(obs["entrada"], 'sigmoide') for obs in dataset]
conf_matrix = confusion_matrix(actual_values, predicted_values)
print("\nMatriz de confusión:")
print(conf_matrix)
