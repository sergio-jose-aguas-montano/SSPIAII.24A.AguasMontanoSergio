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

# Creación de clase para el perceptrón
class ANN:
    # Constructor
    def __init__(self):
        rd.seed(2002)
        # Definir pesos
        # -1, 1 es una convención para el rango de los pesos
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
    
    # Función de coste (existen muchas fórmulas para esto)
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

# Observaciones no utilizadas en el entrenamiento
nuevas_observaciones = [
    {"entrada": [1, 0, 0], "salida": None},
    {"entrada": [0, 1, 0], "salida": None},
    {"entrada": [1, 1, 0], "salida": None}
]

# Realizar predicciones para las nuevas observaciones
for obs in nuevas_observaciones:
    prediccion = neurona.predict(obs["entrada"])
    obs["salida"] = prediccion

# Mostrar las predicciones
for i, obs in enumerate(nuevas_observaciones, 1):
    print(f"Predicción {i}: {obs['salida']}")

# Etiquetas reales
etiquetas_reales = [obs["salida"] for obs in dataset]

# Predicciones del modelo
predicciones_modelo = [neurona.predict(obs["entrada"]) for obs in dataset]

# Construcción de la matriz de confusión
confusion_matrix = [[0, 0], [0, 0]]  # [Verdaderos Positivos, Falsos Positivos], [Falsos Negativos, Verdaderos Negativos]

for i in range(len(etiquetas_reales)):
    if etiquetas_reales[i] == 1:  # Si la etiqueta real es positiva
        if predicciones_modelo[i] >= 0.5:  # Si la predicción del modelo es positiva
            confusion_matrix[0][0] += 1  # Verdadero positivo
        else:
            confusion_matrix[1][0] += 1  # Falso negativo
    else:  # Si la etiqueta real es negativa
        if predicciones_modelo[i] < 0.5:  # Si la predicción del modelo es negativa
            confusion_matrix[1][1] += 1  # Verdadero negativo
        else:
            confusion_matrix[0][1] += 1  # Falso positivo

# Mostrar la matriz de confusión
print("Matriz de Confusión:")
print(confusion_matrix)


#¿Qué función de activación es la adecuada y por qué?, así como el porqué las otras no funcionan de manera adecuada.
#¿Cómo determinaste que era la mejor opción de entrenamiento?

#La mejor para este caso es Binary Step, debido a que es mejor en salidas binarias según si la entrada es mayor o iguala  cero.
#Siendo clara en salidas binarias claras, pero no adecuada a salidas continuas o de regreciones
#Las otras 2, sigmoide y softplus son la sigmoide solo se desea una salida entre 1 y 0, con lo cual tiende a sufrir de problemas de
#desvanecimiento del gradiente, dificultando el entrenamiento de las redes neuronales, y el soft plus es la versión suave de ReLu, dando
#salidas positivas para cualquier valor de entrada, pero puede ser más sensible a valores atípicos debido a su exponencialidad

