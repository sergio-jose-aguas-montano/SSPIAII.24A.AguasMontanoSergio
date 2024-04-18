# -*- coding: utf-8 -*-
"""
Created on Thu Apr 11 14:25:46 2024

@author: Sergio José Aguas Montaño - 217815601

Actividad: p2.2.ANN.Keras
"""

#import tensorflow as tf
#print(tf.__version__)

#Librerias
import pandas as pd
#scikit-learn
from sklearn.preprocessing import LabelEncoder, OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.model_selection import train_test_split

#Dataset
df_Bank = pd.read_csv('Datasets/Bank.csv')

X = df_Bank.iloc[:,3:13].values
Y = df_Bank.iloc[:,13]

#Preprocesamiento
#Dummies
X[:,1] = LabelEncoder().fit_transform(X[:,1])
X[:,2] = LabelEncoder().fit_transform(X[:,2])

one = ColumnTransformer(
    [('one_hot_encoder',OneHotEncoder(categories='auto'),[1])],
    remainder='passthrough'
    )

X = one.fit_transform(X)
X = X[:,1:]

#Escalado
X = StandardScaler().fit_transform(X)

#Split
X_train, X_test, Y_train, Y_test = train_test_split(X,Y,
                                                    test_size=0.2,
                                                    random_state=10)

#Modelo ANN
from keras.models import Sequential
from keras.layers import Dense

ann = Sequential()

#Capa de entrada
ann.add(
        Dense(units = 6,
              kernel_initializer='uniform',
              #Numero de variables que van a entrar
              input_dim = 11)
        ) #Agregarcapas

#Capa oculta
ann.add(
        Dense(units = 10,
              kernel_initializer='uniform',
              activation = 'relu')
        )

#Capa salida
ann.add(
        Dense(units = 1,
              activation = 'relu',
              kernel_initializer='uniform')
        )

#Entrenar
ann.compile(optimizer = 'adam',
            loss = 'binary_crossentropy',
            metrics = ['accuracy'])
ann.fit(X_train, Y_train,
        epochs = 15,
        batch_size = 15)

#Guardar el modelo
from keras.models import load_model
ann.save('p2.2.ANN.Keras.h5')

modelo = load_model('p2.2.ANN.Keras.h5')

#Prediccion
Y_pred = modelo.predict(X_test)
Y_pred = (Y_pred > 0.5)

#Visualizar la arquitectura de la red neuronal
from keras.utils import plot_model
plot_model(modelo,
           to_file='p2.2.ANN.Keras.png',
           show_shapes = True,
           show_layer_activations=True,
           show_layer_names=True,
           )

from sklearn.metrics import confusion_matrix

# Construir la matriz de confusión
cm = confusion_matrix(Y_test, Y_pred)

# Mostrar la matriz de confusión
print("Matriz de Confusión:")
print(cm)

## La siguientes modificaciones son a base de prueba y error, ya que acorde a la cantidad de 
## capas y el tipo de funciín que tenga el perceptrón es el resultado que nos dará, entonces
## se hacen ajustes automáticos hasta obtener mejores resultado, tomando en cuenta la siguiente
## página que explica la función, ejemplo en python y ventajas y desventajas de cada uno:
## https://jahazielponce.com/funciones-de-activacion-y-como-puedes-crear-la-tuya-usando-python-r-y-tensorflow/

## Primero: Modificando la capa oculta poniendo 10 unidades
## Segundo: Modificar la capa de activación a "Relu" tanto en la capa oculta como la final
## usando relu por su eficiencia en modelos de aprendizaje profundo
## Tercero: Cada vez que se quiera entrenar el modelo volver a cargar todo el archivo, sino
## la precisión puede llegar a bajar hasta 0.79 de nuevo

import matplotlib.pyplot as plt

# Crear una figura
plt.figure(figsize=(10, 6))

# Graficar los resultados reales y predichos
plt.plot(Y_test, marker='o', linestyle='', label='Real')
plt.plot(Y_pred, marker='x', linestyle='', label='Predicción')

# Añadir etiquetas y leyenda
plt.xlabel('Índice del caso de prueba')
plt.ylabel('Valor')
plt.title('Comparación entre valores reales y predichos')
plt.legend()

# Mostrar la gráfica
plt.show()

##### REGRESION LINEAL #####
# Importar librerías necesarias
from keras.models import Sequential
from keras.layers import Dense

# Crear un modelo secuencial
modelo = Sequential()

# Agregar la capa de entrada
# El número de unidades/neuronas en esta capa debe ser igual al número de características en tus datos
# 'input_dim' especifica el número de características de entrada
modelo.add(Dense(units=6, input_dim=11, activation='relu'))

# Agregar capas ocultas
# Puedes ajustar la cantidad de capas ocultas y el número de neuronas en cada capa según sea necesario
modelo.add(Dense(units=10, activation='relu'))
modelo.add(Dense(units=8, activation='relu'))

# Agregar la capa de salida
# Para problemas de regresión, la función de activación típicamente es lineal
# La capa de salida tiene una sola unidad ya que estamos prediciendo un valor numérico
modelo.add(Dense(units=1, activation='linear'))

# Compilar el modelo
# Selecciona una función de pérdida adecuada para problemas de regresión, como 'mean_squared_error'
modelo.compile(optimizer='adam', loss='mean_squared_error')

# Entrenar el modelo
modelo.fit(X_train, Y_train, epochs=15, batch_size=15)

# Predicción
Y_pred = modelo.predict(X_test)

# Visualización de la comparación entre valores reales y predichos
import matplotlib.pyplot as plt

plt.figure(figsize=(10, 6))
plt.plot(Y_test, marker='o', linestyle='', label='Real')
plt.plot(Y_pred, marker='x', linestyle='', label='Predicción')
plt.xlabel('Índice del caso de prueba')
plt.ylabel('Valor')
plt.title('Comparación entre valores reales y predichos')
plt.legend()
plt.show()
