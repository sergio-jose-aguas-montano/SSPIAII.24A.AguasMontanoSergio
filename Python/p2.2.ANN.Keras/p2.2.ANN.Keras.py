# -*- coding: utf-8 -*-
"""
Created on Thu Apr 11 14:25:46 2024

@author: Sergio José Aguas Montaño - 217815601

Actividad: p2.2.ANN.Keras
"""

import tensorflow as tf
print(tf.__version__)

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
        Dense(units = 6,
              kernel_initializer='uniform',
              activation = 'sigmoid')
        )

#Capa salida
ann.add(
        Dense(units = 1,
              activation = 'sigmoid',
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
Y_pred = (Y_pred > 0,5)

#Visualizar la arquitectura de la red neuronal
from keras.utils import plot_model
plot_model(modelo,
           to_file='p2.2.ANN.Keras.png',
           show_shapes = True,
           show_layer_activations=True,
           show_layer_names=True,
           )