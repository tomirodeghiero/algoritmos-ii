import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPRegressor
from sklearn.metrics import mean_squared_error

# Cargar la base de datos
data = pd.read_csv('clima_historico_cordoba.csv')

# Exploración de datos
print(data.head())
print(data.describe())
print(data.columns)  # Imprimir los nombres de las columnas para verificar

# Preprocesamiento de datos
# Usar 'MaxTemp' como variable objetivo
column_name = 'MaxTemp'  # Nombre de la columna objetivo

# Verificar si la columna existe
if column_name not in data.columns:
    raise ValueError(f"La columna '{column_name}' no se encuentra en el DataFrame. Las columnas disponibles son: {data.columns}")

X = data.drop(column_name, axis=1)  # Eliminar la columna de temperatura máxima
y = data[column_name]  # Variable objetivo

# Dividir los datos en conjuntos de entrenamiento (60%) y prueba (40%)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.4, random_state=42)

# Entrenar la red neuronal
mlp = MLPRegressor(hidden_layer_sizes=(50,), max_iter=1000, activation='relu', solver='adam', random_state=1)
mlp.fit(X_train, y_train)

# Predecir con el conjunto de prueba
y_pred = mlp.predict(X_test)

# Evaluar el rendimiento
mse = mean_squared_error(y_test, y_pred)
print(f'Error cuadrático medio (MSE): {mse}')

# Mostrar algunas predicciones comparadas con los valores reales
comparison = pd.DataFrame({'Real': y_test, 'Predicción': y_pred})
print(comparison.head(10))
