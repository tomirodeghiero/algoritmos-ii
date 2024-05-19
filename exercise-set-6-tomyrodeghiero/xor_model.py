# Importar librerías
import numpy as np
from sklearn.neural_network import MLPClassifier

# Datos para el operador lógico XOR
X = [[0, 0], [0, 1], [1, 0], [1, 1]]
Y = [[0], [1], [1], [0]]

# Función para entrenar y evaluar MLPClassifier con diferentes parámetros
def train_and_evaluate_mlp(hidden_layer_sizes=(2,), activation='relu', solver='adam', max_iter=2000):
    mlpC = MLPClassifier(max_iter=max_iter, activation=activation, solver=solver, hidden_layer_sizes=hidden_layer_sizes, random_state=1)
    mlpC.fit(X, np.ravel(Y))
    score = mlpC.score(X, Y)
    print(f'MLPClassifier score (hidden_layer_sizes={hidden_layer_sizes}, activation={activation}, solver={solver}, max_iter={max_iter}): {score}')
    return mlpC

# Entrenar y evaluar el MLPClassifier con diferentes configuraciones
mlpC = train_and_evaluate_mlp(hidden_layer_sizes=(2,), activation='identity', solver='sgd', max_iter=2000)
train_and_evaluate_mlp(hidden_layer_sizes=(5,), activation='logistic', solver='adam', max_iter=1000)
train_and_evaluate_mlp(hidden_layer_sizes=(4, 3), activation='tanh', solver='lbfgs', max_iter=1500)
train_and_evaluate_mlp(hidden_layer_sizes=(3, 2, 1), activation='relu', solver='adam', max_iter=500)

# Probar las predicciones finales
print('\nMLPClassifier predicciones para la función XOR')
for index in range(len(X)):
    print(f"XOR({X[index]}) = {mlpC.predict([X[index]])}")
