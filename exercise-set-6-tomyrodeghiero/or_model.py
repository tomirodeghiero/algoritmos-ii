# Import library 
import numpy as np
from sklearn import linear_model
from sklearn.neural_network import MLPClassifier
#from sklearn.metrics import accuracy_score


# Logical OR  / Data loading 

X = [[0, 0], [0, 1], [1, 0], [1, 1]]

Y = [[0], [1], [1], [1]] 

#train our algorithm with Perceptron
p = linear_model.Perceptron(max_iter=200, tol=False)
p.fit(X,np.ravel(Y))
print('Perceptron:',p.score(X,Y))

#train our algorithm with LinearRegression
lr = linear_model.LinearRegression()
lr.fit(X,Y)
print('Regression score:',lr.score(X,Y)) 

# Función para entrenar y evaluar MLPClassifier con diferentes parámetros
def train_and_evaluate_mlp(hidden_layer_sizes=(2,), activation='relu', solver='adam', max_iter=2000):
    mlpC = MLPClassifier(max_iter=max_iter, activation=activation, solver=solver, hidden_layer_sizes=hidden_layer_sizes, random_state=1)
    mlpC.fit(X, np.ravel(Y))
    score = mlpC.score(X, Y)
    print(f'MLPClassifier score (hidden_layer_sizes={hidden_layer_sizes}, activation={activation}, solver={solver}, max_iter={max_iter}): {score}')
    return mlpC

#train our algorithm with MLPClassifier
mlpC = train_and_evaluate_mlp(hidden_layer_sizes=(2,), activation='identity', solver='sgd', max_iter=2000)
train_and_evaluate_mlp(hidden_layer_sizes=(5,), activation='logistic', solver='adam', max_iter=1000)
train_and_evaluate_mlp(hidden_layer_sizes=(4, 3), activation='tanh', solver='lbfgs', max_iter=1500)
train_and_evaluate_mlp(hidden_layer_sizes=(3, 2, 1), activation='relu', solver='adam', max_iter=500)

# Testing final prediction 
print('Perceptron  Vs. LinearRegression Vs MLPClassifier')
for index in range(len(X)):
  print('')
  print("OR(" + str(X[index]) + ") = " + str(p.predict([X[index]])))
  print("OR(" + str(X[index]) + ") = " + str(lr.predict([X[index]])))
  print("OR(" + str(X[index]) + ") = " + str(mlpC.predict([X[index]])))    