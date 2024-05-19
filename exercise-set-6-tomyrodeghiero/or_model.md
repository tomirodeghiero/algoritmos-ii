### Explicación del Código

1. **Importar librerías**: Se importan las librerías necesarias, incluyendo Numpy, LinearRegression y Perceptron de Scikit-Learn, y MLPClassifier para la red neuronal.
   
2. **Definir el conjunto de datos**: `X` contiene las entradas del operador OR y `Y` contiene las salidas esperadas.

3. **Entrenar Perceptron**:
   - Se entrena el algoritmo Perceptron con los datos de entrada `X` y las salidas esperadas `Y`.
   - Se imprime la precisión del modelo.

4. **Entrenar LinearRegression**:
   - Se entrena el algoritmo LinearRegression con los datos de entrada `X` y las salidas esperadas `Y`.
   - Se imprime la precisión del modelo.

5. **Función `train_and_evaluate_mlp`**:
   - Esta función toma como parámetros el tamaño de las capas ocultas (`hidden_layer_sizes`), la función de activación (`activation`), el algoritmo de optimización (`solver`), y el número máximo de iteraciones (`max_iter`).
   - Crea un modelo MLPClassifier con los parámetros especificados, entrena el modelo y calcula su precisión.
   - Imprime la precisión del modelo.

6. **Entrenar y evaluar MLPClassifier con diferentes configuraciones**:
   - Se llama a la función `train_and_evaluate_mlp` con diferentes configuraciones de parámetros para ver cómo afectan al rendimiento de la red neuronal.

7. **Probar las predicciones finales**:
   - Se imprimen las predicciones de Perceptron, LinearRegression y la última configuración de MLPClassifier para cada entrada del conjunto de datos `X`.

Este código realiza pruebas con el Perceptron, la regresión lineal y el MLPClassifier con varias configuraciones, mostrando cómo estos modelos aprenden el comportamiento del operador lógico OR.