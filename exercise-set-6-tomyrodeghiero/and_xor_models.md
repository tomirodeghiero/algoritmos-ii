### Explicación del Código

1. **Importar librerías**: Importamos las librerías necesarias, incluyendo Numpy y MLPClassifier de Scikit-Learn.

2. **Definir el conjunto de datos**: `X` contiene las entradas y `Y` contiene las salidas esperadas para las funciones AND y XOR.

3. **Función `train_and_evaluate_mlp`**:
   - Esta función toma como parámetros el tamaño de las capas ocultas (`hidden_layer_sizes`), la función de activación (`activation`), el algoritmo de optimización (`solver`), y el número máximo de iteraciones (`max_iter`).
   - Crea un modelo MLPClassifier con los parámetros especificados, entrena el modelo y calcula su precisión.
   - Imprime la precisión del modelo.

4. **Entrenar y evaluar MLPClassifier con diferentes configuraciones**:
   - Se llama a la función `train_and_evaluate_mlp` con diferentes configuraciones de parámetros para ver cómo afectan al rendimiento de la red neuronal.

5. **Probar las predicciones finales**:
   - Se imprime las predicciones del MLPClassifier para cada entrada del conjunto de datos `X`.