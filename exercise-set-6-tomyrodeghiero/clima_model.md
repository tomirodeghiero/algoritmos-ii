Con el objeto de modelar una red neuronal que prediga la temperatura promedio máxima en un año determinado utilizando Scikit-Learn, seguiremos los siguientes pasos:

1. Cargar y explorar la base de datos.
2. Preprocesar los datos.
3. Dividir los datos en conjuntos de entrenamiento y prueba.
4. Entrenar la red neuronal.
5. Evaluar el rendimiento de la red neuronal.

### Explicación del Código

1. **Importar librerías**: Se importan las librerías necesarias, incluyendo pandas para manipulación de datos, Numpy para cálculos numéricos, y Scikit-Learn para la red neuronal y la evaluación.

2. **Cargar la base de datos**: Se carga el archivo CSV con los datos históricos del clima de Córdoba.

3. **Exploración de datos**: Se imprimen las primeras filas del dataset y un resumen estadístico para entender mejor los datos.

4. **Preprocesamiento de datos**: 
   - Se separan las características (`X`) de la variable objetivo (`y`).
   - Asegúrate de que los nombres de las columnas sean correctos según tu archivo CSV. Por ejemplo, `TempPromedioMaxima` debe ser el nombre exacto de la columna que contiene las temperaturas promedio máximas.

5. **Dividir los datos**: 
   - Se dividen los datos en conjuntos de entrenamiento (60%) y prueba (40%) utilizando `train_test_split`.

6. **Entrenar la red neuronal**: 
   - Se crea y entrena un `MLPRegressor` con una capa oculta de 50 neuronas, usando la función de activación ReLU y el optimizador Adam.
   - Se entrena el modelo con los datos de entrenamiento.

7. **Predecir con el conjunto de prueba**: 
   - Se utilizan los datos de prueba para obtener predicciones con el modelo entrenado.

8. **Evaluar el rendimiento**: 
   - Se calcula el error cuadrático medio (MSE) para evaluar la precisión del modelo.
   - Se muestran algunas predicciones comparadas con los valores reales.