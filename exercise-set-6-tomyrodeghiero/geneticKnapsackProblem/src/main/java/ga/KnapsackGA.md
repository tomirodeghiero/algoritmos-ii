### Explicación

1. **Clase `KnapsackProblem`**: Define los parámetros del problema de la mochila, incluyendo la capacidad máxima, los pesos y valores de los ítems.
2. **Clase `KnapsackFitness`**: Implementa la función de fitness que evalúa la calidad de un cromosoma en función del valor total de los ítems seleccionados sin exceder la capacidad máxima de la mochila.
3. **Clase `KnapsackProblemGA`**: Configura y ejecuta el algoritmo genético usando JGAP para resolver el problema de la mochila. Inicializa la población, realiza la evolución y obtiene la mejor solución encontrada.