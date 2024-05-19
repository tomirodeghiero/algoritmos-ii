### Explicación

1. **Clase `EightQueensFitness`**: Esta clase define la función de fitness para el problema de las 8 reinas. Evalúa cuántos pares de reinas se atacan entre sí en una configuración dada. El valor de fitness es 28 (máximo número de pares de reinas que no se atacan) menos el número de pares de reinas que se atacan.

2. **Clase `EightQueensGA`**: Esta es la clase principal que configura y ejecuta el algoritmo genético utilizando JGAP para resolver el problema de las 8 reinas. 
   - Se define la configuración del algoritmo genético.
   - Se crea un cromosoma de muestra donde cada gen representa la posición de una reina en una fila.
   - Se inicializa la población y se realiza la evolución durante un número determinado de generaciones.
   - Finalmente, se imprime la mejor solución encontrada y el número de reinas que se atacan entre sí en esa configuración.