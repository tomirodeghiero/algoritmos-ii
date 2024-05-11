El problema de la mochila (Knapsack Problem) es un clásico de la optimización combinatoria. La versión que deseas resolver, conocida como el problema de la mochila 0/1, involucra seleccionar un subconjunto de ítems que maximicen el valor total sin exceder la capacidad de la mochila. 

Un enfoque greedy para este problema es seleccionar los ítems en orden de su ratio valor-peso (valor por unidad de peso) de mayor a menor, añadiendo ítems a la mochila hasta que se alcance la capacidad máxima. Este método no siempre proporciona la solución óptima, pero es eficiente y ofrece buenos resultados en muchas situaciones prácticas.

### Limitaciones del Enfoque Greedy
El enfoque greedy basado en el ratio valor-peso no siempre garantiza una solución óptima. Por ejemplo, si tienes una mochila con una capacidad limitada y los ítems con mayor ratio valor-peso son de gran peso, es posible que no puedas aprovechar pequeños ítems de alto valor que podrían caber juntos en la mochila.

Para obtener siempre la solución óptima, especialmente en variantes 0/1 del problema donde cada ítem solo puede ser tomado entero o dejado, se recomienda utilizar técnicas de programación dinámica. La técnica greedy es excelente para obtener una solución aproximada rápidamente o cuando la exactitud no es crítica.