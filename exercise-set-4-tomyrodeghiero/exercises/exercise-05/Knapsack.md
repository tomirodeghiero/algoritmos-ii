# Problema de la Mochila
El problema de la mochila (Knapsack Problem) es un clásico de la optimización combinatoria. La versión que deseas resolver, conocida como el problema de la mochila 0/1, involucra seleccionar un subconjunto de ítems que maximicen el valor total sin exceder la capacidad de la mochila. 

Un enfoque greedy para este problema es seleccionar los ítems en orden de su ratio valor-peso (valor por unidad de peso) de mayor a menor, añadiendo ítems a la mochila hasta que se alcance la capacidad máxima. Este método no siempre proporciona la solución óptima, pero es eficiente y ofrece buenos resultados en muchas situaciones prácticas.

### Limitaciones del Enfoque Greedy
El enfoque greedy basado en el ratio valor-peso no siempre garantiza una solución óptima. Por ejemplo, si tienes una mochila con una capacidad limitada y los ítems con mayor ratio valor-peso son de gran peso, es posible que no puedas aprovechar pequeños ítems de alto valor que podrían caber juntos en la mochila.

Para obtener siempre la solución óptima, especialmente en variantes 0/1 del problema donde cada ítem solo puede ser tomado entero o dejado, se recomienda utilizar técnicas de programación dinámica. La técnica greedy es excelente para obtener una solución aproximada rápidamente o cuando la exactitud no es crítica.

### Conclusión
El enfoque Greedy podría seleccionar ítems con mayor ratio primero, lo que puede no llevar a la solución óptima. Sin embargo, en este caso particular, la selección basada en el ratio produce la solución correcta, pero no es una garantía general. En resumen, el enfoque Greedy es eficiente y fácil de implementar, pero no siempre proporciona una solución óptima para la versión 0/1 del problema de la mochila.