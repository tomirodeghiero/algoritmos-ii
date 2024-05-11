Las técnicas de "Divide & Conquer", "Decrease & Conquer" y "Programación Dinámica" son fundamentales en el diseño de algoritmos, cada una con sus propias características y aplicaciones, pero también con puntos en común y diferencias importantes.

**Puntos en común**:
1. **Estructura de subproblemas**: Tanto "Divide & Conquer" como "Decrease & Conquer" y "Programación Dinámica" descomponen problemas en subproblemas más pequeños para simplificar la solución del problema global.
2. **Recursión**: Estas técnicas frecuentemente utilizan la recursión para abordar y resolver los subproblemas, aunque la forma en que gestionan y utilizan los resultados puede variar.

**Diferencias**:
1. **Independencia de subproblemas**:
   - **Divide & Conquer**: Divide el problema original en varios subproblemas independientes, los resuelve de manera recursiva, y luego combina sus soluciones para formar la solución al problema original【22†source】.
   - **Decrease & Conquer**: Reduce el problema a un solo subproblema más pequeño por iteración, soluciona ese subproblema y usa su resultado para construir la solución al problema más grande【22†source】.
   
2. **Reutilización de resultados**:
   - **Programación Dinámica**: Es fundamentalmente diferente en que explícitamente busca reutilizar los resultados de los subproblemas anteriores para optimizar el cálculo global, evitando el recálculo innecesario que es común en soluciones puramente recursivas de "Divide & Conquer" y "Decrease & Conquer"【20†source】. En "Programación Dinámica", los subproblemas a menudo se solapan, y la técnica los resuelve de manera que cada subproblema se calcula solo una vez y su solución se almacena para su uso futuro.
   
3. **Orden de solución**:
   - **Programación Dinámica**: A menudo trabaja en un orden "bottom-up", empezando por los casos más pequeños y sencillos y usando estos resultados para construir soluciones a problemas más grandes.
   - **Divide & Conquer** y **Decrease & Conquer**: Generalmente adoptan un enfoque "top-down", comenzando por el problema original y descomponiéndolo en subproblemas que luego se solucionan recursivamente.

Estas técnicas son herramientas poderosas en la caja de herramientas de cualquier diseñador de algoritmos, y elegir entre ellas depende de la naturaleza específica del problema a resolver, especialmente en términos de cómo se estructuran y dependen entre sí los subproblemas.

--

Es importante destacar que la Programación Dinámica y el uso de la memoization es diferente, aunque a menudo se usan en conjunto y pueden confundirse.

La **Programación Dinámica** efectivamente se basa en el principio de reutilizar los resultados de subproblemas previamente calculados para mejorar la eficiencia del algoritmo. Este enfoque puede implementarse de dos maneras:

1. **Bottom-Up (Tabulación)**: En este enfoque, el algoritmo rellena una tabla (generalmente un arreglo o una matriz) de manera iterativa. Comienza con los casos más simples y usa estos resultados para construir soluciones a problemas cada vez más grandes.

2. **Top-Down (Memoization)**: Aquí es donde entra la memoization. Este método también sigue el principio de la Programación Dinámica pero implementa la recursividad más directamente. A medida que el algoritmo resuelve los subproblemas, almacena sus resultados en alguna estructura de datos (como un mapa o arreglo), y verifica esta estructura para evitar recalcular resultados. Esta técnica de almacenamiento de resultados intermedios para evitar recomputaciones innecesarias es lo que conocemos como memoization.

Por lo tanto, la memoization es una técnica que puede ser usada dentro de la Programación Dinámica en su enfoque top-down para mejorar la eficiencia de los cálculos que involucran recursividad, especialmente cuando los subproblemas se solapan【19†source】. Esto permite que los cálculos se realicen una sola vez y sus resultados se guarden para su uso futuro, eliminando la necesidad de realizar los mismos cálculos varias veces.