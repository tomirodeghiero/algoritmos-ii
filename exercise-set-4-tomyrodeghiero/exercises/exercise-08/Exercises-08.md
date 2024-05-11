Con el objeto de implementar los algoritmos de Prim y Kruskal en Java y realizar su análisis de tiempo de ejecución en el peor caso, primero vamos a describir cómo funcionan estos algoritmos bajo el paradigma Greedy y luego proporcionaré implementaciones para cada uno.

### Algoritmo de Prim

El algoritmo de Prim construye un árbol de expansión mínimo (MST) comenzando desde un vértice inicial y añadiendo en cada paso el arco más barato que conecte un vértice ya incluido en el MST con uno que no lo esté. Este proceso continúa hasta que todos los vértices estén incluidos en el MST.

**Complejidad Temporal**: La complejidad en el peor caso del algoritmo de Prim es \(O(E \log V)\) cuando se implementa con una cola de prioridad (min-heap), donde \(V\) es el número de vértices y \(E\) es el número de arcos.

### Algoritmo de Kruskal

El algoritmo de Kruskal construye el MST empezando por ordenar todos los arcos de menor a mayor peso y luego seleccionando los arcos en ese orden, asegurándose de no formar ciclos, hasta que se han seleccionado \(V-1\) arcos, donde \(V\) es el número de vértices.

**Complejidad Temporal**: La complejidad en el peor caso del algoritmo de Kruskal es \(O(E \log E)\), ya que el paso más costoso es ordenar todos los arcos. También se realiza la operación de unión-búsqueda que puede optimizarse a \(O(\log V)\) por operación usando estructuras de datos apropiadas.

Estas implementaciones se apegan al estilo Greedy, seleccionando siempre la opción óptima local con la esperanza de que conduzca a una solución global óptima.