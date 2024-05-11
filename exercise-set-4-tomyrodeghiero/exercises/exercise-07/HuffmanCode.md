La técnica de Huffman en sí misma es una técnica codiciosa (greedy). Selecciona los dos nodos con las menores frecuencias en cada paso, lo cual es un comportamiento codicioso porque siempre busca el costo inmediato mínimo (menor frecuencia) para decidir el siguiente paso. Aquí te proporciono una implementación en Java del algoritmo de Huffman para construir la tabla de codificación:

### Descripción del Código
- **HuffmanNode**: Clase que representa un nodo en el árbol de Huffman.
- **MyComparator**: Un comparador para la cola de prioridad que ordena los nodos por frecuencia de menor a mayor.
- **generateHuffmanTree()**: Construye el árbol de Huffman y genera los códigos.
- **printCode()**: Método recursivo que recorre el árbol para generar los códigos Huffman para cada carácter y los almacena en un mapa.

Este programa leerá un conjunto de caracteres y sus frecuencias, construirá el árbol de Huffman y luego imprimirá los códigos binarios para cada carácter según la codificación de Huffman. Esto sigue un enfoque codicioso al seleccionar siempre los dos nodos con menor frecuencia para combinarlos en cada paso.