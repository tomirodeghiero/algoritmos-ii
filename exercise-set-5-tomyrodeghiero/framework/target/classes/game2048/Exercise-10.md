Con el objetivo de implementar una versión jugable del 2048 que utiliza el algoritmo Minimax con poda alfa-beta para decidir los movimientos de la máquina, vamos a completar los métodos necesarios y crear un loop interactivo para que puedas jugar directamente desde la consola.

### Paso 1: Completando los métodos de movimiento y evaluación

El juego 2048 se basa en deslizar todas las piezas del tablero en una dirección dada. Cuando dos piezas del mismo valor colisionan, se combinan en una pieza cuyo valor es la suma de ambas. El tablero se desliza de modo que todas las piezas se mueven lo más posible en la dirección dada. Vamos a implementar esto, además de una función heurística simple para evaluar los estados del juego.

### Paso 2: Juego Interactivo

Permitiremos que el usuario juegue introduciendo movimientos, y la máquina responderá con los movimientos óptimos calculados por Minimax.

### Descripción del código:

1. **Movimientos y fusiones**: El tablero se actualiza aplicando movimientos en las direcciones especificadas y fusionando piezas de acuerdo con las reglas de 2048.
2. **Juego Interactivo**: Los movimientos del usuario se leen desde la consola, y luego la IA realiza un movimiento basado en Minimax.
3. **Evaluación del juego**: La heurística actual considera tanto el valor máximo en el tablero como el número de celdas vacías para evaluar los estados.

Este juego permite al usuario interactuar y ver cómo responde la IA utilizando estrategias optimizadas calculadas mediante Minimax.