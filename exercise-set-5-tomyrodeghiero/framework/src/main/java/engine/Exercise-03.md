Con el objetivo de abordar el problema de las 8 reinas y aplicar las estrategias de búsqueda mencionadas, comenzaremos definiendo el problema, el modelo de búsqueda y una función heurística novedosa. Luego, adaptaremos las soluciones a las estrategias específicas de búsqueda.

### Modelo del problema de las 8 reinas como problema de búsqueda:
El problema de las 8 reinas consiste en colocar ocho reinas en un tablero de ajedrez de 8x8 sin que se amenacen entre sí. Para el modelo de búsqueda:
- **Estado**: Una configuración particular del tablero con N reinas colocadas (0 <= N <= 8).
- **Estado inicial**: Un tablero vacío.
- **Estado final**: Un tablero con 8 reinas colocadas sin que se amenacen.
- **Sucesores**: A partir de un estado con N reinas, los sucesores son los estados resultantes de agregar una reina en cualquier casilla vacía de la siguiente fila que no resulte en un ataque inmediato.

### Función heurística:
Una heurística útil para el problema de las 8 reinas podría ser:
- **Número de pares de reinas que no se están amenazando**: Cuantas más reinas no se amenacen mutuamente, más cerca estaremos de la solución. Esta función favorece los estados donde hay menos conflictos entre reinas.

### Implementaciones:

#### Depth First Search (DFS)
DFS explorará cada posible configuración de reinas de manera recursiva hasta que todas las reinas estén en el tablero sin amenazarse.

#### Best First Search (BFS)
BFS utilizará la heurística propuesta para priorizar los estados con menos conflictos entre reinas, explorando así los caminos más prometedores primero.

#### Iterative Deepening Search (IDS)
IDS combinará la profundidad de DFS con el control de niveles de BFS, realizando búsquedas repetidas con profundidades crecientes.

#### Breadth First Search (BFS)
Esta búsqueda explora todos los posibles estados nivel por nivel, asegurando que si existe una solución, será encontrada.