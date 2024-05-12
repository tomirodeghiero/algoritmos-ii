Con el objetivo de resolver el ejercicio proporcionado, necesitaremos implementar un motor de búsqueda `DepthFirstSearch` y otro de `IterativeDeepening` basándonos en el framework dado y el ejemplo del `BreadthFirstEngine`. Ambas implementaciones deberán permitir que se integren fácilmente problemas de búsqueda concretos y que se pueda reconstruir el camino desde el estado inicial al estado final en caso de encontrar una solución.

### Notas importantes:
1. **Generación de sucesores**: Los métodos `getSuccessors` se usan para generar los estados sucesores de un estado dado.
2. **Comprobación de estado exitoso**: El método `isSuccess()` determina si un estado es el estado final exitoso.
3. **Reconstrucción del camino**: En ambos motores, el camino se reconstruye navegando hacia atrás desde el estado objetivo hasta el estado inicial, asumiendo que cada estado tiene una referencia a su estado "padre" (esto debe ser manejado en la implementación de los estados).

Estas implementaciones asumen que cada `State` tiene un método `getParent()` para rastrear el camino de regreso al estado inicial, lo cual es crucial para reconstruir el camino cuando se encuentra una solución.