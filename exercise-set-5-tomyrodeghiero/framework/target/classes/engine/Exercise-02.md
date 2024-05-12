Con el objeto de implementar un motor de búsqueda `Best First Search` para problemas de búsqueda genéricos utilizando programación orientada a objetos, primero definiremos una estructura que nos permita manejar de manera genérica los problemas de búsqueda informada. La implementación incluirá una estructura de datos para la cola de prioridad que ordenará los estados según una función de evaluación proporcionada por el problema concreto.

### Notas importantes:
1. **Cola de prioridad**: Usamos `PriorityQueue` para mantener los nodos en orden según una función de comparación que define la prioridad de cada estado. Esta función debe ser proporcionada al motor y es específica para el problema que se está resolviendo.
2. **Función de comparación**: El `Comparator<S>` permite la flexibilidad de definir cómo se comparan los estados para el ordenamiento en la cola de prioridad, lo cual es crucial para personalizar la búsqueda según las necesidades de búsqueda informada.
3. **Reconstrucción del camino**: Similar a las implementaciones anteriores, almacenamos el camino al estado objetivo una vez encontrado, asumiendo que cada estado tiene acceso a su estado padre.

Esta implementación es robusta y flexible para adaptarse a varios tipos de problemas de búsqueda informada, donde la función de comparación puede cambiar según las heurísticas específicas del problema.