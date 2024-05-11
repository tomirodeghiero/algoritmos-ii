Para resolver el problema de los policías y ladrones, donde cada policía puede capturar solo a un ladrón que esté a una distancia máxima \( K \), podemos emplear un algoritmo greedy. Este tipo de algoritmo intentará emparejar cada policía con el ladrón más cercano dentro del rango permitido. La idea básica es iterar sobre la lista de policías y ladrones, intentando capturar un ladrón tan pronto como sea posible.

### Caso donde el valor calculado no es óptimo

El algoritmo greedy puede no ser óptimo si los policías capturan a los ladrones cercanos sin considerar las posiciones de otros policías que podrían capturar a esos mismos ladrones pero que no tendrían otra opción viable.

**Ejemplo:**
- Configuración: `P T T P T`, con \( K = 2 \).
- El primer policía (`P` en posición 0) podría capturar al ladrón en posición 1 o 2, pero si captura al de la posición 2, el segundo policía (en posición 3) podría aún capturar al ladrón en posición 1, optimizando la cantidad de capturas.
- Un algoritmo greedy simple podría hacer que el primer policía capturara al ladrón en posición 1, dejando al segundo policía sin opción de capturar a ningún ladrón.

Este algoritmo greedy proporciona una solución rápida y eficiente, aunque no siempre óptima, pero se comporta bien en muchos escenarios prácticos, especialmente si los policías y ladrones están distribuidos aleatoriamente.