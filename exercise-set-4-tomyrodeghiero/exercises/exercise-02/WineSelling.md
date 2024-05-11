Con el objeto de implementar una técnica greedy en el problema de los vinos, elegiremos siempre vender el vino que genere la mayor ganancia en el momento actual, ya sea desde el principio o el final de la lista. Sin embargo, esta técnica greedy puede no ofrecer siempre la solución óptima, como veremos en un ejemplo.

### Ejemplo donde la Técnica Greedy no es Óptima

Supongamos que se tienen tres vinos con los precios iniciales de \[1, 100, 50\]. Si aplicamos un enfoque greedy para resolver el problema de maximizar la ganancia total vendiendo estos vinos a lo largo de los años, el algoritmo tomaría las siguientes decisiones:

1. **Año 1:** Vender el vino con el mayor precio visible entre los dos extremos, que es el tercer vino (50). Ganancia del año: \$50.
2. **Año 2:** Ahora quedan los precios \[1, 100\]. El vino con el mayor precio es el segundo (100). Al venderlo en el segundo año, la ganancia sería 100 \* 2 = \$200.
3. **Año 3:** Solo queda el primer vino, que es 1. Vendiéndolo en el tercer año, la ganancia sería 1 \* 3 = \$3.

La ganancia total utilizando la técnica greedy sería \$50 + \$200 + \$3 = \$253.

### Estrategia Óptima

Contrastemos esto con la estrategia óptima:

1. **Año 1:** Vender el primer vino, que tiene el precio más bajo. Ganancia del año: \$1.
2. **Año 2:** Ahora quedan los precios \[100, 50\]. Vender el tercer vino (50). Al venderlo en el segundo año, la ganancia sería 50 \* 2 = \$100.
3. **Año 3:** Finalmente, vendemos el vino que queda, que es el segundo vino (100). Vendiéndolo en el tercer año, la ganancia sería 100 \* 3 = \$300.

La ganancia total utilizando la estrategia óptima sería \$1 + \$100 + \$300 = \$401.

### Conclusión

Este ejemplo claramente ilustra cómo la técnica greedy, aunque simple y eficaz en algunos casos, puede fallar al considerar las decisiones óptimas que maximizan la ganancia a largo plazo. El enfoque greedy toma decisiones basadas en el beneficio inmediato sin considerar cómo estas decisiones pueden impactar las ganancias futuras, lo que puede llevar a resultados subóptimos en ciertas situaciones.