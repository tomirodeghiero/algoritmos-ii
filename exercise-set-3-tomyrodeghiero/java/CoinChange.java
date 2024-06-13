import java.util.HashMap;
import java.util.Map;

public class CoinChange {
    /* Método Divide & Conquer para encontrar el mínimo número de monedas */
    public static int coinChangeDC(int[] coins, int C) {
        // Si el cambio a realizar es 0, no se necesitan monedas
        if (C <= 0) {
            return 0;
        }

        // Se inicializa el resultado con un valor grande
        int result = Integer.MAX_VALUE;

        // Probar todas las monedas menores que C para encontrar la solución mínima
        for (int coin : coins) {
            if (coin <= C) {
                int subRes = coinChangeDC(coins, C - coin);

                // Verificar si el subresultado fue encontrado y actualizamos resultado
                if (subRes != Integer.MAX_VALUE && subRes + 1 < result) {
                    result = subRes + 1;
                }
            }
        }

        return result;
    }

    /* Método Memoization para encontrar el mínimo número de monedas */
    private static Map<Integer, Integer> cache = new HashMap<>();

    public static int coinChangeMemo(int[] coins, int C) {
        if (!cache.containsKey(C)) {
            // Si el cambio requerido es 0, no se necesitan monedas
            if (C == 0) {
                return 0;
            } else if (C < 0) {
                cache.put(C, Integer.MAX_VALUE);
            } else {
                // Inicializar el resultado con un valor muy alto
                int result = Integer.MAX_VALUE;

                // Probar todas las monedas menores o iguales que C para encontrar la solución
                // mínima
                for (int coin : coins) {
                    int aux = coinChangeMemo(coins, C - coin);

                    if (aux != Integer.MAX_VALUE && aux + 1 < result) {
                        result = aux + 1;
                    }
                }

                // Guardar el resultado calculado en la caché para uso futuro
                cache.put(C, result);
            }
        }
        return cache.get(C);
    }

    public static void main(String[] args) {
        int[] coins = { 1, 2, 5 }; // Ejemplo de denominaciones de monedas
        int C = 7; // Ejemplo de cambio a dar
        System.out.println("Mínimo número de monedas requerido es (DC): " + coinChangeDC(coins, C));
        System.out.println("Mínimo número de monedas requerido es (Memo): " +
                coinChangeMemo(coins, C));
    }
}
