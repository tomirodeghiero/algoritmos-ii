public class CoinChange {
    // Método Divide & Conquer para encontrar el mínimo número de monedas
    public static int coinChangeDC(int[] coins, int C) {
        // Si el cambio a realizar es 0, no se necesitan monedas
        if (C == 0) {
            return 0;
        }

        // Inicializamos el resultado con un valor grande
        int result = Integer.MAX_VALUE;

        // Probar todas las monedas menores que C para encontrar la solución mínima
        for (int coin : coins) {
            if (coin <= C) {
                int subRes = coinChangeDC(coins, C - coin);

                // Verificamos si el subresultado fue encontrado y actualizamos resultado
                if (subRes != Integer.MAX_VALUE && subRes + 1 < result) {
                    result = subRes + 1;
                }
            }
        }

        return result;
    }

    // Método de Programación Dinámica para encontrar el mínimo número de monedas
    public static int coinChangeDP(int[] coins, int C) {
        // Crear una tabla para almacenar los resultados de los subproblemas
        int[] dp = new int[C + 1];

        // Inicializar la tabla con un valor mayor al máximo posible
        for (int i = 0; i <= C; i++) {
            dp[i] = Integer.MAX_VALUE;
        }

        // Caso base: no se necesitan monedas para dar un cambio de 0
        dp[0] = 0;

        // Construir la tabla dp[] de abajo hacia arriba
        for (int i = 1; i <= C; i++) {
            // Ir a través de todas las monedas más pequeñas que i
            for (int j = 0; j < coins.length; j++) {
                if (coins[j] <= i) {
                    int sub_res = dp[i - coins[j]];
                    if (sub_res != Integer.MAX_VALUE && sub_res + 1 < dp[i]) {
                        dp[i] = sub_res + 1;
                    }
                }
            }
        }
        return dp[C] == Integer.MAX_VALUE ? -1 : dp[C];
    }

    public static void main(String[] args) {
        int[] coins = { 1, 2, 5 }; // Ejemplo de denominaciones de monedas
        int C = 12130; // Ejemplo de cambio a dar
        System.out.println("Mínimo número de monedas requerido es (DC): " + coinChangeDC(coins, C));
        System.out.println("Mínimo número de monedas requerido es (DP): " + coinChangeDP(coins, C));
    }
}
