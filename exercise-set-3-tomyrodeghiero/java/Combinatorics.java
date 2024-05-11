public class Combinatorics {
    public static long comb(int n, int m) {
        if (n < 0 || m < 0)
            throw new IllegalArgumentException("Invalid arguments");
        if (m > n)
            return 0;
        if (m == 0 || n == m)
            return 1;
        return comb((n - 1), (m - 1)) + comb((n - 1), m);
    }

    public static long combDP(int n, int m) {
        // Casos base
        if (n < 0 || m < 0)
            throw new IllegalArgumentException("Invalid arguments");
        if (m > n)
            return 0;
        if (m == 0 || n == m)
            return 1;

        // Crear una tabla DP donde dp[i][j] representa C(i, j) [i filas y j columnas]
        long[][] dp = new long[n + 1][m + 1];

        // Inicializar la primera columna y la diagonal principal de la tabla DP
        for (int i = 0; i <= n; i++) {
            dp[i][0] = 1;
        }

        // Llenar la tabla DP de manera bottom-up (de abajo hacia arriba) e iterativa
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= Math.min(i, m); j++) {
                dp[i][j] = dp[i - 1][j - 1] + dp[i - 1][j];
            }
        }

        // El resultado final estará en dp[n][m]
        return dp[n][m];
    }

    public static void main(String[] args) {
        int n = 5;
        int m = 2;
        System.out.println("C(" + n + ", " + m + ") = " + comb(n, m));
    }

}
