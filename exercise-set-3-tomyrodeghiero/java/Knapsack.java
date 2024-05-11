public class Knapsack {

    static int knapsack(int[] wt, int[] val, int W) {
        int N = wt.length; // Número de items
        int[][] dp = new int[N + 1][W + 1];

        // Construir tabla dp en forma bottom up
        for (int i = 0; i <= N; i++) {
            for (int w = 0; w <= W; w++) {
                if (i == 0 || w == 0)
                    dp[i][w] = 0;
                else if (wt[i - 1] <= w)
                    dp[i][w] = Math.max(val[i - 1] + dp[i - 1][w - wt[i - 1]], dp[i - 1][w]);
                else
                    dp[i][w] = dp[i - 1][w];
            }
        }

        // El valor máximo que se puede obtener con la capacidad dada W es dp[N][W].
        return dp[N][W];
    }

    public static void main(String[] args) {
        int[] wt = { 2, 5, 10, 5 }; // pesos de los items
        int[] val = { 20, 30, 50, 10 }; // valores de los items
        int W = 16; // Capacidad de la mochila

        System.out.println("Valor máximo que se puede obtener = " + knapsack(wt, val, W));
    }
}
