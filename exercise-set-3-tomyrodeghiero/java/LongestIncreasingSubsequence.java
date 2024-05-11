public class LongestIncreasingSubsequence {
    // Function to calculate the maximum incremental subsequence of a student's
    public static int calculateScore(int[] sequence) {
        int n = sequence.length;
        int[] dp = new int[n];

        // Inicializar dp con 1, ya que la LIS mínima para cada elemento es 1
        // (el elemento mismo)
        for (int i = 0; i < n; i++) {
            dp[i] = 1;
        }

        // Calcular la longitud de la LIS para cada posición (para cada elemento)
        for (int i = 1; i < n; i++) {
            for (int j = 0; j < i; j++) {
                // Si el elemento actual es mayor que el elemento anterior y la LIS actual es
                // menor que la LIS anterior + 1
                if (sequence[i] > sequence[j] && dp[i] < dp[j] + 1) {
                    dp[i] = dp[j] + 1;
                }
            }
        }

        // Encontrar y retornar el valor máximo en dp, que representa la longitud máxima
        // de la LIS
        int max = 0;
        for (int i = 0; i < n; i++) {
            if (dp[i] > max) {
                max = dp[i];
            }
        }

        return max;
    }

    public static void main(String[] args) {
        int[] sequenceStudent = { 1, 3, 2, 4 };
        System.out.println("The length of the longest increasing subsequence is " + calculateScore(sequenceStudent));
    }
}
