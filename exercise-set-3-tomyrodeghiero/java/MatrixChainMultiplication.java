public class MatrixChainMultiplication {
    private Integer[] chain; // contains the matrices dimensions
    private int[][] dp; // dp[i][j] will store the minimum number of scalar multiplications needed to
                        // compute the matrix A[i]A[i+1]...A[j]
    private int[][] split; // split[i][j] will store the index where the optimal split occurs

    public MatrixChainMultiplication(Integer[] chain) {
        this.chain = chain;
        this.dp = new int[chain.length][chain.length];
        this.split = new int[chain.length][chain.length];
    }

    public int matrixChainOrder() {
        int n = chain.length - 1; // length of matrix chain
        for (int len = 2; len <= n; len++) { // len is the chain length
            for (int i = 1; i <= n - len + 1; i++) {
                int j = i + len - 1;
                if (j == n + 1)
                    continue;
                dp[i][j] = Integer.MAX_VALUE;
                for (int k = i; k < j; k++) {
                    // q = cost/scalar multiplications
                    int q = dp[i][k] + dp[k + 1][j] + chain[i - 1] * chain[k] * chain[j];
                    if (q < dp[i][j]) {
                        dp[i][j] = q;
                        split[i][j] = k;
                    }
                }
            }
        }
        return dp[1][n];
    }

    public String getParenthesized() {
        return buildParenthesized(1, chain.length - 1);
    }

    private String buildParenthesized(int i, int j) {
        if (i == j) {
            return "M" + i;
        } else {
            int k = split[i][j];
            return "(" + buildParenthesized(i, k) + buildParenthesized(k + 1, j) + ")";
        }
    }

    public static void main(String[] args) {
        Integer[] dimensions = { 1, 2, 3, 4 }; // example dimensions
        MatrixChainMultiplication mcm = new MatrixChainMultiplication(dimensions);
        int result = mcm.matrixChainOrder();
        System.out.println("Minimum number of multiplications is: " + result);
        System.out.println("Optimal parenthesization is: " + mcm.getParenthesized());
    }
}