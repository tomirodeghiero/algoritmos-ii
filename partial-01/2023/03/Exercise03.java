import java.util.HashMap;
import java.util.Map;

public class Exercise03 {
    // Método de Programación Dinámica para calcular el número de Catalan
    public static long catalanPD(int n) {
        long[] catalan = new long[n + 1];
        catalan[0] = 1;

        for (int i = 1; i <= n; i++) {
            catalan[i] = 0;
            for (int j = 0; j < i; j++) {
                catalan[i] += catalan[j] * catalan[i - j - 1];
            }
        }
        return catalan[n];
    }

    // Mapa para memoization
    private static Map<Integer, Long> memo = new HashMap<>();

    // Función memoizada para calcular el n-ésimo número de Catalan
    public static long catalanM(int n) {
        if (n <= 1) {
            return 1;
        }
        if (memo.containsKey(n)) {
            return memo.get(n);
        }

        long res = 0;
        for (int i = 0; i < n; i++) {
            res += catalanM(i) * catalanM(n - i - 1);
        }

        memo.put(n, res);
        return res;
    }

    public static void main(String[] args) {
        int n = 10; // Cambia este valor para calcular otro número de Catalan
        System.out.println("Catalan number with Dynamic Programming for " + n + " is: " + catalanPD(n));
        System.out.println("Catalan number with Memoization for " + n + " is: " + catalanM(n));
    }
}