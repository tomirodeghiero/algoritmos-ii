import java.util.HashMap;
import java.util.Map;

public class Exercise05 {
    private static Map<Integer, Integer> fCache = new HashMap<>();
    private static Map<Integer, Integer> gCache = new HashMap<>();

    public static int memoF(int n) {
        if (n < 0)
            throw new IllegalArgumentException("n must be greater than or equal to 0");

        if (n == 0)
            return 0;

        if (fCache.containsKey(n)) {
            return fCache.get(n);
        }

        int result = memoF(n - 1) + memoG(n - 1);

        fCache.put(n, result);

        return result;
    }

    public static int memoG(int n) {
        if (n < 0)
            throw new IllegalArgumentException("n must be greater than or equal to 0");

        if (n == 0)
            return 1;

        if (gCache.containsKey(n)) {
            return gCache.get(n);
        }

        int result = memoF(n - 1);

        gCache.put(n, result);

        return result;
    }

    public static void main(String[] args) {
        int n = 10;
        System.out.println("f(" + n + ") = " + memoF(n));
        System.out.println("g(" + n + ") = " + memoG(n));
    }
}