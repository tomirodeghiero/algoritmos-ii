import java.util.HashMap;
import java.util.Map;

public class DamerauLevenshtein {
    // Calcula la distancia de Damerau-Levenshtein entre dos cadenas de forma
    // recursiva
    public static int damerauLevenshteinDistanceRecursive(String s1, String s2) {
        return damerauLevenshteinDistanceRecursive(s1, s1.length(), s2, s2.length());
    }

    private static int damerauLevenshteinDistanceRecursive(String s1, int len1, String s2, int len2) {
        if (len1 == 0) {
            return len2;
        }
        if (len2 == 0) {
            return len1;
        }

        int cost = (s1.charAt(len1 - 1) == s2.charAt(len2 - 1)) ? 0 : 1;

        int delete = damerauLevenshteinDistanceRecursive(s1, len1 - 1, s2, len2) + 1;
        int insert = damerauLevenshteinDistanceRecursive(s1, len1, s2, len2 - 1) + 1;
        int replace = damerauLevenshteinDistanceRecursive(s1, len1 - 1, s2, len2 - 1) + cost;

        int distance = Math.min(Math.min(delete, insert), replace);

        if (len1 > 1 && len2 > 1 && s1.charAt(len1 - 1) == s2.charAt(len2 - 2)
                && s1.charAt(len1 - 2) == s2.charAt(len2 - 1)) {
            distance = Math.min(distance, damerauLevenshteinDistanceRecursive(s1, len1 - 2, s2, len2 - 2) + cost);
        }

        return distance;
    }

    //

    private static Map<String, Integer> memo = new HashMap<>();

    public static int damerauLevenshteinDistanceMemo(String s1, String s2) {
        return damerauLevenshteinDistanceMemo(s1, s1.length(), s2, s2.length());
    }

    private static int damerauLevenshteinDistanceMemo(String s1, int len1, String s2, int len2) {
        String key = len1 + "-" + len2;
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        if (len1 == 0) {
            memo.put(key, len2);
            return len2;
        }
        if (len2 == 0) {
            memo.put(key, len1);
            return len1;
        }

        int cost = (s1.charAt(len1 - 1) == s2.charAt(len2 - 1)) ? 0 : 1;

        int delete = damerauLevenshteinDistanceMemo(s1, len1 - 1, s2, len2) + 1;
        int insert = damerauLevenshteinDistanceMemo(s1, len1, s2, len2 - 1) + 1;
        int replace = damerauLevenshteinDistanceMemo(s1, len1 - 1, s2, len2 - 1) + cost;

        int distance = Math.min(Math.min(delete, insert), replace);

        if (len1 > 1 && len2 > 1 && s1.charAt(len1 - 1) == s2.charAt(len2 - 2)
                && s1.charAt(len1 - 2) == s2.charAt(len2 - 1)) {
            distance = Math.min(distance, damerauLevenshteinDistanceMemo(s1, len1 - 2, s2, len2 - 2) + cost);
        }

        memo.put(key, distance);
        return distance;
    }

    public static void main(String[] args) {
        String str1 = "ace";
        String str2 = "abcde";
        System.out.println(
                "Damerau-Levenshtein distance (Recursive): " + damerauLevenshteinDistanceRecursive(str1, str2));
        System.out.println("Damerau-Levenshtein distance (Memoization): " + damerauLevenshteinDistanceMemo(str1, str2));
    }
}
