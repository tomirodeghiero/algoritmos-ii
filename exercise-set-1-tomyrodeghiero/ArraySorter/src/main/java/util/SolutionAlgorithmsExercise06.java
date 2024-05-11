package util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SolutionAlgorithmsExercise06 {

    // Método para decidir si un conjunto de enteros se puede particionar en dos
    // conjuntos de igual suma
    public static boolean canPartition(int[] nums) {
        int totalSum = Arrays.stream(nums).sum();
        if (totalSum % 2 != 0)
            return false;
        int subsetSum = totalSum / 2;
        boolean[] dp = new boolean[subsetSum + 1];
        dp[0] = true;

        for (int num : nums) {
            for (int j = subsetSum; j >= num; j--) {
                dp[j] = dp[j] || dp[j - num];
            }
        }
        return dp[subsetSum];
    }

    // Método para verificar si dos cadenas son anagramas
    public static boolean areAnagrams(String s1, String s2) {
        if (s1.length() != s2.length())
            return false;

        int[] charCounts = new int[26];
        for (char c : s1.toCharArray())
            charCounts[c - 'a']++;
        for (char c : s2.toCharArray()) {
            if (--charCounts[c - 'a'] < 0)
                return false;
        }
        return true;
    }

    // Método para descomponer un número en sus factores primos
    public static List<Integer> primeFactors(int number) {
        List<Integer> factors = new ArrayList<>();
        for (int i = 2; i <= number / i; i++) {
            while (number % i == 0) {
                factors.add(i);
                number /= i;
            }
        }
        if (number > 1) {
            factors.add(number);
        }
        return factors;
    }

    // Método para decidir si `p` es subcadena de `s`
    public static boolean isSubstring(String s, String p) {
        if (s == null || p == null || s.length() < p.length()) {
            return false;
        }

        for (int i = 0; i <= s.length() - p.length(); i++) {
            int j;
            for (j = 0; j < p.length(); j++) {
                if (s.charAt(i + j) != p.charAt(j)) {
                    break;
                }
            }
            if (j == p.length()) {
                return true;
            }
        }
        return false;
    }

    // Método para decidir si `p` es subsecuencia de `s`
    public static boolean isSubsequence(String s, String p) {
        int i = 0, j = 0;
        while (i < s.length() && j < p.length()) {
            if (s.charAt(i) == p.charAt(j)) {
                j++;
            }
            i++;
        }
        return j == p.length();
    }

    // Método para encontrar el k-ésimo elemento más grande en una secuencia
    public static int findKthLargest(int[] nums, int k) {
        int maxValue = Integer.MAX_VALUE;
        int answer = maxValue;

        for (int i = 0; i < k; i++) {
            int currentMax = Integer.MIN_VALUE;
            for (int num : nums) {
                if (num > currentMax && num < answer) {
                    currentMax = num;
                }
            }
            answer = currentMax;
            if (answer == Integer.MIN_VALUE) {
                break; // No se encontró un valor válido, termina el ciclo
            }
        }

        return answer;
    }

    // Método principal para probar los métodos
    public static void main(String[] args) {
        // Test de particionar un conjunto
        int[] nums = { 1, 5, 11, 5 };
        System.out.println("El conjunto se puede particionar en dos subconjuntos de igual suma: " + canPartition(nums));

        // Test de anagramas
        String s1 = "listen";
        String s2 = "silent";
        System.out.println("Las cadenas son anagramas: " + areAnagrams(s1, s2));

        // Test de factorización prima
        int number = 60;
        System.out.println("Factores primos de " + number + ": " + primeFactors(number));

        // Test de subcadena
        String s = "helloworld";
        String p = "low";
        System.out.println("p es subcadena de s: " + isSubstring(s, p));

        // Test de subsecuencia
        p = "hlo";
        System.out.println("p es subsecuencia de s: " + isSubsequence(s, p));

        // Test del k-ésimo elemento más grande
        int[] sequence = { 7, 10, 4, 3, 20, 15 };
        int k = 3;
        System.out.println("El " + k + "-ésimo elemento más grande es: " + findKthLargest(sequence, k));
    }

}
