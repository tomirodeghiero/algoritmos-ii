package com.unrc;

public class LcsDCExercise05 {
    public static String findLcs(String s1, String s2) {
        if (s1.isEmpty() || s2.isEmpty()) {
            return "";
        } else if (s1.charAt(0) == s2.charAt(0)) {
            return s1.charAt(0) + findLcs(s1.substring(1), s2.substring(1));
        } else {
            String x = findLcs(s1, s2.substring(1));
            String y = findLcs(s1.substring(1), s2);
            return x.length() > y.length() ? x : y;
        }
    }

    public static void main(String[] args) {
        String s1 = "AGGTAB";
        String s2 = "GXTXAYB";
        System.out.println(findLcs(s1, s2));
    }
}