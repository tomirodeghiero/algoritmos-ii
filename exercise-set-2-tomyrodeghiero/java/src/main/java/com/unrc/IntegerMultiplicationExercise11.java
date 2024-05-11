package com.unrc;

public class IntegerMultiplicationExercise11 {
    // Decrease & Conquer con decremento por una constante
    public static int multiplyByAddition(int a, int b) {
        if (a % 2 == 0) {
            return a / 2 * 2 * b;
        } else {
            return ((a - 1) / 2) * 2 * b + b;
        }
    }

    // Decrease & Conquer con decremento por un factor constante
    public static long karatsubaMultiplication(long x, long y) {
        if (x < 10 || y < 10)
            return x * y;

        // Calcula el tamaño de los números
        long N = Math.max(String.valueOf(x).length(), String.valueOf(y).length());
        long n = N / 2;

        // Divide x e y en partes más manejables
        long a = x / (long) Math.pow(10, n);
        long b = x % (long) Math.pow(10, n);
        long c = y / (long) Math.pow(10, n);
        long d = y % (long) Math.pow(10, n);

        // 3 llamadas recursivas
        long ac = karatsubaMultiplication(a, c);
        long bd = karatsubaMultiplication(b, d);
        long abcd = karatsubaMultiplication(a + b, c + d);

        return ac * (long) Math.pow(10, 2 * n) + (abcd - ac - bd) * (long) Math.pow(10, n) + bd;
    }

    // Función principal
    public static void main(String[] args) {
        int a = 1234;
        int b = 5678;
        System.out.println("Multiplication by addition: " + multiplyByAddition(a, b));
        System.out.println("Karatsuba multiplication: " + karatsubaMultiplication(a, b));
    }

}
