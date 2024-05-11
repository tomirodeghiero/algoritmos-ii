package util;

public class GCDAlgorithmsExercise04 {

    // Método para calcular el MCD usando el algoritmo de Euclides recursivo
    public static int gcdEuclideanRecursive(int a, int b) {
        if (b == 0) {
            return a;
        }
        return gcdEuclideanRecursive(b, a % b);
    }

    // Método para calcular el MCD usando el algoritmo de Euclides iterativo
    public static int gcdEuclideanIterative(int a, int b) {
        while (b != 0) {
            int r = a % b;
            a = b;
            b = r;
        }
        return a;
    }

    // Método main para probar los algoritmos y medir su tiempo de ejecución
    public static void main(String[] args) {
        int a = 3918848;
        int b = 1653264;

        long start = System.nanoTime();
        int gcdRecursive = gcdEuclideanRecursive(a, b);
        long end = System.nanoTime();
        System.out.println("MCD Recursivo de  (" + a + ", " + b + ") es: " + gcdRecursive);
        System.out.println("Tiempo tomado para la versión recursiva: " + (end - start) + " nanosegundos");

        start = System.nanoTime();
        int gcdIterative = gcdEuclideanIterative(a, b);
        end = System.nanoTime();
        System.out.println("MCD Iterativo de (" + a + ", " + b + ") es: " + gcdIterative);
        System.out.println("Tiempo tomado para la versión iterativa: " + (end - start) + " nanosegundos");
    }
}
