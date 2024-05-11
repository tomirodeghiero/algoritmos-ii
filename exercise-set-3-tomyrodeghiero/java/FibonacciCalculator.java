public class FibonacciCalculator {

    /**
     * Calcula el n-ésimo número de Fibonacci utilizando Programación Dinámica.
     * 
     * @param n el índice del número de Fibonacci a calcular
     * @return el n-ésimo número de Fibonacci
     */
    public static int fibonacci(int n) {
        if (n == 0) {
            return 0; // Caso base: el primer número de Fibonacci es 0
        }
        if (n == 1) {
            return 1; // Caso base: el segundo número de Fibonacci es 1
        }

        int fibAcum0 = 0; // Almacenará F(n-2)
        int fibAcum1 = 1; // Almacenará F(n-1)
        int aux; // Variable temporal para realizar el intercambio

        // Itera desde 2 hasta n (ya que los dos primeros números ya están cubiertos por
        // los casos base)
        for (int i = 2; i <= n; i++) {
            aux = fibAcum0 + fibAcum1; // F(n) = F(n-1) + F(n-2)
            fibAcum0 = fibAcum1; // Prepara F(n-2) para la siguiente iteración
            fibAcum1 = aux; // Prepara F(n-1) para la siguiente iteración
        }

        return fibAcum1; // Retorna el n-ésimo número de Fibonacci
    }

    public static void main(String[] args) {
        int n = 10; // Ejemplo: calcular el 10º número de Fibonacci
        System.out.println("El " + n + "º número de Fibonacci es: " + fibonacci(n));
    }
}
