public class OrdenOptimalSequenceNMatrix {
    // Función recursiva para calcular el costo mínimo de multiplicación de matrices
    public static int minCostDC(int[] p, int i, int j) {
        if (i == j) {
            return 0;
        }

        int min = Integer.MAX_VALUE;

        // Divide el problema entre i y k, y entre k+1 y j
        for (int k = i; k < j; k++) {
            int count = minCostDC(p, i, k) + minCostDC(p, k + 1, j) + p[i - 1] * p[k] * p[j];
            if (count < min) {
                min = count;
            }
        }
        return min;
    }

    // Función con utilización de Programación Dinámica para calcular el costo
    // mínimo de multiplicación de matrices
    public static int minCostDP(int[] p, int n) {
        // m[i][j] representa el mínimo número de multiplicaciones necesarias para
        // multiplicar cadena de matrices de i a j
        int[][] m = new int[n][n]; // Uso de programación dinámica con n para simplificar el acceso a m

        // Llenando la diagonal principal con 0 ya que el costo de multiplicar una
        // matriz por sí misma es 0
        for (int i = 0; i < n; i++) {
            m[i][i] = 0;
        }

        // l es la longitud de la cadena considerada
        for (int l = 2; l < n; l++) { // l=2 porque la longitud mínima de la cadena es 2
            for (int i = 1; i < n - l + 1; i++) {
                int j = i + l - 1;
                m[i][j] = Integer.MAX_VALUE;
                for (int k = i; k <= j - 1; k++) {
                    // q es el costo/scalar de multiplicaciones
                    int q = m[i][k] + m[k + 1][j] + p[i - 1] * p[k] * p[j];
                    if (q < m[i][j]) {
                        m[i][j] = q;
                    }
                }
            }
        }

        // Se resta 1 porque el arreglo p tiene un elemento más que la cantidad de
        // matrices, corregido para reflejar la indexación correcta y condiciones de
        // border, de modo tal que retorne el costo mínimo de multiplicar todas las
        // matrices
        return m[1][n - 1];
    }

    public static void main(String[] args) {
        int[] p = { 1, 2, 3, 4 };
        int n = p.length;
        System.out.println("Minimum number of multiplications is with DC: " + minCostDC(p, 1, n - 1));
        System.out.println("Minimum number of multiplications is with DP: " + minCostDP(p, n));
    }
}