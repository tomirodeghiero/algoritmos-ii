public class WineSelling {
    private int[] prices; // Array para almacenar los precios de los vinos

    // Constructor para inicializar el array de precios
    public WineSelling(int[] prices) {
        this.prices = prices;
    }

    /**
     * Método para calcular la máxima ganancia usando un enfoque Greedy.
     * @return La máxima ganancia posible.
     */
    public int maxProfitGreedy() {
        int n = prices.length; // Número de vinos
        int start = 0; // Índice del vino en el extremo izquierdo
        int end = n - 1; // Índice del vino en el extremo derecho
        int year = 1; // Año actual, empieza en 1
        int totalProfit = 0; // Ganancia total acumulada

        // Bucle mientras haya vinos disponibles
        while (start <= end) {
            int profitStart = year * prices[start]; // Ganancia si se vende el vino en el extremo izquierdo
            int profitEnd = year * prices[end]; // Ganancia si se vende el vino en el extremo derecho
            if (profitStart > profitEnd) { // Si la ganancia del extremo izquierdo es mayor
                totalProfit += profitStart; // Añadir la ganancia a la ganancia total
                start++; // Mover el índice de inicio hacia la derecha
            } else { // Si la ganancia del extremo derecho es mayor o igual
                totalProfit += profitEnd; // Añadir la ganancia a la ganancia total
                end--; // Mover el índice de fin hacia la izquierda
            }
            year++; // Incrementar el año
        }

        return totalProfit; // Retornar la ganancia total calculada
    }

    public static void main(String[] args) {
        int[] prices = { 1, 4, 1, 5 }; // Array de precios de los vinos
        WineSelling wp = new WineSelling(prices); // Crear una instancia de WineSelling
        System.out.println("Maximum profit (Greedy): " + wp.maxProfitGreedy()); // Calcular y mostrar la ganancia máxima
    }
}
