import java.util.HashMap;
import java.util.Map;

public class WineSelling {

    private static Map<String, Integer> cache = new HashMap<>();

    public static int memoizedSellWines(int[] prices) {
        // Inicializa el cache para cada subproblema con un valor indicativo de no
        // calculado
        return memoSellWines(prices, 0, prices.length - 1, 1);
    }

    private static int memoSellWines(int[] prices, int left, int right, int year) {
        if (left > right) {
            return 0;
        }

        // Creamos una clave única para el estado actual del problema
        String key = left + "," + right;

        // Revisamos si el resultado ya está en cache
        if (!cache.containsKey(key)) {
            // Calculamos la ganancia si vendemos el primer o último vino
            int sellLeft = memoSellWines(prices, left + 1, right, year + 1) + year * prices[left];
            int sellRight = memoSellWines(prices, left, right - 1, year + 1) + year * prices[right];

            // Almacenamos el mejor resultado en cache
            cache.put(key, Math.max(sellLeft, sellRight));
        }

        // Retornamos el resultado almacenado en cache
        return cache.get(key);
    }

    public static void main(String[] args) {
        int[] prices = { 2, 4, 6, 2, 5 }; // Ejemplo de precios de los vinos
        System.out.println("La ganancia máxima es: " + memoizedSellWines(prices));
    }
}