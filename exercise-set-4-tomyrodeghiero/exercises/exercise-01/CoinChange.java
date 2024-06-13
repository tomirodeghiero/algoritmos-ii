import java.util.ArrayList;
import java.util.List;

public class CoinChange {
    // Clase interna para representar una moneda con su denominación y cantidad
    static class Coin {
        int denomination; // Valor de la moneda
        int count; // Cantidad de monedas

        Coin(int denomination, int count) {
            this.denomination = denomination;
            this.count = count;
        }
    }

    /**
     * Método para obtener la lista mínima de monedas necesarias para un monto dado.
     *
     * @param denominations Array con las denominaciones de las monedas (de mayor a menor valor).
     * @param maxCoins Array con el número máximo de monedas disponibles para cada denominación.
     * @param amount El monto del cambio a devolver.
     * @return Lista de monedas usadas para el cambio, o una lista con un Coin(-1, -1) si no es posible dar el cambio exacto.
     */
    public static List<Coin> getMinimumCoins(int[] denominations, int[] maxCoins, int amount) {
        List<Coin> result = new ArrayList<>(); // Lista para almacenar las monedas utilizadas
        
        // Recorre cada denominación de moneda
        for (int i = 0; i < denominations.length; i++) {
            if (amount == 0) { // Si el monto restante es 0, termina el bucle
                break;
            }
            int coinValue = denominations[i]; // Valor de la moneda actual
            // Máximo número de monedas de esta denominación que se pueden usar sin exceder el monto restante
            int maxUse = Math.min(amount / coinValue, maxCoins[i]);
            if (maxUse > 0) { // Si se pueden usar monedas de esta denominación
                result.add(new Coin(coinValue, maxUse)); // Añade las monedas a la lista de resultados
                amount -= coinValue * maxUse; // Reduce el monto restante
            }
        }

        // Verifica si se puede dar el cambio exacto
        if (amount > 0) { // Si aún queda monto sin cubrir
            result.clear(); // Limpia la lista de resultados para indicar que no se puede dar el cambio exacto
            result.add(new Coin(-1, -1)); // Añade un indicador de fallo
        }

        return result; // Retorna la lista de monedas usadas, o el indicador de fallo
    }

    public static void main(String[] args) {
        int[] denominations = { 25, 10, 5, 1 }; // Valores de las monedas
        int[] maxCoins = { 4, 5, 10, 100 }; // Máximo de cada moneda disponible
        int amount = 63; // Monto del cambio a devolver

        List<Coin> change = getMinimumCoins(denominations, maxCoins, amount);
        if (change.isEmpty() || change.get(0).denomination == -1) {
            System.out.println("No es posible dar el cambio exacto con las monedas disponibles.");
        } else {
            for (Coin coin : change) {
                System.out.println("Usar " + coin.count + " monedas de $" + coin.denomination);
            }
        }
    }
}
