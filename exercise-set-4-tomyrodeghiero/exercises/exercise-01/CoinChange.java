import java.util.ArrayList;
import java.util.List;

public class CoinChange {
    static class Coin {
        int denomination;
        int count;

        Coin(int denomination, int count) {
            this.denomination = denomination;
            this.count = count;
        }
    }

    public static List<Coin> getMinimumCoins(int[] denominations, int[] maxCoins, int amount) {
        List<Coin> result = new ArrayList<>();
        for (int i = 0; i < denominations.length; i++) {
            if (amount == 0) {
                break;
            }
            int coinValue = denominations[i];
            int maxUse = Math.min(amount / coinValue, maxCoins[i]);
            if (maxUse > 0) {
                result.add(new Coin(coinValue, maxUse));
                amount -= coinValue * maxUse;
            }
        }

        // Verifica si se puede dar el cambio exacto
        if (amount > 0) {
            result.clear(); // Si no se puede dar el cambio exacto, devuelve lista vacía
            result.add(new Coin(-1, -1)); // Indicador de fallo
        }

        return result;
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
