import java.util.*;

public class Knapsack {

    static class Item {
        int weight;
        int value;
        double ratio;

        public Item(int weight, int value) {
            this.weight = weight;
            this.value = value;
            this.ratio = (double) value / weight;
        }
    }

    public static int greedyKnapsack(List<Item> items, int capacity) {
        // Ordenar los ítems por su ratio valor/peso en orden descendente
        Collections.sort(items, (a, b) -> Double.compare(b.ratio, a.ratio));

        int totalValue = 0;
        int currentWeight = 0;

        for (Item item : items) {
            if (currentWeight + item.weight <= capacity) {
                // Añadir el ítem completo a la mochila
                totalValue += item.value;
                currentWeight += item.weight;
            } else {
                // Añadir una fracción del ítem si es necesario (esto no se aplicaría en 0/1
                // Knapsack)
                int weightAvailable = capacity - currentWeight;
                totalValue += item.ratio * weightAvailable; // Solo para ilustrar, no aplicable en 0/1
                break;
            }
        }

        return totalValue;
    }

    public static void main(String[] args) {
        List<Item> items = new ArrayList<>();
        items.add(new Item(2, 20)); // peso, valor
        items.add(new Item(5, 30));
        items.add(new Item(10, 50));
        items.add(new Item(5, 10));

        int capacity = 15; // Capacidad de la mochila
        System.out.println("Maximum value in the knapsack: " + greedyKnapsack(items, capacity));
    }
}
