import java.util.*;

public class Knapsack {

    // Clase interna estática para representar un ítem con peso, valor y ratio valor/peso
    static class Item {
        int weight;
        int value;
        double ratio;

        // Constructor para inicializar el peso, valor y ratio valor/peso del ítem
        public Item(int weight, int value) {
            this.weight = weight;
            this.value = value;
            this.ratio = (double) value / weight;
        }
    }

    /**
     * Método para resolver el problema de la mochila usando un enfoque Greedy.
     *
     * @param items Lista de ítems disponibles.
     * @param capacity Capacidad de la mochila.
     * @return El valor máximo que se puede obtener.
     */
    public static int greedyKnapsack(List<Item> items, int capacity) {
        // Ordenar los ítems por su ratio valor/peso en orden descendente
        Collections.sort(items, (a, b) -> Double.compare(b.ratio, a.ratio));

        int totalValue = 0; // Valor total acumulado en la mochila
        int currentWeight = 0; // Peso actual en la mochila

        // Iterar sobre los ítems ordenados
        for (Item item : items) {
            if (currentWeight + item.weight <= capacity) {
                // Añadir el ítem completo a la mochila
                totalValue += item.value;
                currentWeight += item.weight;
            }
        }

        return totalValue; // Devolver el valor máximo obtenido
    }

    public static void main(String[] args) {
        List<Item> items = new ArrayList<>();
        // Añadir ítems a la lista (peso, valor)
        items.add(new Item(2, 20));
        items.add(new Item(5, 30));
        items.add(new Item(10, 50));
        items.add(new Item(5, 10));

        int capacity = 15; // Capacidad de la mochila
        System.out.println("Máximo valor en la mochila: " + greedyKnapsack(items, capacity));
    }
}
