import java.util.Arrays;
import java.util.Comparator;

class Item {
    int valor, peso;

    Item(int valor, int peso) {
        this.valor = valor;
        this.peso = peso;
    }

    // Método para calcular la relación valor/peso
    double getRatio() {
        return (double) valor / peso;
    }
}

public class MochilaGreedy {
    static double mochilaGreedy(Item[] items, int capacidad) {
        // Ordenar los items por la relación valor/peso en orden descendente
        Arrays.sort(items, new Comparator<Item>() {
            public int compare(Item a, Item b) {
                return Double.compare(b.getRatio(), a.getRatio());
            }
        });

        double valorTotal = 0;
        int pesoActual = 0;

        for (Item item : items) {
            if (pesoActual + item.peso <= capacidad) {
                pesoActual += item.peso;
                valorTotal += item.valor;
            }
        }

        return valorTotal;
    }

    public static void main(String[] args) {
        Item[] items = {
            new Item(60, 10),
            new Item(100, 20),
            new Item(120, 30)
        };
        int capacidad = 50;

        double valorMaximo = mochilaGreedy(items, capacidad);
        System.out.println("Valor máximo en la mochila = " + valorMaximo);
    }
}
