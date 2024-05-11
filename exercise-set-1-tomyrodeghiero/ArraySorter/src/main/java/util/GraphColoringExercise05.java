package util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GraphColoringExercise05 {
    private int V; // Número de vértices
    private List<List<Integer>> adj; // Lista de adyacencia

    // Constructor
    public GraphColoringExercise05(int V) {
        this.V = V;
        adj = new ArrayList<>(V);
        for (int i = 0; i < V; i++) {
            adj.add(new ArrayList<>());
        }
    }

    // Método para añadir una arista
    public void addEdge(int v, int w) {
        adj.get(v).add(w);
        adj.get(w).add(v); // Grafo no dirigido
    }

    // Método para colorear el grafo
    public int[] colorGraph() {
        int[] result = new int[V];
        Arrays.fill(result, -1); // Inicializar todos los vértices como no asignados
        result[0] = 0; // Asignar el primer color al primer vértice

        // Asignar colores a los vértices restantes
        for (int u = 1; u < V; u++) {
            // Verificar colores disponibles según los vértices adyacentes
            boolean[] available = new boolean[V];
            Arrays.fill(available, true);

            // Recorrer todos los nodos adyacentes y marcar sus colores como no disponibles
            for (int i : adj.get(u)) {
                if (result[i] != -1) {
                    available[result[i]] = false;
                }
            }

            // Encontrar el primer color disponible
            int color;
            for (color = 0; color < V; color++) {
                if (available[color]) {
                    break;
                }
            }

            // Asignar el primer color disponible
            result[u] = color;
        }
        return result;
    }

    public static void main(String[] args) {
        GraphColoringExercise05 g = new GraphColoringExercise05(6);
        g.addEdge(0, 1);
        g.addEdge(1, 2);
        g.addEdge(2, 3);
        g.addEdge(3, 4);
        g.addEdge(4, 0);
        g.addEdge(0, 2);
        g.addEdge(3, 5);

        int[] coloringResult = g.colorGraph();
        System.out.println("Coloreo del grafo:");
        for (int i = 0; i < coloringResult.length; i++) {
            System.out.println("Vértice " + i + " --->  Color " + coloringResult[i]);
        }
    }
}
