import java.util.Arrays;
import java.util.LinkedList;

public class GraphColor {

    // Clase interna estática para representar el grafo
    static class Graph {
        int vertices; // Número de vértices en el grafo
        LinkedList<Integer>[] adjacencyList; // Lista de adyacencia para representar el grafo

        // Constructor para inicializar el grafo con un número de vértices dado
        public Graph(int vertices) {
            this.vertices = vertices;
            adjacencyList = new LinkedList[vertices];
            for (int i = 0; i < vertices; i++) {
                adjacencyList[i] = new LinkedList<>();
            }
        }

        // Método para añadir una arista al grafo
        void addEdge(int src, int dest) {
            adjacencyList[src].add(dest);
            adjacencyList[dest].add(src); // Para grafo no dirigido
        }

        // Método para colorear el grafo usando un enfoque Greedy
        void colorGraph() {
            int[] result = new int[vertices]; // Array para almacenar los colores asignados a los vértices
            Arrays.fill(result, -1); // Inicialmente, ningún vértice tiene color asignado
            result[0] = 0; // Asignar el primer color al primer vértice

            boolean[] available = new boolean[vertices]; // Array para marcar los colores disponibles
            Arrays.fill(available, true);

            // Iterar sobre todos los vértices para asignarles un color
            for (int u = 1; u < vertices; u++) {
                // Procesar todos los vértices adyacentes y marcar sus colores como no disponibles
                for (int i : adjacencyList[u]) {
                    if (result[i] != -1) { // Si el vértice i ya tiene un color asignado
                        available[result[i]] = false; // Marcar ese color como no disponible
                    }
                }

                // Encontrar el primer color disponible
                int color;
                for (color = 0; color < vertices; color++) {
                    if (available[color]) {
                        break; // Encontrar el primer color disponible
                    }
                }

                result[u] = color; // Asignar el color encontrado al vértice u

                // Resetear los valores a true para la siguiente iteración
                Arrays.fill(available, true);
            }

            // Imprimir el resultado
            for (int u = 0; u < vertices; u++) {
                System.out.println("Vertex " + u + " --->  Color " + result[u]);
            }
        }
    }

    // Método principal para ejecutar el programa
    public static void main(String[] args) {
        Graph g1 = new Graph(5); // Crear un grafo con 5 vértices
        g1.addEdge(0, 1);
        g1.addEdge(1, 2);
        g1.addEdge(2, 3);
        g1.addEdge(3, 4);
        g1.addEdge(4, 0);
        g1.addEdge(1, 4);
        g1.addEdge(1, 3);

        System.out.println("Coloring of graph 1");
        g1.colorGraph(); // Colorear el grafo
    }
}
