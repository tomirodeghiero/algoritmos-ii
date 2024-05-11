import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class GraphColor {

    static class Graph {
        int vertices;
        LinkedList<Integer>[] adjacencyList;

        public Graph(int vertices) {
            this.vertices = vertices;
            adjacencyList = new LinkedList[vertices];
            for (int i = 0; i < vertices; i++) {
                adjacencyList[i] = new LinkedList<>();
            }
        }

        void addEdge(int src, int dest) {
            adjacencyList[src].add(dest);
            adjacencyList[dest].add(src); // For undirected graph
        }

        void colorGraph() {
            int[] result = new int[vertices]; // Store colors assigned to vertices
            Arrays.fill(result, -1); // No color is assigned initially
            result[0] = 0; // Assign the first color to the first vertex

            boolean[] available = new boolean[vertices]; // Colors available
            Arrays.fill(available, true);

            for (int u = 1; u < vertices; u++) {
                // Process all adjacent vertices and flag their colors as unavailable
                for (int i : adjacencyList[u]) {
                    if (result[i] != -1) { // If the color is assigned to vertex i
                        available[result[i]] = false;
                    }
                }

                // Find the first available color
                int color;
                for (color = 0; color < vertices; color++) {
                    if (available[color])
                        break;
                }

                result[u] = color; // Assign the found color

                // Reset the values back to true for the next iteration
                Arrays.fill(available, true);
            }

            // Print the result
            for (int u = 0; u < vertices; u++)
                System.out.println("Vertex " + u + " --->  Color " + result[u]);
        }
    }

    public static void main(String[] args) {
        Graph g1 = new Graph(5);
        g1.addEdge(0, 1);
        g1.addEdge(1, 2);
        g1.addEdge(2, 3);
        g1.addEdge(3, 4);
        g1.addEdge(4, 0);
        g1.addEdge(1, 4);
        g1.addEdge(1, 3);

        System.out.println("Coloring of graph 1");
        g1.colorGraph();
    }
}
