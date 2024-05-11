import java.util.*;

public class PrimAlgorithm {
    private static class Edge {
        int src, dest, weight;

        Edge(int src, int dest, int weight) {
            this.src = src;
            this.dest = dest;
            this.weight = weight;
        }
    }

    public static List<Edge> primMST(List<List<Edge>> graph) {
        PriorityQueue<Edge> pq = new PriorityQueue<>(Comparator.comparingInt(e -> e.weight));
        List<Edge> result = new ArrayList<>();
        boolean[] inMST = new boolean[graph.size()];

        // Start from vertex 0
        inMST[0] = true;
        pq.addAll(graph.get(0));

        while (!pq.isEmpty()) {
            Edge edge = pq.poll();
            if (!inMST[edge.dest]) {
                inMST[edge.dest] = true;
                result.add(edge);
                for (Edge next : graph.get(edge.dest)) {
                    if (!inMST[next.dest]) {
                        pq.add(next);
                    }
                }
            }
        }
        return result;
    }

    public static void main(String[] args) {
        int V = 5; // Número de vértices
        List<List<PrimAlgorithm.Edge>> graph = new ArrayList<>();

        for (int i = 0; i < V; i++) {
            graph.add(new ArrayList<>());
        }

        // Añade arcos al grafo
        graph.get(0).add(new PrimAlgorithm.Edge(0, 1, 2));
        graph.get(1).add(new PrimAlgorithm.Edge(1, 0, 2));

        graph.get(1).add(new PrimAlgorithm.Edge(1, 2, 3));
        graph.get(2).add(new PrimAlgorithm.Edge(2, 1, 3));

        graph.get(0).add(new PrimAlgorithm.Edge(0, 3, 6));
        graph.get(3).add(new PrimAlgorithm.Edge(3, 0, 6));

        graph.get(1).add(new PrimAlgorithm.Edge(1, 3, 8));
        graph.get(3).add(new PrimAlgorithm.Edge(3, 1, 8));

        graph.get(1).add(new PrimAlgorithm.Edge(1, 4, 5));
        graph.get(4).add(new PrimAlgorithm.Edge(4, 1, 5));

        graph.get(2).add(new PrimAlgorithm.Edge(2, 4, 7));
        graph.get(4).add(new PrimAlgorithm.Edge(4, 2, 7));

        // Ejecuta el algoritmo de Prim
        List<PrimAlgorithm.Edge> mst = PrimAlgorithm.primMST(graph);
        for (PrimAlgorithm.Edge e : mst) {
            System.out.println("Arco (" + e.src + " - " + e.dest + ") con peso " + e.weight);
        }
    }
}
