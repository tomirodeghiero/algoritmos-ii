import java.util.*;

public class KruskalAlgorithm {
    private static class Edge {
        int src, dest, weight;

        Edge(int src, int dest, int weight) {
            this.src = src;
            this.dest = dest;
            this.weight = weight;
        }
    }

    private static class UnionFind {
        private int[] parent, rank;

        public UnionFind(int n) {
            parent = new int[n];
            rank = new int[n];
            for (int i = 0; i < n; i++) {
                parent[i] = i;
            }
        }

        public int find(int x) {
            if (parent[x] != x) {
                parent[x] = find(parent[x]); // Path compression
            }
            return parent[x];
        }

        public void union(int x, int y) {
            int rootX = find(x);
            int rootY = find(y);
            if (rootX != rootY) {
                if (rank[rootX] > rank[rootY]) {
                    parent[rootY] = rootX;
                } else if (rank[rootX] < rank[rootY]) {
                    parent[rootX] = rootY;
                } else {
                    parent[rootY] = rootX;
                    rank[rootX]++;
                }
            }
        }
    }

    public static List<Edge> kruskalMST(List<Edge> edges, int V) {
        Collections.sort(edges, Comparator.comparingInt(e -> e.weight));
        UnionFind uf = new UnionFind(V);
        List<Edge> result = new ArrayList<>();

        for (Edge edge : edges) {
            if (uf.find(edge.src) != uf.find(edge.dest)) {
                uf.union(edge.src, edge.dest);
                result.add(edge);
            }
        }
        return result;
    }

    public static void main(String[] args) {
        int V = 4; // Número de vértices
        List<KruskalAlgorithm.Edge> edges = new ArrayList<>();

        // Añade arcos al grafo
        edges.add(new KruskalAlgorithm.Edge(0, 1, 10));
        edges.add(new KruskalAlgorithm.Edge(0, 2, 6));
        edges.add(new KruskalAlgorithm.Edge(0, 3, 5));
        edges.add(new KruskalAlgorithm.Edge(1, 3, 15));
        edges.add(new KruskalAlgorithm.Edge(2, 3, 4));

        // Ejecuta el algoritmo de Kruskal
        List<KruskalAlgorithm.Edge> mst = KruskalAlgorithm.kruskalMST(edges, V);
        for (KruskalAlgorithm.Edge e : mst) {
            System.out.println("Arco (" + e.src + " - " + e.dest + ") con peso " + e.weight);
        }
    }
}
