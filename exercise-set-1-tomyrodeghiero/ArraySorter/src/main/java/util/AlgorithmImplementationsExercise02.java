package util;

import java.util.Arrays;

public class AlgorithmImplementationsExercise02 {
    // Merge Sort
    public static void mergeSort(int[] array) {
        if (array.length > 1) {
            int mid = array.length / 2;
            int[] left = new int[mid];
            int[] right = new int[array.length - mid];
            for (int i = 0; i < mid; i++) {
                left[i] = array[i];
            }
            for (int i = mid; i < array.length; i++) {
                right[i - mid] = array[i];
            }
            mergeSort(left);
            mergeSort(right);
            merge(array, left, right);
        }
    }

    public static void merge(int[] result, int[] left, int[] right) {
        int i = 0; // Índice para el array izquierdo
        int j = 0; // Índice para el array derecho
        int k = 0; // Índice para el array de resultado

        // Mezcla los elementos de left y right en result hasta que uno de los arrays se
        // acabe
        while (i < left.length && j < right.length) {
            if (left[i] <= right[j]) {
                result[k] = left[i];
                i++;
            } else {
                result[k] = right[j];
                j++;
            }
            k++;
        }

        // Copia los elementos restantes del array left
        while (i < left.length) {
            result[k] = left[i];
            i++;
            k++;
        }

        // Copia los elementos restantes del array right
        while (j < right.length) {
            result[k] = right[j];
            j++;
            k++;
        }
    }

    // Quick Sort
    public static void quickSort(int[] array, int low, int high) {
        if (low < high) {
            int pi = partition(array, low, high);
            quickSort(array, low, pi - 1);
            quickSort(array, pi + 1, high);
        }
    }

    public static int partition(int[] array, int low, int high) {
        int pivot = array[high];
        int i = low - 1;
        for (int j = low; j < high; j++) {
            if (array[j] < pivot) {
                i++;
                int temp = array[i];
                array[i] = array[j];
                array[j] = temp;
            }
        }
        int temp = array[i + 1];
        array[i + 1] = array[high];
        array[high] = temp;
        return i + 1;
    }

    // Dijsktra Algorithm
    public static void dijsktra(int[][] graph, int src) {
        int V = graph.length;
        int[] dist = new int[V];
        boolean[] sptSet = new boolean[V];
        for (int i = 0; i < V; i++) {
            dist[i] = Integer.MAX_VALUE;
            sptSet[i] = false;
        }
        dist[src] = 0;
        for (int count = 0; count < V - 1; count++) {
            int u = minDistance(dist, sptSet);
            sptSet[u] = true;
            for (int v = 0; v < V; v++) {
                if (!sptSet[v] && graph[u][v] != 0 && dist[u] != Integer.MAX_VALUE && dist[u] + graph[u][v] < dist[v]) {
                    dist[v] = dist[u] + graph[u][v];
                }
            }
        }
        printSolution(dist, V);
    }

    public static int minDistance(int[] dist, boolean[] sptSet) {
        int min = Integer.MAX_VALUE;
        int minIndex = -1;
        for (int v = 0; v < dist.length; v++) {
            if (!sptSet[v] && dist[v] <= min) {
                min = dist[v];
                minIndex = v;
            }
        }
        return minIndex;
    }

    public static void printSolution(int[] dist, int V) {
        System.out.println("Vertex \t Distance from Source");
        for (int i = 0; i < V; i++) {
            System.out.println(i + " \t " + dist[i]);
        }
    }

    public static void floydWarshall(int[][] graph) {
        int V = graph.length;
        int[][] dist = new int[V][V];
        for (int i = 0; i < V; i++) {
            for (int j = 0; j < V; j++) {
                dist[i][j] = graph[i][j];
            }
        }
        for (int k = 0; k < V; k++) {
            for (int i = 0; i < V; i++) {
                for (int j = 0; j < V; j++) {
                    if (dist[i][k] != Integer.MAX_VALUE && dist[k][j] != Integer.MAX_VALUE
                            && dist[i][k] + dist[k][j] < dist[i][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j];
                    }
                }
            }
        }
        printSolution(dist);
    }

    public static void printSolution(int[][] dist) {
        int V = dist.length;
        System.out.println("The following matrix shows the shortest distances between every pair of vertices");
        for (int i = 0; i < V; i++) {
            for (int j = 0; j < V; j++) {
                if (dist[i][j] == Integer.MAX_VALUE) {
                    System.out.print("INF ");
                } else {
                    System.out.print(dist[i][j] + " ");
                }
            }
            System.out.println();
        }
    }

    // Kraskal Algorithm
    public static void kruskal(int[][] edges, int V) {
        // Ordena las aristas por su peso de menor a mayor
        java.util.Arrays.sort(edges, (a, b) -> a[2] - b[2]);

        // Inicializa los conjuntos para la unión-búsqueda
        int[] parent = new int[V];
        int[] rank = new int[V];
        for (int i = 0; i < V; ++i) {
            parent[i] = i;
            rank[i] = 0;
        }

        int e = 0; // Contador para las aristas incluidas en el MST
        int i = 0; // Índice para iterar sobre las aristas ordenadas

        while (e < V - 1) {
            int[] nextEdge = edges[i++];
            int x = find(parent, nextEdge[0]);
            int y = find(parent, nextEdge[1]);

            if (x != y) {
                // Aquí se podría añadir la arista al MST, pero en este algoritmo sólo la
                // imprimiremos
                System.out.println(nextEdge[0] + " - " + nextEdge[1] + ": " + nextEdge[2]);
                union(parent, rank, x, y);
                e++;
            }
        }
    }

    // Método para encontrar el conjunto de un elemento
    public static int find(int[] parent, int i) {
        if (parent[i] != i) {
            parent[i] = find(parent, parent[i]);
        }
        return parent[i];
    }

    // Método para unir dos conjuntos
    public static void union(int[] parent, int[] rank, int x, int y) {
        int xRoot = find(parent, x);
        int yRoot = find(parent, y);

        if (rank[xRoot] < rank[yRoot]) {
            parent[xRoot] = yRoot;
        } else if (rank[yRoot] < rank[xRoot]) {
            parent[yRoot] = xRoot;
        } else {
            parent[yRoot] = xRoot;
            rank[xRoot]++;
        }
    }

    // Binary Search Algorithm
    public static int binarySearch(int[] array, int value) {
        int low = 0;
        int high = array.length - 1;
        while (low <= high) {
            int mid = low + (high - low) / 2;
            if (array[mid] == value) {
                return mid;
            }
            if (array[mid] < value) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        return -1;
    }

    // Euclides Algorithm
    public static int gcd(int a, int b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }

    // Breadth First Search
    public static void breadthFirstSearch(int[][] graph, int start) {
        int V = graph.length;
        boolean[] visited = new boolean[V];
        java.util.LinkedList<Integer> queue = new java.util.LinkedList<Integer>();
        visited[start] = true;
        queue.add(start);
        while (queue.size() != 0) {
            start = queue.poll();
            System.out.print(start + " ");
            for (int i = 0; i < V; i++) {
                if (graph[start][i] == 1 && !visited[i]) {
                    visited[i] = true;
                    queue.add(i);
                }
            }
        }
    }

    // Sieve of Eratosthenes
    public static boolean[] sieveOfEratosthenes(int n) {
        boolean[] prime = new boolean[n + 1];
        for (int i = 0; i < n; i++) {
            prime[i] = true;
        }
        for (int p = 2; p * p <= n; p++) {
            if (prime[p] == true) {
                for (int i = p * p; i <= n; i += p) {
                    prime[i] = false;
                }
            }
        }
        return prime;
    }

    public static void main(String[] args) {
        // Prueba de Merge Sort
        int[] arrayToSort = { 5, 1, 6, 2, 3, 4 };
        System.out.println("Original array: " + Arrays.toString(arrayToSort));
        mergeSort(arrayToSort);
        System.out.println("Sorted array with Merge Sort: " + Arrays.toString(arrayToSort));

        // Prueba de Quick Sort
        int[] arrayToQuickSort = { 10, 7, 8, 9, 1, 5 };
        System.out.println("Original array: " + Arrays.toString(arrayToQuickSort));
        quickSort(arrayToQuickSort, 0, arrayToQuickSort.length - 1);
        System.out.println("Sorted array with Quick Sort: " + Arrays.toString(arrayToQuickSort));

        // Prueba de Dijkstra
        int[][] graph = {
                { 0, 4, 0, 0, 0 },
                { 4, 0, 8, 0, 0 },
                { 0, 8, 0, 7, 0 },
                { 0, 0, 7, 0, 9 },
                { 0, 0, 0, 9, 0 }
        };
        System.out.println("Dijkstra: ");
        dijsktra(graph, 0);

        // Prueba de Floyd Warshall
        int[][] graphForFloyd = {
                { 0, 5, INF, 10 },
                { INF, 0, 3, INF },
                { INF, INF, 0, 1 },
                { INF, INF, INF, 0 }
        };
        System.out.println("Floyd Warshall:");
        floydWarshall(graphForFloyd);

        // Prueba de Kruskal
        int[][] edges = {
                { 0, 1, 10 },
                { 0, 2, 6 },
                { 0, 3, 5 },
                { 1, 3, 15 },
                { 2, 3, 4 }
        };
        System.out.println("Kruskal: ");
        kruskal(edges, 4);

        // Prueba de Búsqueda Binaria
        int[] sortedArrayForBinarySearch = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        int index = binarySearch(sortedArrayForBinarySearch, 4);
        System.out.println("Index of 4 in the array: " + index);

        // Prueba de Euclides
        int gcdResult = gcd(54, 24);
        System.out.println("GCD of 54 and 24: " + gcdResult);

        // Prueba de BFS
        int[][] bfsGraph = {
                { 0, 1, 1, 0 },
                { 0, 0, 1, 0 },
                { 1, 0, 0, 1 },
                { 0, 0, 0, 1 }
        };
        System.out.print("BFS starting from vertex 2: ");
        breadthFirstSearch(bfsGraph, 2);

        // Prueba de Criba de Eratóstenes
        boolean[] primes = sieveOfEratosthenes(30);
        System.out.println("\nPrimes up to 30: ");
        for (int i = 2; i < primes.length; i++) {
            if (primes[i]) {
                System.out.print(i + " ");
            }
        }
        System.out.println();
    }

    // Constante para representar infinito en el algoritmo de Floyd Warshall
    public static final int INF = Integer.MAX_VALUE;
}