package util;

import java.util.*;

public class AlgorithmImplementationsExercise03 {
    // Método para ordenar una secuencia usando Merge Sort
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

    // Método para buscar un elemento en un conjunto con búsqueda binaria
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

    // Método para calcular a^n con exponenciación rápida
    public static long fastExponentiation(long a, long n) {
        long result = 1;
        while (n > 0) {
            if (n % 2 == 1) {
                result *= a;
            }
            a *= a;
            n /= 2;
        }
        return result;
    }

    public static double[] gaussianElimination(double[][] matrix) {
        int N = matrix.length;
        double[] result = new double[N];

        // Conversión de la matriz a la forma escalonada
        for (int i = 0; i < N; i++) {
            // Normaliza la fila actual
            double factor = matrix[i][i];
            for (int k = i; k <= N; k++) {
                matrix[i][k] /= factor;
            }

            // Elimina la i-ésima variable de las filas posteriores
            for (int j = i + 1; j < N; j++) {
                factor = matrix[j][i];
                for (int k = i; k <= N; k++) {
                    matrix[j][k] -= factor * matrix[i][k];
                }
            }
        }

        // Resolución del sistema de ecuaciones de la forma escalonada
        for (int i = N - 1; i >= 0; i--) {
            result[i] = matrix[i][N];
            for (int j = i + 1; j < N; j++) {
                result[i] -= matrix[i][j] * result[j];
            }
        }

        return result;
    }

    // Método para encontrar el par de elementos más cercanos
    public static double closestPair(double[][] points) {
        double minDistance = Double.MAX_VALUE;
        for (int i = 0; i < points.length; i++) {
            for (int j = i + 1; j < points.length; j++) {
                double distance = Math.sqrt(Math.pow(points[i][0] - points[j][0], 2) +
                        Math.pow(points[i][1] - points[j][1], 2));
                if (distance < minDistance) {
                    minDistance = distance;
                }
            }
        }
        return minDistance;
    }

    // Método para la composición relacional (multiplicación de matrices)
    public static int[][] relationalComposition(int[][] matrixA, int[][] matrixB) {
        int rowsA = matrixA.length;
        int colsA = matrixA[0].length;
        int colsB = matrixB[0].length;
        int[][] result = new int[rowsA][colsB];

        for (int i = 0; i < rowsA; i++) {
            for (int j = 0; j < colsB; j++) {
                for (int k = 0; k < colsA; k++) {
                    result[i][j] += matrixA[i][k] * matrixB[k][j];
                }
            }
        }

        return result;
    }

    // Método para el ordenamiento topológico
    public static List<Integer> topologicalSort(int V, List<List<Integer>> adj) {
        int[] indegree = new int[V];
        for (List<Integer> edges : adj) {
            for (int edge : edges) {
                indegree[edge]++;
            }
        }

        Queue<Integer> queue = new LinkedList<>();
        for (int i = 0; i < V; i++) {
            if (indegree[i] == 0) {
                queue.add(i);
            }
        }

        List<Integer> order = new ArrayList<>();
        while (!queue.isEmpty()) {
            int current = queue.poll();
            order.add(current);

            for (int neighbor : adj.get(current)) {
                indegree[neighbor]--;
                if (indegree[neighbor] == 0) {
                    queue.add(neighbor);
                }
            }
        }

        if (order.size() != V) {
            // Hay un ciclo, por lo que no es posible un ordenamiento topológico
            return null;
        }

        return order;
    }

    // Método para determinar la existencia de ciclos eulerianos
    public static boolean hasEulerianCycle(int V, List<List<Integer>> adj) {
        // Comprobar que todos los vértices tienen un grado par
        for (List<Integer> edges : adj) {
            if ((edges.size() & 1) == 1) { // Uso de AND bit a bit para comprobar si es impar
                return false;
            }
        }

        // Comprobar la conectividad del grafo
        boolean[] visited = new boolean[V];
        int i;
        for (i = 0; i < V; i++) {
            if (adj.get(i).size() != 0)
                break;
        }

        // Si no hay aristas en el grafo, es euleriano
        if (i == V) {
            return true;
        }

        // Empezar desde un vértice con un grado no cero y hacer DFS
        dfs(i, visited, adj);

        // Comprobar si todos los vértices no cero fueron visitados
        for (i = 0; i < V; i++) {
            if (!visited[i] && adj.get(i).size() > 0) {
                return false;
            }
        }

        return true;
    }

    private static void dfs(int v, boolean[] visited, List<List<Integer>> adj) {
        visited[v] = true;
        for (int next : adj.get(v)) {
            if (!visited[next]) {
                dfs(next, visited, adj);
            }
        }
    }

    public static void main(String[] args) {
        // Prueba de Merge Sort
        int[] arrayToSort = { 3, 1, 4, 1, 5, 9, 2, 6, 5 };
        System.out.println("Original array: " + Arrays.toString(arrayToSort));
        mergeSort(arrayToSort);
        System.out.println("Sorted array: " + Arrays.toString(arrayToSort));

        // Prueba de Búsqueda Binaria
        int[] sortedArrayForBinarySearch = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        int index = binarySearch(sortedArrayForBinarySearch, 4);
        System.out.println("Index of 4 in the array: " + index);

        // Prueba de Exponenciación Rápida
        long base = 2;
        long exponent = 10;
        long resultExp = fastExponentiation(base, exponent);
        System.out.println(base + " raised to the power of " + exponent + " is: " + resultExp);

        // Prueba de Eliminación Gaussiana
        double[][] gaussMatrix = {
                { 2, 1, -1, 8 },
                { -3, -1, 2, -11 },
                { -2, 1, 2, -3 }
        };
        double[] gaussResult = gaussianElimination(gaussMatrix);
        System.out.println("Solution of the system: " + Arrays.toString(gaussResult));

        // Prueba del par más cercano
        double[][] points = { { 1, 2 }, { 4, 6 }, { 5, 1 }, { 1, 1 } };
        double closestPairDistance = closestPair(points);
        System.out.println("Closest pair distance: " + closestPairDistance);

        // Prueba de la Composición Relacional (Multiplicación de Matrices)
        int[][] matrixA = { { 1, 2 }, { 3, 4 } };
        int[][] matrixB = { { 5, 6 }, { 7, 8 } };
        int[][] compositionResult = relationalComposition(matrixA, matrixB);
        System.out.println("Relational composition (Matrix multiplication):");
        for (int[] row : compositionResult) {
            System.out.println(Arrays.toString(row));
        }

        // Prueba de Ordenamiento Topológico
        List<List<Integer>> adjList = new ArrayList<>();
        for (int i = 0; i < 6; i++) {
            adjList.add(new ArrayList<>());
        }
        adjList.get(5).add(2);
        adjList.get(5).add(0);
        adjList.get(4).add(0);
        adjList.get(4).add(1);
        adjList.get(2).add(3);
        adjList.get(3).add(1);
        List<Integer> topologicalOrder = topologicalSort(6, adjList);
        System.out.println("Topological Sorting: " + topologicalOrder);

        // Prueba de Ciclos Eulerianos
        List<List<Integer>> eulerAdjList = new ArrayList<>();

        // Inicializar listas para cada vértice
        for (int i = 0; i < 4; i++) {
            eulerAdjList.add(new ArrayList<>());
        }

        // Añadir aristas para formar el ciclo euleriano
        eulerAdjList.get(0).add(1);
        eulerAdjList.get(1).add(0);
        eulerAdjList.get(1).add(3);
        eulerAdjList.get(3).add(1);
        eulerAdjList.get(3).add(2);
        eulerAdjList.get(2).add(3);
        eulerAdjList.get(2).add(0);
        eulerAdjList.get(0).add(2);

        boolean hasEulerCycle = hasEulerianCycle(4, eulerAdjList);
        System.out.println("The graph has a Eulerian cycle: " + hasEulerCycle);
    }
}
