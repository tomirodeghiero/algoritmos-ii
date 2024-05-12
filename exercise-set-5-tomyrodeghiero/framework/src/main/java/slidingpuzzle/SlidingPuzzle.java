package slidingpuzzle;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

public class SlidingPuzzle {
    private static final int SIZE = 4; // Dimensiones del tablero 4x4

    static class Estado {
        int[][] tablero; // Tablero 4x4
        Estado parent; // Para rastrear el camino de la solución

        public Estado(int[][] tablero, Estado parent) {
            this.tablero = new int[SIZE][SIZE];
            for (int i = 0; i < SIZE; i++) {
                System.arraycopy(tablero[i], 0, this.tablero[i], 0, SIZE);
            }
            this.parent = parent;
        }

        // Método para encontrar la posición del espacio vacío (0)
        int[] findEmpty() {
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (tablero[i][j] == 0) {
                        return new int[] { i, j };
                    }
                }
            }
            return new int[] { -1, -1 }; // No debería ocurrir
        }

        // Método para copiar el estado
        Estado copy() {
            return new Estado(this.tablero, this);
        }

        // Método para generar un nuevo estado moviendo una ficha
        Estado move(int x, int y, int newX, int newY) {
            Estado nuevoEstado = this.copy();
            nuevoEstado.tablero[x][y] = this.tablero[newX][newY];
            nuevoEstado.tablero[newX][newY] = 0;
            return nuevoEstado;
        }

        // Compara tableros para igualdad
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null || getClass() != obj.getClass())
                return false;
            Estado estado = (Estado) obj;
            return Arrays.deepEquals(tablero, estado.tablero);
        }

        @Override
        public int hashCode() {
            return Arrays.deepHashCode(tablero);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            for (int[] row : tablero) {
                sb.append(Arrays.toString(row)).append("\n");
            }
            return sb.toString();
        }
    }

    // Genera todos los sucesores de un estado dado
    public static List<Estado> sucesores(Estado s) {
        List<Estado> sucesores = new ArrayList<>();
        int[] empty = s.findEmpty();
        int emptyX = empty[0], emptyY = empty[1];

        // Intenta mover las fichas adyacentes hacia el espacio vacío
        int[][] directions = { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } }; // Abajo, Arriba, Derecha, Izquierda
        for (int[] dir : directions) {
            int newX = emptyX + dir[0], newY = emptyY + dir[1];
            if (newX >= 0 && newX < SIZE && newY >= 0 && newY < SIZE) {
                sucesores.add(s.move(emptyX, emptyY, newX, newY));
            }
        }
        return sucesores;
    }

    // BFS para encontrar la solución
    public static void findSolution(Estado initialState) {
        Queue<Estado> queue = new LinkedList<>();
        Set<Estado> visited = new HashSet<>();
        queue.add(initialState);
        visited.add(initialState);

        while (!queue.isEmpty()) {
            Estado current = queue.poll();
            if (isSolved(current)) {
                printSolution(current);
                return;
            }
            for (Estado next : sucesores(current)) {
                if (!visited.contains(next)) {
                    queue.add(next);
                    visited.add(next);
                }
            }
        }
        System.out.println("No solution found.");
    }

    // Verifica si el estado es la solución
    public static boolean isSolved(Estado state) {
        int num = 1;
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                if (i == SIZE - 1 && j == SIZE - 1) {
                    if (state.tablero[i][j] != 0)
                        return false;
                } else {
                    if (state.tablero[i][j] != num++)
                        return false;
                }
            }
        }
        return true;
    }

    // Imprime la solución desde el estado inicial al final
    public static void printSolution(Estado state) {
        List<Estado> path = new ArrayList<>();
        for (Estado s = state; s != null; s = s.parent) {
            path.add(s);
        }
        Collections.reverse(path);
        int step = 0;
        for (Estado s : path) {
            System.out.println("Paso " + step++ + ":");
            System.out.println(s);
        }
    }

    public static void main(String[] args) {
        int[][] tableroInicial = {
                { 1, 2, 3, 4 },
                { 5, 6, 7, 8 },
                { 9, 10, 11, 12 },
                { 13, 14, 0, 15 }
        };
        Estado inicial = new Estado(tableroInicial, null);
        List<Estado> result = sucesores(inicial);

        System.out.println("Sucesores del estado inicial:");
        for (Estado e : result) {
            System.out.println(e);
        }

        System.out.println("Solución:");
        findSolution(inicial);
    }
}
