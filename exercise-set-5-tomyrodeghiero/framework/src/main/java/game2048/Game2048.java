package game2048;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.Scanner;

public class Game2048 {
    static final int SIZE = 4; // Tamaño del tablero
    static final Random random = new Random();

    static class GameState {
        int[][] board = new int[SIZE][SIZE];
        int score;

        public GameState(int[][] board, int score) {
            for (int i = 0; i < SIZE; i++) {
                System.arraycopy(board[i], 0, this.board[i], 0, SIZE);
            }
            this.score = score;
        }

        // Método para agregar una ficha nueva al tablero en posición aleatoria
        void addRandomTile() {
            List<int[]> emptyPositions = new ArrayList<>();
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (board[i][j] == 0) {
                        emptyPositions.add(new int[] { i, j });
                    }
                }
            }
            if (!emptyPositions.isEmpty()) {
                int[] position = emptyPositions.get(random.nextInt(emptyPositions.size()));
                board[position[0]][position[1]] = random.nextDouble() < 0.9 ? 2 : 4;
            }
        }

        // Mover y fusionar el tablero en la dirección dada
        boolean move(int direction) {
            boolean moved = false;
            int[][] newBoard = new int[SIZE][SIZE];

            for (int i = 0; i < SIZE; i++) {
                List<Integer> line = new ArrayList<>();
                for (int j = 0; j < SIZE; j++) {
                    int val = 0;
                    if (direction == 0) { // Izquierda
                        val = board[i][j];
                    } else if (direction == 1) { // Arriba
                        val = board[j][i];
                    } else if (direction == 2) { // Derecha
                        val = board[i][SIZE - j - 1];
                    } else if (direction == 3) { // Abajo
                        val = board[SIZE - j - 1][i];
                    }

                    if (val != 0) {
                        line.add(val);
                    }
                }
                line = mergeLine(line);

                for (int j = 0; j < SIZE; j++) {
                    int val = (j < line.size()) ? line.get(j) : 0;
                    if (direction == 0) { // Izquierda
                        newBoard[i][j] = val;
                    } else if (direction == 1) { // Arriba
                        newBoard[j][i] = val;
                    } else if (direction == 2) { // Derecha
                        newBoard[i][SIZE - j - 1] = val;
                    } else if (direction == 3) { // Abajo
                        newBoard[SIZE - j - 1][i] = val;
                    }
                }
            }

            if (!Arrays.deepEquals(board, newBoard)) {
                board = newBoard;
                addRandomTile();
                moved = true;
            }
            return moved;
        }

        // Fusionar líneas durante un movimiento
        private List<Integer> mergeLine(List<Integer> line) {
            List<Integer> newLine = new ArrayList<>();
            for (int i = 0; i < line.size(); i++) {
                int num = line.get(i);
                if (i < line.size() - 1 && Objects.equals(line.get(i), line.get(i + 1))) {
                    num *= 2;
                    score += num;
                    i++;
                }
                newLine.add(num);
            }
            return newLine;
        }

        // Evaluar el estado del tablero para Minimax
        int evaluate() {
            int emptyCells = 0;
            int maxValue = 0;
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (board[i][j] == 0) {
                        emptyCells++;
                    }
                    if (board[i][j] > maxValue) {
                        maxValue = board[i][j];
                    }
                }
            }
            return maxValue + emptyCells;
        }

        // Comprobar si el juego ha terminado
        boolean isTerminal() {
            for (int direction = 0; direction < 4; direction++) {
                GameState testState = new GameState(board, score);
                if (testState.move(direction)) {
                    return false; // Si cualquier movimiento es posible, no es terminal
                }
            }
            return true;
        }

        List<GameState> generateSuccessors() {
            List<GameState> successors = new ArrayList<>();
            for (int direction = 0; direction < 4; direction++) {
                GameState newState = new GameState(board.clone(), score);
                if (newState.move(direction)) {
                    newState.addRandomTile();
                    successors.add(newState);
                }
            }
            return successors;
        }
    }

    // Minimax con poda alfa-beta
    int minimax(GameState state, int depth, int alpha, int beta, boolean maximizingPlayer) {
        if (depth == 0 || state.isTerminal()) {
            return state.evaluate();
        }

        if (maximizingPlayer) {
            int maxEval = Integer.MIN_VALUE;
            for (GameState child : state.generateSuccessors()) {
                int eval = minimax(child, depth - 1, alpha, beta, false);
                maxEval = Math.max(maxEval, eval);
                alpha = Math.max(alpha, eval);
                if (beta <= alpha)
                    break;
            }
            return maxEval;
        } else {
            int minEval = Integer.MAX_VALUE;
            for (GameState child : state.generateSuccessors()) {
                int eval = minimax(child, depth - 1, alpha, beta, true);
                minEval = Math.min(minEval, eval);
                beta = Math.min(beta, eval);
                if (beta <= alpha)
                    break;
            }
            return minEval;
        }
    }

    public static void main(String[] args) {
        try (Scanner scanner = new Scanner(System.in)) {
            Game2048 game = new Game2048();
            int[][] startBoard = new int[SIZE][SIZE];
            GameState state = new GameState(startBoard, 0);
            state.addRandomTile();
            state.addRandomTile();

            while (!state.isTerminal()) {
                System.out.println("Current board:");
                printBoard(state.board);
                System.out.println("Enter direction (0: Left, 1: Up, 2: Right, 3: Down):");
                int direction = scanner.nextInt();
                if (!state.move(direction)) {
                    System.out.println("Invalid move, try again.");
                    continue;
                }

                // Let AI decide the next move (minimizing player)
                GameState bestState = null;
                int bestValue = Integer.MAX_VALUE;
                for (GameState successor : state.generateSuccessors()) {
                    int value = game.minimax(successor, 3, Integer.MIN_VALUE, Integer.MAX_VALUE, false);
                    if (value < bestValue) {
                        bestValue = value;
                        bestState = successor;
                    }
                }
                if (bestState != null) {
                    state = bestState;
                    System.out.println("AI made its move.");
                }
            }

            System.out.println("Game Over!");
            printBoard(state.board);
        }
    }

    private static void printBoard(int[][] board) {
        for (int[] row : board) {
            for (int value : row) {
                System.out.format("%4d", value);
            }
            System.out.println();
        }
    }
}
