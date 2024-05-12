package minmax;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class MinmaxTicTacToe {
    static final int SIZE = 3; // Tamaño del tablero (3x3)
    static final int EMPTY = 0, X = 1, O = -1; // Representación de vacío, X y O

    static class GameState {
        int player; // Jugador actual: 1 para X, -1 para O
        int[][] board; // Tablero del juego

        public GameState(int player, int[][] board) {
            this.player = player;
            this.board = new int[SIZE][SIZE];
            for (int i = 0; i < SIZE; i++) {
                System.arraycopy(board[i], 0, this.board[i], 0, SIZE);
            }
        }

        List<GameState> generateSuccessors() {
            List<GameState> successors = new ArrayList<>();
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (board[i][j] == EMPTY) {
                        int[][] newBoard = copyBoard();
                        newBoard[i][j] = player;
                        successors.add(new GameState(-player, newBoard));
                    }
                }
            }
            return successors;
        }

        private int[][] copyBoard() {
            int[][] newBoard = new int[SIZE][SIZE];
            for (int i = 0; i < SIZE; i++) {
                System.arraycopy(board[i], 0, newBoard[i], 0, SIZE);
            }
            return newBoard;
        }

        int evaluate() {
            // Evaluación del estado del juego (verificar líneas completas de X o O)
            int[] lines = new int[8]; // 3 filas, 3 columnas, 2 diagonales
            for (int i = 0; i < SIZE; i++) {
                lines[i] = board[i][0] + board[i][1] + board[i][2]; // Filas
                lines[i + 3] = board[0][i] + board[1][i] + board[2][i]; // Columnas
            }
            lines[6] = board[0][0] + board[1][1] + board[2][2]; // Diagonal principal
            lines[7] = board[0][2] + board[1][1] + board[2][0]; // Diagonal secundaria

            for (int line : lines) {
                if (line == 3 * X)
                    return 10; // X wins
                if (line == 3 * O)
                    return -10; // O wins
            }
            return 0; // No winner yet
        }

        boolean esTerminal() {
            if (Math.abs(evaluate()) == 10)
                return true; // Alguien ha ganado
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (board[i][j] == EMPTY)
                        return false; // Juego aún no terminado
                }
            }
            return true; // Empate
        }

        int minimax(int depth, int alpha, int beta) {
            if (depth == 0 || this.esTerminal()) {
                return this.evaluate();
            }

            int eval;
            if (player == X) { // Maximizador
                int maxEval = Integer.MIN_VALUE;
                for (GameState successor : this.generateSuccessors()) {
                    eval = successor.minimax(depth - 1, alpha, beta);
                    maxEval = Math.max(maxEval, eval);
                    alpha = Math.max(alpha, eval);
                    if (beta <= alpha)
                        break;
                }
                return maxEval;
            } else { // Minimizador
                int minEval = Integer.MAX_VALUE;
                for (GameState successor : this.generateSuccessors()) {
                    eval = successor.minimax(depth - 1, alpha, beta);
                    minEval = Math.min(minEval, eval);
                    beta = Math.min(beta, eval);
                    if (beta <= alpha)
                        break;
                }
                return minEval;
            }
        }
    }

    public static void main(String[] args) {
        try (Scanner scanner = new Scanner(System.in)) {
            int[][] initialBoard = {
                    { EMPTY, EMPTY, EMPTY },
                    { EMPTY, EMPTY, EMPTY },
                    { EMPTY, EMPTY, EMPTY }
            };
            GameState currentState = new GameState(X, initialBoard);

            while (!currentState.esTerminal()) {
                System.out.println("Current board:");
                printBoard(currentState.board);

                if (currentState.player == X) {
                    int row, col;
                    do {
                        System.out.println("Enter your move (row and column): ");
                        row = scanner.nextInt();
                        col = scanner.nextInt();
                    } while (row < 0 || col < 0 || row >= SIZE || col >= SIZE || currentState.board[row][col] != EMPTY);

                    currentState.board[row][col] = X;
                    currentState = new GameState(O, currentState.board);
                } else {
                    int bestVal = Integer.MAX_VALUE;
                    GameState bestState = null;
                    for (GameState state : currentState.generateSuccessors()) {
                        int value = state.minimax(9, Integer.MIN_VALUE, Integer.MAX_VALUE);
                        if (value < bestVal) {
                            bestVal = value;
                            bestState = state;
                        }
                    }
                    currentState = bestState != null ? bestState : currentState;
                    System.out.println("AI has made its move.");
                }
            }

            System.out.println("Final board:");
            printBoard(currentState.board);
            int result = currentState.evaluate();
            if (result == 10) {
                System.out.println("X wins!");
            } else if (result == -10) {
                System.out.println("O wins!");
            } else {
                System.out.println("It's a draw!");
            }
        }
    }

    private static void printBoard(int[][] board) {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                char c = board[i][j] == EMPTY ? '.' : (board[i][j] == X ? 'X' : 'O');
                System.out.print(c + " ");
            }
            System.out.println();
        }
    }
}
