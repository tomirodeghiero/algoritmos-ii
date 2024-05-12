package minmax;

import java.util.ArrayList;
import java.util.List;

public class MinmaxGame {
    static final int SIZE = 3; // Tamaño del tablero para el ejemplo, e.g., Tic-Tac-Toe
    static final int EMPTY = 0, X = 1, O = -1; // Representación de vacío, X y O
    static final int MAX_DEPTH = 9; // Profundidad máxima para la búsqueda

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
            if (isTerminal()) {
                return calculateScore();
            }
            return heuristicEvaluation();
        }

        boolean isTerminal() {
            return Math.abs(calculateScore()) == 10 || getEmptyCells() == 0;
        }

        int calculateScore() {
            int[] lines = new int[8];
            for (int i = 0; i < SIZE; i++) {
                lines[i] = board[i][0] + board[i][1] + board[i][2];
                lines[i + 3] = board[0][i] + board[1][i] + board[2][i];
            }
            lines[6] = board[0][0] + board[1][1] + board[2][2];
            lines[7] = board[0][2] + board[1][1] + board[2][0];

            for (int line : lines) {
                if (line == 3 * X)
                    return 10;
                if (line == 3 * O)
                    return -10;
            }
            return 0;
        }

        int getEmptyCells() {
            int emptyCount = 0;
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (board[i][j] == EMPTY)
                        emptyCount++;
                }
            }
            return emptyCount;
        }

        int heuristicEvaluation() {
            // Implementar una evaluación heurística si es necesario
            return 0; // Placeholder para heurística simple
        }
    }

    int minimax(GameState state, int depth, int alpha, int beta) {
        if (depth == 0 || state.isTerminal()) {
            return state.evaluate();
        }

        int eval;
        if (state.player == X) {
            int maxEval = Integer.MIN_VALUE;
            for (GameState successor : state.generateSuccessors()) {
                eval = minimax(successor, depth - 1, alpha, beta);
                maxEval = Math.max(maxEval, eval);
                alpha = Math.max(alpha, eval);
                if (beta <= alpha)
                    break;
            }
            return maxEval;
        } else {
            int minEval = Integer.MAX_VALUE;
            for (GameState successor : state.generateSuccessors()) {
                eval = minimax(successor, depth - 1, alpha, beta);
                minEval = Math.min(minEval, eval);
                beta = Math.min(beta, eval);
                if (beta <= alpha)
                    break;
            }
            return minEval;
        }
    }

    public static void main(String[] args) {
        MinmaxGame game = new MinmaxGame();
        int[][] initialBoard = { { EMPTY, EMPTY, EMPTY }, { EMPTY, EMPTY, EMPTY }, { EMPTY, EMPTY, EMPTY } };
        GameState initialState = new GameState(X, initialBoard);
        int bestMoveValue = game.minimax(initialState, MAX_DEPTH, Integer.MIN_VALUE, Integer.MAX_VALUE);
        System.out.println("Best possible outcome for X with optimal play: " + bestMoveValue);
    }
}
