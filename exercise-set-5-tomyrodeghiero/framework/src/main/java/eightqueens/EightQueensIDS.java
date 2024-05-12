package eightqueens;

import java.util.Arrays;

public class EightQueensIDS {
    static final int SIZE = 8;
    int[] board = new int[SIZE];
    boolean solutionFound = false;

    public EightQueensIDS() {
        Arrays.fill(board, -1);
    }

    public void solve() {
        int depth = 0;
        while (!solutionFound && depth <= SIZE) {
            iterativeDeepening(0, depth);
            depth++;
        }
    }

    private boolean iterativeDeepening(int row, int limit) {
        if (row == SIZE) {
            printBoard(board);
            solutionFound = true;
            return true;
        }
        if (row > limit)
            return false;

        for (int col = 0; col < SIZE; col++) {
            if (isSafe(row, col)) {
                board[row] = col;
                if (iterativeDeepening(row + 1, limit))
                    return true;
                board[row] = -1; // backtrack
            }
        }
        return false;
    }

    private boolean isSafe(int row, int col) {
        for (int i = 0; i < row; i++) {
            if (board[i] == col || Math.abs(board[i] - col) == Math.abs(i - row)) {
                return false;
            }
        }
        return true;
    }

    private void printBoard(int[] board) {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                System.out.print(board[i] == j ? "Q " : ". ");
            }
            System.out.println();
        }
        System.out.println("\nSolution end\n");
    }

    public static void main(String[] args) {
        new EightQueensIDS().solve();
    }
}
