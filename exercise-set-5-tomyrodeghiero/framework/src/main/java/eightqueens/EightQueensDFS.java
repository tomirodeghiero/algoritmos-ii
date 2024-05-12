package eightqueens;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class EightQueensDFS {
    static final int SIZE = 8;
    int[] board = new int[SIZE];
    List<int[]> solutions = new ArrayList<>();

    public EightQueensDFS() {
        Arrays.fill(board, -1);
    }

    public boolean solve(int row) {
        if (row == SIZE) {
            solutions.add(board.clone());
            return true;
        }
        for (int col = 0; col < SIZE; col++) {
            if (isSafe(row, col)) {
                board[row] = col;
                solve(row + 1);
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

    public void printSolutions() {
        for (int[] sol : solutions) {
            for (int row = 0; row < SIZE; row++) {
                for (int col = 0; col < SIZE; col++) {
                    System.out.print(sol[row] == col ? "Q " : ". ");
                }
                System.out.println();
            }
            System.out.println("\nSolution end\n");
        }
    }

    public static void main(String[] args) {
        EightQueensDFS eq = new EightQueensDFS();
        eq.solve(0);
        eq.printSolutions();
    }
}
