package eightqueens;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

public class EightQueensBFS {
    static final int SIZE = 8;

    public void solve() {
        Queue<int[]> queue = new LinkedList<>();
        queue.add(new int[SIZE]);
        Arrays.fill(queue.peek(), -1);

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int row = getNextRow(current);
            if (row == SIZE) {
                printBoard(current);
                return; // Found a solution
            }
            for (int col = 0; col < SIZE; col++) {
                if (isSafe(current, row, col)) {
                    int[] newBoard = Arrays.copyOf(current, SIZE);
                    newBoard[row] = col;
                    queue.add(newBoard);
                }
            }
        }
    }

    private int getNextRow(int[] board) {
        for (int i = 0; i < SIZE; i++) {
            if (board[i] == -1)
                return i;
        }
        return SIZE;
    }

    private boolean isSafe(int[] board, int row, int col) {
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
        new EightQueensBFS().solve();
    }
}
