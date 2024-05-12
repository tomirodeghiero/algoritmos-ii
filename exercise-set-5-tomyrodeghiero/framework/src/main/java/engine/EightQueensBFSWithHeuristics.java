package engine;

import java.util.Arrays;
import java.util.Comparator;
import java.util.PriorityQueue;

import eightqueens.EightQueensBFS;

public class EightQueensBFSWithHeuristics {
    static final int SIZE = 8;
    PriorityQueue<State> queue = new PriorityQueue<>(Comparator.comparingInt(s -> -s.heuristic));

    public void solve() {
        queue.add(new State(new int[SIZE], 0, -1));
        while (!queue.isEmpty()) {
            State current = queue.poll();
            if (current.row == SIZE) {
                printBoard(current.board);
                return; // Found a solution
            }
            for (int col = 0; col < SIZE; col++) {
                if (isSafe(current.board, current.row, col)) {
                    int[] newBoard = Arrays.copyOf(current.board, SIZE);
                    newBoard[current.row] = col;
                    queue.add(new State(newBoard, current.row + 1, heuristic(newBoard)));
                }
            }
        }
    }

    private int heuristic(int[] board) {
        int threateningPairs = 0;
        for (int i = 0; i < SIZE; i++) {
            for (int j = i + 1; j < SIZE; j++) {
                if (board[i] == board[j] || Math.abs(board[i] - board[j]) == Math.abs(i - j)) {
                    threateningPairs++;
                }
            }
        }
        return -threateningPairs;
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

    static class State {
        int[] board;
        int row;
        int heuristic;

        State(int[] board, int row, int heuristic) {
            this.board = board;
            this.row = row;
            this.heuristic = heuristic;
        }
    }

    public static void main(String[] args) {
        new EightQueensBFS().solve();
    }
}
