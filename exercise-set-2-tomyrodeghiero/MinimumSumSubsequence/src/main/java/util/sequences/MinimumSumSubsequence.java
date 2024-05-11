package util.sequences;

import java.util.Arrays;

/**
 * Computeds minimum sum subsequence of Integer sequences
 * 
 * @author Tomás Rodeghiero
 */

public class MinimumSumSubsequence {

	/**
	 * Computes a minimum sum subsequence by divide and conquer strategy.
	 * 
	 * @param sequence is an Integer sequence.
	 * @return return a tuple containing, value of the sum, the begin index
	 *         of subsequence and the end of the subsequence
	 */
	public static Tuple<Integer, Integer, Integer> minimumSumSubsequence(Integer[] sequence) {
		if (sequence == null || sequence.length == 0) {
			throw new IllegalArgumentException("The sequence cannot be null or empty.");
		}
		// If all values are positive, the minimum sum is 0 (the empty subsequence)
		boolean allPositive = true;
		for (int value : sequence) {
			if (value < 0) {
				allPositive = false;
				break;
			}
		}
		if (allPositive) {
			return new Tuple<Integer, Integer, Integer>(0, -1, -1);
		} else {
			return foundMinimumSumSubsequence(sequence, 0, sequence.length - 1);
		}
	}

	/**
	 * Computes a minimum sum subsequence by divide and conquer strategy.
	 * 
	 * @param sequence is an Integer sequence.
	 * @param left     is the left index of the sequence.
	 * @param right    is the right index of the sequence.
	 * @return return a tuple containing, value of the sum, the begin index
	 *         of subsequence and the end of the subsequence
	 */
	public static Tuple<Integer, Integer, Integer> foundMinimumSumSubsequence(Integer[] sequence, int left, int right) {
		// Base case: if the sequence has 1 element, the minimum sum is the element
		// itself
		if (left == right) {
			return new Tuple<Integer, Integer, Integer>(sequence[left], left, right);
		}

		// Divide: found the middle of the sequence
		int middle = (left + right) / 2;
		// Conquer: found the minimum sum of the left and right subsequences
		Tuple<Integer, Integer, Integer> leftSum = foundMinimumSumSubsequence(sequence, left, middle);
		Tuple<Integer, Integer, Integer> rightSum = foundMinimumSumSubsequence(sequence, middle + 1, right);
		Tuple<Integer, Integer, Integer> crossingSum = foundCrossingSum(sequence, left, middle, right);

		if (leftSum.getFirst() <= rightSum.getFirst() && leftSum.getFirst() <= crossingSum.getFirst()) {
			return leftSum;
		} else if (rightSum.getFirst() <= leftSum.getFirst() && rightSum.getFirst() <= crossingSum.getFirst()) {
			return rightSum;
		} else {
			return crossingSum;
		}
	}

	private static Tuple<Integer, Integer, Integer> foundCrossingSum(Integer[] sequence, int left, int middle,
			int right) {
		int sum = 0;
		int leftMinSum = Integer.MAX_VALUE;
		int leftIndex = middle;
		for (int i = middle; i >= left; i--) {
			sum += sequence[i];
			if (sum < leftMinSum) {
				leftMinSum = sum;
				leftIndex = i;
			}
		}

		sum = 0;
		int rightMinSum = Integer.MAX_VALUE;
		int rightIndex = middle + 1;
		for (int i = middle + 1; i <= right; i++) {
			sum += sequence[i];
			if (sum < rightMinSum) {
				rightMinSum = sum;
				rightIndex = i;
			}
		}

		// Sum the minimum sums found on both sides of the middle, excluding the
		// overlapping middle value
		int totalSum = leftMinSum + rightMinSum;
		return new Tuple<Integer, Integer, Integer>(totalSum, leftIndex, rightIndex - 1);
	}

	public static void main(String[] args) {
		Integer[] sequence = { -4, -70, -13, 1, 7, 5 };
		Tuple<Integer, Integer, Integer> result = minimumSumSubsequence(sequence);

		System.out.println("Sequence: " + Arrays.toString(sequence));
		System.out.println("Minimum sum of a subsequence: " + result.getFirst() +
				", From index: " + result.getSecond() +
				", To index: " + result.getThird());
	}
}