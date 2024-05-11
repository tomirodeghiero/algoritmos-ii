package util.math;

import java.util.Arrays;

/**
 * Computeds greatest common divisor of two nonnegative, not-both-zero
 * integers using diferents algorithms.
 * 
 * @author scilingo
 */

public class GreatestCommonDivisor {

	/**
	 * Computes greatest common divisor by Euclid's algorithm
	 * @param m is a nonnegative integer fisrt argument.
	 * @param n is second nonnegative integer argument.
	 * @return the greatest common divisor between m and n.
	 */
	public static int euclidAlgorithm(int m, int n){
		if (m < 0 || n < 0 || (m == 0 && n == 0)) throw new IllegalArgumentException("numbers must be nonnegative and not-both-zero");
		if(n == 0)
			return m;
		return euclidAlgorithm(n,m%n);
	}

	/**
	 * Computes greatest common divisor by definition based algorithm
	 * @param m is a nonnegative integer fisrt argument.
	 * @param n is second nonnegative integer argument.
	 * @return the greatest common divisor between m and n.
	 */
	public static int definitionBasedAlgorithm(int m, int n) {
		if (m < 0 || n < 0 || (m == 0 && n == 0))
			throw new IllegalArgumentException("numbers must be nonnegative and not-both-zero");

		while (n != 0) {
            int r = m % n;
            m = n;
            n = r;
        }
        return m;
	}

	/**
	* Computes greatest common divisor by middle school procedure
	* @param m is a nonnegative integer fisrt argument.
	 * @param n is second nonnegative integer argument.
	 * @return the greatest common divisor between m and n.
	 */
	public static int middleSchoolAlgorithm(int m, int n){
		int max = Math.max(m, n);
		int[] primes = sieve(max);
		int gcd = 1;
	
		for (int prime : primes) {
			int exponentM = 0;
			int tempM = m; 
			while (tempM % prime == 0) {
				tempM /= prime;
				exponentM++;
			}
	
			int exponentN = 0;
			int tempN = n;
			while (tempN % prime == 0) {
				tempN /= prime;
				exponentN++;
			}
	
			int minExponent = Math.min(exponentM, exponentN);
			if (minExponent > 0) {
				gcd *= Math.pow(prime, minExponent);
			}
		}
	
		return gcd;
	}

	/**
	* Implements the sieve of Eratosthenes
	 * @param n is a number greater than 1
	 * @return Array of all prime numbers less than or equal to n.
	 */
	private static int[] sieve(int n){
		boolean[] isPrime = new boolean[n + 1];
		Arrays.fill(isPrime, 2, n + 1, true); // Fill the array with true starting from index 2 to n

		for (int p = 2; p * p <= n; p++) {
			if (isPrime[p]) {
				for (int j = p * p; j <= n; j += p) {
					isPrime[j] = false; // Mark the number as not prime
				}
			}
		}

		// Count the number of prime numbers found
		int primesCount = 0;
		for (int i = 2; i <= n; i++) {
			if (isPrime[i]) {
				primesCount++;
			}
		}

		// Populate the result array with the prime numbers
		int[] primes = new int[primesCount];
		int index = 0;
		for (int i = 2; i <= n; i++) {
			if (isPrime[i]) {
				primes[index++] = i;
			}
		}

		return primes;
	}
}
