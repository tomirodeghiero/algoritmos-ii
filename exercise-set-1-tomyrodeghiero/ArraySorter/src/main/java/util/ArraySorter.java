package util;

import java.util.ArrayList;
import java.util.List;

/**
 * Sorts arrays of comparable objects using a variety of options.
 * 
 * @author aguirre
 *
 * @param <E> is the type of the elements of the array.
 */
public class ArraySorter<E extends Comparable<E>> {

	/**
	 * The array to sort.
	 */
	private E[] array;

	/**
	 * The algorithm to use for sorting.
	 */
	private SortAlgorithm algorithm = SortAlgorithm.SELECTIONSORT;

	/**
	 * Default constructor. Sets the array to sort and sorting algorithm to
	 * INSERTION SORT.
	 * 
	 * @param array is the array to sort.
	 */
	public ArraySorter(E[] array) {
		if (array == null)
			throw new IllegalArgumentException("array must be non-null");
		this.array = array;
	}

	/**
	 * Constructor that sets array and sorting algorithm.
	 * 
	 * @param array     is the array to sort.
	 * @param algorithm is the algorithm to use for sorting.
	 */
	public ArraySorter(E[] array, SortAlgorithm algorithm) {
		if (array == null)
			throw new IllegalArgumentException("array must be non-null");
		this.array = array;
		this.algorithm = algorithm;
	}

	/**
	 * Sets the algorithm to use for sorting.
	 * 
	 * @param algorithm is the algorithm to set for sorting.
	 */
	public void setAlgorithm(SortAlgorithm algorithm) {
		if (algorithm == null)
			throw new IllegalArgumentException("algorithm can't be null");
		this.algorithm = algorithm;
	}

	/**
	 * Retrieves the (sorted or yet unsorted) array within the ArraySorter.
	 * 
	 * @return the array stored within the ArraySorter object.
	 */
	public E[] getArray() {
		return this.array;
	}

	/**
	 * Sets the array to be sorted.
	 * 
	 * @param array is the new array to sort.
	 */
	public void setArray(E[] array) {
		throw new UnsupportedOperationException("method not yet implemented");
	}

	/**
	 * Sorts the array.
	 * The array can then be retrieved using getArray() method.
	 */
	public void sort() {
		switch (this.algorithm) {
			case INSERTIONSORT:
				insertionSort(array);
				break;
			case BUBBLESORT:
				bubbleSort(array);
				break;
			case SELECTIONSORT:
				selectionSort(array);
				break;
			case SLOWSORT:
				slowSort(array);
				break;
			default:
				throw new UnsupportedOperationException("sorting method not yet implemented");
		}
	}

	/**
	 * Sorts an array. Implements the insertion sort algorithm.
	 * 
	 * @param <T>   is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	private static <T extends Comparable<T>> void insertionSort(T[] array) {
		if (array == null)
			throw new IllegalArgumentException("array is null, can't sort");
		for (int unsorted = 1; unsorted < array.length; unsorted++) {
			T key = array[unsorted];
			int loc = unsorted - 1;
			while ((loc >= 0) && (array[loc].compareTo(key) > 0)) {
				array[loc + 1] = array[loc];
				loc--;
			}
			array[loc + 1] = key;
		}
	}

	/**
	 * Sorts an array. Implements the selection sort algorithm.
	 * 
	 * @param <T>   is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	private static <T extends Comparable<T>> void selectionSort(T[] array) {
		if (array == null)
			throw new IllegalArgumentException("Array is null, can't sort");
		for (int i = 0; i < array.length - 1; i++) {
			int min = i;
			for (int j = i + 1; j < array.length; j++) {
				if (array[j].compareTo(array[min]) < 0) {
					min = j;
				}
			}
			T temp = array[i];
			array[i] = array[min];
			array[min] = temp;
		}
	}

	/**
	 * Sorts an array. Implements the bubblesort sort algorithm.
	 * 
	 * @param <T>   is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	private static <T extends Comparable<T>> void bubbleSort(T[] array) {
		if (array == null)
			throw new IllegalArgumentException("Array is null, can't sort");
		for (int i = 0; i < array.length; i++) {
			for (int j = 0; j < array.length - 1; j++) {
				if (array[j].compareTo(array[j + 1]) > 0) {
					T temp = array[j];
					array[j] = array[j + 1];
					array[j + 1] = temp;
				}
			}
		}
	}

	/**
	 * Sorts an array. Implements the slow sort algorithm.
	 * 
	 * @param <T>   is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	public static <T extends Comparable<T>> T[] slowSort(T[] original) {
		List<T[]> allPermutations = generatePermutations(original); // Utiliza el método adaptado para arreglos
		for (T[] perm : allPermutations) {
			if (isSorted(perm)) {
				return perm;
			}
		}
		return original; // En caso de no encontrar una permutación ordenada, lo cual es teóricamente
							// imposible, retorna el original
	}

	/**
	 * Checks if a given array is sorted.
	 * 
	 * @param <T>   is the type of the elements in the array.
	 * @param array is the array to be checked for sortedness.
	 * @return true iff the array is sorted.
	 */
	public static <T extends Comparable<T>> boolean isSorted(T[] array) {
		if (array == null)
			throw new IllegalArgumentException("Array is null, can't check for sortedness");
		for (int i = 0; i < array.length - 1; i++) {
			if (array[i].compareTo(array[i + 1]) > 0) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Generates all permutations of an array.
	 * 
	 * @param <T>
	 * @param original
	 * @return
	 */
	public static <T> List<T[]> generatePermutations(T[] original) {
		List<T[]> results = new ArrayList<>();
		permute(original, 0, original.length, results);
		return results;
	}

	private static <T> void permute(T[] arr, int start, int end, List<T[]> results) {
		if (start == end - 1) {
			// Cuando start es igual a end - 1, hemos encontrado una permutación
			results.add(arr.clone());
		} else {
			for (int i = start; i < end; i++) {
				// Intercambiamos el elemento en 'start' con el elemento en 'i'
				T temp = arr[start];
				arr[start] = arr[i];
				arr[i] = temp;

				// Recursivamente generamos permutaciones del resto del arreglo
				permute(arr, start + 1, end, results);

				// Volvemos a intercambiar los elementos para regresar al estado original
				temp = arr[start];
				arr[start] = arr[i];
				arr[i] = temp;
			}
		}
	}

	public static void main(String[] args) {
		Integer[] original = { 1, 2, 3 };
		List<Integer[]> permutations = generatePermutations(original);
		for (Integer[] perm : permutations) {
			for (int num : perm) {
				System.out.print(num + " ");
			}
			System.out.println();
		}

		// Slow sort
		Integer[] unsorted = { 5, 2, 4 };
		Integer[] sorted = slowSort(unsorted);
		for (int num : sorted) {
			System.out.print(num + " ");
		}
	}
}
