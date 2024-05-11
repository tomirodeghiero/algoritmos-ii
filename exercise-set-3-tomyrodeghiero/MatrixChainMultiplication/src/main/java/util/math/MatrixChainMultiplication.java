package util.math;

import java.util.ArrayList;

/**
 * This class implements several algorithms to compute Matrix Chain
 * Multiplication problem. Wich consists to find the right parenthesize
 * of a matrices chain. In order to compute the product of a matrices
 * chain with the minimum number of multiplication operations.
 */
public class MatrixChainMultiplication
{
    private Integer[] chain; // contains the matrices dimensions.

    private int [][] p; // p will store index of parenthesized split point

    /*
    * Matrix A[i] has dimension chain[i-1] x chain[i] for i = 1..n
    * @param chain contains the matrices dimensions.
    */
	public MatrixChainMultiplication(Integer[] chain){
		this.chain = chain;
		this.p = new int [chain.length][chain.length];
	}

	/* Method to build parenthesized solution */
	public String getParenthesized(){

		return buildParenthesized(1,chain.length-1);
	}

	private String buildParenthesized(int i, int j){
		throw new UnsupportedOperationException("method not yet implemented");
	}

	/*
	* This method compute the best matrix chain multiplication
	* by diferent techniques.
	* @param chain contains matrices dimensions
	*/  
	public int matrixChainMultiplication(Integer[] chain){
		throw new UnsupportedOperationException("method not yet implemented");
	}

	/*
	* This method compute the best matrix chain multiplication
	* by Divide & Conquer technique.
	* @param chain contains matrices dimensions
	*/  
	public int divideAndConquerMCM(Integer[] chain, int i, int j){
		throw new UnsupportedOperationException("method not yet implemented");
	}

	/*
	* This method compute the best matrix chain multiplication
	* by Dynamic technique.
	* @param chain contains matrices dimensions
	*/  
	public int dynamicMCM(Integer[] chain, int i, int j){
		throw new UnsupportedOperationException("method not yet implemented");
	}



}
