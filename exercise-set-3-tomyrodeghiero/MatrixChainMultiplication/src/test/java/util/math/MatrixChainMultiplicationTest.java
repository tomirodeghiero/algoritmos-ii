package util.math;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collection;

//import util;

/**
 * Checks properties of the Matrix Chain Multiplication algorithms.
 * It is implemented using jUnit's parameterized tests.
 * 
 * @author scilingo
 *
 */

@RunWith(Parameterized.class)
public class MatrixChainMultiplicationTest {

    private Integer[] chain;
    private int spectedMult;
    private int multiplications;
    MatrixChainMultiplication calculator;
    String spectedParenthesized;


    /**
    * Default constructor for parameterized tests.
    * Sets elements from Parameters into the variables for input.
    * @param chain is a list of matrix.
    * @param multiplications is the number of operations spected.
    */
    public MatrixChainMultiplicationTest(Integer[] chain, int multiplications, String parenthesized){
        this.chain = chain;
        this.spectedMult = multiplications;
        this.spectedParenthesized = parenthesized;
    } 

    /**
     * Contains the chain values to use for tests.
     * This method contains the inputs to be used for testing.
     * New tests are added by adding new inputs here.
     * See test methods in this class, for actual properties
     * being checked.
     * @return an array of integer arrays and integer values, used as
     * parameters for tests.
     */
    @Parameters
    public static Collection<Object[]> parameters(){
        return Arrays.asList(new Object[][] {
            // [13x5],[5x89],[89x3],[3x34]
            { new Integer[] {13, 5, 89, 3, 34}, 2856, "(M1(M2M3)M4)"},
            { new Integer[] {10, 30, 5, 60}, 4500, "((M1M2)M3)"},
            { new Integer[] {10, 30, 5}, 1500,"(M1M2)"},
            { new Integer[] {10, 30}, 0, "M1"},
        });
    }

    @Before
    public void setUp(){
        calculator = new MatrixChainMultiplication(chain);
        multiplications = calculator.divideAndConquerMCM(chain,1,chain.length-1);
    }

    @Test
    public void test1(){
        assertEquals (spectedParenthesized,calculator.getParenthesized());
    }

    /**
    * Checks whether mcm method computes minimum number of multiplications;
    */
    @Test
    public void multiplicationsTest(){
        assertEquals("multiplications", spectedMult,multiplications);
    }


}
