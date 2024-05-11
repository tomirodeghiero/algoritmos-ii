package util.sequences;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import java.util.Arrays;
import java.util.Collection;

import util.sequences.MinimumSumSubsequence;

/**
 * Checks properties of the Minimum Sum Subsequence algorithm.
 * It is implemented using jUnit's parameterized tests.
 * 
 * @author scilingo
 *
 */

@RunWith(Parameterized.class)
public class MinimumSumSubsequenceTest {

    private Integer[] sequence;
    private int minSum;
    private int lowerIndex;
    private int upperIndex;
    private Tuple<Integer,Integer,Integer> result;


    /**
    * Default constructor for parameterized tests.
    * Sets elements from Parameters into the variables for input.
    * @param sequence is the source sequence. 
    * @param minSum is minimum sum spected.
    * @param lowerIndex is subsequence begin.
    * @param upperIndex is subsequence end.
    */
    public MinimumSumSubsequenceTest(Integer[] sequence, int minSum, int lowerIndex, int upperIndex){
        this.sequence = sequence;
        this.minSum = minSum;
        this.lowerIndex = lowerIndex;
        this.upperIndex = upperIndex;
    } 

    /**
     * Contains the sequence values to use for tests.
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
            { new Integer[] {1, 2, 3}, 0, -1, -1},
            { new Integer[] {-2, 1, 2, -10, 3, -20}, -27, 3 , 5},
            { new Integer[] {}, 0, -1, -1},
            { new Integer[] {1}, 0, -1, -1},
            { new Integer[] {1, -2, 3,-1}, -2, 1, 1},
            { new Integer[] {1, -2}, -2, 1, 1},
            { new Integer[] {-2, 1, -2, 5, -2, 1, -2}, -3, 0, 2},
        });
    }

    @Before
    public void setUp(){
        result = MinimumSumSubsequence.minimumSumSubsequence(sequence);
    }

    /**
    * Checks whether minimumSumSubsequence implementation corectly compute
    * the minimum sum.
    */
    @Test
    public void sumTest(){
        int  sum = (Integer)result.getFirst();
        assertEquals("sum value", minSum,sum);
    }

    /**
    * Checks whether minimumSumSubsequence implementation corectly compute
    * the subsequence of minimum sum. Verifies spected index.
    */
    @Test
    public void indexTest(){
        assertEquals("lowerIndex ",lowerIndex,(int)result.getSecond());
        assertEquals("upperIndex ",upperIndex,(int)result.getThird());
    }

}
