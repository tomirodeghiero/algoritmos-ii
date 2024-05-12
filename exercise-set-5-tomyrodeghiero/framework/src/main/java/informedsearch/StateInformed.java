package informedsearch;

import conventionalsearch.State;

/**
 * Title: StateInformed
 * Description:  Interface which extends the State search characterization 
 * with additional value of a state information. Instances of these problems should 
 * implement this interface, to be able to use the search strategies.        
 * @author Nazareno Aguirre
 */



public interface StateInformed extends State {
  /** 
   * Computes the value of this state. If the state is a leaf
   * (end state), then this value is an exact value.
   * If the state is not an end state, then
   * this value is an approximate value. Its estimation plays a
   * crucial role in the performance of search. 
   * @return an integer value, representing the value of the state.
   * @pre. this!=null.
   * @post. an integer value, representing the value of the state.   
   */
  public int value();

}
