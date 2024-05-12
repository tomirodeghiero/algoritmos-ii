package adversarysearch;

import conventionalsearch.State;

/**
 * Interface which defines the basic requirements on states, needed 
 * when characterising problems as adversary search problems. State 
 * definitions for particular adversary search problems should 
 * implement this interface, so that general adversary search 
 * strategies can be used.
 * @author Nazareno Aguirre
 */

public interface StateAdversary extends State {
  /** 
   * Returns the parent of the current state. This method
   * must be implemented by all concrete classes implementing StateAdversary.
   * @return the parent of the current state or null if this does not have a parent..
   */
  public StateAdversary getParent();
  
  /** 
   * Indicates whether the current state is a max state or not.
   * If the current state is not a 'max' state, then it is assumed
   * to be a min state. 
   * @return true iff 'this' is a max state.
   * @pre. true.
   * @post. true is returned iff 'this' is a max state.
   */
  boolean isMax();
   
   

  /** 
   * Indicates whether this state is an end state, i.e., a 
   * state with no successors. 
   * @return true iff state is an end state.
   * @pre. this!=null.
   * @post. true is returned iff this is an end state.  
   */
  boolean end();

  /** 
   * Computes the value of this  state. If the state is a leaf
   * (end state), then this value is an exact value, and indicates
   * the outcome of the game. If the state is not an end state, then
   * this value is an approximate value. Its estimation plays a
   * crucial role in the performance of search. This value must 
   * be greater than minValue(), and smaller than maxValue().
   * @return an integer value, representing the value of the state.
   * @pre. this!=null.
   * @post. an integer value, representing the value of the state.   
   */
  int value();



  /** 
   * Checks whether 'this' is equal to another state. 
   * @param other is the state to compare 'this' to.
   * @return true iff 'this' is equal, as a state, to 'other'.
   * @pre. other!=null.
   * @post. true is returned iff 'this' is equal, as a state, 
     to 'other'.
   */
  @Override
  boolean equals(Object other);

  /** 
   * Returns a representation as a string of the current state. 
   * @return a string representing the current state.
   * @pre. true.
   * @post. A text representation of the current state is returned.
   */
  @Override 
  String toString();

  /** 
   * Returns an object representing the rule applied, leading to the
   * current state. 
   * @return an object representing the rule applied, leading to the
     current state. If the state is the initial state, then null is 
     returned.
   * @pre. true.
   * @post. An object representing the rule applied, leading to the
     current state, is returned. If the state is the initial state, 
     then null is returned.
     TODO Replace Object by a more specific class or interface.
   */
  Object ruleApplied();



}
