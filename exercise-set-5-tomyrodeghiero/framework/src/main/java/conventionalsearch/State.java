package conventionalsearch;

/**
 * Title: State.
 * Description:  Interface which defines the basic requirements on
 * states, needed when characterising problems as search problems. 
 * State definitions for particular search problems should extend this class,
 * so the general search strategies can be used.
 * @author Nazareno Aguirre
 */

public interface State {
  /** 
   * Returns the parent of the current state. This method
   * must be implemented by all concrete classes implementing State.
   * @return the parent of the current state or null if this does not have a parent.
   */
  State getParent();
  
 
  
  
  /** 
   * Indicates whether this state is a successful state, in the context of
   * the current problem. Concrete implementations of State 
   * must implement this routine, to indicate when the search has been  
   * successful.
   * @return true iff s is a successful state.
   */
  boolean isSuccess();

  
  /** 
   * Checks whether 'this' is equal to another state. This must be implemented
   * by every concrete class implementing State.
   * @param other State object to compare with this.
   * @return true iff 'this' is equal, as a state, to 'other'.
   */
  @Override
  boolean equals(Object other);
  
  /**
   * Returns a hash code value for the concrete State object.
   * This method is supported for the benefit of hash tables such as those
   * provided by java.util.Hashtable.
   * @return a hash code value for the concrete State object.
   */
  @Override
  int hashCode();

  /** 
   * Returns a representation as a string of the current state. This method
   * must be implemented by all concrete classes implementing State.
   * @return a String representation of the current state.
   */
  @Override
  String toString();
  


}
