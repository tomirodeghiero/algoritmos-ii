package conventionalsearch;

import java.util.List;

/**
 * Title: StateProblem
 * Description:  Interface which defines the basic elements necessary for 
 * characterize a problem as a search. Instances of these problems should 
 * implement this interface, to be able to use the search strategies.        
 * @author Sonia (Comments from a previous version of Nazareno)
 */



public interface StateProblem<S extends State> {
  /**
   * Returns the initial state corresponding to the problem. Concrete
   * implementations of StateProblem must implement this routine,
   * to indicate the starting point for the search.
   * @return the initial state for the problem  
   */
  S initialState();
  
  /** 
   * Returns the list of successor states for a given state, in the context of
   * the current problem. Concrete implementations of StateProblem 
   * must implement this routine, to indicate the 'advance' rules for the 
   * search.
   * @param s is the state for which its successors are being computed.
   * @return the list of successor states of s.  
   */
  List<S> getSuccessors(S s);
 
  

}
