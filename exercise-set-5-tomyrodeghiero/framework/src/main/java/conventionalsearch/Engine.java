package conventionalsearch;

import java.util.List;

/**
 * Title:  Engine
 * Description:  class Engine: Interface which defines the basic 
 * elements that are required for a search engine. Different search strategies 
 * should extend this class.  
 * @author Nazareno Aguirre 
 * 
 */



public interface Engine<S extends State,P extends StateProblem<S>> {



  /** 
  * Starts the search for successful states for problem.
  * @return  a successful state if is found or null.
  * @pre. problem!=null.
  * @post. A success State or null.  
  */
  S performSearch();

   /** 
   * Returns the path to a previously calculated successful state for problem. 
   * Extensions of this Interface should implement this routine.
   * @return the list of nodes corresponding to the path from the root to
   * the current node.
   * @pre. performSearch() has been executed and finished successfully.
   * @post. the path to the found success node is returned.  
   */
  List<S> getPath();

  /** 
  * Reports information regarding a previously executed search.   
  * @pre. performSearch() has been executed and finished.
  * @post. A report regarding the search is printed to standard output.
  */
  void report();

}
