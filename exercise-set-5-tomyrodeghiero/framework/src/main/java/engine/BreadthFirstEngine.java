package engine;

import conventionalsearch.Engine;
import conventionalsearch.State;
import conventionalsearch.StateProblem;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

/**
* Title:        BreadthFirstEngine
* Description:  Class BreadthFirstEngine implements a Breadth-first search 
                strategy which can be used with any instance of StateProblem.
* @author Nazareno Aguirre
*/
public class BreadthFirstEngine<S extends State, P extends StateProblem<S>> implements Engine<S,P>  {
 
  /**
   * Internal representation of the StateProblem.
   */
  private P sp;
  
  /**
   * path stores the path to the goal.
   */
  private List<S> path; 
  
  /** 
   * Constructor for class BreadthFirstEngine.  
   * @pre. true.
   * @post. Lists path is initialized as empty.
   */
  public BreadthFirstEngine() {
    path = new LinkedList<S>();
  }
  
  /** 
   * Constructor for class BreadthFirstEngine.
   * @param sp is the search problem associated with the engine
     being created.
   * @pre. p!=null.
   * @post. A reference to p is stored in field problem. 
   */  
  public BreadthFirstEngine(P sp) {
    this.sp = sp; 
    path = new LinkedList<S>();
    
  }

  /** 
   * Starts the search for successful states for problem, following a 
   * breadth-first strategy.
   * @return true iff a successful state is found.
   * @pre. problem!=null.
   * @post. the search is performed, the path in list path, and true is returned iff a       
     successful state is found.
   */
  public S performSearch() {
    Queue<S> queue = new LinkedList<S>();
    queue.add(sp.initialState());
    boolean found = false;
    S goal = null;
    while (!queue.isEmpty() && !found) {
      S current = queue.poll();
      if (current.isSuccess()) {
        found = true;
        goal = current;
      } else {
        List<S> succs = sp.getSuccessors(current);
        queue.addAll(succs);
      } 
    }
    if (!(goal == null)) {
      S s = goal;
      while (!(s == null)) {
        path.add(0,s);
        s = (S)s.getParent();
      }
    }
    return goal;

  }
  
  /** 
   * Returns the path to a previously calculated successful state for problem.
   * @return the list of nodes corresponding to the path from the root to
     the successful node.
   * @pre. performSearch() has been executed and finished successfully.
   * @post. the path to the found goal node is returned.  
   */
  public List<S> getPath() {
    return path;
  } // end of getPath()

  /** 
   * Reports information regarding a previously executed search.   
   * @pre. performSearch() has been executed and finished.
   * @post. A report regarding the search is printed to standard output.
       This report consists of .
   */    
  public void report() {
    System.out.println("Length of path to state when search finished: " + path.size());

  } // end of report()


}
