package adversarysearch;

public interface EngineAdversary<P extends StateProblemAdversary<S>, S extends StateAdversary> {



  /** 
   * Returns the maximum depth to be used for search. This value
   * indicates how many game moves in the future are going to be
   * explored in order to compute the value for a state.
   * @return the maximum depth to be used for search.
   * @pre. true.
   * @post. the value of maxDepth is returned.
   */    
  int getMaxDepth();

  /** 
   * Sets the maximum depth to be used for search. This value
   * indicates how many game moves in the future are going to be
   * explored in order to compute the value for a state.
   * @param maxDepth is the value used to set this.maxDepth. 
   * @pre. maxDepth >= 1.
   * @post. this.maxDepth is set to maxDepth.
   */  
  void setMaxDepth(int maxDepth);

  /** 
   * Returns the problem associated with this search engine.
   * @return a reference to the problem associated with this search engine. 
   * @pre. true.
   * @post. this.problem is returned.
   */  
  P getProblem();

  /** 
   * Sets the problem associated with the search engine.
   * @param p is the search problem to be used for search (to set 'problem' to).
   * @pre. p!=null.
   * @post. 'problem' is set to p.
   */ 
  void setProblem(P p);

  /** 
   * Starts the search in order to compute a value for a state. The
   * computation is performed by exploring the game tree corresponding
   * to the problem being analysed, considering state as the root,
   * and with maximum depth maxDepth.
   * @param state is the state for which its value is being computed.
   * @return the value computed for the state.
   * @pre. problem!=null and state!=null.
   * @post. the value for the state is computed, via a search in the
     game tree for state as the root, and maxDepth as the maximum 
     depth. 
   */
  int computeValue(S state);

  /** 
   * Starts the search in order to compute a most promising successor
   * for a state. The computation is performed by exploring the game 
   * tree corresponding to the problem being analysed, considering 
   * state as the root, and with maximum depth maxDepth.
   * @param state is the state for which its most promising successor
    is being computed.
   * @return the most promising successor for state. The method
     ruleApplied() in the result indicates which rule led to the 
     state. 
   * @pre. problem!=null and state!=null.
   * @post. the most promising successor for the state is computed, 
     via a search in the game tree for state as the root, and 
     maxDepth as the maximum depth. 
   */
  S computeSuccessor(S state);


  /** 
   * Reports information regarding a previously executed search.   
   * @pre. computeSuccessor(s) or computeValue(s) have been 
     executed and finished.
   * @post. A report regarding the last performed search is printed 
     to standard output.
   */
  void report();

}
