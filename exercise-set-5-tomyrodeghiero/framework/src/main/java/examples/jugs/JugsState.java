package examples.jugs;

import conventionalsearch.State;

public class JugsState implements State {
  private JugsState parent = null;
  private int contentsA; // contents of jug A
  private int contentsB; // contents of jug B
  
  public boolean repOK() {
    return (contentsA >= 0 && contentsA <= 4 && contentsB >= 0 && contentsB <= 3);
  }
  
  /** Constructor for JugsState class. It initializes the contents of the 
   * jugs with the provided parameters.
   * @param valueA is the integer value used to set the initial contents of the
     first jug (A).
   * @param valueB is the integer value used to set the initial contents of the
     second jug (B).
   * @pre. 0<=valueA<=4 and 0<=valueB<=3
   * @post. contentsA is set to valueA, and contentsB is set to valueB.
   */
  public JugsState(int valueA, int valueB)  {
    this.contentsA = valueA;
    this.contentsB = valueB;
    if (!repOK()) {
      throw new IllegalArgumentException("invalid values");
    }
  }
  
  /**
   * Get the contents of Jug A.
   * @return the contentsA
   */
  public int getContentsA() {
    return contentsA;
  }

  /**
   * Set the contents of Jug A.
   * @param contentsA the contentsA to set
   */
  public void setContentsA(int contentsA) {
    this.contentsA = contentsA;
  }

  /**
   * Get the contents of Jug B.
   * @return the contentsB
   */
  public int getContentsB() {
    return contentsB;
  }

  /**
   * Set the contents of Jug B.
   * @param contentsB the contentsB to set
   */
  public void setContentsB(int contentsB) {
    this.contentsB = contentsB;
  }

  /** Constructor for JugsState class. It initializes the contents of the 
   * jugs with the provided parameters.
   * @param valueA is the integer value used to set the initial contents of the
     first jug (A).
   * @param valueB is the integer value used to set the initial contents of the
     second jug (B).
   * @param parent is the parent state of this.
   * @pre. 0<=valueA<=4 and 0<=valueB<=3
   * @post. contentsA is set to valueA, and contentsB is set to valueB.
   */
  public JugsState(int valueA, int valueB, JugsState parent)  {
    this.contentsA = valueA;
    this.contentsB = valueB;
    this.parent = parent;
    if (!repOK()) {
      throw new IllegalArgumentException("invalid values");
    }
  }
 
  /**
   * This State is field by field copied.
   * @return a copy of this JugsState.
   */
  public JugsState clone() {
    JugsState clon = new JugsState(this.contentsA,this.contentsB, this.parent);
    return clon;
  }
  /** 
   * Returns the parent of the current state.
   * @return the parent of the current state or null if this does not have a parent.
   */
  
  @Override
  public State getParent() {
    return parent;
    
  }
  
  /** 
   * Set the parent of the current state. This method
   * must be implemented by all concrete classes implementing State.
   * @param parent to be set to the current state.
   */
  public void setParent(JugsState parent) {
    this.parent = parent;
  }
  
    
  /** 
   * Indicates whether a given state is a successful state, in the context of
   * the current problem. Concrete implementations of AbstractSearchProblem 
   * must implement this routine, to indicate when the search has been  
     successful.
   * @return true iff s is a successful state.
   */
  public  boolean isSuccess() {
    return (this.contentsA == 2 || this.contentsB == 2);
    
  }

  
  /** 
   * Checks whether 'this' is equal to another state. This must be implemented
   * by every concrete class implementing State.
   * @param other State object to compare with this.
   * @return true iff 'this' is equal, as a state, to 'other'.
   */
  @Override
  public  boolean equals(Object other) {
    if (other == this) {
      return true;
    }

    if (!(other instanceof JugsState)) {
      return false;
    }

    JugsState otherOne = (JugsState) other;

    return otherOne.contentsA == this.contentsA && otherOne.contentsB == this.contentsB;
    
  }
  
  /**
   * Returns a hash code value for the the concrete State object. 
   * This method is supported for the benefit of hashtables such as those 
   * provided by java.util.Hashtable.
   * @return a hash code value for the concrete State object.
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + this.contentsA;
    result = prime * result + this.contentsB;
    return result;
    
  }

  /** 
   * Returns a representation as a string of the current state. This method
   * must be implemented by all concrete classes implementing State.
   * @return a String representation of the current state.
   */
  @Override
  public  String toString() {
    return "(" + this.contentsA + ", " + this.contentsB + ")";
    
  }
  


}
