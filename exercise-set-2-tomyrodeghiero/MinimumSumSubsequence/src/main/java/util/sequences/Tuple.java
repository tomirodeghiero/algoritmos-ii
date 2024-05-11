package util.sequences;


public class Tuple<F,S,T> {

	private F first;
	private S second;
	private T third;

	/** 
   	* Default Constructor.
   	*/
  	public Tuple() {
	    this.first = null;
	    this.second = null;
	    this.third = null;
	}
  
	/** 
	* Constructor Parametrizado.
	*/
	public Tuple(F first, S second, T third) {
		this.first = first;
		this.second = second;
		this.third = third;
	}

	public F getFirst(){
		return first;
	}

	public void setFirst(F first){
		this.first = first;
	}

	public S getSecond(){
		return second;
	}

	public void setSecond(S second){
		this.second = second;
	}

	public T getThird(){
		return third;
	}

	public void setThird(T third){
		this.third = third;
	}

	/** 
	*Overriding toString() to convert Tuple objects to String. 
	*/
	@Override
	public String toString() {
		return ("(" + first.toString() + "," + second.toString() + "," + third.toString() + "," + ")");
	}
  
  	/** 
  	* Overriding equals() to compare two Tuple objects.
  	*/
  	@Override
  	public boolean equals(Object other) {
	    if (other == null) {
	      return false;
	    } 
	    if (!(other instanceof Tuple)) {
	      return false;
	    }
	    Tuple<F,S,T> actualOther = (Tuple<F,S,T>) other;
	    boolean equals = actualOther.getFirst().equals(this.first);
	    equals &= actualOther.getSecond().equals(this.second);
	    equals &= actualOther.getThird().equals(this.third);
	    return equals;    
	}

}