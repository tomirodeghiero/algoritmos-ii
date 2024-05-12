package examples.jugs;

import engine.BreadthFirstEngine;

import java.util.List;

public class JugsSearchApp {
  /**
   * Main for Jugs App.
   * @param args contents Jug A and contents Jug B are expected.
   */
  
  public static void main(String[] args) {
    
    if ((args.length > 2) || (args.length == 0)) {
      System.out.println("*** Usage: java JugsSearchApp <int> <int>");
    } else {
      
      int a = Integer.parseInt(args[0]);
      int b = Integer.parseInt(args[1]);
      
      JugsStateProblem sp1 = new JugsStateProblem(a,b);
      
           
      /*App using Breadth-first search */ 
      BreadthFirstEngine<JugsState,JugsStateProblem> engineBfs = 
          new BreadthFirstEngine<JugsState,JugsStateProblem>(sp1);
      JugsState successBfS = engineBfs.performSearch();
      System.out.println();     
      System.out.println("*** Result using Breadth-first search ***");
      System.out.println("Solution found? " + successBfS.toString());
      if (! (successBfS == null)) {
        System.out.print("Path to goal: ");
        List<JugsState> pathBfS = engineBfs.getPath();
        for (int i = 0; i < pathBfS.size(); i++) {
          JugsState current = (JugsState) pathBfS.get(i);
          System.out.print(current.toString());
        }
        System.out.println();
      }
      engineBfs.report();
      
      /*App using Breadth-first search */ 
          

    }
    
  } 
    
}
