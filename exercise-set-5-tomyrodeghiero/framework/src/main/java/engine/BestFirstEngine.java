package engine;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;

import conventionalsearch.Engine;
import conventionalsearch.State;
import conventionalsearch.StateProblem;

public class BestFirstEngine<S extends State, P extends StateProblem<S>> implements Engine<S, P> {

    private P sp;
    private List<S> path;
    private Comparator<S> statePriorityComparator;

    public BestFirstEngine(P sp, Comparator<S> statePriorityComparator) {
        this.sp = sp;
        this.statePriorityComparator = statePriorityComparator;
        path = new LinkedList<S>();
    }

    public S performSearch() {
        PriorityQueue<S> openSet = new PriorityQueue<>(11, statePriorityComparator);
        openSet.add(sp.initialState());
        S goal = null;
        boolean found = false;

        while (!openSet.isEmpty() && !found) {
            S current = openSet.poll();
            if (current.isSuccess()) {
                goal = current;
                found = true;
            } else {
                List<S> successors = sp.getSuccessors(current);
                for (S succ : successors) {
                    openSet.add(succ);
                }
            }
        }
        if (goal != null) {
            reconstructPath(goal);
        }
        return goal;
    }

    private void reconstructPath(S goal) {
        S s = goal;
        while (s != null) {
            path.add(0, s);
            s = (S) s.getParent();
        }
    }

    public List<S> getPath() {
        return path;
    }

    public void report() {
        System.out.println("Length of path to state when search finished: " + path.size());
    }
}
