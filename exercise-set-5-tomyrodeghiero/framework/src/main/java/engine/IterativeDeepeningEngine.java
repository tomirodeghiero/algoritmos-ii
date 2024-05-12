package engine;

import java.util.LinkedList;
import java.util.List;

import conventionalsearch.Engine;
import conventionalsearch.State;
import conventionalsearch.StateProblem;

public class IterativeDeepeningEngine<S extends State, P extends StateProblem<S>> implements Engine<S, P> {

    private P sp;
    private List<S> path;

    public IterativeDeepeningEngine(P sp) {
        this.sp = sp;
        path = new LinkedList<S>();
    }

    public S performSearch() {
        int depth = 0;
        S goal = null;

        while (goal == null) {
            goal = depthLimitedSearch(sp.initialState(), depth);
            depth++;
        }
        if (goal != null) {
            S s = goal;
            while (s != null) {
                path.add(0, s);
                s = (S) s.getParent();
            }
        }
        return goal;
    }

    private S depthLimitedSearch(S current, int limit) {
        if (current.isSuccess())
            return current;
        else if (limit == 0)
            return null;
        else {
            List<S> succs = sp.getSuccessors(current);
            for (S s : succs) {
                S found = depthLimitedSearch(s, limit - 1);
                if (found != null)
                    return found;
            }
            return null;
        }
    }

    public List<S> getPath() {
        return path;
    }

    public void report() {
        System.out.println("Length of path to state when search finished: " + path.size());
    }
}
