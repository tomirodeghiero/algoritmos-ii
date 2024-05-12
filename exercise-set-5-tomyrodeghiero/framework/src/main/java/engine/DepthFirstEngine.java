package engine;

import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import conventionalsearch.Engine;
import conventionalsearch.State;
import conventionalsearch.StateProblem;

public class DepthFirstEngine<S extends State, P extends StateProblem<S>> implements Engine<S, P> {

    private P sp;
    private List<S> path;

    public DepthFirstEngine(P sp) {
        this.sp = sp;
        path = new LinkedList<S>();
    }

    public S performSearch() {
        Stack<S> stack = new Stack<S>();
        stack.push(sp.initialState());
        S goal = null;
        boolean found = false;

        while (!stack.isEmpty() && !found) {
            S current = stack.pop();
            if (current.isSuccess()) {
                goal = current;
                found = true;
            } else {
                List<S> succs = sp.getSuccessors(current);
                for (S s : succs) {
                    stack.push(s);
                }
            }
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

    public List<S> getPath() {
        return path;
    }

    public void report() {
        System.out.println("Length of path to state when search finished: " + path.size());
    }
}
