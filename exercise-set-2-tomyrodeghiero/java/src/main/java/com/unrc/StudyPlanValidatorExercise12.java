package com.unrc;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class StudyPlanValidatorExercise12 {
    private Map<String, List<String>> courseGraph;
    private Set<String> visiting;
    private Set<String> visited;
    private Deque<String> courseOrder;
    private boolean hasCycle;

    public StudyPlanValidatorExercise12() {
        courseGraph = new HashMap<>();
        visiting = new HashSet<>();
        visited = new HashSet<>();
        courseOrder = new ArrayDeque<>();
        hasCycle = false;
    }

    // Función para agregar correlatividades al grafo
    public void addPrerequisite(String course, String prerequisite) {
        courseGraph.computeIfAbsent(prerequisite, k -> new ArrayList<>()).add(course);
    }

    // DFS para detectar ciclos y obtener un orden posible de cursado
    private void dfs(String course) {
        if (visiting.contains(course)) {
            hasCycle = true; // Ciclo detectado
            return;
        }

        if (visited.contains(course)) {
            return;
        }

        visiting.add(course);

        for (String neighbor : courseGraph.getOrDefault(course, new ArrayList<>())) {
            dfs(neighbor);
        }

        visiting.remove(course);
        visited.add(course);
        courseOrder.push(course); // Agregar al orden de cursado en postorden
    }

    // Verificar si el plan de estudios es consistente y retornar un orden de
    // cursado en caso de ser consistente
    public String validatePlan() {
        for (String course : courseGraph.keySet()) {
            if (!visited.contains(course)) {
                dfs(course);
            }
        }

        if (hasCycle) {
            return "Inconsistent plan, a cycle was detected.";
        } else {
            List<String> order = new ArrayList<>();
            while (!courseOrder.isEmpty()) {
                order.add(courseOrder.pop());
            }
            return "Consistent plan. Order: " + String.join(", ", order);
        }
    }

    public static void main(String[] args) {
        StudyPlanValidatorExercise12 validator = new StudyPlanValidatorExercise12();

        // Asumiendo que "course1" es un prerrequisito para "course2",
        // agregar correlatividades
        validator.addPrerequisite("course2", "course1");
        validator.addPrerequisite("course3", "course2");
        // validator.addPrerequisite("course1", "course3");

        // Verificar si el plan de estudios es consistente
        System.out.println(validator.validatePlan());
    }
}