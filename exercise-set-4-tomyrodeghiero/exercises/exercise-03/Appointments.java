import java.util.*;

public class Appointments {

    // Clase estática interna para representar una cita
    static class Appointment {
        int start, end, priority; // Hora de inicio, fin y prioridad de la cita
        String description; // Descripción de la cita

        public Appointment(String description, int start, int end, int priority) {
            this.description = description;
            this.start = start;
            this.end = end;
            this.priority = priority;
        }
    }

    /**
     * Método para seleccionar las citas de mayor prioridad que no se solapen.
     *
     * @param appointments Lista de citas disponibles.
     * @return Lista de citas seleccionadas.
     */
    public static List<Appointment> selectAppointments(List<Appointment> appointments) {
        // Ordenar las citas por prioridad en orden descendente usando un comparador anónimo
        Collections.sort(appointments, new Comparator<Appointment>() {
            public int compare(Appointment a1, Appointment a2) {
                return Integer.compare(a2.priority, a1.priority); // Orden descendente por prioridad
            }
        });

        List<Appointment> result = new ArrayList<>(); // Lista para almacenar las citas seleccionadas
        Appointment lastSelected = null; // Última cita seleccionada

        // Recorrer la lista de citas ordenadas
        for (Appointment current : appointments) {
            // Si no hay citas seleccionadas o la cita actual no se solapa con la última seleccionada
            if (lastSelected == null || current.start >= lastSelected.end) {
                result.add(current); // Añadir la cita a la lista de resultados
                lastSelected = current; // Actualizar la última cita seleccionada
            }
        }

        return result; // Retornar la lista de citas seleccionadas
    }

    public static void main(String[] args) {
        List<Appointment> appointments = new ArrayList<>();
        // Añadir citas a la lista
        appointments.add(new Appointment("Cita 1", 9, 11, 5));
        appointments.add(new Appointment("Cita 2", 10, 12, 1));
        appointments.add(new Appointment("Cita 3", 11, 13, 4));
        appointments.add(new Appointment("Cita 4", 12, 14, 8));
        appointments.add(new Appointment("Cita 5", 13, 15, 3));

        // Seleccionar las citas utilizando el método Greedy
        List<Appointment> selectedAppointments = selectAppointments(appointments);

        // Imprimir las citas seleccionadas
        for (Appointment app : selectedAppointments) {
            System.out.println(app.description + " (" + app.start + " - " + app.end + "), Prioridad: " + app.priority);
        }
    }
}
