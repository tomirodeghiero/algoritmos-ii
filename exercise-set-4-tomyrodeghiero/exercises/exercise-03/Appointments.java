import java.util.*;

public class Appointments {

    static class Appointment {
        int start, end, priority;
        String description;

        public Appointment(String description, int start, int end, int priority) {
            this.description = description;
            this.start = start;
            this.end = end;
            this.priority = priority;
        }
    }

    public static List<Appointment> selectAppointments(List<Appointment> appointments) {
        // Ordenar citas por prioridad usando un comparador anónimo
        Collections.sort(appointments, new Comparator<Appointment>() {
            public int compare(Appointment a1, Appointment a2) {
                return Integer.compare(a2.priority, a1.priority); // Orden descendente por prioridad
            }
        });

        List<Appointment> result = new ArrayList<>();
        Appointment lastSelected = null;

        for (Appointment current : appointments) {
            if (lastSelected == null || current.start >= lastSelected.end) {
                result.add(current);
                lastSelected = current;
            }
        }

        return result;
    }

    public static void main(String[] args) {
        List<Appointment> appointments = new ArrayList<>();
        appointments.add(new Appointment("Cita 1", 9, 11, 5));
        appointments.add(new Appointment("Cita 2", 10, 12, 1));
        appointments.add(new Appointment("Cita 3", 11, 13, 4));
        appointments.add(new Appointment("Cita 4", 12, 14, 8));
        appointments.add(new Appointment("Cita 5", 13, 15, 3));

        List<Appointment> selectedAppointments = selectAppointments(appointments);

        for (Appointment app : selectedAppointments) {
            System.out.println(app.description + " (" + app.start + " - " + app.end + "), Prioridad: " + app.priority);
        }
    }
}
