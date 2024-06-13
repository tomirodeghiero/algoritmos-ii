// Interfaz Triangulo
interface Triangulo {
    void descripcion();
}

// Clase concreta Equilatero
class Equilatero implements Triangulo {
    @Override
    public void descripcion() {
        System.out.println("Soy un triángulo equilátero");
    }
}

// Clase concreta Isosceles
class Isosceles implements Triangulo {
    @Override
    public void descripcion() {
        System.out.println("Soy un triángulo isósceles");
    }
}

// Clase concreta Escaleno
class Escaleno implements Triangulo {
    @Override
    public void descripcion() {
        System.out.println("Soy un triángulo escaleno");
    }
}

// Clase abstracta TrianguloFactory
abstract class TrianguloFactory {
    public abstract Triangulo crearTriangulo();
}

// Clase concreta EquilateroFactory
class EquilateroFactory extends TrianguloFactory {
    @Override
    public Triangulo crearTriangulo() {
        return new Equilatero();
    }
}

// Clase concreta IsoscelesFactory
class IsoscelesFactory extends TrianguloFactory {
    @Override
    public Triangulo crearTriangulo() {
        return new Isosceles();
    }
}

// Clase concreta EscalenoFactory
class EscalenoFactory extends TrianguloFactory {
    @Override
    public Triangulo crearTriangulo() {
        return new Escaleno();
    }
}

// Clase principal para probar la implementación
public class Main {
    public static void main(String[] args) {
        TrianguloFactory factoryEquilatero = new EquilateroFactory();
        Triangulo trianguloEquilatero = factoryEquilatero.crearTriangulo();
        trianguloEquilatero.descripcion();

        TrianguloFactory factoryIsosceles = new IsoscelesFactory();
        Triangulo trianguloIsosceles = factoryIsosceles.crearTriangulo();
        trianguloIsosceles.descripcion();

        TrianguloFactory factoryEscaleno = new EscalenoFactory();
        Triangulo trianguloEscaleno = factoryEscaleno.crearTriangulo();
        trianguloEscaleno.descripcion();
    }
}
