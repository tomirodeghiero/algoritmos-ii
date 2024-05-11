package util;

import java.util.*;

public class ListUtilsExercise07 {

    // Método para calcular todas las permutaciones de una lista
    public static <T> List<List<T>> generatePermutations(List<T> original) {
        if (original.isEmpty()) {
            List<List<T>> result = new ArrayList<>();
            result.add(new ArrayList<>());
            return result;
        }

        T firstElement = original.remove(0);
        List<List<T>> returnValue = new ArrayList<>();
        List<List<T>> permutations = generatePermutations(original);

        for (List<T> smallerPermutated : permutations) {
            for (int index = 0; index <= smallerPermutated.size(); index++) {
                List<T> temp = new ArrayList<>(smallerPermutated);
                temp.add(index, firstElement);
                returnValue.add(temp);
            }
        }
        return returnValue;
    }

    // Método para calcular todos los subconjuntos de un conjunto
    public static <T> List<List<T>> generateSubsets(List<T> set) {
        List<List<T>> allSubsets = new ArrayList<>();
        allSubsets.add(new ArrayList<>()); // add the empty set

        for (T element : set) {
            List<List<T>> moreSubsets = new ArrayList<>();
            for (List<T> subset : allSubsets) {
                List<T> newSubset = new ArrayList<>(subset);
                newSubset.add(element);
                moreSubsets.add(newSubset);
            }
            allSubsets.addAll(moreSubsets);
        }
        return allSubsets;
    }

    // Método para calcular todas las sublistas de una lista
    public static <T> List<List<T>> generateSublists(List<T> list) {
        List<List<T>> sublists = new ArrayList<>();
        sublists.add(new ArrayList<>());

        for (int start = 0; start < list.size(); start++) {
            for (int end = start + 1; end <= list.size(); end++) {
                sublists.add(new ArrayList<>(list.subList(start, end)));
            }
        }

        return sublists;
    }

    // Método para verificar si dos cadenas son anagramas utilizando permutaciones
    public static boolean areAnagrams(String s1, String s2) {
        char[] chars1 = s1.toCharArray();
        char[] chars2 = s2.toCharArray();
        Arrays.sort(chars1);
        Arrays.sort(chars2);
        return Arrays.equals(chars1, chars2);
    }

    // Método para verificar si existe un subconjunto cuya suma sea igual a n
    public static boolean hasSubsetWithSumN(List<Integer> set, int n) {
        List<List<Integer>> allSubsets = generateSubsets(set);
        for (List<Integer> subset : allSubsets) {
            int sum = 0;
            for (Integer num : subset) {
                sum += num;
            }
            if (sum == n) {
                return true;
            }
        }
        return false;
    }

    // Método para verificar si p es subcadena de s
    public static boolean isSubstring(String p, String s) {
        // Si p es más largo que s, p no puede ser subcadena de s.
        if (p.length() > s.length()) {
            return false;
        }

        // Si p está vacío, es subcadena de cualquier cadena incluida la vacía.
        if (p.isEmpty()) {
            return true;
        }

        // Recorrer la cadena s.
        for (int i = 0; i <= s.length() - p.length(); i++) {
            // Asumir que encontramos una subcadena hasta que se demuestre lo contrario.
            boolean found = true;

            // Comprobar si la subcadena de s comenzando en i es igual a p.
            for (int j = 0; j < p.length(); j++) {
                if (s.charAt(i + j) != p.charAt(j)) {
                    found = false;
                    break; // Salir del bucle interno si algún carácter no coincide.
                }
            }

            // Si encontramos una subcadena, retornar true.
            if (found) {
                return true;
            }
        }

        // Si terminamos de recorrer s sin encontrar p, retornar false.
        return false;
    }

    // Método para verificar si p es subsecuencia de s
    public static boolean isSubsequence(String s, String p) {
        int indexP = 0;
        for (int indexS = 0; indexP < p.length() && indexS < s.length(); indexS++) {
            if (p.charAt(indexP) == s.charAt(indexS)) {
                indexP++;
            }
        }
        return indexP == p.length();
    }

    // Método principal para probar las funcionalidades
    public static void main(String[] args) {
        // Generar permutaciones de una lista
        List<Integer> fixedList = Arrays.asList(1, 2, 3);
        List<Integer> list = new ArrayList<>(fixedList); // Ahora 'list' es modificable
        System.out.println("Permutaciones de " + list + ": " + generatePermutations(list));

        // Generar subconjuntos de un conjunto
        list = new ArrayList<>(fixedList); // Reiniciar la lista
        System.out.println("Subconjuntos de " + list + ": " + generateSubsets(list));

        // Generar sublistas de una lista
        list = new ArrayList<>(fixedList); // Reiniciar la lista
        System.out.println("Sublistas de " + list + ": " + generateSublists(list));

        // Probar si dos secuencias son anagramas
        String s1 = "listen";
        String s2 = "silent";
        System.out.println("¿Son anagramas? " + areAnagrams(s1, s2));

        // Probar si existe un subconjunto de s cuya suma sea n
        List<Integer> s = Arrays.asList(9, 34, 4, 12, 10, 1);
        int n = 9;
        System.out.println("¿Existe un subconjunto con suma " + n + "? " + hasSubsetWithSumN(s, n));

        // Probar si p es subcadena de s
        String p = "hello";
        String str = "helloworld";
        System.out.println("¿Es '" + p + "' subcadena de '" + str + "'? " + isSubstring(p, str));

        // Probar si p es subsecuencia de s
        p = "hlo";
        System.out.println("¿Es '" + p + "' subsecuencia de '" + str + "'? " + isSubsequence(str, p));
    }
}