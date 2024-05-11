package util;

import java.util.ArrayList;
import java.util.List;

public class UniqueElementsExercise09 {

    public static List<Integer> removeDuplicates(List<Integer> list) {
        List<Integer> resultList = new ArrayList<>();
        for (Integer item : list) {
            if (!resultList.contains(item)) {
                resultList.add(item);
            }
        }
        return resultList;
    }

    public static void main(String[] args) {
        List<Integer> numbers = List.of(1, 2, 5, 2, 3, 3, 4, 5, 6, 1, 7);
        List<Integer> uniqueNumbers = removeDuplicates(numbers);
        System.out.println(uniqueNumbers);
    }
}
