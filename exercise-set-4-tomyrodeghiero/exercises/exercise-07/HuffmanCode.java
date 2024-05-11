import java.util.PriorityQueue;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

public class HuffmanCode {

    // Clase interna para el árbol
    static class HuffmanNode {
        char data;
        int frequency;
        HuffmanNode left, right;

        HuffmanNode(char data, int frequency) {
            this.data = data;
            this.frequency = frequency;
            this.left = null;
            this.right = null;
        }
    }

    // Comparador para la cola de prioridad
    static class MyComparator implements Comparator<HuffmanNode> {
        public int compare(HuffmanNode x, HuffmanNode y) {
            return x.frequency - y.frequency;
        }
    }

    // Método para construir el árbol de Huffman y generar los códigos
    public static void generateHuffmanTree(char[] charArray, int[] charFreq) {
        PriorityQueue<HuffmanNode> q = new PriorityQueue<HuffmanNode>(charArray.length, new MyComparator());

        for (int i = 0; i < charArray.length; i++) {
            HuffmanNode hn = new HuffmanNode(charArray[i], charFreq[i]);
            q.add(hn);
        }

        HuffmanNode root = null;

        while (q.size() > 1) {
            HuffmanNode x = q.poll();
            HuffmanNode y = q.poll();

            HuffmanNode f = new HuffmanNode('-', x.frequency + y.frequency);
            f.left = x;
            f.right = y;
            root = f;

            q.add(f);
        }

        Map<Character, String> huffmanCode = new HashMap<>();
        printCode(root, "", huffmanCode);
        System.out.println("Huffman Codes: " + huffmanCode);
    }

    // Método para imprimir los códigos de cada carácter
    private static void printCode(HuffmanNode root, String s, Map<Character, String> huffmanCode) {
        if (root.left == null && root.right == null && Character.isLetter(root.data)) {
            huffmanCode.put(root.data, s);
            return;
        }
        printCode(root.left, s + "0", huffmanCode);
        printCode(root.right, s + "1", huffmanCode);
    }

    public static void main(String[] args) {
        char[] charArray = { 'a', 'b', 'c', 'd', 'e', 'f' };
        int[] charFreq = { 5, 9, 12, 13, 16, 45 };

        generateHuffmanTree(charArray, charFreq);
    }
}
