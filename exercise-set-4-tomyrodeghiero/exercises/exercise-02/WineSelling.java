public class WineSelling {
    private int[] prices;

    public WineSelling(int[] prices) {
        this.prices = prices;
    }

    // Método para calcular la máxima ganancia usando un enfoque Greedy
    public int maxProfitGreedy() {
        int n = prices.length;
        int start = 0;
        int end = n - 1;
        int year = 1;
        int totalProfit = 0;

        while (start <= end) {
            int profitStart = year * prices[start];
            int profitEnd = year * prices[end];
            if (profitStart > profitEnd) {
                totalProfit += profitStart;
                start++;
            } else {
                totalProfit += profitEnd;
                end--;
            }
            year++;
        }

        return totalProfit;
    }

    public static void main(String[] args) {
        int[] prices = { 1, 4, 1, 5 };
        WineSelling wp = new WineSelling(prices);
        System.out.println("Maximum profit (Greedy): " + wp.maxProfitGreedy());
    }
}
