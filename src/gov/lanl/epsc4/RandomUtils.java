package gov.lanl.epsc4;

public class RandomUtils {

    // --- Static state for ran2 ---
    private static final int NTAB = 32;
    private static int idum2 = 123456789;
    private static int iy = 0;
    private static int[] iv = new int[NTAB + 1]; // 1-based

    // --- Constants for ran2 ---
    private static final int IM1 = 2147483563, IM2 = 2147483399;
    private static final double AM = (1.0 / IM1);
    private static final int IMM1 = IM1 - 1;
    private static final int IA1 = 40014, IA2 = 40692;
    private static final int IQ1 = 53668, IQ2 = 52774;
    private static final int IR1 = 12211, IR2 = 3791;
    private static final int NDIV = (1 + IMM1 / NTAB);
    private static final double EPS = 1.2e-7;
    private static final double RNMX = (1.0 - EPS);

    /**
     * Full translation of the ran2 function.
     * @param idum A mutable wrapper for the seed.
     * @return A random double between 0.0 and 1.0.
     */
    public static double ran2(IntHolder idum) {
        if (idum.value <= 0) {
            idum.value = Math.max(-idum.value, 1);
            idum2 = idum.value;
            for (int j = NTAB + 8; j >= 1; j--) {
                int k = idum.value / IQ1;
                idum.value = IA1 * (idum.value - k * IQ1) - k * IR1;
                if (idum.value < 0) idum.value += IM1;
                if (j <= NTAB) iv[j] = idum.value;
            }
            iy = iv[1];
        }

        int k = idum.value / IQ1;
        idum.value = IA1 * (idum.value - k * IQ1) - k * IR1;
        if (idum.value < 0) idum.value += IM1;
        k = idum2 / IQ2;
        idum2 = IA2 * (idum2 - k * IQ2) - k * IR2;
        if (idum2 < 0) idum2 += IM2;

        int j = 1 + iy / NDIV;
        iy = iv[j] - idum2;
        iv[j] = idum.value;
        if (iy < 1) iy += IMM1;
        return Math.min(AM * iy, RNMX);
    }
}
