package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Contains the full translation of the ran2 random number generator.
 * It uses and modifies the static state variables in ModRan2.
 */
public class RandomUtils {

    private static final int IM1 = 2147483563;
    private static final int IM2 = 2147483399;
    private static final double AM = (1.0 / IM1);
    private static final int IMM1 = IM1 - 1;
    private static final int IA1 = 40014;
    private static final int IA2 = 40692;
    private static final int IQ1 = 53668;
    private static final int IQ2 = 52774;
    private static final int IR1 = 12211;
    private static final int IR2 = 3791;
    private static final int NDIV = (1 + IMM1 / ModRan2.NTAB);
    private static final double EPS = 1.2e-7;
    private static final double RNMX = (1.0 - EPS);

    /**
     * Full translation of the ran2 function.
     * <p>
     * Note: This function modifies the 'idum' IntHolder (pass-by-reference)
     * and the static fields in ModRan2.
     *
     * @param idum A mutable wrapper for the seed.
     * @return A random double between 0.0 and 1.0.
     */
    public static double ran2(IntHolder idum) {

        if (idum.value <= 0) {
            // Initialize
            idum.value = Math.max(-idum.value, 1);
            ModRan2.idum2 = idum.value;
            for (int j = ModRan2.NTAB + 8; j >= 1; j--) {
                int k = idum.value / IQ1;
                idum.value = IA1 * (idum.value - k * IQ1) - k * IR1;
                if (idum.value < 0) idum.value += IM1;
                if (j <= ModRan2.NTAB) ModRan2.iv[j] = idum.value;
            }
            ModRan2.iy = ModRan2.iv[1];
        }

        int k = idum.value / IQ1;
        idum.value = IA1 * (idum.value - k * IQ1) - k * IR1;
        if (idum.value < 0) idum.value += IM1;

        k = ModRan2.idum2 / IQ2;
        ModRan2.idum2 = IA2 * (ModRan2.idum2 - k * IQ2) - k * IR2;
        if (ModRan2.idum2 < 0) ModRan2.idum2 += IM2;

        int j = 1 + ModRan2.iy / NDIV;
        ModRan2.iy = ModRan2.iv[j] - ModRan2.idum2;
        ModRan2.iv[j] = idum.value;
        if (ModRan2.iy < 1) ModRan2.iy += IMM1;

        return Math.min(AM * ModRan2.iy, RNMX);
    }
}

