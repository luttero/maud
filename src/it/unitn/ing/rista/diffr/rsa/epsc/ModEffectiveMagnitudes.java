package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_effective_magnitudes module.
 *
 * Holds static variables for calculating equivalent (von Mises-like)
 * magnitudes, pressure, volume, and work.
 * Arrays are 1-based to match Fortran indexing.
 */
public class ModEffectiveMagnitudes {

    // --- Work Arrays (1-based) ---
    /** Plastic strain (Voigt) */
    public static double[] etsspl = new double[7];
    /** Plastic strain increment (Voigt) */
    public static double[] etrsspl = new double[7];

    // --- Scalar Magnitudes ---
    /** Equivalent stress */
    public static double stsseq;
    /** Equivalent stress increment */
    public static double strsseq;
    /** Equivalent total strain */
    public static double etsseq;
    /** Equivalent total strain increment */
    public static double etrsseq;
    /** Equivalent plastic strain */
    public static double etsspleq;
    /** Equivalent plastic strain increment */
    public static double etrsspleq;
    /** Hydrostatic pressure */
    public static double pressure;
    /** Elastic strain energy (sigma:epsilon) */
    public static double stet;
    /** Volume-averaged elastic strain energy */
    public static double stetav;
    /** Volumetric strain (trace of strain tensor) */
    public static double volume;
    /** Accumulated plastic work (incremented) */
    public static double wplastic;
    /** Accumulated total work (incremented) */
    public static double wtotal;
}
