package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the common_constants module.
 *
 * <p>The Fortran 'precisions' module (dp = kind(1.d0))
 * simply maps to the Java 'double' type.
 */
public final class CommonConstants {

    // Private constructor to prevent instantiation
    private CommonConstants() {}

    public static final double PI = 3.1415926535898;
    public static final double DEG_TO_RAD = PI / 180.0;
    public static final double RAD_TO_DEG = 180.0 / PI;
    public static final double PI2 = 2.0 * PI;
    public static final double PI_2 = 0.5 * PI;
    public static final double BOLTZ = 1.380622d-23; // Boltzmann's constant
}
