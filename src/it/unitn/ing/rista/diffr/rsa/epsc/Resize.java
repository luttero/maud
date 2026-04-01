package it.unitn.ing.rista.diffr.rsa.epsc;

import java.util.Arrays;

/**
 * Contains utility methods for resizing arrays, preserving contents.
 *
 * <p><b>IMPORTANT:</b> Java does not pass arrays by reference in the same way
 * Fortran does. These methods create and return a *new* array. The caller
 * *must* re-assign the result.
 *
 * <p>Example:
 * <pre>
 * double[] myArray = new double[10];
 * // ...
 * myArray = Resize.resize_double1(myArray, 20); // Re-assign the returned array
 * </pre>
 */
public class Resize {

    /**
     * Resizes a 1D double array, preserving contents.
     * Initializes new elements to 0.0.
     *
     * @param var The original array.
     * @param n   The new size.
     * @return A new array of size n, or the original array if n was smaller.
     */
    public static double[] resize_double1(double[] var, int n) {
        if (var != null) {
            int this_size = var.length;
            if (this_size >= n) {
                return var; // Already big enough
            }
        }

        // Allocate new array (Java initializes to 0.0 by default)
        double[] newVar = new double[n];

        if (var != null) {
            // Copy data from old array (tmp) to new array
            int copyLength = Math.min(var.length, newVar.length);
            System.arraycopy(var, 0, newVar, 0, copyLength);
        }

        return newVar;
    }

    /**
     * Resizes a 2D double array, preserving contents.
     * Initializes new elements to 0.0.
     *
     * @param var The original array.
     * @param n1  The new size of the first dimension (rows).
     * @param n2  The new size of the second dimension (columns).
     * @return A new array of size [n1][n2], or the original if sizes were smaller.
     */
    public static double[][] resize_double2(double[][] var, int n1, int n2) {
        if (var != null) {
            int this_size1 = var.length;
            int this_size2 = (this_size1 > 0) ? var[0].length : 0;
            if (this_size1 >= n1 && this_size2 >= n2) {
                return var; // Already big enough
            }
        }

        // Allocate new array
        double[][] newVar = new double[n1][n2];

        if (var != null) {
            // Copy data
            int copyRows = Math.min(var.length, newVar.length);
            int copyCols = (var.length > 0) ? Math.min(var[0].length, newVar[0].length) : 0;

            if (copyCols > 0) {
                for (int i = 0; i < copyRows; i++) {
                    System.arraycopy(var[i], 0, newVar[i], 0, copyCols);
                }
            }
        }

        return newVar;
    }

    /**
     * Resizes a 3D double array, preserving contents.
     */
    public static double[][][] resize_double3(double[][][] var, int n1, int n2, int n3) {
        if (var != null) {
            int this_size1 = var.length;
            int this_size2 = (this_size1 > 0) ? var[0].length : 0;
            int this_size3 = (this_size2 > 0) ? var[0][0].length : 0;
            if (this_size1 >= n1 && this_size2 >= n2 && this_size3 >= n3) {
                return var; // Already big enough
            }
        }

        // Allocate new array
        double[][][] newVar = new double[n1][n2][n3];

        if (var != null) {
            // Copy data
            int copyL1 = Math.min(var.length, newVar.length);
            int copyL2 = (var.length > 0) ? Math.min(var[0].length, newVar[0].length) : 0;
            int copyL3 = (copyL2 > 0) ? Math.min(var[0][0].length, newVar[0][0].length) : 0;

            if (copyL3 > 0) {
                for (int i = 0; i < copyL1; i++) {
                    for (int j = 0; j < copyL2; j++) {
                        System.arraycopy(var[i][j], 0, newVar[i][j], 0, copyL3);
                    }
                }
            }
        }

        return newVar;
    }

    /**
     * Resizes a 4D double array, preserving contents.
     */
    public static double[][][][] resize_double4(double[][][][] var, int n1, int n2, int n3, int n4) {
        if (var != null) {
            int s1 = var.length;
            int s2 = (s1 > 0) ? var[0].length : 0;
            int s3 = (s2 > 0) ? var[0][0].length : 0;
            int s4 = (s3 > 0) ? var[0][0][0].length : 0;
            if (s1 >= n1 && s2 >= n2 && s3 >= n3 && s4 >= n4) {
                return var;
            }
        }

        double[][][][] newVar = new double[n1][n2][n3][n4];

        if (var != null) {
            int c1 = Math.min(var.length, newVar.length);
            int c2 = (c1 > 0) ? Math.min(var[0].length, newVar[0].length) : 0;
            int c3 = (c2 > 0) ? Math.min(var[0][0].length, newVar[0][0].length) : 0;
            int c4 = (c3 > 0) ? Math.min(var[0][0][0].length, newVar[0][0][0].length) : 0;

            if (c4 > 0) {
                for (int i = 0; i < c1; i++) {
                    for (int j = 0; j < c2; j++) {
                        for (int k = 0; k < c3; k++) {
                            System.arraycopy(var[i][j][k], 0, newVar[i][j][k], 0, c4);
                        }
                    }
                }
            }
        }
        return newVar;
    }

    /**
     * Resizes a 5D double array, preserving contents.
     */
    public static double[][][][][] resize_double5(double[][][][][] var, int n1, int n2, int n3, int n4, int n5) {
        if (var != null) {
            int s1 = var.length;
            int s2 = (s1 > 0) ? var[0].length : 0;
            int s3 = (s2 > 0) ? var[0][0].length : 0;
            int s4 = (s3 > 0) ? var[0][0][0].length : 0;
            int s5 = (s4 > 0) ? var[0][0][0][0].length : 0;
            if (s1 >= n1 && s2 >= n2 && s3 >= n3 && s4 >= n4 && s5 >= n5) {
                return var;
            }
        }

        double[][][][][] newVar = new double[n1][n2][n3][n4][n5];

        if (var != null) {
            int c1 = Math.min(var.length, newVar.length);
            int c2 = (c1 > 0) ? Math.min(var[0].length, newVar[0].length) : 0;
            int c3 = (c2 > 0) ? Math.min(var[0][0].length, newVar[0][0].length) : 0;
            int c4 = (c3 > 0) ? Math.min(var[0][0][0].length, newVar[0][0][0].length) : 0;
            int c5 = (c4 > 0) ? Math.min(var[0][0][0][0].length, newVar[0][0][0][0].length) : 0;

            if (c5 > 0) {
                for (int i = 0; i < c1; i++) {
                    for (int j = 0; j < c2; j++) {
                        for (int k = 0; k < c3; k++) {
                            for (int l = 0; l < c4; l++) {
                                System.arraycopy(var[i][j][k][l], 0, newVar[i][j][k][l], 0, c5);
                            }
                        }
                    }
                }
            }
        }
        return newVar;
    }

    /**
     * Resizes a 3D double array based on Fortran-style lower/upper bounds.
     *
     * <p><b>CRITICAL TRANSLATION NOTE:</b>
     * Fortran's {@code allocate(var(n1_0:n1, n2_0:n2, n3_0:n3))} creates an array
     * with custom lower bounds. Java *does not support this*; arrays are *always*
     * 0-indexed.
     *
     * <p>This method creates a 0-indexed Java array with the correct *lengths*:
     * <ul>
     * <li>{@code len1 = n1 - n1_0 + 1}
     * <li>{@code len2 = n2 - n2_0 + 1}
     * <li>{@code len3 = n3 - n3_0 + 1}
     * </ul>
     *
     * <p>All calling code must be refactored to map Fortran indices to Java indices:
     * <br>
     * Fortran: {@code var(i, j, k)}
     * <br>
     * Java:    {@code var[i - n1_0][j - n2_0][k - n3_0]}
     *
     * <p>The copy logic here is based on the *other* resize functions (copying from
     * the start of the arrays), as the original Fortran code for this
     * specific subroutine's copy operation appeared to be buggy.
     */
    public static double[][][] resize_double0(double[][][] var, int n1_0, int n1, int n2_0, int n2, int n3_0, int n3) {
        
        int len1 = n1 - n1_0 + 1;
        int len2 = n2 - n2_0 + 1;
        int len3 = n3 - n3_0 + 1;

        if (len1 <= 0 || len2 <= 0 || len3 <= 0) {
            // Or throw an IllegalArgumentException
            return new double[0][0][0]; 
        }

        if (var != null) {
            int this_size1 = var.length;
            int this_size2 = (this_size1 > 0) ? var[0].length : 0;
            int this_size3 = (this_size2 > 0) ? var[0][0].length : 0;
            if (this_size1 >= len1 && this_size2 >= len2 && this_size3 >= len3) {
                return var; // Already big enough
            }
        }

        // Allocate new array with 0-based index and correct lengths
        double[][][] newVar = new double[len1][len2][len3];

        if (var != null) {
            // Copy data
            int copyL1 = Math.min(var.length, newVar.length);
            int copyL2 = (var.length > 0) ? Math.min(var[0].length, newVar[0].length) : 0;
            int copyL3 = (copyL2 > 0) ? Math.min(var[0][0].length, newVar[0][0].length) : 0;

            if (copyL3 > 0) {
                for (int i = 0; i < copyL1; i++) {
                    for (int j = 0; j < copyL2; j++) {
                        System.arraycopy(var[i][j], 0, newVar[i][j], 0, copyL3);
                    }
                }
            }
        }

        return newVar;
    }

    /**
     * Resizes a 1D int array, preserving contents.
     * Initializes new elements to 0.
     *
     * @param var The original array.
     * @param n   The new size.
     * @return A new array of size n, or the original array if n was smaller.
     */
    public static int[] resize_int1(int[] var, int n) {
        if (var != null) {
            int this_size = var.length;
            if (this_size >= n) {
                return var; // Already big enough
            }
        }

        // Allocate new array (Java initializes to 0 by default)
        int[] newVar = new int[n];

        if (var != null) {
            // Copy data from old array (tmp) to new array
            int copyLength = Math.min(var.length, newVar.length);
            System.arraycopy(var, 0, newVar, 0, copyLength);
        }

        return newVar;
    }

    /**
     * Resizes a 2D int array, preserving contents.
     * Initializes new elements to 0.
     *
     * @param var The original array.
     * @param n1  The new size of the first dimension (rows).
     * @param n2  The new size of the second dimension (columns).
     * @return A new array of size [n1][n2], or the original if sizes were smaller.
     */
    public static int[][] resize_int2(int[][] var, int n1, int n2) {
        if (var != null) {
            int this_size1 = var.length;
            int this_size2 = (this_size1 > 0) ? var[0].length : 0;
            if (this_size1 >= n1 && this_size2 >= n2) {
                return var; // Already big enough
            }
        }

        // Allocate new array
        int[][] newVar = new int[n1][n2];

        if (var != null) {
            // Copy data
            int copyRows = Math.min(var.length, newVar.length);
            int copyCols = (var.length > 0) ? Math.min(var[0].length, newVar[0].length) : 0;

            if (copyCols > 0) {
                for (int i = 0; i < copyRows; i++) {
                    System.arraycopy(var[i], 0, newVar[i], 0, copyCols);
                }
            }
        }

        return newVar;
    }
}

