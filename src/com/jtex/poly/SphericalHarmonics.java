/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.poly;

/**
 *
 * @author hios
 */
import com.jtex.arrays.Array1C;

public class SphericalHarmonics {

    public static void main(String argv[]) {

        int l = 31;
        double theta = 1.5;
        double phi = .25;

        Y(l, theta, phi).print();
    }

    /**
     * Computes spherical harmonic of order l, index m at colatitude theta and
     * longitude phi.
     *
     * @param l The spherical harmonic order.
     * @param theta The angle of colatitude.
     * @param phi The angle of longitude.
     *
     * @return Y_lm(theta, phi)
     */
    public static Array1C Y(int l, double theta, double phi) {

        double[] re = new double[2 * l + 1];
        double[] im = new double[2 * l + 1];
        double th = 0;
        double cl = 0;
        double sl = 0;

        double ct = Math.cos(theta);

        for (int m = 0; m < l + 1; m++) {
            //Associated Legendre poly of cos(theta)
            th = (((m % 2) > 0) ? -1 : 1) * factor(l, m) * legendre(l, m, ct);
            cl = Math.cos(m * phi) * th;
            sl = Math.sin(m * phi) * th;
            re[l - m] = cl;
            re[l + m] = cl;
            im[l - m] = sl;
            im[l + m] = -sl;

        }
        return new Array1C(re, im);
    }

    /**
     * Computes the factor of the Legendre polynomial used to compute the
     * spherical harmonics.
     *
     * @param l Spherical harmonic order
     * @param m Spherical harmonic index
     *
     * @return The scaling factor.
     */
    private static double factor(int l, int m) {
        double factor = 1D;
        for (double i = l - m + 1; i <= l + m; i++) {
            factor *= i;
        }
        return Math.sqrt((2D * l + 1D) / (4D * Math.PI) / factor);
    }

    /**
     * NRC method for computing associated Legendre polynomial values.
     *
     * @param l The order of the polynomial
     * @param m The index of the polynomial
     * @param x The argument.
     *
     * @return the value of the polynomial at x.
     */
    public static double legendre(int l, int m, double x) {
        double Plm = 1D;
        if (m > 0) {
            double somx2 = Math.sqrt((1D - x) * (1D + x));
            for (int ll = 0; ll < m; ll++) {
                Plm *= -(2 * ll + 1) * somx2;
            }
        }
        if (l == m) {
            return Plm;
        } else {
            double Plm1 = x * (2 * m + 1) * Plm;
            double Plm2 = Plm;

            if (l == (m + 1)) {
                return Plm1;
            } else {
                for (int ll = m + 2; ll <= l; ll++) {
                    Plm = (x * (2 * ll - 1) * Plm1 - (ll + m - 1) * Plm2) / (ll - m);
                    Plm2 = Plm1;
                    Plm1 = Plm;
                }
                return Plm;
            }
        }
    }

}
