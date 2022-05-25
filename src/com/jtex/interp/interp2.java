/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.interp;

import java.util.Arrays;

/**
 *
 * @author hios
 */
public class interp2 {

    double[] xx;
    double[] yy;
    double[] data;
    int nx;
    int ny;
    public interp2(double[] xx, double[] yy, double[] data) {
        this.xx = xx;
        this.yy = yy;
        this.data = data;

        this.nx = xx.length;
        this.ny = yy.length;
    }

    public double[] f(double[] x, double[] y) {

        double[] ff = new double[x.length];
        for (int i = 0; i < x.length; i++) {
            ff[i] = f(x[i], y[i]);
        }
        return ff;

    }

    public double f(double x, double y) {

        int i1 = Arrays.binarySearch(xx, x);
        int j1 = Arrays.binarySearch(yy, y);
        if (i1 < 0) {
            i1 = -i1 - 1;
        }
        if (j1 < 0) {
            j1 = -j1 - 1;
        }

        if (i1 >= nx) {
            i1--;
        }

        if (j1 >= ny) {
            j1--;
        }

        int i2 = i1 - (i1 > 0 ? 1 : 0);
        int j2 = j1 - (j1 > 0 ? 1 : 0);

        double fi1j1 = data[j1 * nx + i1];
        double fi1j2 = data[j1 * nx + i2];
        double fi2j1 = data[j2 * nx + i1];
        double fi2j2 = data[j2 * nx + i2];

        double dx2 = xx[i2] - x;
        double dx1 = x - xx[i1];
        double dy2 = yy[j2] - y;
        double dy1 = y - yy[j1];

        double frac = (xx[i2] - xx[i1]) * (yy[j2] - yy[j1]);

//        return fi1j1;
        if (xx[i2] - xx[i1] == 0) {
            return (fi1j1 * dx2 + fi1j2 * dx1) / dx2;
        } else if ((yy[j2] - yy[j1]) == 0) {
            return (fi1j1 * dy2 + fi2j1 * dy1) / dy2;
        }
        return (fi1j1 * dx2 * dy2
                + fi1j2 * dx1 * dy2
                + fi2j1 * dx2 * dy1
                + fi2j2 * dx1 * dy1)
                / frac;

    }

}
