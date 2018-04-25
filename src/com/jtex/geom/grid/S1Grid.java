/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom.grid;

import com.jtex.arrays.Array1D;

/**
 *
 * @author hios
 */
public class S1Grid {

    public static void main(String[] args) {

        double[] x = Array1D.linspace(-1, 1, 100).toDoubleArray();

        S1Grid g = new S1Grid(x, x[0], 2);
        g.find_lower(-1);

        new Array1D(g.find_region(-.8, .1)).print();

    }

    private static double MOD3(double A, double B, double C) {
        return A - B * Math.floor((A - C) / B);
    }

    private double[] x;
    private double min, period = 0;

    public S1Grid(double[] x, double min, double period) {
        this.x = x;
        this.min = min;
        this.period = period;
    }

    public int size() {
        return x.length;
    }

    public double getMin() {
        return min;
    }

    public double getPeriod() {
        return period;
    }

    public double[] x() {
        return x;
    }

    public int find_lower(double y) {
        int a = 0, b, c;
        b = x.length - 1;

        if (x[b] <= y) {
            return b;
        }
        if (x[0] > y) {
            return -1;
        }

        while (b - a > 1) {
            c = (a + b) / 2;
            if (y >= x[c]) {
                a = c;
            } else {
                b = c;
            }
        }

        return a;
    }

    public int find_greater(double y) {
        int a = 0, b, c;
        b = x.length - 1;

        if (x[0] >= y) {
            return 0;
        }
        if (x[b] < y) {
            return b + 1;
        }

        while (b - a > 1) {
            c = (a + b) / 2;
            if (y > x[c]) {
                a = c;
            } else {
                b = c;
            }
        }
        return b;
    }

    public int find(double y) {
        int a = 0, b, c;
        b = x.length - 1;

        if (period > 0) {
            y = MOD3(y, period, min);
            if (y < x[a] && x[a] - y >= y + period - x[b]) {
                return b;
            }

            if (y > x[b] && period - y + x[a] <= y - x[b]) {
                return a;
            }
        }

        while (b - a > 1) {
            c = (a + b) / 2;
            if (y > x[c]) {
                a = c;
            } else {
                b = c;
            }
        }

        if (y - x[a] < x[b] - y) {
            return a;
        } else {
            return b;
        }

    }

    public boolean[] find_region(double y, double e) {

        boolean[] regionmatch = new boolean[x.length];

        if (period > 0) {
            y = MOD3(y, period, min);

            int minp = find_greater(MOD3(y - e, period, min));
            int maxp = find_lower(MOD3(y + e, period, min));

            if (y - e >= min && y + e < min + period) {
                set_match(regionmatch, minp, maxp);
            } else if (y - e <= min && y + e >= min + period) {
                set_match(regionmatch, 0, x.length - 1);
            } else {
                set_match(regionmatch, 0, maxp);
                set_match(regionmatch, Math.max(minp, maxp + 1), x.length - 1);
            }
        } else {
            set_match(regionmatch,
                    find_greater(y - e),
                    find_lower(y + e));

        }

        return regionmatch;
    }

    private static void set_match(boolean[] m, int min, int max) {
        for (int i = min; i <= max; i++) {
            m[i] = true;
        }
    }

    public double x(int j) {
        return x[j];
    }

    void setPeriod(double p) {
        this.period = p;
    }

}
