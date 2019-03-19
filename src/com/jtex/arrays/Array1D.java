/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.arrays;

import java.util.*;

/**
 *
 * @author hios
 */
public class Array1D {

    public static double[] concat(double[]  
        ... arry) {
        int size = 0;

        for (double[] rho : arry) {
            size += rho.length;
        }

        double d[] = new double[size];
        int offset = 0;
        for (double[] rho : arry) {
            System.arraycopy(rho, 0, d, offset, rho.length);
            offset += rho.length;
        }
        return d;
    }

    public static Array1D concat(Array1D... arry) {
        int size = 0;

        for (Array1D rho : arry) {
            size += rho.size();
        }

        double d[] = new double[size];
        int offset = 0;
        for (Array1D rho : arry) {
            System.arraycopy(rho.a, 0, d, offset, rho.size());
            offset += rho.size();
        }
        return new Array1D(d);
    }

    private double[] a;

    public static Array1D linspace(float d1, float d2, int n) {
        return linspace((double) d1, (double) d2, n);
    }

    public static Array1D linspace(double d1, double d2, int n) {
//         % at least two end points
        double n1 = (double) (n - 1D);
        double c = (d2 - d1) * (n1 - 1D);
        double y[] = new double[(int) n1 + 1];

        if (n == 1) {
            return new Array1D(d1);
        }

        if (Double.isInfinite(c)) {//        y = d1 + (d2/n1).*(0:n1) - (d1/n1).*(0:n1); 
            for (int i = 0; i < n1 + 1; i++) {
                y[i] = d1 + (d2 / n1) * i - (d1 / n1) * i;
            }
        } else {
            c = (d2 - d1);
            if (c == 0) {
                for (int i = 0; i < (n1 + 1D); i++) {
                    y[i] = d1;
                }
            } else {
                for (int i = 0; i < (n1 + 1D); i++) {
                    y[i] = d1 + i * c / n1;
                }
            }

        }
        return new Array1D(y);
    }

    public static Array1D zeros(int c) {
//         % at least two end points
        return new Array1D(new double[c]);
    }

    public static Array1D ones(int c) {
//         % at least two end points
        double y[] = new double[c];
        for (int i = 0; i < c; i++) {
            y[i] = 1.0;
        }
        return new Array1D(y);
    }

    public static Array1D index(int size) {
        double y[] = new double[size];
        for (int i = 0; i < size; i++) {
            y[i] = i;
        }
        return new Array1D(y);
    }

    public static Integer[] indexed(int size) {
        Integer y[] = new Integer[size];
        for (int i = 0; i < size; i++) {
            y[i] = i;
        }
        return y;
    }

    public static Array1D fill(int size, double val) {
        double y[] = new double[size];
        for (int i = 0; i < size; i++) {
            y[i] = val;
        }
        return new Array1D(y);
    }

    public static Array1D rand(int c) {
//         % at least two end points
        double y[] = new double[c];
        for (int i = 0; i < c; i++) {
            y[i] = Math.random();
        }
        return new Array1D(y);
    }

    public static Array1D fill(double d1, double d2, double inc) {
//         % at least two end points
        double n = Math.round((d2 - d1) / inc) + 1;
        double y[] = new double[(int) n];
        for (int i = 0; i < n; i++) {
            y[i] = d1 + i * inc;
        }
        return new Array1D(y);
    }

    public Array1D() {
        this.a = new double[0];
    }

    public Array1D(boolean... ele) {
        this.a = new double[ele.length];
        for (int i = 0; i < a.length; i++) {
            this.a[i] = (ele[i]) ? 1.0 : 0.0;
        }
    }

    public Array1D(int... ele) {
        this.a = new double[ele.length];
        for (int i = 0; i < a.length; i++) {
            this.a[i] = ele[i];
        }
    }

    public Array1D(double... ele) {
        this.a = ele;
    }

    public double[] copy() {

//        double[] b = new double[a.length];
//        int i = 0;
//        for (double v : a) {
//            b[i++] = v;
//        }
        return Arrays.copyOf(a, a.length);
    }

    public Array1D getCopy() {
        return new Array1D(copy());
    }

    public void setValues(double[] a) {
        this.a = a;
    }

    public Array1D mod(double[] b) {
        double[] c = copy();
        for (int i = 0; i < a.length; i++) {
            c[i] %= b[i];
        }
        return new Array1D(c);
    }

    public Array1D mod(double b) {
//        double[] c = copy();
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
//            MOD3 = @(A, B, C)	((A)-floor((A-C)/(B))*(B))
//            c[i] = a[i] - Math.floor(a[i] / b) * b;
            c[i] = a[i] % b;
        }
        return new Array1D(c);
    }

    public Array1D mods(double b) {
        double[] c = copy();
//        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            if (a[i] > b) {
                c[i] -= Math.floor(a[i] / b) * b;
            } else if (a[i] <= -b) {
                c[i] += Math.floor(a[i] / -b) * b;
            }

//            MOD3 = @(A, B, C)	((A)-floor((A-C)/(B))*(B))
//            c[i] = a[i] - Math.floor(a[i] / b) * b;
//            c[i] =a[i]%b;
        }
        return new Array1D(c);
    }

    public Array1D mods2(double b, double c) {
        double[] d = copy();
//        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
//            if (a[i] > b) {
//                d[i] -= Math.floor((a[i]-c) / b) * b;
//            } else if (a[i] <= -b) {
            d[i] += Math.floor((a[i] - c) / -b) * b;
//            }

//            MOD3 = @(A, B, C)	((A)-floor((A-C)/(B))*(B))
//            c[i] = a[i] - Math.floor(a[i] / b) * b;
//            c[i] =a[i]%b;
        }
        return new Array1D(d);
    }

//    public Array1D mod(int b) {
//        double[] c = copy();
//        for (int i = 0; i < a.length; i++) {
//            c[i] %= b;
//        }
//        return new Array1D(c);
//    }
    public Array1D max(Array1D b) {
        return this.max(b.a);
    }

    public Array1D max(double[] b) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.max(a[i], b[i]);
        }
        return new Array1D(c);
    }

    public Array1D max(double b) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.max(a[i], b);
        }
        return new Array1D(c);
    }

    public double max() {
        double m = -Double.MAX_VALUE;
        for (int i = 0; i < a.length; i++) {
            m = Math.max(m, a[i]);
        }
        return m;
    }

    public Array1D min(Array1D b) {
        return this.min(b.a);
    }

    public Array1D min(double[] b) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.min(a[i], b[i]);
        }
        return new Array1D(c);
    }

    public Array1D min(double b) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.min(a[i], b);
        }
        return new Array1D(c);
    }

    public double min() {
        double m = Double.MAX_VALUE;
        for (int i = 0; i < a.length; i++) {
            m = Math.min(m, a[i]);
        }
        return m;
    }

    public double nanmin() {
        double m = Double.MAX_VALUE;
        for (int i = 0; i < a.length; i++) {
            if (a[i] < m) {
                m = a[i];
            }
        }
        return m;
    }

    public double nanmax() {
        double m = -Double.MAX_VALUE;
        for (int i = 0; i < a.length; i++) {
            if (a[i] > m) {
                m = a[i];
            }
        }
        return m;
    }

    public Array1D floor() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.floor(a[i]);
        }
        return new Array1D(c);
    }

    public Array1D ceil() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.ceil(a[i]);
        }
        return new Array1D(c);
    }

    public Array1D round() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.round(a[i]);
        }
        return new Array1D(c);
    }

    public Array1D cos() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.cos(a[i]);
        }
        return new Array1D(c);
    }

    public Array1D acos() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.acos(a[i]);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.acos(a));
    }

    public Array1D sin() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.sin(a[i]);
        }
        return new Array1D(c);
    }

    public Array1D asin() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.asin(a[i]);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.asin(a));
    }

    public Array1D tan() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.tan(a[i]);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.tan(a));
    }

    public Array1D atan() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.atan(a[i]);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.atan(a));
    }

    public static Array1D atan2(double[] y, double[] x) {
        double[] c = new double[y.length];
        for (int i = 0; i < c.length; i++) {
            c[i] = Math.atan2(y[i], x[i]);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.atan2(y.a, x.a));

    }

    public static Array1D atan2(Array1D y, Array1D x) {
        double[] c = new double[y.size()];
        for (int i = 0; i < c.length; i++) {
            c[i] = Math.atan2(y.a[i], x.a[i]);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.atan2(y.a, x.a));

    }

    public Array1D atan2(Array1D x) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.atan2(a[i], x.a[i]);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.atan2(a, x.a));
    }

    public Array1D abs() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.abs(a[i]);
        }
        return new Array1D(c);
    }

    public Array1D toRadians() {
        return multiply(Math.PI / 180);
    }

    public Array1D toDegrees() {
        return multiply(180 / Math.PI);
    }

    public double[] toDoubleArray() {
        return this.a;
    }

    public double get(int ndx) {
        return a[ndx];
    }

    public Array1D get(int... ndx) {
        double[] c = new double[ndx.length];
        for (int i = 0; i < ndx.length; i++) {
            c[i] = a[ndx[i]];
        }
        return new Array1D(c);
    }

    public static double[] get(double[] a, int... ndx) {
        double[] c = new double[ndx.length];
        for (int i = 0; i < ndx.length; i++) {
            c[i] = a[ndx[i]];
        }
        return c;
    }

    public static double[] get(double[] a, boolean[] ndx) {
        int count = 0;
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            if (ndx[i]) {
                c[count++] = a[i];
            }
        }
        return Arrays.copyOf(c, count);
    }

    public Array1D get(boolean[] ndx) {
        int count = 0;
        int[] idx = new int[a.length];
        for (int i = 0; i < ndx.length; i++) {
            if (ndx[i]) {
                idx[count++] = i;
            }
        }

        double[] c = new double[count];
        for (int i = 0; i < count; i++) {
            c[i] = a[idx[i]];
        }
        return new Array1D(c);
    }

    public void set(int ndx, double val) {
        a[ndx] = val;
    }

    public static double[] set(double[] a, int[] ndx, double[] b) {
        for (int i = 0; i < ndx.length; i++) {
            a[ndx[i]] = b[i];
        }
        return a;
    }

    public static double[] set(double[] a, double[] b) {
        for (int i = 0; i < a.length; i++) {
            a[i] = b[i];
        }
        return a;
    }

    public static double[] set(double[] a, boolean[] ndx, double[] val) {
        int j = 0;
        for (int i = 0; i < a.length; i++) {
            if (ndx[i]) {
                a[i] = val[j++];
            }
        }
        return a;
    }

    public void set(int[] ndx, double val) {
        for (int i = 0; i < ndx.length; i++) {
            a[ndx[i]] = val;
        }
    }

    public void set(int[] ndx, Array1D val) {
        this.set(ndx, val.a);
    }

    public void set(int[] ndx, double[] val) {
        if (val.length == 1 && ndx.length > 1) {
            val = ones(ndx.length).multiply(val[1]).a;
        }

        for (int i = 0; i < ndx.length; i++) {
            a[ndx[i]] = val[i];
        }
    }

    public void set(boolean[] ndx, Array1D val) {
        this.set(ndx, val.a);
    }

    public void set(boolean[] ndx, double[] val) {
        int j = 0;
        for (int i = 0; i < a.length; i++) {
            if (ndx[i]) {
                a[i] = val[j++];
            }
        }
    }

    public void set(boolean[] ndx, double val) {
        for (int i = 0; i < a.length; i++) {
            if (ndx[i]) {
                a[i] = val;
            }
        }
    }

    public int[] find() {
        int count = 0;
        int[] idx = new int[a.length];
        for (int i = 0; i < a.length; i++) {
            if (a[i] != 0) {
                idx[count++] = i;
            }
        }
        return Arrays.copyOf(idx, count);
    }

    public int[] toIntArray() {
        int[] b = new int[a.length];
        for (int i = 0; i < a.length; i++) {
            b[i] = (int) Math.round(a[i]);
        }
        return b;
    }

    public boolean[] toBooleanArray() {
        boolean[] b = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            b[i] = a[i] != 0;
        }
        return b;
    }

    public Array1D multiplyd(double b) {
        for (int i = 0; i < a.length; i++) {
            a[i] *= b;
        }
        return this;
    }

    public Array1D multiplyd(double[] b) {
        for (int i = 0; i < a.length; i++) {
            a[i] *= b[i];
        }
        return this;
    }

    public Array1D multiplyd(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.multiplyd(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            System.out.println("not supported");
            return null;
        } else {
            return this.multiplyd(b.a);
        }
    }

    public Array1D multiply(double b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] *= b;
        }
        return new Array1D(c);
    }

    public Array1D multiply(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.multiply(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            return b.multiply(a[0]);
        } else {
            return this.multiply(b.a);
        }
    }

    public Array1D squared() {
        return this.multiply(a);
    }

    public Array1D multiply(double[] b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] *= b[i];
        }
        return new Array1D(c);
    }

    public Array1D pow(Array1D b) {
        return this.pow(b.a);
    }

    public Array1D pow(double[] b) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.pow(a[i], b[i]);
        }
        return new Array1D(c);
    }

    public Array1D sqrt() {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.sqrt(a[i]);
        }
        return new Array1D(c);
    }

    public Array1D pow(double b) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = Math.pow(a[i], b);
        }
        return new Array1D(c);
//        return new Array1D(NativeMath.pow(a, b));
    }

    public Array1D divided(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.divided(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            return this.ldivided(b.a);
        } else {
            return this.divide(b.a);
        }
    }

    public Array1D divided(double[] b) {
        for (int i = 0; i < a.length; i++) {
            a[i] /= a[i];
        }
        return this;
    }

    public Array1D divided(double b) {
        for (int i = 0; i < a.length; i++) {
            a[i] /= b;
        }
        return this;
    }

    public Array1D divide(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.divide(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            return this.ldivide(b.a[0]);
        } else {
            return this.divide(b.a);
        }
    }

    public Array1D divide(double[] b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] /= b[i];
        }
        return new Array1D(c);
    }

    public Array1D divide(double b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] /= b;
        }
        return new Array1D(c);
    }

    public Array1D plusd(double b) {
        for (int i = 0; i < a.length; i++) {
            a[i] += b;
        }
        return this;
    }

    public Array1D plusd(double[] b) {
        for (int i = 0; i < a.length; i++) {
            a[i] += b[i];
        }
        return this;
    }

    public Array1D plusd(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.plusd(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            System.out.println("not supported");
            return null;
        } else {
            return this.plusd(b.a);
        }
    }

    public Array1D plus(double b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] += b;
        }
        return new Array1D(c);
    }

    public Array1D plus(double[] b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] += b[i];
        }
        return new Array1D(c);
    }

    public Array1D plus(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.plus(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            return b.plus(a[0]);
        } else {
            return this.plus(b.a);
        }
    }

    public Array1D minus(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.minus(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            return this.lminus(b.a);
        } else {
            return this.minus(b.a);
        }
    }

    public Array1D minus(double[] b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] -= b[i];
        }
        return new Array1D(c);
    }

    public Array1D minus(double b) {
        double[] c = copy();
        for (int i = 0; i < c.length; i++) {
            c[i] -= b;
        }
        return new Array1D(c);
    }

    public Array1D lminus(double b) {
        double[] c = new double[a.length];
        for (int i = 0; i < c.length; i++) {
            c[i] = b - a[i];
        }
        return new Array1D(c);
    }

    public Array1D lminus(double[] b) {
        double[] c = new double[a.length];
        for (int i = 0; i < c.length; i++) {
            c[i] = b[i] - a[i];
        }
        return new Array1D(c);
    }

    public Array1D minusd(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.minusd(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            System.out.println("not supported");
            return null;
        } else {
            return this.minusd(b.a);
        }
    }

    public Array1D minusd(double[] b) {
        for (int i = 0; i < a.length; i++) {
            a[i] -= b[i];
        }
        return this;
    }

    public Array1D minusd(double b) {
        for (int i = 0; i < a.length; i++) {
            a[i] -= b;
        }
        return this;
    }

    public Array1D le(double d) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = (a[i] <= d) ? 1 : 0;
        }
        return new Array1D(c);
    }

    public Array1D l(double d) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = (a[i] < d) ? 1 : 0;
        }
        return new Array1D(c);
    }

    public boolean[] lb(double d) {
        boolean[] c = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = a[i] < d;
        }
        return c;
    }

    public boolean[] gb(double d) {
        boolean[] c = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = a[i] > d;
        }
        return c;
    }

    public boolean[] isNaN() {
        boolean[] d = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            d[i] = Double.isNaN(a[i]);
        }
        return d;
    }

    public Array1D g(double d) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = (a[i] > d) ? 1 : 0;
        }
        return new Array1D(c);
    }

    public Array1D ge(double d) {
        double[] c = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = (a[i] >= d) ? 1 : 0;
        }
        return new Array1D(c);
    }

    public Array1D expand(int count) {
        double d[] = new double[a.length * count];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < count; j++) {
                d[i * count + j] = a[i];
            }
        }
        return new Array1D(d);
    }

    public Array1D expand(int[] count) {
        int total = 0;
        int cs[] = new int[1 + count.length];
        for (int i = 0; i < count.length; i++) {
            total += count[i];
            cs[i + 1] = total;
        }
        double d[] = new double[total];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < count[i]; j++) {
                d[cs[i] + j] = a[i];
            }
        }
        return new Array1D(d);
    }

    public Array1D append(Array1D b) {
        double[] d = new double[a.length + b.size()];
        for (int i = 0; i < a.length; i++) {
            d[i] = a[i];
        }

        int j = a.length;
        for (int i = 0; i < b.a.length; i++) {
            d[j++] = b.a[i];

        }
        return new Array1D(d);
    }

    public int size() {
        return a.length;
    }

    public Array1D flipSign() {
        double c[] = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = -a[i];
        }
        return new Array1D(c);
    }

    public Array1D isNull() {
        return this.abs().le(1e-8);
    }

    public Array1D isNotNull() {
        return this.abs().g(1e-8);
    }

    public boolean any() {
        for (double d : a) {
            if (d != 0) {
                return true;
            }
        }
        return false;
    }

    public boolean notAny() {
        for (double d : a) {
            if (d == 0) {
                return true;
            }
        }
        return false;
    }

    public Array1D repeat(int i) {
        double d[] = new double[i * a.length];

        for (int offset = 0; offset < i * a.length; offset += a.length) {
            System.arraycopy(this.a, 0, d, offset, a.length);
        }
        return new Array1D(d);

    }

    public Array1D eq(double b) {
        return new Array1D(eqb(b));
    }

    public boolean[] eqb(double b) {
        boolean[] c = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = a[i] == b;
        }
        return c;
    }

    public boolean[] neqb(double b) {
        boolean[] c = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = a[i] != b;
        }
        return c;
    }

    public Array1D neq(double b) {
        return new Array1D(neqb(b));
    }

    public Array1D ldivide(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.ldivide(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            return this.divide(b.a);
        } else {
            return this.ldivide(b.a);
        }
    }

    public Array1D ldivide(double[] b) {
        double[] c = Arrays.copyOf(b, b.length);
        for (int i = 0; i < c.length; i++) {
            c[i] /= a[i];
        }
        return new Array1D(c);
    }

    public Array1D ldivide(double b) {
        double[] c = new double[a.length];
        for (int i = 0; i < c.length; i++) {
            c[i] = b / a[i];
        }
        return new Array1D(c);
    }

    public Array1D ldivided(Array1D b) {
        if (b.size() == 1 && a.length > 1) {
            return this.ldivided(b.a[0]);
        } else if (b.size() > 1 && a.length == 1) {
            return this.divided(b.a);
        } else {
            return this.ldivided(b.a);
        }
    }

    public Array1D ldivided(double[] b) {
        double[] c = Arrays.copyOf(b, b.length);
        for (int i = 0; i < c.length; i++) {
            c[i] /= a[i];
        }
        return new Array1D(c);
    }

    public Array1D ldivided(double b) {
        for (int i = 0; i < a.length; i++) {
            a[i] = b / a[i];
        }
        return this;
    }

    public double sum() {
        double s = 0;
        for (double b : a) {
            s += b;
        }
        return s;
    }

    public int nnz() {
        int s = 0;
        for (double b : a) {
            s += (b != 0) ? 1 : 0;
        }
        return s;
    }

    public double mean() {
        return sum() / size();
    }

    public double var() {
        return minus(mean()).squared().sum() / size();
    }

    public double std() {
        return Math.sqrt(var());
    }

    public static String print(double[] y) {
        Formatter formatter = new Formatter(Locale.US);
        formatter.format("%s", "[");
        for (int i = 0; i < y.length; i++) {
            formatter.format("%3.6f", y[i]);
            if (i < y.length - 1) {
                formatter.format(", ");
            }
        }
        formatter.format("%s", "]");
        return formatter.toString();
    }

    public void print() {
        System.out.println(print(this.a));
    }

    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    private String paramString() {
        Formatter formatter = new Formatter(Locale.US);
        boolean trunkate = (this.size() > 50) ? true : false;
        int s = (trunkate) ? 50 : this.size();
        for (int i = 0; i < s; i++) {
            formatter.format("%.4f%s", this.a[i], (i < s - 1) ? "," : (trunkate) ? ",..." : "");
        }
        String str = "size=" + size();
        str += ",data=(" + formatter.toString() + ")";
        return str;
    }

	public String toSaveString() { // Luca
		String saveStr = "";
		for (int i = 0; i < this.size(); i++) {
			saveStr += Double.toString(this.a[i]);
			if (i < size() - 1)
				saveStr += " ";
		}
		return saveStr;
	}

	public static Array1D parse(String inputString) {

		Vector<Double> myRe = new Vector(100, 100);
		Scanner ss = new Scanner(inputString);
		ss.useDelimiter(" ");
		while (ss.hasNext()) {
			myRe.add(Double.parseDouble(ss.next()));
		}

		int size = myRe.size();

		double[] mre = new double[size];
		for (int i = 0; i < size; i++) {
			mre[i] = myRe.elementAt(i);
		}
		Array1D array = new Array1D(mre);
		return array;
	}

	public double dot(Array1D b) {
        double c = 0;
        for (int i = 0; i < a.length; i++) {
            c += a[i] * b.a[i];
        }
        return c;
    }

    public Array1D sort() {
        double c[] = Arrays.copyOf(a, a.length);
        Arrays.sort(c);
        return new Array1D(c);
    }

    public Array1D sortDescending() {
        double c[] = Arrays.copyOf(a, a.length);
        Arrays.sort(c);
        return new Array1D(c).reverse();
    }

    public Array1D reverse() {
        double c[] = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = a[a.length - 1 - i];
        }
        return new Array1D(c);
    }

    public int[] find(double from, double to) {

//        Arrays.sort(a);
//        Arrays.binarySearch(a, to)
        return null;
    }

    public Array1D find(double from, double to, double periodic) {

//        Arrays.sort(a);
        return null;
    }

    public float[] toFloatArray() {
        float[] f = new float[a.length];

        for (int i = 0; i < a.length; i++) {
            f[i] = (float) a[i];
        }
        return f;
    }

    public boolean[] andb(Array1D eq) {
        boolean d[] = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            d[i] = a[i] != 0 & eq.a[i] != 0;
        }
        return d;
    }

    public Array1D and(Array1D eq) {
        return new Array1D(andb(eq));
    }

    public int[] sortIndex() {

        final Integer[] idx = Array1D.indexed(a.length);

        Arrays.sort(idx, new Comparator<Integer>() {
            @Override
            public int compare(final Integer o1, final Integer o2) {
                return Double.compare(a[o1], a[o2]);
            }
        });

        int[] ndx = new int[idx.length];
        for (int i = 0; i < idx.length; i++) {
            ndx[i] = idx[i];
        }
        return ndx;
    }

    public boolean[] ord(Array1D eq) {
        boolean d[] = new boolean[a.length];
        for (int i = 0; i < a.length; i++) {
            d[i] = a[i] != 0 | eq.a[i] != 0;
        }
        return d;
    }

    public Array1D or(Array1D eq) {
        return new Array1D(ord(eq));
    }

    public Array1D mean(int rept) {
        int n = a.length / rept;
        double[] d = new double[n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < rept; j++) {
                d[i] += a[i * rept + j];
            }
            d[i] /= rept;
        }
        return new Array1D(d);
    }

    public Array1D[] split(int[] sz) {
        Array1D[] sp = new Array1D[sz.length];

        int off = 0;
        for (int i = 0; i < sz.length; i++) {
            double d[] = new double[sz[i]];
            System.arraycopy(a, off, d, 0, sz[i]);
            sp[i] = new Array1D(d);
            off += sz[i];
        }
        return sp;
    }

    public Array1D log() {
        double[] lg = new double[a.length];

        for (int i = 0; i < lg.length; i++) {
            lg[i] = Math.log(a[i]);
        }
        return new Array1D(lg);
    }

    public double nansum() {
        double s = 0;
        for (double v : a) {
            if (Double.isInfinite(v)) {
                s += v;
            }
        }
        return s;
    }

    public double norm() {
        double n = 0;
        for (double v : a) {
            n += v * v;
        }
        return Math.sqrt(n);
    }

    public double quantile(double q) {
        double[] d = this.copy();
        Arrays.sort(d);

        if (q <= 0D) {
            return d[0];
        } else if (q >= 1D) {
            return d[d.length - 1];
        } else {
            return d[ (int) Math.max(0, Math.min(Math.round((d.length - 1) * q), d.length - 1))];
        }
    }
}
