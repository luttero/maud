/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom;

import com.jtex.arrays.Array1D;
import java.util.Arrays;
import java.util.Formatter;
import java.util.Locale;

/**
 *
 * @author flb
 */
public class Vec2 {

    public static Vec2 concat(Vec2... vs) {
        int sz = 0;
        for (Vec2 v : vs) {
            sz += v.x.length;
        }

        Vec2 vn = new Vec2(sz);
        sz = 0;
        for (Vec2 v : vs) {
            System.arraycopy(v.x, 0, vn.x, sz, v.x.length);
            System.arraycopy(v.y, 0, vn.y, sz, v.y.length);
            sz += v.x.length;
        }
        return vn;
    }

    private double[] x, y;

    public Vec2() {
        this(0);
    }

    public Vec2(int n) {
        this.x = new double[n];
        this.y = new double[n];
    }

    public static Vec2 xvector() {
        return new Vec2(1, 0);
    }

    public static Vec2 yvector() {
        return new Vec2(0, 1);
    }

//    public static vec2 rand(int n) {
//
//        double[] theta = new double[n];
//        double[] rho = new double[n];
//        for (int i = 0; i < n; i++) {
//            theta[i] = Math.acos(2 * (Math.random() - .5));
//            rho[i] = 2 * Math.PI * Math.random();
//        }
//
//        return new vec2(theta, rho);
//    }
    public Vec2(Vec2 v) {
        this(v.x, v.y);
    }

    public Vec2(double x, double y) {
        this(1);
        this.x[0] = x;
        this.y[0] = y;
    }

    public Vec2(Array1D x, Array1D y) {
        this(x.toDoubleArray(), y.toDoubleArray());
    }

    public Vec2(double[] x, double[] y) {
        this.x = Arrays.copyOf(x, x.length);
        this.y = Arrays.copyOf(y, y.length);
    }

    public Vec2(double rho) {
        this(new double[]{rho});
    }

    public Vec2(Array1D rho) {
        this(rho.toDoubleArray());
    }

    public Vec2(double[] rho) {
        int sz = rho.length;

        this.x = new double[sz];
        this.y = new double[sz];
        for (int i = 0; i < sz; i++) {
            this.x[i] = Math.cos(rho[i]);
            this.y[i] = Math.sin(rho[i]);
        }
    }

    public Vec2 plus(double y) {
        return plus(new Vec2(y, y));
    }

    public Vec2 plus(Vec2 y) {
        int n = Math.max(x.length, y.x.length);
        Vec2 v = new Vec2(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] + y.x[i];
                v.y[i] = this.y[0] + y.y[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] + y.x[0];
                v.y[i] = this.y[i] + y.y[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] + y.x[i];
                v.y[i] = this.y[i] + y.y[i];
            }
        }
        return v;
    }

    public Vec2 minus(double y) {
        return minus(new Vec2(y, y));
    }

    public Vec2 minus(Vec2 y) {
        int n = Math.max(x.length, y.x.length);
        Vec2 v = new Vec2(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] - y.x[i];
                v.y[i] = this.y[0] - y.y[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] - y.x[0];
                v.y[i] = this.y[i] - y.y[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] - y.x[i];
                v.y[i] = this.y[i] - y.y[i];
            }
        }
        return v;
    }

    public Vec2 multiply(double y) {
        return multiply(new Vec2(y, y));
    }

    public Vec2 multiply(double y[]) {
        Vec2 x = new Vec2();
        x.x = y;
        x.y = y;
        return multiply(x);
    }

    public Vec2 multiply(Array1D y) {
        return multiply(y.toDoubleArray());
    }

    public Vec2 multiply(Vec2 y) {
        int n = Math.max(x.length, y.x.length);
        Vec2 v = new Vec2(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] * y.x[i];
                v.y[i] = this.y[0] * y.y[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] * y.x[0];
                v.y[i] = this.y[i] * y.y[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] * y.x[i];
                v.y[i] = this.y[i] * y.y[i];
            }
        }
        return v;
    }

    public Vec2 divide(double y) {
        return divide(new Vec2(y, y));
    }

    public Vec2 divide(double y[]) {
        Vec2 x = new Vec2();
        x.x = y;
        x.y = y;
        return divide(x);
    }

    public Vec2 divide(Array1D y) {
        return divide(y.toDoubleArray());
    }

    public Vec2 divide(Vec2 y) {
        int n = Math.max(x.length, y.x.length);
        Vec2 v = new Vec2(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] / y.x[i];
                v.y[i] = this.y[0] / y.y[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] / y.x[0];
                v.y[i] = this.y[i] / y.y[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] / y.x[i];
                v.y[i] = this.y[i] / y.y[i];
            }
        }
        return v;
    }

    public Array1D dot(Vec2 y) {
        int n = Math.max(x.length, y.x.length);
        double d[] = new double[n];
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                d[i] = this.x[0] * y.x[i] + this.y[0] * y.y[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                d[i] = this.x[i] * y.x[0] + this.y[i] * y.y[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                d[i] = this.x[i] * y.x[i] + this.y[i] * y.y[i];
            }
        }
        return new Array1D(d);
    }

    public Array1D angle(Vec2 y) {
        return normalize().dot(y.normalize()).acos();
    }

    public Vec2 crossx(Vec2 y) {
        int n = Math.max(x.length, y.x.length);
        Vec2 v = new Vec2(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.y[0] * y.x[i] - this.x[0] * y.y[i];
                v.y[i] = this.x[0] * y.y[i] - this.y[0] * y.x[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.y[i] * y.x[0] - this.x[i] * y.y[0];
                v.y[i] = this.x[i] * y.x[0] - this.x[i] * y.x[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.y[i] * y.x[i] - this.x[i] * y.y[i];
                v.y[i] = this.y[i] * y.x[i] - this.x[i] * y.y[i];
            }
        }
        return v;
    }

    public Vec2 negate() {
        Vec2 v = new Vec2(x.length);
        for (int i = 0; i < x.length; i++) {
            v.x[i] = -this.x[i];
            v.y[i] = -this.y[i];
        }
        return v;
    }

    public Array1D isNull() {
        return norm().le(1e-14);
    }

    public Array1D norm() {
        return dot(this).sqrt();
    }

    public Vec2 normalize() {
        return divide(norm());
    }

    public double[] getDouble() {
        return Array1D.concat(this.x, this.y);
    }
//

    public double[] getDouble(int... ndx) {
        Vec2 v = get(ndx);
        return Array1D.concat(v.x, v.y);
    }

    public Vec2 get(int... ndx) {
        Vec2 v = new Vec2();
        v.x = Array1D.get(x, ndx);
        v.y = Array1D.get(y, ndx);
        return v;
    }

    public Vec2 get(boolean ndx[]) {
        Vec2 v = new Vec2();
        v.x = Array1D.get(x, ndx);
        v.y = Array1D.get(y, ndx);
        return v;
    }

    public void set(Vec2 v) {
        this.x = v.x;
        this.y = v.y;
    }

    public void set(int[] ndx, Vec2 n) {
        Array1D.set(x, ndx, n.x);
        Array1D.set(y, ndx, n.y);
    }

    public void set(boolean[] ndx, Vec2 n) {
        Array1D.set(x, ndx, n.x);
        Array1D.set(y, ndx, n.y);
    }

    public double[] x() {
        return x;
    }

    public double[] y() {
        return y;
    }

    public Array1D getX() {
        return new Array1D(x);
    }

    public Array1D getY() {
        return new Array1D(y);
    }

    public Vec2 setX(Array1D x) {
        this.x = x.toDoubleArray();
        return this;
    }

    public Vec2 setY(Array1D y) {
        this.y = y.toDoubleArray();
        return this;
    }

    public Array1D getRho() {
        return Array1D.atan2(this.y, this.x);
    }

    public Array1D getXY() {
        double[] rr = new double[2 * x.length];
        for (int i = 0; i < x.length; i++) {
            rr[2 * i] = x[i];
            rr[2 * i + 1] = y[i];
        }
        return new Array1D(rr);
    }

//    public Array1D getFFTRhoTheta() {
//        double[] theta = getTheta().toDoubleArray();
//        double[] rho = getRho().toDoubleArray();
//
//        double[] rr = new double[2 * theta.length];
//        for (int i = 0; i < theta.length; i++) {
//            rr[2 * i] = rho[i] / (2 * Math.PI);
//            rr[2 * i + 1] = theta[i] / (2 * Math.PI);
//        }
//        return new Array1D(rr);
//    }
    public void print() {
        Formatter formatter = new Formatter(Locale.US);

        formatter.format("vec2 (size = %d): \n", size());
        formatter.format("     [ ");
        for (double d : x) {
            formatter.format(" %6.2f", d);
        }
        formatter.format("\n       ");

        for (double d : y) {
            formatter.format(" %6.2f", d);
        }
        formatter.format(" ]\n");

        System.out.println(formatter.toString());
    }

    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    protected String paramString() {
        Formatter formatter = new Formatter(Locale.US);

        boolean trunkate = (this.size() > 50) ? true : false;
        int s = (trunkate) ? 50 : this.size();
        for (int i = 0; i < s; i++) {
            formatter.format("(%.4f,%.4f)%s", this.x[i], this.y[i], (i < s - 1) ? "," : (trunkate) ? ",..." : "");
        }
        String str = "size=" + size();
        str += ",data=[" + formatter.toString() + "]";
        return str;
    }

    public Vec2 toRadians() {
        return multiply(Math.PI / 180);
    }

    public Vec2 toDegrees() {
        return multiply(180 / Math.PI);
    }

    public Vec2 orth() {
        Vec2 v = new Vec2(x.length);
        for (int i = 0; i < x.length; i++) {
            v.x[i] = Math.abs(this.x[i]) < 1e-14 ? 1 : -this.y[i];
            v.y[i] = this.x[i];
        }
        return v.normalize();
    }

    public int size() {
        return this.x.length;
    }

    public Vec2 copy() {
        return new Vec2(x, y);
    }

}
