/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom;

import com.jtex.arrays.Array1D;
import com.jtex.external.MTEX;
import com.jtex.qta.kernel.Kernel;
import java.util.Arrays;
import java.util.Formatter;
import java.util.Locale;

/**
 *
 * @author flb
 */
public class Vec3 {

    public static Array1D kernelDensity(Vec3 in, Vec3 out, Kernel psi) {
        return kernelDensity(in, out, psi, Array1D.fill(in.size(), 1D / in.size()));
    }

    public static Array1D kernelDensity(Vec3 in, Vec3 out, Kernel psi, Array1D weights) {

        Array1D inRhoTheta = in.getRhoTheta();
        Array1D outRhoTheta = out.getRhoTheta();

        Array1D A = psi.A();

        double[] f = MTEX.odf2pf(inRhoTheta.toDoubleArray(), outRhoTheta.toDoubleArray(), weights.toDoubleArray(), A.toDoubleArray());

        return new Array1D(f);
    }

    public static Vec3 concat(Vec3... vs) {
        int sz = 0;
        for (Vec3 v : vs) {
            sz += v.x.length;
        }

        Vec3 vn = new Vec3(sz);
        sz = 0;
        for (Vec3 v : vs) {
            System.arraycopy(v.x, 0, vn.x, sz, v.x.length);
            System.arraycopy(v.y, 0, vn.y, sz, v.y.length);
            System.arraycopy(v.z, 0, vn.z, sz, v.z.length);
            sz += v.x.length;
        }
        return vn;
    }

    private double[] x, y, z;

    public Vec3() {
        this(0);
    }

    public Vec3(int n) {
        this.x = new double[n];
        this.y = new double[n];
        this.z = new double[n];
    }

    public static Vec3 xvector() {
        return new Vec3(1, 0, 0);
    }

    public static Vec3 yvector() {
        return new Vec3(0, 1, 0);
    }

    public static Vec3 zvector() {
        return new Vec3(0, 0, 1);
    }

    public static Vec3 rand(int n) {

        double[] theta = new double[n];
        double[] rho = new double[n];
        for (int i = 0; i < n; i++) {
            theta[i] = Math.acos(2 * (Math.random() - .5));
            rho[i] = 2 * Math.PI * Math.random();
        }

        return new Vec3(theta, rho);
    }

    public Vec3(double[] y) {
        this(y[0], y[1], y[2]);
    }

    public Vec3(Vec3 v) {
        this(v.x, v.y, v.z);
    }

    public Vec3(double x, double y, double z) {
        this(1);
        this.x[0] = x;
        this.y[0] = y;
        this.z[0] = z;
    }

    public Vec3(Array1D x, Array1D y, Array1D z) {
        this(x.toDoubleArray(), y.toDoubleArray(), z.toDoubleArray());
    }

    public Vec3(double[] x, double[] y, double[] z) {
        this.x = Arrays.copyOf(x, x.length);
        this.y = Arrays.copyOf(y, y.length);
        this.z = Arrays.copyOf(z, z.length);
    }

    public Vec3(double theta, double rho) {
        this(new double[]{theta}, new double[]{rho});
    }

    public Vec3(Array1D theta, Array1D rho) {
        this(theta.toDoubleArray(), rho.toDoubleArray());
    }

    public Vec3(double[] theta, double[] rho) {
        int sz = theta.length;
        double ct, st, cr, sr;

        this.x = new double[sz];
        this.y = new double[sz];
        this.z = new double[sz];
        for (int i = 0; i < sz; i++) {
            ct = Math.cos(theta[i]);
            st = Math.sin(theta[i]);
            cr = Math.cos(rho[i]);
            sr = Math.sin(rho[i]);

            this.x[i] = st * cr;
            this.y[i] = st * sr;
            this.z[i] = ct;
        }
    }

    public Vec3 plus(double y) {
        return plus(new Vec3(y, y, y));
    }

    public Vec3 plus(Vec3 y) {
        int n = Math.max(x.length, y.x.length);
        Vec3 v = new Vec3(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] + y.x[i];
                v.y[i] = this.y[0] + y.y[i];
                v.z[i] = this.z[0] + y.z[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] + y.x[0];
                v.y[i] = this.y[i] + y.y[0];
                v.z[i] = this.z[i] + y.z[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] + y.x[i];
                v.y[i] = this.y[i] + y.y[i];
                v.z[i] = this.z[i] + y.z[i];
            }
        }
        return v;
    }

    public Vec3 minus(double y) {
        return minus(new Vec3(y, y, y));
    }

    public Vec3 minus(Vec3 y) {
        int n = Math.max(x.length, y.x.length);
        Vec3 v = new Vec3(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] - y.x[i];
                v.y[i] = this.y[0] - y.y[i];
                v.z[i] = this.z[0] - y.z[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] - y.x[0];
                v.y[i] = this.y[i] - y.y[0];
                v.z[i] = this.z[i] - y.z[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] - y.x[i];
                v.y[i] = this.y[i] - y.y[i];
                v.z[i] = this.z[i] - y.z[i];
            }
        }
        return v;
    }

    public Vec3 multiply(double y) {
        return multiply(new Vec3(y, y, y));
    }

    public Vec3 multiply(double y[]) {
        Vec3 x = new Vec3();
        x.x = y;
        x.y = y;
        x.z = y;
        return multiply(x);
    }

    public Vec3 multiply(Array1D y) {
        return multiply(y.toDoubleArray());
    }

    public Vec3 multiply(Vec3 y) {
        int n = Math.max(x.length, y.x.length);
        Vec3 v = new Vec3(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] * y.x[i];
                v.y[i] = this.y[0] * y.y[i];
                v.z[i] = this.z[0] * y.z[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] * y.x[0];
                v.y[i] = this.y[i] * y.y[0];
                v.z[i] = this.z[i] * y.z[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] * y.x[i];
                v.y[i] = this.y[i] * y.y[i];
                v.z[i] = this.z[i] * y.z[i];
            }
        }
        return v;
    }

    public Vec3 divide(double y) {
        return divide(new Vec3(y, y, y));
    }

    public Vec3 divide(double y[]) {
        Vec3 x = new Vec3();
        x.x = y;
        x.y = y;
        x.z = y;
        return divide(x);
    }

    public Vec3 divide(Array1D y) {
        return divide(y.toDoubleArray());
    }

    public Vec3 divide(Vec3 y) {
        int n = Math.max(x.length, y.x.length);
        Vec3 v = new Vec3(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[0] / y.x[i];
                v.y[i] = this.y[0] / y.y[i];
                v.z[i] = this.z[0] / y.z[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] / y.x[0];
                v.y[i] = this.y[i] / y.y[0];
                v.z[i] = this.z[i] / y.z[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.x[i] / y.x[i];
                v.y[i] = this.y[i] / y.y[i];
                v.z[i] = this.z[i] / y.z[i];
            }
        }
        return v;
    }

    public Array1D dot(Vec3 y) {
        int n = Math.max(x.length, y.x.length);
        double d[] = new double[n];
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                d[i] = this.x[0] * y.x[i] + this.y[0] * y.y[i] + this.z[0] * y.z[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                d[i] = this.x[i] * y.x[0] + this.y[i] * y.y[0] + this.z[i] * y.z[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                d[i] = this.x[i] * y.x[i] + this.y[i] * y.y[i] + this.z[i] * y.z[i];
            }
        }
        return new Array1D(d);
    }

    public Array1D angle(Vec3 y) {
        return normalize().dot(y.normalize()).acos();
    }

    public Vec3 cross(Vec3 y) {
        int n = Math.max(x.length, y.x.length);
        Vec3 v = new Vec3(n);
        if (size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.y[0] * y.z[i] - this.z[0] * y.y[i];
                v.y[i] = this.z[0] * y.x[i] - this.x[0] * y.z[i];
                v.z[i] = this.x[0] * y.y[i] - this.y[0] * y.x[i];
            }
        } else if (y.size() == 1) {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.y[i] * y.z[0] - this.z[i] * y.y[0];
                v.y[i] = this.z[i] * y.x[0] - this.x[i] * y.z[0];
                v.z[i] = this.x[i] * y.y[0] - this.y[i] * y.x[0];
            }
        } else {
            for (int i = 0; i < n; i++) {
                v.x[i] = this.y[i] * y.z[i] - this.z[i] * y.y[i];
                v.y[i] = this.z[i] * y.x[i] - this.x[i] * y.z[i];
                v.z[i] = this.x[i] * y.y[i] - this.y[i] * y.x[i];
            }
        }
        return v;
    }

    public Vec3 rotate(Quaternion q) {
        int n = Math.max(x.length, q.size());

        double a[] = q.a();
        double b[] = q.b();
        double c[] = q.c();
        double d[] = q.d();

        double aa = 0, bb = 0, cc = 0, dd = 0,
                ab = 0, ac = 0, ad = 0, bc = 0, bd = 0, cd = 0;
        Vec3 v = new Vec3(n);

        if (q.size() == 1) {
            aa = a[0] * a[0];
            ab = a[0] * b[0];
            ac = a[0] * c[0];
            ad = a[0] * d[0];

            bb = b[0] * b[0];
            bc = b[0] * c[0];
            bd = b[0] * d[0];

            cc = c[0] * c[0];
            cd = c[0] * d[0];

            dd = d[0] * d[0];

            double c0 = (aa + bb - cc - dd);
            double c3 = (aa - bb + cc - dd);
            double c6 = (aa - bb - cc + dd);
            double c1 = (ac + bd);
            double c4 = (ad + bc);
            double c7 = (ab + cd);
            double c2 = (bc - ad);
            double c5 = (cd - ab);
            double c8 = (bd - ac);

            for (int i = 0; i < n; i++) {

                v.x[i] = c0 * this.x[i] + 2 * (c1 * this.z[i] + c2 * this.y[i]);
                v.y[i] = c3 * this.y[i] + 2 * (c4 * this.x[i] + c5 * this.z[i]);
                v.z[i] = c6 * this.z[i] + 2 * (c7 * this.y[i] + c8 * this.x[i]);
            }
        } else if (size() == 1) {
            double xx = x[0], yy = y[0], zz = z[0];

            for (int i = 0; i < n; i++) {
                aa = a[i] * a[i];
                ab = a[i] * b[i];
                ac = a[i] * c[i];
                ad = a[i] * d[i];

                bb = b[i] * b[i];
                bc = b[i] * c[i];
                bd = b[i] * d[i];

                cc = c[i] * c[i];
                cd = c[i] * d[i];

                dd = d[i] * d[i];

                v.x[i] = (aa + bb - cc - dd) * xx + 2 * ((ac + bd) * zz + (bc - ad) * yy);
                v.y[i] = (aa - bb + cc - dd) * yy + 2 * ((ad + bc) * xx + (cd - ab) * zz);
                v.z[i] = (aa - bb - cc + dd) * zz + 2 * ((ab + cd) * yy + (bd - ac) * xx);
            }
        } else {
            for (int i = 0; i < n; i++) {
                aa = a[i] * a[i];
                ab = a[i] * b[i];
                ac = a[i] * c[i];
                ad = a[i] * d[i];

                bb = b[i] * b[i];
                bc = b[i] * c[i];
                bd = b[i] * d[i];

                cc = c[i] * c[i];
                cd = c[i] * d[i];

                dd = d[i] * d[i];

                v.x[i] = (aa + bb - cc - dd) * this.x[i] + 2 * ((ac + bd) * this.z[i] + (bc - ad) * this.y[i]);
                v.y[i] = (aa - bb + cc - dd) * this.y[i] + 2 * ((ad + bc) * this.x[i] + (cd - ab) * this.z[i]);
                v.z[i] = (aa - bb - cc + dd) * this.z[i] + 2 * ((ab + cd) * this.y[i] + (bd - ac) * this.x[i]);
            }
        }

        return v;
    }

    public Vec3 negate() {
        Vec3 v = new Vec3(x.length);
        for (int i = 0; i < x.length; i++) {
            v.x[i] = -this.x[i];
            v.y[i] = -this.y[i];
            v.z[i] = -this.z[i];
        }
        return v;
    }

    public Array1D isNull() {
        return norm().le(1e-14);
    }

    public Array1D norm() {
        return dot(this).sqrt();
    }

    public Vec3 normalize() {
        return divide(norm());
    }

    public double[] getDouble() {
        return Array1D.concat(this.x, this.y, this.z);
    }
//

    public double[] getDouble(int... ndx) {
        Vec3 v = get(ndx);
        return Array1D.concat(v.x, v.y, v.z);
    }

    public Vec3 get(int... ndx) {
        Vec3 v = new Vec3();
        v.x = Array1D.get(x, ndx);
        v.y = Array1D.get(y, ndx);
        v.z = Array1D.get(z, ndx);
        return v;
    }

    public Vec3 get(boolean ndx[]) {
        Vec3 v = new Vec3();
        v.x = Array1D.get(x, ndx);
        v.y = Array1D.get(y, ndx);
        v.z = Array1D.get(z, ndx);
        return v;
    }

    public void set(Vec3 v) {
        this.x = v.x;
        this.y = v.y;
        this.z = v.z;
    }

    public void set(int[] ndx, Vec3 n) {
        Array1D.set(x, ndx, n.x);
        Array1D.set(y, ndx, n.y);
        Array1D.set(z, ndx, n.z);
    }

    public void set(boolean[] ndx, Vec3 n) {
        Array1D.set(x, ndx, n.x);
        Array1D.set(y, ndx, n.y);
        Array1D.set(z, ndx, n.z);
    }

    public double[] x() {
        return x;
    }

    public double[] y() {
        return y;
    }

    public double[] z() {
        return z;
    }

    public Array1D getX() {
        return new Array1D(x);
    }

    public Array1D getY() {
        return new Array1D(y);
    }

    public Array1D getZ() {
        return new Array1D(z);
    }

    public Vec3 setX(Array1D x) {
        this.x = x.toDoubleArray();
        return this;
    }

    public Vec3 setY(Array1D y) {
        this.y = y.toDoubleArray();
        return this;
    }

    public Vec3 setZ(Array1D z) {
        this.z = z.toDoubleArray();
        return this;
    }

    public Array1D getTheta() {
        double th[] = new double[x.length];
        for (int i = 0; i < x.length; i++) {
            th[i] = Math.acos(z[i] / Math.sqrt(x[i] * x[i] + y[i] * y[i] + z[i] * z[i]));
        }
        return new Array1D(th);
    }

    public Array1D getRho() {
        return Array1D.atan2(this.y, this.x);
    }

    public Vec2 toVec2() {
        return new Vec2(getTheta().toDoubleArray(), getRho().toDoubleArray());
    }

    public Array1D getRhoTheta() {
        double[] theta = getTheta().toDoubleArray();
        double[] rho = getRho().toDoubleArray();

        double[] rr = new double[2 * theta.length];
        for (int i = 0; i < theta.length; i++) {
            rr[2 * i] = rho[i] / (2 * Math.PI);
            rr[2 * i + 1] = theta[i] / (2 * Math.PI);
        }
        return new Array1D(rr);
    }

    public Array1D getXYZ() {
        double[] rr = new double[3 * x.length];
        for (int i = 0; i < x.length; i++) {
            rr[3 * i] = x[i];
            rr[3 * i + 1] = y[i];
            rr[3 * i + 2] = z[i];
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

        formatter.format("vec3 (size = %d): \n", size());
        formatter.format("     [ ");
        for (double d : x) {
            formatter.format(" %6.2f", d);
        }
        formatter.format("\n       ");

        for (double d : y) {
            formatter.format(" %6.2f", d);
        }
        formatter.format("\n       ");

        for (double d : z) {
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
            formatter.format("(%.4f,%.4f,%.4f)%s", this.x[i], this.y[i], this.z[i], (i < s - 1) ? "," : (trunkate) ? ",..." : "");
        }
        String str = "size=" + size();
        str += ",data=[" + formatter.toString() + "]";
        return str;
    }

    public Vec3 toRadians() {
        return multiply(Math.PI / 180);
    }

    public Vec3 toDegrees() {
        return multiply(180 / Math.PI);
    }

    public Vec3 orth() {
        Vec3 v = new Vec3(x.length);
        for (int i = 0; i < x.length; i++) {
            v.x[i] = Math.abs(this.x[i]) < 1e-14 ? 1 : -this.y[i];
            v.y[i] = this.x[i];
        }
        return v.normalize();
    }

    public int size() {
        return this.x.length;
    }

    public Vec3 copy() {
        return new Vec3(x, y, z);
    }

}
