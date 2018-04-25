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
 * @author hios
 */
public class Quaternion {

    public static Quaternion rotAxisAngle(Vec3 ax, double omega) {
        return rotAxisAngle(ax, new Array1D(omega));
    }

    public static Quaternion rotAxisAngle(Vec3 ax, Array1D omega) {

        int n = Math.max(ax.size(), omega.size());

        Vec3 nx = ax.normalize();
        double x[] = nx.x();
        double y[] = nx.y();
        double z[] = nx.z();

        double omega1[] = omega.copy();
        for (int i = 0; i < omega1.length; i++) {
            omega1[i] /= 2;
        }

        Quaternion q = new Quaternion(n);
        double cw = Math.cos(omega1[0]), sw = Math.sin(omega1[0]);
        int ix;
        boolean iw = omega1.length > 1;
        boolean ib = x.length > 1;

        for (int i = 0; i < n; i++) {
            if (iw) {
                cw = Math.cos(omega1[i]);
                sw = Math.sin(omega1[i]);
            }
            ix = ib ? i : 0;
            q.a[i] = cw;
            q.b[i] = x[ix] * sw;
            q.c[i] = y[ix] * sw;
            q.d[i] = z[ix] * sw;

        }
        return q;
    }

    public static Quaternion rotX(double omega) {
        return rotX(new Array1D(omega));
    }

    public static Quaternion rotX(Array1D omega) {
        double omega1[] = omega.toDoubleArray();
        int n = omega1.length;

        Quaternion q = new Quaternion(n);
        double w2;
        for (int i = 0; i < n; i++) {
            w2 = omega1[i] / 2;
            q.a[i] = Math.cos(w2);
            q.b[i] = Math.sin(w2);
        }
        return q;
    }

    public static Quaternion rotY(double omega) {
        return rotY(new Array1D(omega));
    }

    public static Quaternion rotY(Array1D omega) {
        double omega1[] = omega.toDoubleArray();
        int n = omega1.length;

        Quaternion q = new Quaternion(n);
        double w2;
        for (int i = 0; i < n; i++) {
            w2 = omega1[i] / 2;
            q.a[i] = Math.cos(w2);
            q.c[i] = Math.sin(w2);
        }
        return q;
    }

    public static Quaternion rotZ(double omega) {
        return rotZ(new Array1D(omega));
    }

    public static Quaternion rotZ(Array1D omega) {
        double omega1[] = omega.toDoubleArray();
        int n = omega1.length;

        Quaternion q = new Quaternion(n);
        double w2;
        for (int i = 0; i < n; i++) {
            w2 = omega1[i] / 2;
            q.a[i] = Math.cos(w2);
            q.d[i] = Math.sin(w2);
        }
        return q;
    }

    public static Quaternion rotZXZ(double phi1, double Phi, double phi2) {
        return rotZXZ(new double[]{phi1}, new double[]{Phi}, new double[]{phi2});

    }

    public static Quaternion rotZXZ(Array1D phi1, Array1D Phi, Array1D phi2) {
        return rotZXZ(phi1.toDoubleArray(), Phi.toDoubleArray(), phi2.toDoubleArray());
    }

    public static Quaternion rotZXZ(double[] phi1, double[] Phi, double[] phi2) {
        double alpha[] = new double[phi1.length];
        for (int i = 0; i < phi1.length; i++) {
            alpha[i] = phi1[i] - Math.PI / 2D;
        }

        double gamma[] = new double[phi2.length];
        for (int i = 0; i < phi2.length; i++) {
            gamma[i] = phi2[i] - 3D * Math.PI / 2D;

        }
        return rotZYZ(alpha, Phi, gamma);
    }

    public static Quaternion rotZYZ(double alpha, double beta, double gamma) {
        return rotZYZ(new double[]{alpha}, new double[]{beta}, new double[]{gamma});
    }

    public static Quaternion rotZYZ(Array1D alpha, Array1D beta, Array1D gamma) {
        return rotZYZ(alpha.toDoubleArray(), beta.toDoubleArray(), gamma.toDoubleArray());
    }

    public static Quaternion rotZYZ(double[] alpha, double[] beta, double[] gamma) {

        int n = Math.max(alpha.length, Math.max(beta.length, gamma.length));

        boolean doalpha = alpha.length > 1;
        boolean dobeta = beta.length > 1;
        boolean dogamma = gamma.length > 1;
        boolean dobetagamma = dobeta || dogamma;

        double aa, bb, gg,
                ca = Math.cos(alpha[0] / 2),
                sa = Math.sin(alpha[0] / 2),
                cb = Math.cos(beta[0] / 2),
                sb = Math.sin(beta[0] / 2),
                cg = Math.cos(gamma[0] / 2),
                sg = Math.sin(gamma[0] / 2),
                cbcg = cb * cg,
                cbsg = cb * sg,
                sbsg = sb * sg,
                sbcg = sb * cg;
        Quaternion q = new Quaternion(n);

        for (int i = 0; i < n; i++) {
            if (doalpha) {
                aa = alpha[i] / 2;
                ca = Math.cos(aa);
                sa = Math.sin(aa);
            }
            if (dobeta) {
                bb = beta[i] / 2;
                cb = Math.cos(bb);
                sb = Math.sin(bb);
            }
            if (dogamma) {
                gg = gamma[i] / 2;
                cg = Math.cos(gg);
                sg = Math.sin(gg);
            }

            if (dobetagamma) {
                cbcg = cb * cg;
                cbsg = cb * sg;
                sbsg = sb * sg;
                sbcg = sb * cg;
            }

            q.a[i] = ca * cbcg - sa * cbsg;
            q.b[i] = ca * sbsg - sa * sbcg;
            q.c[i] = ca * sbcg + sa * sbsg;
            q.d[i] = ca * cbsg + sa * cbcg;
        }
        return q;

    }

    public static Quaternion rand(int n) {
        Array1D alpha = Array1D.rand(n).multiplyd(2 * Math.PI);
        Array1D beta = Array1D.rand(n).minusd(.5).multiplyd(2).acos();
        Array1D gamma = Array1D.rand(n).multiplyd(2 * Math.PI);

        return rotZYZ(alpha, beta, gamma);

    }

    public static Quaternion concat(Quaternion... qr) {
        int sz = 0;
        for (Quaternion q : qr) {
            sz += q.a.length;
        }

        Quaternion qn = new Quaternion(sz);
        sz = 0;
        for (Quaternion q : qr) {
            System.arraycopy(q.a, 0, qn.a, sz, q.a.length);
            System.arraycopy(q.b, 0, qn.b, sz, q.b.length);
            System.arraycopy(q.c, 0, qn.c, sz, q.c.length);
            System.arraycopy(q.d, 0, qn.d, sz, q.d.length);
            sz += q.a.length;
        }
        return qn;
    }

    static Quaternion identity() {
        return new Quaternion(1, 0, 0, 0);
    }

    private double[] a, b, c, d;

    public Quaternion() {
        this(0);
    }

    public Quaternion(int n) {
        this.a = new double[n];
        this.b = new double[n];
        this.c = new double[n];
        this.d = new double[n];
    }

    public Quaternion(Array1D a, Array1D b, Array1D c, Array1D d) {
        this(a.toDoubleArray(), b.toDoubleArray(), c.toDoubleArray(), d.toDoubleArray());

    }

    public Quaternion(double a, double b, double c, double d) {
        this.a = new double[]{a};
        this.b = new double[]{b};
        this.c = new double[]{c};
        this.d = new double[]{d};
    }

    public Quaternion(double[] a, double[] b, double[] c, double[] d) {
        this.a = Arrays.copyOf(a, a.length);
        this.b = Arrays.copyOf(b, b.length);
        this.c = Arrays.copyOf(c, c.length);
        this.d = Arrays.copyOf(d, d.length);
    }

    public Quaternion get(int... ndx) {
        Quaternion q = new Quaternion();
        q.a = Array1D.get(this.a, ndx);
        q.b = Array1D.get(this.b, ndx);
        q.c = Array1D.get(this.c, ndx);
        q.d = Array1D.get(this.d, ndx);
        return q;
    }

    public Quaternion get(boolean[] ndx) {
        Quaternion q = new Quaternion();
        q.a = Array1D.get(this.a, ndx);
        q.b = Array1D.get(this.b, ndx);
        q.c = Array1D.get(this.c, ndx);
        q.d = Array1D.get(this.d, ndx);
        return q;
    }

    public double[] a() {
        return a;
    }

    public double[] b() {
        return b;
    }

    public double[] c() {
        return c;
    }

    public double[] d() {
        return d;
    }

    public void set(Quaternion q) {
        this.a = q.a;
        this.b = q.b;
        this.c = q.c;
        this.d = q.d;
    }

    public void set(int[] ndx, Quaternion q) {
        Array1D.set(a, ndx, q.a);
        Array1D.set(b, ndx, q.b);
        Array1D.set(c, ndx, q.c);
        Array1D.set(d, ndx, q.d);
    }

    public void set(boolean[] ndx, Quaternion q) {
        Array1D.set(a, ndx, q.a);
        Array1D.set(b, ndx, q.b);
        Array1D.set(c, ndx, q.c);
        Array1D.set(d, ndx, q.d);
    }

    public Quaternion multiply(Quaternion q) {
        int n = Math.max(a.length, q.a.length);
        Quaternion qn = new Quaternion(n);
        if (q.a.length == 1) {
            double aa = q.a[0], bb = q.b[0], cc = q.c[0], dd = q.d[0];
            for (int i = 0; i < n; i++) {
                qn.a[i] = a[i] * aa - b[i] * bb - c[i] * cc - d[i] * dd;
                qn.b[i] = a[i] * bb + b[i] * aa + c[i] * dd - d[i] * cc;
                qn.c[i] = a[i] * cc + c[i] * aa + d[i] * bb - b[i] * dd;
                qn.d[i] = a[i] * dd + d[i] * aa + b[i] * cc - c[i] * bb;
            }
        } else if (a.length == 1) {
            for (int i = 0; i < n; i++) {
                qn.a[i] = a[0] * q.a[i] - b[0] * q.b[i] - c[0] * q.c[i] - d[0] * q.d[i];
                qn.b[i] = a[0] * q.b[i] + b[0] * q.a[i] + c[0] * q.d[i] - d[0] * q.c[i];
                qn.c[i] = a[0] * q.c[i] + c[0] * q.a[i] + d[0] * q.b[i] - b[0] * q.d[i];
                qn.d[i] = a[0] * q.d[i] + d[0] * q.a[i] + b[0] * q.c[i] - c[0] * q.b[i];
            }
        } else {
            for (int i = 0; i < n; i++) {
                qn.a[i] = a[i] * q.a[i] - b[i] * q.b[i] - c[i] * q.c[i] - d[i] * q.d[i];
                qn.b[i] = a[i] * q.b[i] + b[i] * q.a[i] + c[i] * q.d[i] - d[i] * q.c[i];
                qn.c[i] = a[i] * q.c[i] + c[i] * q.a[i] + d[i] * q.b[i] - b[i] * q.d[i];
                qn.d[i] = a[i] * q.d[i] + d[i] * q.a[i] + b[i] * q.c[i] - c[i] * q.b[i];
            }
        }
        return qn;
    }

    public Vec3 rotate(Vec3 v) {
        return v.rotate(this);
    }

    public Array1D dot(Quaternion q) {

        double dot[];
        int n = Math.max(a.length, q.a.length);

        dot = new double[n];

        if (q.a.length == 1) {
            double aa = q.a[0], bb = q.b[0], cc = q.c[0], dd = q.d[0];
            for (int i = 0; i < n; i++) {
                dot[i] = a[i] * aa + b[i] * bb + c[i] * cc + d[i] * dd;
            }
        } else if (a.length == 1) {
            double aa = a[0], bb = b[0], cc = c[0], dd = d[0];
            for (int i = 0; i < n; i++) {
                dot[i] = aa * q.a[i] + bb * q.b[i] + cc * q.c[i] + dd * q.d[i];
            }
        } else {
            for (int i = 0; i < n; i++) {
                dot[i] = a[i] * q.a[i] + b[i] * q.b[i] + c[i] * q.c[i] + d[i] * q.d[i];
            }
        }

        return new Array1D(dot);
    }

    public Array1D angle() {
        double w[] = new double[a.length];
        for (int i = 0; i < a.length; i++) {
            w[i] = 2 * Math.acos(Math.abs(this.a[i]));

            if (Double.isNaN(w[i])) {
                w[i] = 0;
            }
            if (Double.isInfinite(w[i])) {
                w[i] = 0;
            }
        }
        return new Array1D(w);
    }

    public Array1D angle(Quaternion q) {
        Array1D w = this.dot(q);
        double ww[] = w.toDoubleArray();
        for (int i = 0; i < a.length; i++) {
            ww[i] = 2 * Math.acos(Math.abs(ww[i]));

            if (Double.isNaN(ww[i])) {
                ww[i] = 0;
            }
            if (Double.isInfinite(ww[i])) {
                ww[i] = 0;
            }
        }
        return w;
    }

    public Vec3 axis() {
        double x[] = new double[b.length];
        double y[] = new double[c.length];
        double z[] = new double[d.length];
        double norm;
        for (int i = 0; i < x.length; i++) {
            norm = Math.sqrt(b[i] * b[i] + c[i] * c[i] + d[i] * d[i]);
            if (norm < 1e-8) {
                x[i] = 1;
            } else if (a[i] < 0) {
                x[i] = -b[i] / norm;
                y[i] = -c[i] / norm;
                z[i] = -d[i] / norm;
            } else {
                x[i] = b[i] / norm;
                y[i] = c[i] / norm;
                z[i] = d[i] / norm;
            }
        }
        return new Vec3(x, y, z);
    }

    public Quaternion inverse() {
        Quaternion q = new Quaternion(a.length);
        q.a = Arrays.copyOf(a, a.length);
        for (int i = 0; i < a.length; i++) {
            q.b[i] = -this.b[i];
            q.c[i] = -this.c[i];
            q.d[i] = -this.d[i];
        }
        return q;
    }

    public Array1D isNull() {
        return norm().le(1e-14);
    }

    public Array1D norm() {
        return dot(this).sqrt();
    }

    public Quaternion normalize() {
        return divide(norm());
    }

    public Quaternion divide(double v) {
        return divide(new double[]{v});
    }

    public Quaternion divide(Array1D v) {
        return divide(v.toDoubleArray());
    }

    public Quaternion divide(double[] v) {
        int n = Math.max(a.length, v.length);
        Quaternion q = new Quaternion(n);
        if (v.length == 1) {
            for (int i = 0; i < n; i++) {
                q.a[i] = a[i] / v[0];
                q.b[i] = b[i] / v[0];
                q.c[i] = c[i] / v[0];
                q.d[i] = d[i] / v[0];
            }
        } else if (a.length == 1) {
            for (int i = 0; i < n; i++) {
                q.a[i] = a[0] / v[i];
                q.b[i] = b[0] / v[i];
                q.c[i] = c[0] / v[i];
                q.d[i] = d[0] / v[i];
            }
        } else {
            for (int i = 0; i < n; i++) {
                q.a[i] = a[i] / v[i];
                q.b[i] = b[i] / v[i];
                q.c[i] = c[i] / v[i];
                q.d[i] = d[i] / v[i];
            }
        }
        return q;
    }

    public Quaternion multiply(double v) {
        return multiply(new double[]{v});
    }

    public Quaternion multiply(Array1D v) {
        return multiply(v.toDoubleArray());
    }

    public Quaternion multiply(double[] v) {
        int n = Math.max(a.length, v.length);
        Quaternion q = new Quaternion(n);
        if (v.length == 1) {
            for (int i = 0; i < n; i++) {
                q.a[i] = a[i] * v[0];
                q.b[i] = b[i] * v[0];
                q.c[i] = c[i] * v[0];
                q.d[i] = d[i] * v[0];
            }
        } else if (a.length == 1) {
            for (int i = 0; i < n; i++) {
                q.a[i] = a[0] * v[i];
                q.b[i] = b[0] * v[i];
                q.c[i] = c[0] * v[i];
                q.d[i] = d[0] * v[i];
            }
        } else {
            for (int i = 0; i < n; i++) {
                q.a[i] = a[i] * v[i];
                q.b[i] = b[i] * v[i];
                q.c[i] = c[i] * v[i];
                q.d[i] = d[i] * v[i];
            }
        }
        return q;
    }

    public Quaternion prod(Quaternion q) {
        Quaternion qr[] = new Quaternion[q.size()];
        for (int i = 0; i < q.size(); i++) {
            qr[i] = this.multiply(q.get(i));
        }
        return Quaternion.concat(qr);
    }

    public Vec3 rodrigues() {
        Vec3 v = new Vec3(a.length);
        double x[] = v.x();
        double y[] = v.y();
        double z[] = v.z();
        double aa;
        for (int i = 0; i < a.length; i++) {
            aa = (Math.abs(a[i]) < 1e-16) ? 1e-50 : a[i];
            x[i] = b[i] / aa;
            y[i] = c[i] / aa;
            z[i] = d[i] / aa;
        }
        return v;
    }

    public Vec3 euler() {
        return euler("ZYZ");
    }

    public Vec3 euler(String convention) {

        double at1, at2;

        boolean ZYZ = "zyz".equals(convention.toLowerCase());
        boolean ZXZ = "zxz".equals(convention.toLowerCase());
        boolean NFFT = "nfft".equals(convention.toLowerCase());

        Vec3 v = new Vec3(a.length);
        double alpha[] = v.x();
        double beta[] = v.y();
        double gamma[] = v.z();

        for (int i = 0; i < a.length; i++) {
            at1 = Math.atan2(d[i], a[i]);
            at2 = Math.atan2(b[i], c[i]);

            alpha[i] = at1 - at2;
            beta[i] = 2 * Math.atan2(Math.sqrt(b[i] * b[i] + c[i] * c[i]), Math.sqrt(a[i] * a[i] + d[i] * d[i]));
            gamma[i] = at1 + at2;

            if (Math.abs(beta[i]) < 1e-8) {
                alpha[i] += gamma[i];
                gamma[i] = 0;
            }

            if (NFFT) {
                alpha[i] = ((alpha[i] + 3 * Math.PI) % (2 * Math.PI)) - Math.PI;
                gamma[i] = ((gamma[i] + 3 * Math.PI) % (2 * Math.PI)) - Math.PI;
            } else if (ZXZ) {
                if (Math.abs(beta[i]) > 1e-8) {
                    alpha[i] += Math.PI / 2D;
                    gamma[i] += 3D * Math.PI / 2D;
                }

                alpha[i] = (alpha[i] + 4 * Math.PI) % (2 * Math.PI);
                gamma[i] = (gamma[i] + 4 * Math.PI) % (2 * Math.PI);
            } else {
                alpha[i] %= (2 * Math.PI);
                gamma[i] %= (2 * Math.PI);
            }
        }

        return v;
    }

    public int size() {
        return this.a.length;
    }

    public void print() {
        Formatter formatter = new Formatter(Locale.US);

        formatter.format("quat (size = %d): \n", size());
        formatter.format("     [");
        for (double v : a) {
            formatter.format(" %6.2f", v);
        }
        formatter.format("\n       ");

        for (double v : b) {
            formatter.format(" %6.2f", v);
        }
        formatter.format("\n       ");

        for (double v : c) {
            formatter.format(" %6.2f", v);
        }
        formatter.format("\n       ");

        for (double v : d) {
            formatter.format(" %6.2f", v);
        }
        formatter.format(" ]\n");

        System.out.println(formatter.toString());
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
            formatter.format("(%.4f,%.4f,%.4f,%.4f)%s", this.a[i], this.b[i], this.c[i], this.d[i], (i < s - 1) ? "," : (trunkate) ? ",..." : "");
        }
        String str = "size=" + size();
        str += ",data=[" + formatter.toString() + "]";
        return str;
    }

    public Quaternion copy() {
        return new Quaternion(a, b, c, d);

    }

}
