/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom;

import java.util.Formatter;
import java.util.Locale;

/**
 *
 * @author flb
 */
public class Mat3 {

    private double m[];

    public static Mat3 identity() {
        double m[] = new double[9];
        m[0] = 1;
        m[4] = 1;
        m[8] = 1;
        return new Mat3(m);
    }

    public Mat3() {
        this.m = new double[9];
    }

    public Mat3(double a00, double a01, double a02, double a10, double a11, double a12, double a20, double a21, double a22) {
        this.m = new double[9];
        this.m[0] = a00;
        this.m[1] = a01;
        this.m[2] = a02;
        this.m[3] = a10;
        this.m[4] = a11;
        this.m[5] = a12;
        this.m[6] = a20;
        this.m[7] = a21;
        this.m[8] = a22;
    }

    public Mat3(double[] m) {
        assert (m.length == 9) : "double[] must be of length 9";
        this.m = m;
    }

    public static Mat3 fromVec3(Vec3 xyz) {
        return fromVec3(xyz.get(0), xyz.get(1), xyz.get(2));
    }

    public static Mat3 fromVec3(Vec3 x, Vec3 y, Vec3 z) {
        double m[] = new double[9];

        double v[] = x.getDouble(0);
        m[0] = v[0];
        m[3] = v[1];
        m[6] = v[2];

        v = y.getDouble(0);
        m[1] = v[0];
        m[4] = v[1];
        m[7] = v[2];

        v = z.getDouble(0);
        m[2] = v[0];
        m[5] = v[1];
        m[8] = v[2];
        return new Mat3(m);
    }

    public static Mat3 fromDiag(double d1, double d2, double d3) {
        double m[] = new double[9];
        m[0] = d1;
        m[4] = d2;
        m[8] = d3;
        return new Mat3(m);
    }

    public static Mat3 fromDiag(double[] d) {
        double m[] = new double[9];
        m[0] = d[0];
        m[4] = d[1];
        m[8] = d[2];
        return new Mat3(m);
    }

    public static Mat3 rotX(double omega) {
        double m[] = new double[9];
        double c = Math.cos(omega), s = Math.sin(omega);
        m[0] = 1;
        m[4] = c;
        m[5] = -s;
        m[7] = s;
        m[8] = c;
        return new Mat3(m);
    }

    public static Mat3 rotY(double omega) {
        double m[] = new double[9];
        double c = Math.cos(omega), s = Math.sin(omega);
        m[0] = c;
        m[2] = s;
        m[4] = 1;
        m[6] = -s;
        m[8] = c;
        return new Mat3(m);
    }

    public static Mat3 rotZ(double omega) {
        double m[] = new double[9];
        double c = Math.cos(omega), s = Math.sin(omega);
        m[0] = c;
        m[1] = -s;
        m[3] = s;
        m[4] = c;
        m[8] = 1;
        return new Mat3(m);
    }

    public static Mat3 rotZXZ(double phi1, double Phi, double phi2) {
        return Mat3.rotZ(phi1).multiply(Mat3.rotX(Phi)).multiply(Mat3.rotZ(phi2));
    }

    public static Mat3 rotZYZ(double alpha, double beta, double gamma) {
        return Mat3.rotZ(alpha).multiply(Mat3.rotY(beta)).multiply(Mat3.rotZ(gamma));
    }

    public static Mat3 rotAxisAngle(Vec3 axis, double angle) throws Exception {
        Vec3 n = axis.normalize();

        double a[] = axis.getDouble(0);

        double n0 = a[0], n1 = a[1], n2 = a[2],
                c = Math.cos(angle), s = Math.sin(angle), c1 = 1 - c;

        double m[] = new double[9];
        m[0] = n0 * n0 * c1 + c;
        m[1] = n0 * n1 * c1 - n2 * s;
        m[2] = n0 * n2 * c1 + n1 * s;

        m[3] = n1 * n0 * c1 + n2 * s;
        m[4] = n1 * n1 * c1 + c;
        m[5] = n1 * n2 * c1 - n0 * s;

        m[6] = n2 * n0 * c1 - n1 * s;
        m[7] = n2 * n1 * c1 + n0 * s;
        m[8] = n2 * n2 * c1 + c;

        return new Mat3(m);
    }

    public static Mat3 rotH2R(Vec3 a1, Vec3 a2) throws Exception {
        Vec3 b1 = a1.normalize();
        Vec3 b2 = a2.normalize();

        Vec3 a = b1.cross(b2);
        if (a.isNull().get(0) == 1) {
            a = b1.orth();
        }
        return Mat3.rotAxisAngle(a, b1.angle(b2).get(0));

    }

    public static Mat3 rotU1V1U2V2(Vec3 a1, Vec3 b1, Vec3 a2, Vec3 b2) throws Exception {
        Vec3 u1 = a1.normalize();
        Vec3 v1 = b1.normalize();
        Vec3 u2 = a2.normalize();
        Vec3 v2 = b2.normalize();

        Vec3 d1 = u1.minus(v1);
        Vec3 d2 = u2.minus(v2);

        return new Mat3();
    }

    public Mat3 multiply(double a) {
        double N[] = new double[9];
        for (int i = 0; i < N.length; i++) {
            N[i] = this.m[i] * a;
        }
        return new Mat3(N);
    }
//

    public Vec3 multiply(Vec3 a) {
        Vec3 vn = new Vec3();
        vn.setX(a.dot(new Vec3(m[0], m[1], m[2])));
        vn.setY(a.dot(new Vec3(m[3], m[4], m[5])));
        vn.setZ(a.dot(new Vec3(m[6], m[7], m[8])));
        return vn;

    }

    public Mat3 minus(Mat3 B) {
        double out[] = new double[9];
        for (int i = 0; i < out.length; i++) {
            out[i] = this.m[i] - B.m[i];
        }
        return new Mat3(out);
    }

    public Mat3 transpose() {
        double out[] = new double[9];
        out[0] = this.m[0];
        out[1] = this.m[3];
        out[2] = this.m[6];
        out[3] = this.m[1];
        out[4] = this.m[4];
        out[5] = this.m[7];
        out[6] = this.m[2];
        out[7] = this.m[5];
        out[8] = this.m[8];
        return new Mat3(out);
    }

    public Mat3 inverse() {
        double a00 = this.m[0], a01 = this.m[1], a02 = this.m[2],
                a10 = this.m[3], a11 = this.m[4], a12 = this.m[5],
                a20 = this.m[6], a21 = this.m[7], a22 = this.m[8],
                b01 = a22 * a11 - a12 * a21,
                b11 = -a22 * a10 + a12 * a20,
                b21 = a21 * a10 - a11 * a20,
                // Calculate the determinant
                det = a00 * b01 + a01 * b11 + a02 * b21;

        if (det == 0) {
            return null;
        }
        det = 1.0 / det;

        double out[] = new double[9];
        out[0] = b01 * det;
        out[1] = (-a22 * a01 + a02 * a21) * det;
        out[2] = (a12 * a01 - a02 * a11) * det;
        out[3] = b11 * det;
        out[4] = (a22 * a00 - a02 * a20) * det;
        out[5] = (-a12 * a00 + a02 * a10) * det;
        out[6] = b21 * det;
        out[7] = (-a21 * a00 + a01 * a20) * det;
        out[8] = (a11 * a00 - a01 * a10) * det;
        return new Mat3(out);
    }

    public Mat3 adjoint() {
        double a00 = this.m[0], a01 = this.m[1], a02 = this.m[2],
                a10 = this.m[3], a11 = this.m[4], a12 = this.m[5],
                a20 = this.m[6], a21 = this.m[7], a22 = this.m[8];

        double[] out = new double[9];
        out[0] = (a11 * a22 - a12 * a21);
        out[1] = (a02 * a21 - a01 * a22);
        out[2] = (a01 * a12 - a02 * a11);
        out[3] = (a12 * a20 - a10 * a22);
        out[4] = (a00 * a22 - a02 * a20);
        out[5] = (a02 * a10 - a00 * a12);
        out[6] = (a10 * a21 - a11 * a20);
        out[7] = (a01 * a20 - a00 * a21);
        out[8] = (a00 * a11 - a01 * a10);
        return new Mat3(out);
    }

    public Mat3 multiply(Mat3 b) {
        double a00 = this.m[0], a01 = this.m[1], a02 = this.m[2],
                a10 = this.m[3], a11 = this.m[4], a12 = this.m[5],
                a20 = this.m[6], a21 = this.m[7], a22 = this.m[8],
                b00 = b.m[0], b01 = b.m[1], b02 = b.m[2],
                b10 = b.m[3], b11 = b.m[4], b12 = b.m[5],
                b20 = b.m[6], b21 = b.m[7], b22 = b.m[8];

        double out[] = new double[9];
        out[0] = a00 * b00 + a01 * b10 + a02 * b20;
        out[1] = a00 * b01 + a01 * b11 + a02 * b21;
        out[2] = a00 * b02 + a01 * b12 + a02 * b22;

        out[3] = a10 * b00 + a11 * b10 + a12 * b20;
        out[4] = a10 * b01 + a11 * b11 + a12 * b21;
        out[5] = a10 * b02 + a11 * b12 + a12 * b22;

        out[6] = a20 * b00 + a21 * b10 + a22 * b20;
        out[7] = a20 * b01 + a21 * b11 + a22 * b21;
        out[8] = a20 * b02 + a21 * b12 + a22 * b22;
        return new Mat3(out);
    }

    public double determinant() {
        double a00 = this.m[0], a01 = this.m[1], a02 = this.m[2],
                a10 = this.m[3], a11 = this.m[4], a12 = this.m[5],
                a20 = this.m[6], a21 = this.m[7], a22 = this.m[8];

        return a00 * (a22 * a11 - a12 * a21) + a01 * (-a22 * a10 + a12 * a20) + a02 * (a21 * a10 - a11 * a20);
    }

    public Vec3 diag() {
        return new Vec3(this.m[0], this.m[4], this.m[8]);
    }

    public double trace() {
        return this.m[0] + this.m[4] + this.m[8];
    }

//    public vec3 axis() {
//        vec3 b1 = new vec3(this.m[0] - 1, this.m[1], this.m[2]).normalize();
//        vec3 b2 = new vec3(this.m[3], this.m[4] - 1, this.m[5]).normalize();
//
//        vec3 a = b1.cross(b2).normalize();
//        return a.multiply(Math.signum(a.cross(b1).normalize().dot(multiply(b1))));
//    }
    public double angle() {
        return Math.acos((trace() - 1) / 2);
    }

    public Vec3 toVec3() {
        return Vec3.concat(new Vec3(this.m[0], this.m[3], this.m[6]),
                new Vec3(this.m[1], this.m[4], this.m[7]),
                new Vec3(this.m[2], this.m[5], this.m[8])
        );
    }

    @Override
    public String toString() {
        Formatter formatter = new Formatter(Locale.US);
        formatter.format("[ %2.4f %2.4f %2.4f \n  %2.4f %2.4f %2.4f \n  %2.4f %2.4f %2.4f ]", m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8]);
        return formatter.toString();
    }

}
