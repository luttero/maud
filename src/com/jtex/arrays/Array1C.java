/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.arrays;

import java.io.BufferedInputStream;
import java.util.*;

/**
 *
 * @author hios
 */
public class Array1C {

    double re[];
    double im[];

    public static Array1C zeros(int n) {
        return new Array1C(new double[n], new double[n]);
    }

    public static Array1C ones(int n) {
        return new Array1C(Array1D.ones(n), Array1D.zeros(n));
    }

    public Array1C(double... ond2) {
        re = new double[ond2.length / 2];
        im = new double[ond2.length / 2];

//        System.out.println(ond2.length);
        for (int i = 0; i < ond2.length / 2; i++) {
            re[i] = ond2[2 * i];
            im[i] = ond2[2 * i + 1];
        }
    }

    public Array1C(Array1D re, Array1D im) {
        this.re = re.copy();
        this.im = im.copy();
    }

    public Array1C(double[] re, double[] im) {
        this.re = re;
        this.im = im;
    }

    public Array1C plus(Array1C b) {
        int n = re.length;

        double re2[] = new double[n];
        double im2[] = new double[n];

        for (int i = 0; i < n; i++) {
            re2[i] = this.re[i] + b.re[i];
            im2[i] = this.im[i] + b.im[i];
        }

        return new Array1C(re2, im2);
    }

    public Array1C sum() {
        int n = re.length;

        double re2 = 0;
        double im2 = 0;

        for (int i = 0; i < n; i++) {
            re2 += this.re[i];
            im2 += this.im[i];
        }

        return new Array1C(re2, im2);
    }

    public Array1C minus(Array1C b) {
        int n = re.length;

        double re2[] = new double[n];
        double im2[] = new double[n];

        for (int i = 0; i < n; i++) {
            re2[i] = this.re[i] - b.re[i];
            im2[i] = this.im[i] - b.im[i];
        }

        return new Array1C(re2, im2);
    }

    public Array1C multiply(double b) {
        int n = re.length;

        double re2[] = new double[n];
        double im2[] = new double[n];
        for (int i = 0; i < n; i++) {
            re2[i] = this.re[i] * b;
            im2[i] = this.im[i] * b;
        }
        return new Array1C(re2, im2);
    }

    public Array1C multiply(double bre, double bim) {
        int n = re.length;
        double re2[] = new double[n];
        double im2[] = new double[n];
        for (int i = 0; i < n; i++) {
            re2[i] = this.re[i] * bre - this.im[i] * bim;
            im2[i] = this.re[i] * bim + this.im[i] * bre;
        }
        return new Array1C(re2, im2);
    }

    public Array1C multiply(Array1C b) {
        int n = re.length;

        double re2[] = new double[n];
        double im2[] = new double[n];
        for (int i = 0; i < n; i++) {
            re2[i] = this.re[i] * b.re[i] - this.im[i] * b.im[i];
            im2[i] = this.re[i] * b.im[i] + this.im[i] * b.re[i];
        }
        return new Array1C(re2, im2);
    }

    // arry is columen wise
    public Array1C matrixMultiply(Array1C b, int d1, int d2) {

        int n = re.length;

        int d12 = re.length / d1; // === b.re.length/d2 !!

        double re2[] = new double[d1 * d2];
        double im2[] = new double[d1 * d2];
        int ij, jk, ik;
        double reij, imij;
        for (int i = 0; i < d1; i++) {
            for (int j = 0; j < d12; j++) {
                ij = i + j * d1;
                reij = this.re[ij];
                imij = this.im[ij];
                for (int k = 0; k < d2; k++) {
                    jk = j + k * d12;
                    ik = i + k * d1;
                    re2[ik] += (reij * b.re[jk] - imij * b.im[jk]);
                    im2[ik] += (reij * b.im[jk] + imij * b.re[jk]);
                }

            }
        }

        return new Array1C(re2, im2);
    }

    public Array1D abs() {
        double a[] = new double[re.length];
        for (int i = 0; i < re.length; i++) {
            a[i] = Math.sqrt(re[i] * re[i] + im[i] * im[i]);
        }
        return new Array1D(a);
    }

    public Array1C divide(double b) {
        int n = re.length;

        double re2[] = new double[n];
        double im2[] = new double[n];
        for (int i = 0; i < n; i++) {
            re2[i] = this.re[i] / b;
            im2[i] = this.im[i] / b;
        }
        return new Array1C(re2, im2);
    }

    public Array1C divide(Array1C b) {
        int n = re.length;

        double re2[] = new double[n];
        double im2[] = new double[n];
        for (int i = 0; i < n; i++) {
            double cc = b.re[i] * b.re[i] + b.im[i] * b.im[i];
            if (cc > 0) {
                re2[i] = (this.re[i] * b.re[i] + this.im[i] * b.im[i]) / cc;
                im2[i] = (this.im[i] * b.re[i] - this.re[i] * b.im[i]) / cc;
            }
        }
        return new Array1C(re2, im2);
    }

    private static String print(double[] re, double[] im) {
        Formatter formatter = new Formatter(Locale.US);
        formatter.format("%s", "[\n");
        for (int i = 0; i < re.length; i++) {
            formatter.format("   %7.4f %s %6.4fi\n", re[i], im[i] > 0 ? "+" : "-", Math.abs(im[i]));
        }
        formatter.format("%s", "                    ]");
        return formatter.toString();
    }

    public void print() {
        System.out.println(print(this.re, this.im));
    }

    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    private String paramString() {
	    Formatter formatter = new Formatter(Locale.US);
	    boolean trunkate = this.size() > 50;
	    int s = (trunkate) ? 50 : this.size();
	    for (int i = 0; i < s; i++) {
		    formatter.format("%.4f %s %.4fi%s", this.re[i], this.im[i] > 0 ? "+" : "-", Math.abs(this.im[i]), (i < s - 1) ? "," : (trunkate) ? ",..." : "");
	    }
	    String str = "size=" + size();
	    str += ",data=(" + formatter.toString() + ")";
	    return str;
    }

	public String toSaveString() { // Luca
		String saveStr = "";
		for (int i = 0; i < this.size(); i++) {
			saveStr += this.re[i] + " " + this.im[i];
			if (i < size() - 1)
				saveStr += " ";
		}
		return saveStr;
	}

	public static Array1C parse(String inputString) {

    	Vector<Double> myRe = new Vector(100, 100);
		Vector<Double> myIm = new Vector(100, 100);
		Scanner ss = new Scanner(inputString);
		ss.useDelimiter(" ");
		while (ss.hasNext()) {
			myRe.add(Double.parseDouble(ss.next()));
			myIm.add(Double.parseDouble(ss.next()));
		}

		int size = Math.min(myRe.size(), myIm.size());

		double[] mre = new double[size];
		double[] mim = new double[size];
		for (int i = 0; i < size; i++) {
			mre[i] = myRe.elementAt(i);
			mim[i] = myIm.elementAt(i);
		}
    	Array1C array = new Array1C(mre, mim);
    	return array;
	}

    public int size() {
        return this.re.length;
    }

    public Array1C get(int... ndx) {
        double[] re2 = new double[ndx.length];
        double[] im2 = new double[ndx.length];

        for (int i = 0; i < ndx.length; i++) {
            re2[i] = re[ndx[i]];
            im2[i] = im[ndx[i]];
        }
        return new Array1C(re2, im2);
    }

    public void set(int ndx, Array1C b) {
        re[ndx] = b.re[0];
        im[ndx] = b.im[0];
    }

    public void set(int[] ndx, Array1C b) {
        for (int i = 0; i < ndx.length; i++) {
            re[ndx[i]] = b.re[i];
            im[ndx[i]] = b.im[i];
        }
    }

    public double[] toDoubleArray() {
        double[] d = new double[2 * re.length];

        for (int i = 0; i < re.length; i++) {
            d[2 * i] = re[i];
            d[2 * i + 1] = im[i];
        }

        return d;

    }

    public Array1C matrixMultiplyTransposed(Array1C b, int d1, int d2) {
        int n = re.length;

        int d12 = re.length / d1; // === b.re.length/d2 !!

        double re2[] = new double[d1 * d2];
        double im2[] = new double[d1 * d2];
        int ij, jk, ik;
        double reij, imij;
        for (int i = 0; i < d1; i++) {
            for (int j = 0; j < d12; j++) {

                for (int k = 0; k < d2; k++) {
                    jk = j * d2 + k;
                    ik = i * d2 + k;
                    ij = i * d12 + j;
                    reij = this.re[ij];
                    imij = this.im[ij];
                    re2[ik] += (reij * b.re[jk] - imij * b.im[jk]);
                    im2[ik] += (reij * b.im[jk] + imij * b.re[jk]);
                }

            }
        }

        return new Array1C(re2, im2);
    }

    public Array1C conjugate() {
        double[] re2 = Arrays.copyOf(re, re.length);
        double[] im2 = new double[im.length];
        for (int i = 0; i < re.length; i++) {
            im2[i] = -im[i];
        }
        return new Array1C(re2, im2);
    }

    public Array1C log() {
        double[] re2 = new double[re.length];
        double[] im2 = new double[im.length];
        for (int i = 0; i < re.length; i++) {
            re2[i] = Math.log(Math.sqrt(this.re[i] * this.re[i] + this.im[i] * this.im[i]));
            im2[i] = Math.atan2(this.im[i], this.re[i]);
        }
        return new Array1C(re2, im2);
    }

    public Array1C sqrt() {

        double[] re2 = new double[re.length];
        double[] im2 = new double[im.length];
        double a, b, aa, bb;
        for (int i = 0; i < re.length; i++) {
            a = re[i];
            b = im[i];

            if (a == 0D && b == 0D) {
            } else if (b == 0D) {
                if (a >= 0D) {
                    re2[i] = Math.sqrt(a);
                } else {
                    im2[i] = Math.sqrt(-a);
                }
            } else {
                aa = Math.abs(a);
                bb = Math.abs(b);
                double dd = (aa >= bb) ? Math.sqrt(aa) * Math.sqrt(0.5D * (1.0D + Math.sqrt(1.0D + b * b / (a * a))))
                        : Math.sqrt(bb) * Math.sqrt(0.5D * (Math.abs(a / b) + Math.sqrt(1.0D + a * a / (b * b))));

                if (a >= 0D) {
                    re2[i] = dd;
                    im2[i] = b / (2D * dd);
                } else if (b >= 0D) {
                    re2[i] = b / (2D * dd);
                    im2[i] = dd;
                } else {
                    re2[i] = -b / (2D * dd);
                    im2[i] = -dd;
                }
            }
        }
        return new Array1C(re2, im2);

    }

    public Array1D real() {
        return new Array1D(re);
    }

    private Array1D imag() {
        return new Array1D(im);
    }

    public Array1C exp() {

        double[] re2 = Arrays.copyOf(re, re.length);
        double[] im2 = new double[im.length];

        double a, b, c;
        for (int i = 0; i < re.length; i++) {
            a = re[i];
            b = im[i];

            if (b == 0D) {
                re2[i] = Math.exp(a);
            } else {
                if (a == 0D) {
                    re2[i] = Math.cos(b);
                    im2[i] = Math.sin(b);
                } else {
                    c = Math.exp(a);
                    re2[i] = c * Math.cos(b);
                    im2[i] = c * Math.sin(b);
                }
            }
        }

        return new Array1C(re2, im2);
    }

}
