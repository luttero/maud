/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.kernel;

import com.jtex.arrays.Array1D;

/**
 *
 * @author hios
 */
public class DeLaValleePoussin extends Kernel {

    private double C = 0;
    private double p = 50;

    protected Array1D a;

    public DeLaValleePoussin(double hw) {
        super();
        hw2p(hw);
        this.a = constructA();
    }

    @Override
    public double p2hw() {
        return 2.0 * Math.acos(Math.pow(0.5, (0.5 / p)));
    }

    @Override
    public void hw2p(double hw) {
//        double hw = Math.toRadians(5);
        this.p = 0.5 * Math.log(0.5) / Math.log(Math.cos(hw / 2.0));

//        System.out.println(hw + " " + p);
        double SqrtPi = Math.sqrt(Math.PI);
        this.C = SqrtPi * Math.pow(p, 3.0 / 2.0) + 9.0 * SqrtPi * Math.sqrt(p) / 8.0 + 17.0 / 128.0 * SqrtPi * Math.sqrt(1 / p);

    }

    @Override
    public Array1D A() {
        return a;
    }

    private Array1D constructA() {

        int L = Math.min(1000, Math.max(10, (int) p + 1));
		  System.out.println("DeLaValle dim " + L);

        Array1D A = Array1D.fill(L, 1.0);
        double a[] = A.toDoubleArray();
        a[1] = p / (p + 2.0);

        for (int i = 0; i < L - 2; i++) {
            double l = i + 1;
            a[i + 2] = ((p - l + 1) * a[i] - (2 * l + 1) * a[i + 1]) / (p + l + 2);
        }

        for (int i = 0; i < L; i++) {
            double l = (double) i;
            a[i] = (2 * l + 1) * a[i];
        }
        return (A.get(A.ge(1e-3).find()));
    }

    @Override
    public Array1D K(Array1D dist) {
        double EPS = Math.cos(3 * p2hw());

        double d[] = dist.toDoubleArray();
        double f[] = new double[d.length];
        for (int i = 0; i < d.length; i++) {
            if (d[i] > EPS) {
                f[i] = C * Math.pow(d[i], 2 * p);
            }
        }
        return new Array1D(f);
    }

    @Override
    public Array1D RK(Array1D dist) {
        double EPS = Math.cos(3 * p2hw());
        Array1D fo = dist.plus(1).divided(2);
        int imp[] = fo.g(EPS).find();
        Array1D res = Array1D.zeros(dist.size());
        res.set(imp, fo.get(imp).pow(p).multiply(1 + p));
        return res;
//        return dist.plus(1).divided(2).pow(p).multiplyd(1 + p);
    }

    @Override
    public Array1D RRK(Array1D dh, Array1D dr) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public int bandwidth() {
        return a.size() - 1;
    }

}
