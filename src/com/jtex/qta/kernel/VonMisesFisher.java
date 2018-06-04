/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.kernel;

import com.jtex.arrays.Array1D;
import com.jtex.util.SpecFun;

/**
 *
 * @author hios
 */
public class VonMisesFisher extends Kernel {

    private double p;
    private double C;
    private Array1D a;

    public static void main(String[] args) {

        VonMisesFisher psi = new VonMisesFisher(Math.toRadians(10));

        System.out.println(psi.RK(new Array1D(1)));

//    ();
    }

    public VonMisesFisher(double hw) {
        super();
        hw2p(hw);

        double[] X = new double[2];
        SpecFun.ribesl(this.p, 0, 2, X);
        this.C = (X[0] - X[1]);

        this.a = constructA();

    }

    @Override
    public void hw2p(double hw) {
        this.p = Math.log(2) / (1 - Math.cos(hw));
    }

    @Override
    public double p2hw() {
        return Math.acos(1 - Math.log(2) / this.p);
    }

    @Override
    public Array1D A() {
        return a;
    }

    private Array1D constructA() {
        double[] X = new double[1000];
        SpecFun.ribesl(this.p, 0, 2, X);
        double a[] = new double[1000];

        for (int i = 0; i < X.length - 1; i++) {
            a[i] = (X[i + 1] - X[i]) / (X[1] - X[0]);
        }
        Array1D A = new Array1D(a);

        return (A.get(A.ge(1e-3).find()));
    }

    @Override
    public Array1D K(Array1D dist) {
        double[] d = dist.toDoubleArray();
        for (int i = 0; i < d.length; i++) {
            d[i] = Math.exp(2 * p * (d[i] * d[i] - 1)) / this.C;
        }
        return new Array1D(d);
    }

    @Override
    public Array1D RK(Array1D dist) {

        double res[] = new double[1];
        double[] d = dist.toDoubleArray();
        for (int i = 0; i < d.length; i++) {
            SpecFun.ribesl(this.p * (1 + d[i]) / 2, 0D, 2, res);
            d[i] = Math.exp(p * (d[i] - 1)) * res[0] / this.C;
        }
        return new Array1D(d);

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
