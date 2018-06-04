/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom.grid;

import com.jtex.arrays.Array1D;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Vec2;
import com.jtex.geom.Vec3;

/**
 *
 * @author hios
 */
public class S2Grid {

    public static S2Grid regularS2Grid(double res, double mintheta, double maxtheta, double minrho, double maxrho, boolean no_center) {
        double dtheta = maxtheta - mintheta;
        double drho = maxrho - minrho;

        res = 2 * dtheta / Math.round(2 * dtheta / res);
        double ntheta = Math.floor(Math.round(2 * dtheta / res + (no_center ? 1 : 0)) / 2);

        Array1D theta = Array1D.fill(mintheta + (no_center ? res / 2 : 0), mintheta - (no_center ? res / 2 : 0) + (ntheta * res), res);
        Array1D rhos[] = new Array1D[theta.size()];
        double steps = Math.round((maxrho - minrho) / res);

        for (int i = 0; i < theta.size(); i++) {
            rhos[i] = Array1D.fill(0, drho * (steps - 1) / steps, drho / steps);
        }

        S2Grid g = new S2Grid(theta, rhos);
        return g;
    }

    public static S2Grid equispacedS2Grid(double res, double mintheta, double maxtheta, double minrho, double maxrho, boolean no_center) {

        double dtheta = maxtheta - mintheta;
        double drho = maxrho - minrho;

        res = 2 * dtheta / Math.round(2 * dtheta / res);
        double ntheta = Math.floor(Math.round(2 * dtheta / res + (no_center ? 1 : 0)) / 2);

        Array1D theta = Array1D.fill(mintheta + (no_center ? res / 2 : 0), mintheta - (no_center ? res / 2 : 0) + (ntheta * res), res);
        Array1D rhos[] = new Array1D[theta.size()];
        double[] steps = theta.sin().multiply(drho / dtheta * ntheta).round().max(1).toDoubleArray();

        for (int i = 0; i < theta.size(); i++) {
            double step = steps[i];
            double shift = minrho + ((i + 1) % 2) * drho / step / 2;
            rhos[i] = Array1D.fill(shift, shift + drho * (step - 1) / step, drho / step);
        }

        S2Grid g = new S2Grid(theta, rhos);
        return g;
    }

    private S1Grid theta;
    private S1Grid[] rho;

    public S2Grid(S1Grid theta, S1Grid[] rho) {
        this.theta = theta;
        this.rho = rho;
    }

    private S2Grid(Array1D theta, Array1D[] rhos) {
        double[] th = theta.toDoubleArray();
        this.theta = new S1Grid(th, th[0], Math.PI);

        this.rho = new S1Grid[th.length];
        for (int i = 0; i < rhos.length; i++) {
            double[] rh = rhos[i].toDoubleArray();
            this.rho[i] = new S1Grid(rh, rh[0], 2 * Math.PI);
        }
    }

    public int getThetaSize() {
        return theta.size();
    }

    public int getRhoSize(int i) {
        return rho[i].size();
    }

    public S1Grid[] getRho() {
        return rho;
    }

    public S1Grid getRho(int i) {
        return rho[i];
    }

    public double getRho(int i, int j) {
        return rho[i].x(j);
    }

    public S1Grid getTheta() {
        return theta;
    }

    public double getThetaMin() {
        return theta.getMin();
    }

    public double getThetaPeriod() {
        return theta.getPeriod();
    }

    public double getRhoMin(int i) {
        return rho[i].getMin();
    }

    public double getRhoPeriod(int i) {
        return rho[i].getPeriod();
    }

//    public void find(double theta, double rho) {
//    }
    public boolean[][] find_region(double ytheta, double yrho, double e) {
        boolean[] found = theta.find_region(ytheta, e);

        boolean[][] found2 = new boolean[theta.size()][];

        double cyt = Math.cos(ytheta), syt = Math.sin(ytheta);
        double cs, ss, ce = Math.cos(e);

        double th[] = theta.x();
        for (int i = 0; i < theta.size(); i++) {
            if (found[i]) {
                cs = cyt * Math.cos(th[i]);
                ss = syt * Math.sin(th[i]);

                if (ss < 0.0001 || (ce - cs) / ss < -0.9999) {
                    boolean[] t = new boolean[rho[i].size()];
                    for (int j = 0; j < t.length; j++) {
                        t[j] = true;
                    }
                    found2[i] = t;
                } else if (cs + ss > ce) {
                    found2[i] = rho[i].find_region(yrho, Math.acos((ce - cs) / ss));
                }
            }
        }

        return found2;
    }

    public double getTheta(int k) {
        return theta.x(k);
    }

    void setPeriod(double maxbeta, double maxalpha) {
        theta.setPeriod(maxbeta);

        for (S1Grid r : rho) {
            r.setPeriod(maxalpha);
        }
    }

    private int size() {
        int i = 0;
        for (S1Grid r : rho) {
            i += r.size();
        }
        return i;
    }

    public Vec2 expand() {
        int n = size();
        double[] xrho = new double[n];
        double[] xtheta = new double[n];

        int ib[] = new int[getThetaSize() + 1];
        int i = 1;
        int offset = 0;
        for (S1Grid r : rho) {
            System.arraycopy(r.x(), 0, xrho, offset, r.size());
            offset += r.size();
            ib[i++] = offset;
        }

        System.out.println(new Array1D(ib));

        double th[] = getTheta().x();
        for (i = 0; i < getThetaSize(); i++) {
            double beta = th[i];
            for (int j = ib[i]; j < ib[i + 1]; j++) {
                xtheta[j] = beta;
            }
        }
        return new Vec2(xtheta, xrho);
    }

    public Vec3 toVec3() {
        Vec2 thetarho = expand();
        return new Vec3(thetarho.x(), thetarho.y());
    }

}
