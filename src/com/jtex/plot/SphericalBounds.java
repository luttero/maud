/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

/**
 *
 * @author flb
 */
public class SphericalBounds {

    private double thetamin;
    private double thetamax;
    private double rhomin;
    private double rhomax;

    public SphericalBounds() {
        this(Math.PI / 2, 2 * Math.PI);
    }

    public SphericalBounds(double thetamax, double rhomax) {
        this(0, thetamax, 0, rhomax);
    }

    public SphericalBounds(double thetamin, double thetamax, double rhomin, double rhomax) {
        this.thetamin = thetamin;
        this.thetamax = thetamax;
        this.rhomin = rhomin;
        this.rhomax = rhomax;
    }

    public boolean isThetaFun() {
        return false;
    }

    public double getRhomax() {
        return rhomax;
    }

    public double getRhomin() {
        return rhomin;
    }

    public double getThetamax() {
        return thetamax;
    }

    public double getThetamin() {
        return thetamin;
    }

    public double maxTheta(double rho) {
        return Double.NaN;
    }

}
