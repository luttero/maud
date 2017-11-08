/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.odf;

import com.jtex.arrays.Array1C;
import com.jtex.arrays.Array1D;
import com.jtex.geom.Miller;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Vec3;

/**
 *
 * @author hios
 */
public class UniformComponent implements ODFComponent {

    Symmetry cs, ss;
    double p;

    public UniformComponent(Symmetry cs, Symmetry ss) {
        this.cs = cs;
        this.ss = ss;
        this.p = 1D;
    }

    @Override
    public Array1D pdf(Miller h, Vec3 r) {
        return Array1D.fill(h.size() * r.size(), 1);
    }

    @Override
    public Array1D eval(Quaternion qr) {
        return Array1D.fill(qr.size(), 1);
    }

    @Override
    public Array1C calcFourier(int L) {
        Array1C c_hat = Array1C.zeros(FourierComponent.deg2dim(L));
        c_hat.set(0, new Array1C(1, 0));
        return c_hat;
    }

    @Override
    public double volume(Quaternion q, double radius) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public int bandwidth() {
        return 1;
    }

    @Override
    public double fibreVolume(Miller h, Vec3 r, double radius) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public ODFComponent rotate(Quaternion q) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public double getPortion() {
        return p;
    }

    @Override
    public void setPortion(double p) {
        this.p = p;
    }

}
