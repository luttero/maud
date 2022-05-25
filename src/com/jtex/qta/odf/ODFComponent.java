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
import com.jtex.geom.Vec3;
import it.unitn.ing.rista.diffr.XRDcat;

/**
 *
 * @author hios
 */
public class ODFComponent extends XRDcat {

    public double getPortion() {
    	return 0;
    }

    public void setPortion(double p) {}

    public Array1D pdf(Miller h, Vec3 r) {
    	return null;
    }

    public Array1D eval(Quaternion qr) {
    	return null;
    }

    public Array1C calcFourier(int L) {
    	return null;
    }

    public int bandwidth() { return 0; }

    public double volume(Quaternion q, double radius) { return 0; }

    public double fibreVolume(Miller h, Vec3 r, double radius) { return 0; }

    public ODFComponent rotate(Quaternion q) { return null; }


    // interface for Maud by Luca Lutterotti


	public ODFComponent(XRDcat aobj, String alabel) {
		super(aobj, alabel);
	}

	public ODFComponent(XRDcat aobj) {
		this(aobj, "Component x");
	}

	public ODFComponent() {
	}

}
