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

/**
 *
 * @author hios
 */
public interface ODFComponent {

    public double getPortion();

    public void setPortion(double p);

    public Array1D pdf(Miller h, Vec3 r);

    public Array1D eval(Quaternion qr);

    public Array1C calcFourier(int L);

    public int bandwidth();

    public double volume(Quaternion q, double radius);

    public double fibreVolume(Miller h, Vec3 r, double radius);

    public ODFComponent rotate(Quaternion q);

}
