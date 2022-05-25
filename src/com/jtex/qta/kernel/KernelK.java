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
public interface KernelK {

//    void construct_A();
//
//    void construct_K();
//
//    void construct_RK();
//    public Array1D hw2p()
    public void hw2p(double hw);

    public double p2hw();

    public Array1D A();

    public Array1D K(Array1D dist);

    public Array1D RK(Array1D dist);

    public Array1D RRK(Array1D dh, Array1D dr);
}
