/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

/**
 *
 * @author hios
 */
public interface ColorMapCanvas {

    public double getCLimMin();

    public double getCLimMax();

    public void setCLim(double cmin, double cmax);

    public void setColorMap(ColorMap cmp);

}
