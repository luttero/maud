/*
 * @(#)DiffractionImageDatafile.java created Nov 8, 2007 Mesiano
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */
package it.unitn.ing.rista.diffr;

/**
 * The DiffractionImageDatafile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Nov 8, 2007 5:35:44 PM $
 * @since JDK1.1
 */
public class DiffractionImageDatafile extends XRDcat {

  protected double[] x = null;
  protected double[] y = null;
  protected double[] imageIntensity = null;

  public DiffractionImageDatafile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public DiffractionImageDatafile(XRDcat aobj) {
    this(aobj, "Datafile_x");
  }

  public DiffractionImageDatafile() {
  }

    @Override
  public void initParameters() {
    super.initParameters();
//    setDataType(DIFFRACTION_IMAGE);
  }

  public void setSize(int numberOfPixels) {
    if (x == null || size() != numberOfPixels) {
      x = new double[numberOfPixels];
      y = new double[numberOfPixels];
      imageIntensity = new double[numberOfPixels];
    }
  }

  public int size() {
    return x.length;
  }

  public double[] getX() {
    return x;
  }

  public void setX(int i, double x1) {
    x[i] = x1;
  }

  public double[] getY() {
    return y;
  }

  public void setY(int i, double y1) {
    y[i] = y1;
  }

  public double[] getIntensity() {
    return imageIntensity;
  }

  public void setIntensity(int i, double i1) {
    imageIntensity[i] = i1;
  }

}
