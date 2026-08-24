/*
 * @(#)SampleShapeRectangularInReflection.java created 23/06/2025 Caen
 *
 * Copyright (c) 2025-2099 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.shape;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;

/**
 *  The SampleShapeRectangularInReflection is a class
 *
 *
 * @version $Revision: 1.00 $, $Date: 2025/06/23 10:13:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SampleShapeRectangularInReflection extends SampleShape {

  public static int NdivisionX = MaudPreferences.getInteger("SampleShapeRectangularInReflection.integralDivisionsX", 1);
  public static int NdivisionY = MaudPreferences.getInteger("SampleShapeRectangularInReflection.integralDivisionsY", 7);

  protected static String[] diclistc = {
      "_rita_shape_fe_division_number_x",
      "_rita_shape_fe_division_number_y"
  };
  protected static String[] diclistcrm = {
      "_rita_shape_fe_division_number_x",
      "_rita_shape_fe_division_number_y"
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  int divisionsX = 1;
  int divisionsY = 1;

  double[][] coordinates = new double[2][1];

  private static String namePlugin = "Rectangular in reflection";

  public SampleShapeRectangularInReflection(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = namePlugin;
    IDlabel = namePlugin;
    description = "Rectangular beam on the sample shape, numerical integration for geometry effects";
  }

  public SampleShapeRectangularInReflection(XRDcat aobj) {
    this(aobj, "Rectangular beam on the sample shape, numerical integration for geometry effects");
  }

  public SampleShapeRectangularInReflection() {
    identifier = namePlugin;
    IDlabel = namePlugin;
    description = "Rectangular beam on the sample shape, numerical integration for geometry effects";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
    setDivisionsX(NdivisionX);
    setDivisionsY(NdivisionY);
  }

  public void setDivisionsX(int value) {
    stringField[0] = Integer.toString(value);
  }

  public String getDivisionXS() {
    return stringField[0];
  }

  public int getDivisionsX() {
    return 1; // Integer.parseInt(stringField[0]);  // todo: get it working forx also
  }

  public void setDivisionsY(int value) {
    stringField[1] = Integer.toString(value);
  }

  public String getDivisionYS() {
    return stringField[1];
  }

  public int getDivisionsY() {
    return Integer.parseInt(stringField[1]);
  }

  public void setRefreshComputationForAbsorption(boolean value) {
//    refreshComputation = value;
  }

  public void setRefreshComputationForGeometry(boolean value) {
    refreshComputation = value;
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    divisionsX = getDivisionsX();
    if (!MoreMath.odd(divisionsX))
      divisionsX++;
    divisionsY = getDivisionsY();
    if (!MoreMath.odd(divisionsY))
      divisionsY++;
    refreshComputation = true;
  }

  public void updateGeometry() {
    Sample asample = (Sample) getParent();
    double yDim = asample.getAxialDimensionD();
    if (yDim <= 0)
      yDim = 1.0;
    double yStep = yDim / divisionsY;
    double yDim2 = (yDim - yStep) / 2;
    double xDim = 1.0;
    double xStep = xDim / divisionsX;
    double xDim2 = (xDim - xStep) / 2;
    int coordNumber = getDifferentPositionsNumber();
    if (coordinates[0].length != coordNumber)
      coordinates = new double[2][coordNumber];
    for (int i = 0; i < divisionsX; i++) {
      for (int j = 0; j < divisionsY; j++) {
        coordinates[0][i * divisionsY + j * divisionsX] = i * xStep - xDim2;
        coordinates[1][i * divisionsY + j * divisionsX] = j * yStep - yDim2;
      }
    }
    // swap the center on the first element of the array
    double temp1 = coordinates[0][0];
    double temp2 = coordinates[1][0];
    int centerM = coordNumber / 2;
    coordinates[0][0] = coordinates[0][centerM];
    coordinates[1][0] = coordinates[1][centerM];
    coordinates[0][centerM] = temp1;
    coordinates[1][centerM] = temp2;

    refreshComputation = false;
  }

  public int getDifferentPositionsNumber() {
    return divisionsX * divisionsY;
  }

  public double getOmegaStepFor(int ppp, double radius) {
    // in radiant
    if (refreshComputation)
      updateGeometry();
    return coordinates[0][ppp] / radius;
  }

  public double getChiStepFor(int ppp, double radius) {
    // in radiant
    if (refreshComputation)
      updateGeometry();
    return coordinates[1][ppp] / radius;
  }

  public double getXShiftFor(int ppp) {
    if (refreshComputation)
      updateGeometry();
    return coordinates[0][ppp];
  }

  public double getYShiftFor(int ppp) {
    if (refreshComputation)
      updateGeometry();
    return coordinates[1][ppp];
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JShapeRectangularOptionsD(parent, this);
    return adialog;
  }

  public class JShapeRectangularOptionsD extends JOptionsDialog {

    JTextField divisionXTF;
    JTextField divisionYTF;

    public JShapeRectangularOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2));
      principalPanel.add(BorderLayout.NORTH, jPanel8);

      jPanel8.add(new JLabel("X divisions number: "));
      jPanel8.add(divisionXTF = new JTextField(6));
      divisionXTF.setToolTipText("Use > 1 only if you have a incident beam monochromator and account for wavelengths spatial aberrations");
      jPanel8.add(new JLabel("Y divisions number: "));
      jPanel8.add(divisionYTF = new JTextField(6));
      divisionYTF.setToolTipText("Optimum value: 7-9; don't exceed 15");

      initParameters();

      setTitle("Rectangular beam spot");

      setHelpFilename("sampleshape_rectangular.txt");
      pack();

    }

    public void initParameters() {
      divisionXTF.setText(getDivisionXS());
      divisionYTF.setText(getDivisionYS());
    }

    public void retrieveParameters() {
      try {
        int div = Integer.parseInt(divisionXTF.getText());
        if (div > 0 && div < 8) {
          setDivisionsX(div);
          divisionsX = div;
        }
        div = Integer.parseInt(divisionYTF.getText());
        if (div > 0 && div < 16) {
          setDivisionsY(div);
          divisionsY = div;
        }
      } catch (Exception e) {
        e.printStackTrace();
      }
      super.retrieveParameters();
    }

  }

}
