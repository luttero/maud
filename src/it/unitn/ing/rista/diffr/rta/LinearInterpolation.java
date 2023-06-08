/*
 * @(#)LinearInterpolation.java created 29/09/1998 Hamburg-Harburg.
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.io.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;

import java.util.*;

/**
 *  The LinearInterpolation is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:57 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LinearInterpolation extends Interpolation {
  public static String[] diclistc = {"_rita_interpol_dist_control"};
  public static String[] diclistcrm = {"_rita_interpol_dist_control"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public LinearInterpolation(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Linear";
    IDlabel = "Linear";
    description = "select this to apply Linear interpolation";
  }

  public LinearInterpolation(XRDcat aobj) {
    this(aobj, "Linear interpolation");
  }

  public LinearInterpolation() {
    identifier = "Linear";
    IDlabel = "Linear";
    description = "select this to apply linear interpolation";

  }

  public void initConstant() {
    Nstring = 1;
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
    stringField[0] = new String("20.0");
  }

  public double getDistControlD() {
    return Double.valueOf(getDistControl()).doubleValue();
  }

  public String getDistControl() {
    return stringField[0];
  }

  public void setDistControl(String value) {
    stringField[0] = new String(value);
  }

  public void setDistControl(double value) {
    setDistControl(Double.toString(value));
  }

  public WIMVTexture getWIMV() {
    return (WIMVTexture) getParent();
  }

  public int getPoleFigureNumber() {
    return getWIMV().getPoleFigureNumber();
  }

  public int getPointNumber(int pole) {
    return getWIMV().getPointNumber(pole);
  }

  public double[] getTextureAngles(int pole, int point) {
    return getWIMV().getTextureAngles(pole, point);
  }

  public double getPoleIntensity(int pole, int point) {
    return getWIMV().getPoleIntensity(pole, point);
  }

  public double getWeight(int pole, int point) {
    return getWIMV().getWeight(pole, point);
  }

  public int getH(int pole) {
    return getWIMV().getH(pole);
  }

  public int getK(int pole) {
    return getWIMV().getK(pole);
  }

  public int getL(int pole) {
    return getWIMV().getL(pole);
  }

  public int getIzoveri(int pole) {
    return getWIMV().getIzoveri(pole);
  }

  public int getH(int pole, int partial) {
    return getWIMV().getH(pole, partial);
  }

  public int getK(int pole, int partial) {
    return getWIMV().getK(pole, partial);
  }

  public int getL(int pole, int partial) {
    return getWIMV().getL(pole, partial);
  }

  public double getWeightSingle(int pole, int partial) {
    return getWIMV().getWeightSingle(pole, partial);
  }

  public double getResolution() {
    return getWIMV().getResolutionD();
  }

  public double[][] computeInterpolation() {

    Phase thephase = (Phase) ((XRDcat) getParent()).getParent();
    FilePar aparFile = (FilePar) thephase.getFilePar();
    BufferedWriter PFwriter = Misc.getWriter(((FilePar) thephase.getFilePar()).getDirectory() +
            thephase.toXRDcatString() + ".xpe");

    String title = new String(thephase.toXRDcatString() + ": interpolated experimental pole figure, ");

    int[] milhkl = new int[3];
    double resolution = getResolution();
    int thetaNumber = (int) (90.00001 / resolution) + 1;
    int phiNumber = (int) (360.00001 / resolution) + 1;
    int numberOfPoints = thetaNumber * phiNumber;
    int numberPoleFigures = getPoleFigureNumber();

    ProgressFrame prF = null;
    if (!Constants.textonly && Constants.showProgressFrame)
      try {
          prF = new ProgressFrame(numberPoleFigures);
      } catch (NullPointerException npe) {
        System.out.println("Not able to create frame, MacOSX display sleep bug?");
      }
    printf("Pole figure interpolation for phase: " + thephase.toXRDcatString() + "       ", prF);

    double[][] poleIntensity = new double[numberPoleFigures][numberOfPoints];
    double[][][] poleCoordinates = new double[thetaNumber][phiNumber][2];
    for (int i = 0; i < thetaNumber; i++)
      for (int j = 0; j < phiNumber; j++) {
        double sintheta = MoreMath.sind(i * resolution);
        poleCoordinates[i][j][0] = sintheta * MoreMath.cosd(j * resolution);
        poleCoordinates[i][j][1] = sintheta * MoreMath.sind(j * resolution);
      }

    for (int k = 0; k < numberPoleFigures; k++) {
      milhkl[0] = getH(k);
      milhkl[1] = getK(k);
      milhkl[2] = getL(k);

      double[][] singlePoleIntensity = getPoleIntensity(k, thetaNumber, phiNumber,
              poleCoordinates, resolution);
      for (int i = 0; i < thetaNumber; i++)
        for (int j = 0; j < phiNumber; j++)
          poleIntensity[k][i * phiNumber + j] = singlePoleIntensity[i][j];

      poleFigureOutput(PFwriter, thephase, singlePoleIntensity, milhkl, title,
              resolution, thetaNumber, phiNumber, k);

      if (prF != null)
        prF.increaseProgressBarValue();
      printf("Interpoled pole n: " + Integer.toString(k) + ", " +
              Integer.toString(milhkl[0]) + "," +
              Integer.toString(milhkl[1]) + "," +
              Integer.toString(milhkl[2]), prF);
    }

    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }
    try {
      PFwriter.write(Constants.lineSeparator);
      PFwriter.flush();
      PFwriter.close();
    } catch (IOException io) {
    }

    return poleIntensity;
  }

  public double[][] getPoleIntensity(int poleNumber, int thetaNumber, int phiNumber,
                                     double[][][] poleCoordinates, double resolution) {

    double distctr = getDistControlD();
    if (distctr < 0)
      distctr = 20.0;
    double minDistance = MoreMath.cosd(distctr);

    double[][] singlePoleIntensity = new double[thetaNumber][phiNumber];
    for (int i = 0; i < thetaNumber; i++)
      for (int j = 0; j < phiNumber; j++)
        singlePoleIntensity[i][j] = -1.0;

    int expPointsNumber = getPointNumber(poleNumber);
    double[][] expPole = new double[expPointsNumber][4];

    double thetaMax = 0.0;
    double thetaMin = 90.0;
    for (int point = 0; point < expPointsNumber; point++) {
      double[] angles = getTextureAngles(poleNumber, point);
      double sintheta = MoreMath.sind(angles[0]);
      expPole[point][0] = sintheta * MoreMath.cosd(angles[1]);
      expPole[point][1] = sintheta * MoreMath.sind(angles[1]);
      expPole[point][2] = getPoleIntensity(poleNumber, point);
      if (thetaMax < angles[0])
        thetaMax = angles[0];
      if (thetaMin > angles[0])
        thetaMin = angles[0];
    }

    thetaMax += 0.3;
    thetaMin -= 0.3;
    if (thetaMin <= resolution / 2)
      thetaMin = 0.0;

    double[] A = new double[3];
    double[] B = new double[3];
    double[] C = new double[3];
    for (int i = 0; i < thetaNumber; i++) {
      double theta = resolution * i;
      if (theta <= thetaMax && theta >= thetaMin) {
        for (int j = 0; j < phiNumber; j++) {
          Vector neighboor = new Vector(0, 10);
          for (int point = 0; point < expPointsNumber; point++) {
            expPole[point][3] = MoreMath.getDistanceBetweenPoints(expPole[point][0],
                    expPole[point][1],
                    poleCoordinates[i][j][0],
                    poleCoordinates[i][j][1]);
            if (expPole[point][3] < minDistance) {
              double[] pole = new double[4];
              for (int m = 0; m < 4; m++)
                pole[m] = expPole[point][m];
              neighboor.add(pole);
            }
          }

          singlePoleIntensity[i][j] = -1.0;
          if (neighboor.size() > 0) {
            double[] pole = (double[]) neighboor.elementAt(0);
            if (pole[3] < 1.0)
              singlePoleIntensity[i][j] = pole[2];
          }
          if (neighboor.size() > 2) {
            Collections.sort(neighboor, new DistanceComparer());
            collapse(neighboor);
            if (neighboor.size() > 2) {
              double[] pole = (double[]) neighboor.elementAt(0);
              for (int m = 0; m < 3; m++)
                A[m] = pole[m];
              pole = (double[]) neighboor.elementAt(1);
              for (int m = 0; m < 3; m++)
                B[m] = pole[m];
              int lastElement = 2;
              boolean finish = false;
              while (!finish && lastElement < neighboor.size()) {
                pole = (double[]) neighboor.elementAt(lastElement++);
                for (int m = 0; m < 3; m++)
                  B[m] = pole[m];
                try {
                  double[] plane = MoreMath.getPlaneBy3Points(A, B, C);
                  singlePoleIntensity[i][j] = plane[0] * poleCoordinates[i][j][0] +
                          plane[1] * poleCoordinates[i][j][1] +
                          plane[2];
                  finish = true;
                } catch (Exception e) {
                }
              }
            }
          }
        }
        boolean killGirdle = false;
        for (int j = 0; j < phiNumber; j++) {
          if (singlePoleIntensity[i][j] < 0.0)
            killGirdle = true;
        }
        if (killGirdle)
          for (int j = 0; j < phiNumber; j++)
            singlePoleIntensity[i][j] = 0.0;
      } else {
        for (int j = 0; j < phiNumber; j++)
          singlePoleIntensity[i][j] = 0.0;
      }
    }
    return singlePoleIntensity;
  }

  public void collapse(Vector neighboor) {
    double[] pole = (double[]) neighboor.elementAt(0);
    int wgt = 1;
    for (int i = 1; i < neighboor.size();) {
      double[] pole1 = (double[]) neighboor.elementAt(i);
      double distance = MoreMath.getDistanceBetweenPoints(pole[0],
              pole[1],
              pole1[0],
              pole1[1]);
      if (distance < 0.1) {
        pole[2] = (pole[2] * wgt + pole1[2]) / (wgt + 1);
        wgt++;
        neighboor.removeElementAt(i);
        neighboor.setElementAt(pole, i - 1);
      } else {
        pole = pole1;
        wgt = 1;
        i++;
      }
    }
  }

  void poleFigureOutput(BufferedWriter PFwriter, Phase thephase, double[][] singlePoleIntensity,
                        int[] milhkl, String title, double resolution, int thetaNumber, int phiNumber, int npole) {

    StringBuffer tmp = new StringBuffer(title);
    tmp = tmp.append(" ").append(Integer.toString(milhkl[0])).
            append(",").append(Integer.toString(milhkl[1])).
            append(",").append(Integer.toString(milhkl[2]));
    int bufflength = tmp.length();
    for (int i = 0; i < 79 - bufflength; i++)
      tmp = tmp.append(" ");

    String commentLine = new String(tmp.toString().substring(0, 79) + "#");

    try {
      PFwriter.write(commentLine);
      PFwriter.write(Constants.lineSeparator);
      int lineend = 0;
      int izoveri = getIzoveri(npole);
      if (izoveri > 1) {
        StringBuffer tmp1 = new StringBuffer();
        tmp1.append(Integer.toString(izoveri));
        for (int j = 0; j < izoveri; j++) {
          int h = getH(npole, j);
          int k = getK(npole, j);
          int l = getL(npole, j);
          double wgt = getWeightSingle(npole, j);
          String temp = new String(" " + Integer.toString(h) + " " + Integer.toString(k) + " " + Integer.toString(l) + " " + Misc.getDoubleStringFormatted(wgt, 4, 2));
          if (tmp1.length() + temp.length() <= 79)
            tmp1.append(temp);
          else {
            if (lineend < 4) {
              PFwriter.write(tmp1.toString());
              PFwriter.write(Constants.lineSeparator);
              lineend++;
            }
            tmp1 = new StringBuffer();
            tmp1.append(temp);
          }
        }
        while (lineend < 4) {
          PFwriter.write(Constants.lineSeparator);
          lineend++;
        }
      } else
        for (int i = 0; i < 4; i++)
          PFwriter.write(Constants.lineSeparator);

      String firstline = Misc.getFirstPFline(thephase);
      PFwriter.write(firstline);
      PFwriter.write(Constants.lineSeparator);

      firstline = new String(" " + Misc.getIntStringFormatted(milhkl[0], 3) +
              Misc.getIntStringFormatted(milhkl[1], 3) +
              Misc.getIntStringFormatted(milhkl[2], 3) +
              "   .0 90.0" + Misc.getDoubleStringFormatted(resolution, 3, 1) +
              "   .0360.0" + Misc.getDoubleStringFormatted(resolution, 3, 1) +
              " 1 1");
      PFwriter.write(firstline);
      PFwriter.write(Constants.lineSeparator);
    } catch (IOException io) {
    }

    int until18 = 0;
    for (int nb = 0; nb < thetaNumber; nb++) {
      for (int na = 0; na < phiNumber; na++) {
        int imh = (int) (singlePoleIntensity[nb][na] * 100.00001);
        if (imh < 0)
          imh = 0;
        try {
          if (until18 == 0)
            PFwriter.write(" ");
          PFwriter.write(Misc.getIntStringFormatted(imh, 4));
          if (++until18 >= 18) {
            until18 = 0;
            PFwriter.write(Constants.lineSeparator);
          }
        } catch (IOException io) {
        }
      }
    }
    if (until18 != 0)
      try {
        PFwriter.write(Constants.lineSeparator);
      } catch (IOException io) {
      }
    try {
      PFwriter.write(Constants.lineSeparator);
    } catch (IOException io) {
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTInterpolationOptionsD(parent, this);
    return adialog;
  }

  class JTInterpolationOptionsD extends JOptionsDialog {

    JTextField maxPolarTF;
    JTextField minAngleTF;
    JTextField distCtrlTF;
    JSlider degreeJS;

    public JTInterpolationOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel up = new JPanel();
      up.setLayout(new GridLayout(1, 1, 6, 6));
      principalPanel.add(BorderLayout.CENTER, up);
      JPanel jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      up.add(jPanel9);
      jPanel9.add(new JLabel("Distance control value: "));
      distCtrlTF = new JTextField(Constants.FLOAT_FIELD);
      distCtrlTF.setToolTipText("Maximum distance from one measured point");
      jPanel9.add(distCtrlTF);

      setTitle("Triangular interpolation options");
      initParameters();
      pack();

    }

    public void initParameters() {
      distCtrlTF.setText(getDistControl());
    }

    public void retrieveParameters() {
      setDistControl(distCtrlTF.getText());
    }

  }

  class DistanceComparer implements Comparator {
    public int compare(Object obj1, Object obj2) {
      double[] pole1 = (double[]) obj1;
      double[] pole2 = (double[]) obj2;
      double diff = pole1[3] - pole2[3];

      if (diff == 0.0)
        return 0;
      else if (diff > 0)
        return 1;
      return -1;
    }
  }

}

