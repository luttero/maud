/*
 * @(#)PolarPlotData2D.java created Jun 12, 2011 Caen
 *
 * Copyright (c) 2011 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.jgraph.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import ru.sscc.util.data.*;
import ru.sscc.spline.reduction.ReducedMesh;
import ru.sscc.spline.reduction.StrictScatteredMesh;
import ru.sscc.spline.Spline;
import ru.sscc.spline.analytic.GSplineCreator;

/**
 * The PolarPlotData2D is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 12, 2011 11:16:26 AM $
 * @since JDK1.1
 */
public class PolarPlotData2D extends myJFrame {

  CopyPrintPanel pfPanel = null;
  JMenu editMenu = null;
  PoleFigureMap[] ccolorMap = null;

  public PolarPlotData2D(Frame parent, DataFileSet adataset,
                        int numberofPoints, double zoom, double filterWidth,
                        boolean grayScale, double maxAngle, boolean logScale, int colrsNumber) {

    super(parent);

    boolean editable = !MaudPreferences.getBoolean("polarPlotData2D.classicalPlot", false);
    frameWLabel = "polarPlotData2D.frameWidth";
    frameHLabel = "polarPlotData2D.frameHeight";
    defaultFrameW = 600;
    defaultFrameH = 400;
    setOwnSize = true;
    framePositionX = "polarPlotData2D.framePositionX";
    framePositionY = "polarPlotData2D.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

    createDefaultMenuBar();

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));
    c1.setBackground(Color.white);

    String first = new String("PolarPlotData2D");
    String log = "";
    if (logScale)
      log = " (Log scale, contours in log units)";
    Label title = new Label(first + log, Label.CENTER);
    title.setFont(new Font("TimesRoman", Font.PLAIN, 12));
    JPanel p1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
    p1.setBackground(Color.white);
    p1.add(title);
    c1.add(BorderLayout.NORTH, p1);

    p1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
    p1.setBackground(Color.white);
    p1.add(new Label(" "));
    c1.add(BorderLayout.WEST, p1);
    p1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
    p1.setBackground(Color.white);
    p1.add(new Label(" "));
    c1.add(BorderLayout.SOUTH, p1);

    int numberPoles = 1;

    pfPanel = new CopyPrintPanelNoBkg();

    ProgressFrame prF = null;
    try {
      if (Constants.showProgressFrame)
        prF = new ProgressFrame(numberPoles);
    } catch (NullPointerException npe) {
      System.out.println("Not able to create frame, MacOSX display sleep bug?");
    }
    if (prF != null) {
      prF.setProgressText("Polar plot computation....");
    }

    Object[] listGrid = new Object[numberPoles];
    String[] label = new String[numberPoles];
    double min = 1.0E30;
    double max = -1.0E30;
    int izoom = (int) Math.pow(2, zoom);
    int pixelsNumber = MaudPreferences.getInteger("polarPlotData2D.defaultPoleSize", 400);
    if (editable) {
      izoom = 1;
      int tempGrid = numberofPoints;
      while (tempGrid < pixelsNumber) {
        izoom *= 2;
        tempGrid *= 2;
      }
    }
    int gridNumber = getNewGridNumber(numberofPoints, izoom);
    int size = pixelsNumber + PoleFigureMap.inset * 2;
    if (!editable)
      size = gridNumber + PoleFigureMap.inset * 2;
    int col = 0;
    Dimension screenSize = getToolkit().getScreenSize();
    while (col * size + 50 < screenSize.width)
      col++;
    if (col > 1)
      col--;
    int row = (int) (0.99 + (1.0 + numberPoles) / col);
    col = (int) (0.99 + (1.0 + numberPoles) / row);

    pfPanel.setLayout(new GridLayout(0, col, 0, 0));
    pfPanel.setBackground(Color.white);

    for (int i = 0; i < numberPoles; i++) {
      double[][] grid = null;
      grid = createGrid(adataset, numberofPoints, maxAngle, izoom, filterWidth);
//      System.out.println("Grid done!");
      if (prF != null)
        prF.increaseProgressBarValue();
      label[i] = adataset.getLabel();

      listGrid[i] = grid;
      for (int j = 0; j < gridNumber; j++)
        for (int k = 0; k < gridNumber; k++) {
          if (grid[j][k] != ColorMap.DUMMY_VALUE && !Double.isNaN(grid[j][k])) {
            min = Math.min(grid[j][k], min);
            max = Math.max(grid[j][k], max);
          }
        }
    }

    double[] limits = confirmLimits(min, max);
    if (logScale) {
      if (limits[0] <= 0)
        limits[0] = 0.01;
      for (int j = 0; j < 2; j++)
        limits[j] = MoreMath.log10(limits[j]);
    }

    PoleFigureMap[] ccolorMap = null;
    if (editable)
      ccolorMap = new PoleFigureMap[numberPoles];
    for (int i = 0; i < numberPoles; i++) {
      double[][] grid = (double[][]) listGrid[i];
      if (logScale) {
        for (int j = 0; j < gridNumber; j++)
          for (int k = 0; k < gridNumber; k++)
            if (grid[j][k] != ColorMap.DUMMY_VALUE && !Double.isNaN(grid[j][k]) && grid[j][k] > 0.0)
              grid[j][k] = MoreMath.log10(grid[j][k]);
      }

      if (!editable) {
        ColorMap colorMap =  new ColorMap(grid, gridNumber, limits[0], limits[1],
                grayScale, label[i], colrsNumber);
        pfPanel.add(colorMap);
      } else {
        ccolorMap[i] =  new PoleFigureMap(grid, gridNumber, limits[0], limits[1], grayScale, label[i],
              colrsNumber, editMenu, izoom, pixelsNumber, true, 0);
        pfPanel.add(ccolorMap[i]);
      }
    }
    int legendHeight = (int) (0.85 * pixelsNumber * izoom + 10);
    if (!editable)
      legendHeight = gridNumber;
    if (limits[0] != limits[1]) {
      int pwidth = legendHeight / 5;
      int pheight = legendHeight;
      double[] legendGrid = new double[pheight];
      double step = (limits[1] - limits[0]) / pheight;
      for (int j = 0; j < pheight; j++) {
        legendGrid[j] = step * j + limits[0];
      }

      MapLegend mapLegend = new MapLegend(legendGrid, pwidth, pheight, limits[0], limits[1], grayScale,
              logScale, 0, "", colrsNumber, 100);
      pfPanel.add(mapLegend);
    }

    int dummyAdded = 1;
    while (row * col != numberPoles + dummyAdded) {
      pfPanel.add(new PoleFigureMap.WhiteMap(pixelsNumber, pixelsNumber));
      dummyAdded++;
    }

    c1.add(BorderLayout.CENTER, pfPanel); //scrollPane);

    setComponentToPrint(pfPanel);
    listGrid = null;

    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }

    setVisible(true);
    pack();
    setBatch(false);
  }

  public void setBatch(boolean value) {
    if (ccolorMap != null) {
      for (int i = 0; i < ccolorMap.length; i++)
        ccolorMap[i].setBatch(value);
    }
  }

  public JMenu createEditMenu() {
    editMenu = super.createEditMenu();
    return editMenu;
  }

  double intensityMin = 0.0;
  double intensityMax = 1.0;

  public double[] confirmLimits(double min, double max) {
    double[] limits = new double[2];
    intensityMin = min;
    intensityMax = max;
    LimitsDialog rangeDialog = new LimitsDialog();
    rangeDialog.setVisible(true);
    while (rangeDialog.isVisible()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException ie) {
      }
    }
    limits[0] = intensityMin;
    limits[1] = intensityMax;
    return limits;
  }

  public double[][] createGrid(DataFileSet adataset, int numberofPoints,
                               double maxAngle, int zoom, double filterWidth) {
    double PF[][] = null;
//    double[] texture_angles = null;

    PF = getDatasetPoleFigureGrid(adataset.getPolarPlotData(), numberofPoints, maxAngle);
//    if (zoom > 1 && filterWidth < 0.7)
//      filterWidth = 0.7;
//    int gridnumber = getNewGridNumber(numberofPoints, zoom);
    return smooth(enlargeGrid(PF, numberofPoints, zoom, 0.7), numberofPoints * zoom, filterWidth);
  }

  public static double[][] enlargeGrid(double[][] PF, int numberofPoints, int zoom, double smooth) {

    System.out.println("Enlarge grid");
    if (zoom == 1)
      return PF;
    if (zoom / 2 > 1) {
      PF = enlargeGrid(PF, numberofPoints, zoom / 2, smooth);
      numberofPoints = getNewGridNumber(numberofPoints, zoom / 2);
    }

    int numberPoints = numberofPoints * 2;
    double[][] hrPF = new double[numberPoints][numberPoints];

    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        double value = PF[i][j];
        hrPF[i * 2][j * 2] = value;
        hrPF[i * 2 + 1][j * 2] = value;
        hrPF[i * 2][j * 2 + 1] = value;
        hrPF[i * 2 + 1][j * 2 + 1] = value;
      }
    return smooth(hrPF, numberPoints, smooth);
  }

  public static double[][] smooth(double[][] PF, int numberofPoints, double filterHWHM) {

    if (filterHWHM == 0)
      return PF;

    System.out.println("Smooth");
    double norm = -Constants.LN2 / filterHWHM * filterHWHM;
    double[][] hrPF = new double[numberofPoints][numberofPoints];
    int filterExt = (int) (filterHWHM * 3);
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        double posvalue = 0.0;
        double posweigth = 0.0;
        int poscount = 0;
        int dummycount = 0;
        for (int i1 = i - filterExt; i1 < i + filterExt + 1; i1++) {
          for (int j1 = j - filterExt; j1 < j + filterExt + 1; j1++) {
            if ((i1 >= 0 && i1 < numberofPoints) &&
                (j1 >= 0 && j1 < numberofPoints)) {
              if (!Double.isNaN(PF[i1][j1])) {
                int dx = (i1-i) * (i1-i) + (j1-j) * (j1-j);
                double tvalue = Math.exp(norm * dx);
                posvalue += PF[i1][j1] * tvalue;
                posweigth += tvalue;
                poscount++;
              } else {
                dummycount++;
              }
            }else {
              dummycount++;
            }
          }
          if (poscount == dummycount && !Double.isNaN(PF[i][j]))
            poscount++;
        }
        if (poscount < dummycount)
          hrPF[i][j] = Double.NaN;
        else
          hrPF[i][j] = posvalue / posweigth;
      }
    System.out.println("End of smooth");
    return hrPF;
  }

  public static int getNewGridNumber(int numberofPoints, int zoom) {
    if (zoom <= 1)
      return numberofPoints;
    if (zoom / 2 > 1)
      numberofPoints = getNewGridNumber(numberofPoints, zoom / 2);
    return numberofPoints * 2;
  }

  public double[][] getDatasetPoleFigureGrid(double[][] expTextureFactorsAndAngles,
                                                int numberofPoints, double maxAngle) {

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];
    if (expTextureFactorsAndAngles != null)
    try {
      int mode = 1;

      double x, y;//, r;
      int numberExpPoints = expTextureFactorsAndAngles[0].length;
      System.out.println("Compute projection and overlapping for " + numberExpPoints + " number of points");
      int overlapCount[] = new int[numberExpPoints];

      int index = 0;
      double[][] xyF = new double[3][numberExpPoints];
      double max = 0.0;
      for (int i = 0; i < numberExpPoints; i++) {
        if (expTextureFactorsAndAngles[2][i] > max)
          max = expTextureFactorsAndAngles[2][i];
      }
      double threshold = MaudPreferences.getDouble("polarPlotData2D.threshold", 3.0) / 100.0;
      max *= threshold;
        for (int i = 0; i < numberExpPoints; i++) {
          if (expTextureFactorsAndAngles[2][i] > max) {
        double projection = Constants.sqrt2 * Math.sin(expTextureFactorsAndAngles[0][i] * Constants.DEGTOPI / 2.0);
        if (expTextureFactorsAndAngles[0][i] > 90.) {
          projection = Constants.sqrt2 * Math.sin((180. - expTextureFactorsAndAngles[0][i]) * Constants.DEGTOPI / 2.0);
        }

        x = projection * Math.cos(expTextureFactorsAndAngles[1][i] * Constants.DEGTOPI);
        y = projection * Math.sin(expTextureFactorsAndAngles[1][i] * Constants.DEGTOPI);

        boolean overlapped = false;
        for (int j = 0; j < index; j++) {
          if (Math.abs(x - xyF[0][j]) < .001 && Math.abs(y - xyF[1][j]) < .001) {
            overlapped = true;
            xyF[2][j] = Math.sqrt(xyF[2][j] * xyF[2][j] * overlapCount[j] + expTextureFactorsAndAngles[2][i] * expTextureFactorsAndAngles[2][i]);
            overlapCount[j]++;
            xyF[2][j] /= overlapCount[j];
          }
          if (overlapped)
            break;
        }
        if (!overlapped) {
          xyF[0][index] = x;
          xyF[1][index] = y;
          xyF[2][index] = expTextureFactorsAndAngles[2][i];
          if (xyF[2][index] < 0)
            xyF[2][index] = 0;
          overlapCount[index] = 1;
          index++;
        }
          }
      }
      System.out.println("Start meshing with " + index + " number of points");
      RealVectors measuredMesh = new DoubleVectors(2, index);
      double[] expTF = new double[index];
      for (int i = 0; i < index; i++) {
        measuredMesh.set(i, 0, xyF[0][i]);
        measuredMesh.set(i, 1, xyF[1][i]);
        expTF[i] = xyF[2][i];
//        System.out.println(i + " " + xyF[0][i] + " " + xyF[1][i] + " " + xyF[2][i]);
      }

      System.out.println("Reduced mesh");
      ReducedMesh rMesh = new StrictScatteredMesh(measuredMesh);
      System.out.println("Create spline");
      Spline spl = GSplineCreator.createSpline(mode, rMesh, expTF);

      double dxy = 2.0 * maxAngle / numberofPoints;

      int k = 0;
      System.out.println("Interpolate mesh");
      RealVectors interpolatedMesh = new DoubleVectors(2, numberofPoints * numberofPoints);
      for (int i = 0; i < numberofPoints; i++)
        for (int j = 0; j < numberofPoints; j++, k++) {
          x = (i + 0.5) * dxy - maxAngle;
          y = (j + 0.5) * dxy - maxAngle;
          interpolatedMesh.set(k, 0, x);
          interpolatedMesh.set(k, 1, y);
        }
      System.out.println("Get mesh");
      RealPointers interpolatedPoint = new RealPointers(interpolatedMesh);
      System.out.println("Mesh done!");

      double minDistance = MaudPreferences.getDouble("plotExpPF.minimumDistanceDeg", 3.0) * Constants.DEGTOPI;
      k = 0;
      double maxAngle2 = maxAngle * maxAngle;
      for (int i = 0; i < numberofPoints; i++)
        for (int j = 0; j < numberofPoints; j++, k++) {
          interpolatedPoint.select(k);
          x = (i + 0.5) * dxy - maxAngle;
          y = (j + 0.5) * dxy - maxAngle;
          boolean near = false;
          if (x * x + y * y <= maxAngle2) {
            for (int ij = 0; ij < index; ij++) {
              double dx1 = x - xyF[0][ij];
              double dy1 = y - xyF[1][ij];
              if (dx1 * dx1 + dy1 * dy1 < minDistance) {
                near = true;
                break;
              }
            }
          }
          if (near) {
            PFreconstructed[i][j] = spl.value(interpolatedPoint);
//            System.out.println(i + " " + j + " " + PFreconstructed[i][j]);
          } else {
            PFreconstructed[i][j] = Double.NaN;
          }
        }
      System.out.println("Finished all meshing stuff!");

    } catch (ru.sscc.util.CalculatingException ce) {
      ce.printStackTrace();
    }
    return PFreconstructed;
  }

  double[] value = null;
  double[][] textureAngles = null;

  public double[][] getPoleFigureGrid(int numberofPoints, double maxAngle) {

    double[][] PFreconstructed = null;
    try {
      int mode = 1;

      double[] texture_angles;

      double x, y;//, r;
      int numberExpPoints = value.length;

      int index = 0;
      double[][] xyF = new double[3][numberExpPoints];
      for (int i = 0; i < numberExpPoints; i++) {
        texture_angles = textureAngles[i];
        double projection = Constants.sqrt2 * Math.sin(texture_angles[0] * Constants.DEGTOPI / 2.0);
        if (texture_angles[0] > 90.) {
          projection = Constants.sqrt2 * Math.sin((180. - texture_angles[0]) * Constants.DEGTOPI / 2.0);
        }

        x = projection * Math.cos(texture_angles[1] * Constants.DEGTOPI);
        y = projection * Math.sin(texture_angles[1] * Constants.DEGTOPI);

        double expTF = value[i];
        boolean overlapped = false;
        for (int j = 0; j < index; j++) {
          if (Math.abs(x - xyF[0][j]) < .00001 && Math.abs(y - xyF[1][j]) < .00001) {
            overlapped = true;
            xyF[2][j] = (xyF[2][j] + expTF) * 0.5;
          }
          if (overlapped)
            break;
        }
        if (!overlapped) {
          xyF[0][index] = x;
          xyF[1][index] = y;
          xyF[2][index] = expTF;
          index++;
        }
      }
      RealVectors measuredMesh = new DoubleVectors(2, index);
      double[] expTextureFactors = new double[index];
      for (int i = 0; i < index; i++) {
        measuredMesh.set(i, 0, xyF[0][i]);
        measuredMesh.set(i, 1, xyF[1][i]);
        expTextureFactors[i] = xyF[2][i];
      }

      ReducedMesh rMesh = new StrictScatteredMesh(measuredMesh);
      Spline spl = GSplineCreator.createSpline(mode, rMesh, expTextureFactors);

      PFreconstructed = new double[numberofPoints][numberofPoints];

      double dxy = 2.0 * maxAngle / numberofPoints;

      int k = 0;
      RealVectors interpolatedMesh = new DoubleVectors(2, numberofPoints * numberofPoints);
      for (int i = 0; i < numberofPoints; i++)
        for (int j = 0; j < numberofPoints; j++, k++) {
          x = (i + 0.5) * dxy - maxAngle;
          y = (j + 0.5) * dxy - maxAngle;
          interpolatedMesh.set(k, 0, x);
          interpolatedMesh.set(k, 1, y);
        }
      RealPointers interpolatedPoint = new RealPointers(interpolatedMesh);

      double minDistance = MaudPreferences.getDouble("polarDataPlot.minimumDistanceDeg", 0.1) / 180.0;
      k = 0;
      double maxAngle2 = maxAngle * maxAngle;
      for (int i = 0; i < numberofPoints; i++)
        for (int j = 0; j < numberofPoints; j++, k++) {
          interpolatedPoint.select(k);
          x = (i + 0.5) * dxy - maxAngle;
          y = (j + 0.5) * dxy - maxAngle;
          boolean near = false;
          if (x * x + y * y <= maxAngle2) {
            for (int ij = 0; ij < index; ij++) {
              double dx1 = x - xyF[0][ij];
              double dy1 = y - xyF[1][ij];
              if (dx1 * dx1 + dy1 * dy1 < minDistance) {
                near = true;
                break;
              }
            }
          }
          if (near) {
            PFreconstructed[i][j] = spl.value(interpolatedPoint);
          } else {
            PFreconstructed[i][j] = Double.NaN;
          }
        }
    } catch (ru.sscc.util.CalculatingException ce) {
      ce.printStackTrace();
    }
    return PFreconstructed;
  }

  class LimitsDialog extends JDialog {

//     private JTextField xminText = null;
//     private JTextField xmaxText = null;
    private JTextField yminText = null;
    private JTextField ymaxText = null;

    public LimitsDialog() {

      Container pane = LimitsDialog.this.getContentPane();
      pane.setLayout(new BorderLayout());

      JPanel rangepane = new JPanel();
      rangepane.setLayout(new BorderLayout(6, 6));
      pane.add(rangepane, BorderLayout.CENTER);

      JPanel jp1 = new JPanel();
      jp1.setLayout(new GridLayout(0, 1, 6, 6));
//        xminText = addRow(jp1, new JLabel("Xmin:"), xaxis.minimum);
//        xmaxText = addRow(jp1, new JLabel("Xmax:"), xaxis.maximum);
      yminText = addRow(jp1, new JLabel("Min:"), intensityMin);
      ymaxText = addRow(jp1, new JLabel("Max:"), intensityMax);
      rangepane.add("Center", jp1);

      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      JButton cancel = new JButton("No common range");
      cancel.setToolTipText("Each pole figure will have is own intensity range");
      JButton reset = new JButton("Reset");
      reset.setToolTipText("Use default common range");
      JButton done = new JButton("Accept");
      done.setToolTipText("Accept and use displayed values for range");
      jp1.add(cancel);
      jp1.add(reset);
      jp1.add(done);
      rangepane.add("South", jp1);

      cancel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          LimitsDialog.this.setVisible(false);
          LimitsDialog.this.dispose();
        }
      });

      reset.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          intensityMin = 0.0;
          intensityMax = 0.0;
          LimitsDialog.this.setVisible(false);
          LimitsDialog.this.dispose();
        }
      });

      done.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Double d;
//            double txmin = xaxis.minimum;
//            double txmax = xaxis.maximum;
          double tymin = intensityMin;
          double tymax = intensityMax;

//            d = Double.valueOf(xminText.getText());
//            if(d != null) txmin = d.doubleValue();
//            d = Double.valueOf(xmaxText.getText());
//            if(d != null) txmax = d.doubleValue();
          d = Double.valueOf(yminText.getText());
          if (d != null) tymin = d.doubleValue();
          d = Double.valueOf(ymaxText.getText());
          if (d != null) tymax = d.doubleValue();

          if (tymax < tymin) {
//                 xaxis.minimum = txmin;
//                 xaxis.maximum = txmax;
            double tmp = tymin;
            tymin = tymax;
            tymax = tmp;
          } else if (tymin == tymax) {
            tymin -= .1;
            tymax += .1;
          }
          intensityMin = tymin;
          intensityMax = tymax;

          LimitsDialog.this.setVisible(false);
          LimitsDialog.this.dispose();
        }
      });

      LimitsDialog.this.getRootPane().setDefaultButton(done);
      LimitsDialog.this.setTitle("Pole figure(s) intensity range");
      LimitsDialog.this.pack();

    }

    public JTextField addRow(JPanel panel, JLabel l1, double value) {

      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      jp.add(l1);
      JTextField textfield = new JTextField(20);
      textfield.setText(String.valueOf(value));
      jp.add(textfield);
      panel.add(jp);
      return textfield;
    }

  }

  Component focusedComponent = null;

  public class PoleFigureFocusListener implements FocusListener {
     public void focusGained(FocusEvent fe) {
       focusedComponent = fe.getComponent();

     }

     public void focusLost(FocusEvent fe) {

     }
  }

}
