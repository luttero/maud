/*
 * @(#)PlotPoleFigure.java created 3/10/1998 TUHH, Harburg-Hamburg
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

package it.unitn.ing.rista.awt;

import it.unitn.ing.jgraph.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import ru.sscc.util.data.*;
import ru.sscc.spline.reduction.ReducedMesh;
import ru.sscc.spline.reduction.StrictScatteredMesh;
import ru.sscc.spline.Spline;
import ru.sscc.spline.analytic.GSplineCreator;

/**
 * The PlotPoleFigure is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.14 $, $Date: 2006/11/10 09:32:59 $
 * @since JDK1.1
 */

public class PlotPoleFigure extends myJFrame {

	Panel pfPanel = null;
	static String[] rollingString = {"ND", "RD", "TD"};
	JMenu editMenu = null;
	PoleFigureMap[] ccolorMap = null;

	public PlotPoleFigure(Frame parent, Sample asample, Reflection[] pole, int mode,
	                      int numberofPoints, double zoom, double filterWidth,
	                      boolean grayScale, double maxAngle, boolean logScale, int colrsNumber) {

		super(parent);

		boolean editable = !MaudPreferences.getBoolean("plotPF.classicalPlot", false);
		frameWLabel = "plotPF.frameWidth";
		frameHLabel = "plotPF.frameHeight";
		defaultFrameW = 600;
		defaultFrameH = 400;
		setOwnSize = true;
		framePositionX = "plotPF.framePositionX";
		framePositionY = "plotPF.framePositionY";
		defaultFramePositionX = 10;
		defaultFramePositionY = 20;
		setOwnPosition = true;

		createDefaultMenuBar();

		Container c1 = getContentPane();
		c1.setLayout(new BorderLayout(6, 6));
		c1.setBackground(Color.white);

		String meanLabel = "1 mrd";
		double meanValue = 1.0;
		int decimals = 100;

		String first = "";
		switch (mode) {
			case 0:
				first = new String("Reconstructed pole figures");
				break;
			case 1:
				first = new String("Experimental pole figures");
				break;
			case 2:
				first = new String("Sample shape absorption pole figures");
				break;
			case 3:
				first = new String("Inverse pole figures");
				break;
			case 4:
				first = new String("Reconstructed strain pole figures");
				meanLabel = "0.0";
				meanValue = 0.0;
				decimals = 100000;
				break;
			case 5:
				first = new String("Experimental strain pole figures");
				meanLabel = "0.0";
				meanValue = 0.0;
				decimals = 100000;
				break;
			case 6:
				first = new String("Pole figures");
				break;
			default: {
			}
		}

		String log = "";
//		if (logScale)
//			log = " (Log scale, contours in log units)";
		Label title = new Label(first + log, Label.CENTER);
		title.setFont(new Font("TimesRoman", Font.PLAIN, 12));
		JPanel p1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
		p1.setBackground(Color.white);
		p1.add(title);
		c1.add(BorderLayout.NORTH, p1);

		p1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
		p1.setBackground(Color.white);
//    p1.add(new Label(""));
		c1.add(BorderLayout.WEST, p1);
		p1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
		p1.setBackground(Color.white);
//    p1.add(new Label(""));
		c1.add(BorderLayout.SOUTH, p1);

		int numberPoles = pole.length;
		if (mode == 3)
			numberPoles = 3;

		pfPanel = new Panel();

		ProgressFrame prF = null;
		try {
			if (Constants.showProgressFrame)
				prF = new ProgressFrame(numberPoles);
		} catch (NullPointerException npe) {
			System.out.println("Not able to create frame, MacOSX display sleep bug?");
		}
		if (prF != null) {
			prF.setProgressText("Pole figure computation....");
		}

		Object[] listGrid = new Object[numberPoles];
		String[] label = new String[numberPoles];
		double min = 1.0E30;
		double max = -1.0E30;
		int izoom = (int) Math.pow(2, zoom);
		int pixelsNumber = MaudPreferences.getInteger("plotPF.defaultPoleSize", 200);
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
		col--;
		int row = (int) (0.99 + (1.0 + numberPoles) / col);
		col = (int) (0.99 + (1.0 + numberPoles) / row);

		pfPanel.setLayout(new GridLayout(0, col, 0, 0));
		pfPanel.setBackground(Color.white);

		boolean inverse = false;
		if (mode == 3)
			inverse = true;
		for (int i = 0; i < numberPoles; i++) {
			double[][] grid = null;
			if (inverse) {
				mode = -i - 1;
				grid = createGrid(asample, pole[0], mode, numberofPoints, maxAngle, izoom, filterWidth);
			} else
				grid = createGrid(asample, pole[i], mode, numberofPoints, maxAngle, izoom, filterWidth);
			if (prF != null)
				prF.increaseProgressBarValue();
			if (inverse)
				label[i] = rollingString[i];
			else
				label[i] = Integer.toString(pole[i].getH()) + " " +
						Integer.toString(pole[i].getK()) + " " +
						Integer.toString(pole[i].getL());

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
		if (limits[1] <= limits[0])
			limits[1] = limits[0] + 1;
/*		if (logScale) {
			if (limits[0] <= 0)
				limits[0] = 0.01;
			if (limits[1] <= limits[0])
				limits[1] = limits[0] + 1;
			for (int j = 0; j < 2; j++)
				limits[j] = MoreMath.log10(limits[j]);
		}*/
		PoleFigureMap[] ccolorMap = null;
		if (editable)
			ccolorMap = new PoleFigureMap[numberPoles];
		for (int i = 0; i < numberPoles; i++) {
			double[][] grid = (double[][]) listGrid[i];
/*			if (logScale) {
				for (int j = 0; j < gridNumber; j++)
					for (int k = 0; k < gridNumber; k++)
						if (grid[j][k] != ColorMap.DUMMY_VALUE && !Double.isNaN(grid[j][k]) && grid[j][k] > 0.0)
							grid[j][k] = MoreMath.log10(grid[j][k]);
			}*/

			if (!editable) {
				ColorMap colorMap = new ColorMap(grid, gridNumber, limits[0], limits[1],
						grayScale, label[i], colrsNumber);
				pfPanel.add(colorMap);
			} else {
				int scaleType = 0;
				if (logScale) {
//					meanValue = 0;
//					unit = "Log(mrd)";
					scaleType = 1;
				}
				ccolorMap[i] = new PoleFigureMap(grid, gridNumber, limits[0], limits[1], grayScale, label[i],
						colrsNumber, editMenu, zoom, pixelsNumber, !inverse, scaleType);
				pfPanel.add(ccolorMap[i]);
			}
		}
		if (limits[0] != limits[1]) {
			int legendHeight = gridNumber;
			int pwidth = legendHeight / 5;
			int pheight = legendHeight;
			if (!editable) {
				double[] legendGrid = new double[pheight];
				double step = (limits[1] - limits[0]) / pheight;
				for (int j = 0; j < pheight; j++) {
					legendGrid[j] = step * j + limits[0];
				}
//				if (logScale)
//					meanValue = Math.log(1.0) / Math.log(10.0);

				MapLegend mapLegend = new MapLegend(legendGrid, pwidth, pheight, limits[0], limits[1], grayScale,
						logScale, meanValue, meanLabel, colrsNumber, decimals);
				pfPanel.add(mapLegend);
			} else {
				double[][] legendGrid = new double[pwidth][pheight];
				int scaleType = 0;
				if (logScale) {
					scaleType = 1;
					double startx = limits[0];
					double endx = limits[1];
					limits[0] = MoreMath.log10(limits[0]);
					limits[1] = MoreMath.log10(limits[1]);
					double step = (limits[1] - limits[0]) / pheight;
//			  System.out.println(step + " " + limits[1] + " " + limits[0] + " " + pheight);
					for (int j = 0; j < pheight; j++) {
						legendGrid[0][j] = step * j + limits[0];
						for (int i = 1; i < pwidth; i++)
							legendGrid[i][j] = legendGrid[0][j];
					}
//					limits[0] = startx;
//					limits[1] = endx;
				} else {
					double step = (limits[1] - limits[0]) / pheight;
//			  System.out.println(step + " " + limits[1] + " " + limits[0] + " " + pheight);
					for (int j = 0; j < pheight; j++) {
						legendGrid[0][j] = step * j + limits[0];
						for (int i = 1; i < pwidth; i++)
							legendGrid[i][j] = legendGrid[0][j];
					}
				}
				String unit = "mrd";
//				if (logScale) {
//					meanValue = 0;
//					unit = "Log(mrd)";
//				}
				LegendPoleFigureMap mapLegend = new LegendPoleFigureMap(legendGrid, pwidth, pheight, limits[0], limits[1], grayScale, unit,
							colrsNumber, editMenu, zoom, pixelsNumber, meanValue, scaleType);
				pfPanel.add(mapLegend);
			}
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

	public static double[][] createGrid(Sample asample, Reflection pole, int mode, int numberofPoints,
	                                    double maxAngle, int zoom, double filterWidth) {
		double PF[][] = null;
		double[] texture_angles = null;

		switch (mode) {
			case 0:
				PF = pole.getPoleFigureGrid(numberofPoints, maxAngle);
				break;
			case 1:
				PF = getExpPoleFigureGrid(pole.getExpPoleFigureGrid(), numberofPoints, maxAngle);
				break;
			case 2:
				PF = pole.getShapeAbsorptionPoleFigureGrid(numberofPoints, maxAngle, asample);
				break;
			case -1:
				texture_angles = new double[2];
				texture_angles[0] = 0.;
				texture_angles[1] = 0.;
				PF = pole.getInversePoleFigureGrid(texture_angles, numberofPoints, asample);
				break;
			case -2:
				texture_angles = new double[2];
				texture_angles[0] = 90.;
				texture_angles[1] = 90.;
				PF = pole.getInversePoleFigureGrid(texture_angles, numberofPoints, asample);
				break;
			case -3:
				texture_angles = new double[2];
				texture_angles[0] = 90.;
				texture_angles[1] = 0.;
				PF = pole.getInversePoleFigureGrid(texture_angles, numberofPoints, asample);
				break;
			case 4:
				PF = pole.getPoleFigureGridStrain(numberofPoints, maxAngle);
/*            for (int i = 0; i < numberofPoints; i++)
              for (int j = 0; j < numberofPoints; j++) {
                PF[i][j] *= 1000;
              }*/
				break;
			case 5:
				PF = pole.getExpPoleFigureGridStrain(numberofPoints, maxAngle);
/*            for (int i = 0; i < numberofPoints; i++)
              for (int j = 0; j < numberofPoints; j++) {
                PF[i][j] *= 1000;
              }*/
				break;
			case 6:

				break;
			default: {
			}
		}
//    if (zoom > 1 && filterWidth < 0.7)
//      filterWidth = 0.7;
		int gridnumber = getNewGridNumber(numberofPoints, zoom);
		return enlargeGrid(PF, numberofPoints, zoom, filterWidth);
	}

	public static double[][] enlargeGrid(double[][] PF, int numberofPoints, int zoom, double smooth) {

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
								int dx = (i1 - i) * (i1 - i) + (j1 - j) * (j1 - j);
								double tvalue = Math.exp(norm * dx);
								posvalue += PF[i1][j1] * tvalue;
								posweigth += tvalue;
								poscount++;
							} else {
								dummycount++;
							}
						} else {
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
		return hrPF;
	}

	public static int getNewGridNumber(int numberofPoints, int zoom) {
		if (zoom <= 1)
			return numberofPoints;
		if (zoom / 2 > 1)
			numberofPoints = getNewGridNumber(numberofPoints, zoom / 2);
		return numberofPoints * 2;
	}

	public double[][] getExpPoleFigureGridFake(int numberofPoints, double maxAngle) {
		return getPoleFigureGrid(numberofPoints, maxAngle);
	}

	public static double[][] getExpPoleFigureGrid(double[][] expTextureFactorsAndAngles,
	                                              int numberofPoints, double maxAngle) {

		double[][] PFreconstructed = new double[numberofPoints][numberofPoints];
		if (expTextureFactorsAndAngles != null) {
			int mode = MaudPreferences.getInteger("plotExpPF.splineMode", 1);

			double x, y;//, r;
			int numberExpPoints = expTextureFactorsAndAngles[0].length;

			int index = 0;
			double[][] xyF = new double[3][numberExpPoints];
			int[] wgt = new int[numberExpPoints];
			for (int i = 0; i < numberExpPoints; i++) {
				double projection = Constants.sqrt2 * Math.sin(expTextureFactorsAndAngles[0][i] * Constants.DEGTOPI / 2.0);
				if (expTextureFactorsAndAngles[0][i] > 90.) {
					projection = Constants.sqrt2 * Math.sin((180. - expTextureFactorsAndAngles[0][i]) * Constants.DEGTOPI / 2.0);
				}

				x = projection * Math.cos(expTextureFactorsAndAngles[1][i] * Constants.DEGTOPI);
				y = projection * Math.sin(expTextureFactorsAndAngles[1][i] * Constants.DEGTOPI);

				boolean overlapped = false;
				for (int j = 0; j < index; j++) {
					if (Math.abs(x - xyF[0][j]) < .00001 && Math.abs(y - xyF[1][j]) < .00001) {
						overlapped = true;
						xyF[2][j] += expTextureFactorsAndAngles[2][i];
						wgt[j]++;
					}
					if (overlapped)
						break;
				}
				if (!overlapped) {
					xyF[0][index] = x;
					xyF[1][index] = y;
					xyF[2][index] = expTextureFactorsAndAngles[2][i];
					wgt[index] = 1;
					index++;
				}
			}
			RealVectors measuredMesh = new DoubleVectors(2, index);
			double[] expTF = new double[index];
			for (int i = 0; i < index; i++) {
				measuredMesh.set(i, 0, xyF[0][i]);
				measuredMesh.set(i, 1, xyF[1][i]);
				xyF[2][i] /= wgt[i];
				expTF[i] = xyF[2][i];
			}
			double dxy = 2.0 * maxAngle / numberofPoints;
			int k = 0;
			double minDistance = MaudPreferences.getDouble("plotExpPF.minimumDistanceDeg", 5) * Constants.DEGTOPI;
			minDistance *= minDistance;
			double maxAngle2 = maxAngle * maxAngle;
			int maxForInterpolation = MaudPreferences.getInteger("plotExpPF.maxInterpolatedPoints", 50);
			if (index < numberofPoints) {

				try {
					ReducedMesh rMesh = new StrictScatteredMesh(measuredMesh);
					Spline spl = GSplineCreator.createSpline(mode, rMesh, expTF);

					RealVectors interpolatedMesh = new DoubleVectors(2, numberofPoints * numberofPoints);
					for (int i = 0; i < numberofPoints; i++)
						for (int j = 0; j < numberofPoints; j++, k++) {
							x = (i + 0.5) * dxy - maxAngle;
							y = (j + 0.5) * dxy - maxAngle;
							interpolatedMesh.set(k, 0, x);
							interpolatedMesh.set(k, 1, y);
						}
					RealPointers interpolatedPoint = new RealPointers(interpolatedMesh);

					k = 0;
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
			} else {
				try {
					int division = (int) Math.sqrt(index / 200);
					ReducedMesh rMesh = new StrictScatteredMesh(measuredMesh);
					Spline spl = GSplineCreator.createSpline(mode, rMesh, expTF);

					RealVectors interpolatedMesh = new DoubleVectors(2, numberofPoints * numberofPoints);
					for (int i = 0; i < numberofPoints; i++)
						for (int j = 0; j < numberofPoints; j++, k++) {
							x = (i + 0.5) * dxy - maxAngle;
							y = (j + 0.5) * dxy - maxAngle;
							interpolatedMesh.set(k, 0, x);
							interpolatedMesh.set(k, 1, y);
						}
					RealPointers interpolatedPoint = new RealPointers(interpolatedMesh);

					k = 0;
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
			}
		}
		return PFreconstructed;
	}

	public static double[][] getExpPoleFigureGridOld(double[][] expTextureFactorsAndAngles,
	                                                 int numberofPoints, double maxAngle) {

		double[][] PFreconstructed = new double[numberofPoints][numberofPoints];
		if (expTextureFactorsAndAngles != null) {
			int mode = MaudPreferences.getInteger("plotExpPF.splineMode", 1);

			double x, y;//, r;
			int numberExpPoints = expTextureFactorsAndAngles[0].length;

			int index = 0;
			double[][] xyF = new double[3][numberExpPoints];
			int[] wgt = new int[numberExpPoints];
			for (int i = 0; i < numberExpPoints; i++) {
				double projection = Constants.sqrt2 * Math.sin(expTextureFactorsAndAngles[0][i] * Constants.DEGTOPI / 2.0);
				if (expTextureFactorsAndAngles[0][i] > 90.) {
					projection = Constants.sqrt2 * Math.sin((180. - expTextureFactorsAndAngles[0][i]) * Constants.DEGTOPI / 2.0);
				}

				x = projection * Math.cos(expTextureFactorsAndAngles[1][i] * Constants.DEGTOPI);
				y = projection * Math.sin(expTextureFactorsAndAngles[1][i] * Constants.DEGTOPI);

				boolean overlapped = false;
				for (int j = 0; j < index; j++) {
					if (Math.abs(x - xyF[0][j]) < .00001 && Math.abs(y - xyF[1][j]) < .00001) {
						overlapped = true;
						xyF[2][j] += expTextureFactorsAndAngles[2][i];
						wgt[j]++;
					}
					if (overlapped)
						break;
				}
				if (!overlapped) {
					xyF[0][index] = x;
					xyF[1][index] = y;
					xyF[2][index] = expTextureFactorsAndAngles[2][i];
					wgt[index] = 1;
					index++;
				}
			}
			RealVectors measuredMesh = new DoubleVectors(2, index);
			double[] expTF = new double[index];
			for (int i = 0; i < index; i++) {
				measuredMesh.set(i, 0, xyF[0][i]);
				measuredMesh.set(i, 1, xyF[1][i]);
				xyF[2][i] /= wgt[i];
				expTF[i] = xyF[2][i];
			}
			double dxy = 2.0 * maxAngle / numberofPoints;
			int k = 0;
			double minDistance = MaudPreferences.getDouble("plotExpPF.minimumDistanceDeg", 5) * Constants.DEGTOPI;
			minDistance *= minDistance;
			double maxAngle2 = maxAngle * maxAngle;
			int maxForInterpolation = MaudPreferences.getInteger("plotExpPF.maxInterpolatedPoints", 50);
			if (index < numberofPoints) {

				try {
					ReducedMesh rMesh = new StrictScatteredMesh(measuredMesh);
					Spline spl = GSplineCreator.createSpline(mode, rMesh, expTF);

					RealVectors interpolatedMesh = new DoubleVectors(2, numberofPoints * numberofPoints);
					for (int i = 0; i < numberofPoints; i++)
						for (int j = 0; j < numberofPoints; j++, k++) {
							x = (i + 0.5) * dxy - maxAngle;
							y = (j + 0.5) * dxy - maxAngle;
							interpolatedMesh.set(k, 0, x);
							interpolatedMesh.set(k, 1, y);
						}
					RealPointers interpolatedPoint = new RealPointers(interpolatedMesh);

					k = 0;
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
			} else {
				boolean useSpline = true;
				if (maxForInterpolation < 0) {
					maxForInterpolation = -maxForInterpolation;
					useSpline = false;
				}
				if (maxForInterpolation < 2) {
					maxForInterpolation = 2;
				}
				int[] neighboor = new int[maxForInterpolation];
				double[] distance = new double[maxForInterpolation];
				for (int i = 0; i < numberofPoints; i++)
					for (int j = 0; j < numberofPoints; j++, k++) {
						x = (i + 0.5) * dxy - maxAngle;
						y = (j + 0.5) * dxy - maxAngle;
						boolean near = false;
						for (int h = 0; h < maxForInterpolation; h++)
							distance[h] = minDistance + 1.0;
						if (x * x + y * y <= maxAngle2) {
							for (int ij = 0; ij < index; ij++) {
								double dx1 = x - xyF[0][ij];
								double dy1 = y - xyF[1][ij];
								double dist2 = dx1 * dx1 + dy1 * dy1;
								if (dist2 < minDistance) {
									near = true;
									for (int h = 0; h < maxForInterpolation; h++) {
										if (dist2 < distance[h]) {
											for (int g = maxForInterpolation - 1; g > h; g--) {
												distance[g] = distance[g - 1];
												neighboor[g] = neighboor[g - 1];
											}
											distance[h] = dist2;
											neighboor[h] = ij;
											break;
										}
									}
								}
							}
						}
						if (near) {

							int maxNeighboor = 0;
							for (int h = 0; h < maxForInterpolation; h++)
								if (distance[h] < minDistance)
									maxNeighboor++;
							PFreconstructed[i][j] = Double.NaN;
							if (useSpline) {
								try {
									measuredMesh = new DoubleVectors(2, maxNeighboor);
									double[] measTF = new double[maxNeighboor];
									for (int h = 0; h < maxNeighboor; h++) {
										measuredMesh.set(h, 0, xyF[0][neighboor[h]]);
										measuredMesh.set(h, 1, xyF[1][neighboor[h]]);
										measTF[h] = xyF[2][neighboor[h]];
									}
									ReducedMesh rMesh = new StrictScatteredMesh(measuredMesh);
									Spline spl = GSplineCreator.createSpline(mode, rMesh, measTF);

									RealVectors interpolatedMesh = new DoubleVectors(2, 1);
									interpolatedMesh.set(0, 0, x);
									interpolatedMesh.set(0, 1, y);
									RealPointers interpolatedPoint = new RealPointers(interpolatedMesh);
									interpolatedPoint.select(0);
									PFreconstructed[i][j] = spl.value(interpolatedPoint);
								} catch (ru.sscc.util.CalculatingException ce) {
									ce.printStackTrace();
								}
							}
							if (Double.isNaN(PFreconstructed[i][j])) {
								double valueIntensity = 0;
								double weight = 0;
								for (int h = 0; h < maxNeighboor; h++) {
									if (distance[h] < 1.0E-9) {
										valueIntensity = xyF[2][neighboor[h]];
										weight = 1.0;
										break;
									}
									double partialWeight = 1.0 / Math.sqrt(Math.sqrt(distance[h]));
									valueIntensity += xyF[2][neighboor[h]] * partialWeight;
									weight += partialWeight;
								}
								PFreconstructed[i][j] = valueIntensity / weight;
							}

						} else {
							PFreconstructed[i][j] = Double.NaN;
						}
					}
			}
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

			double minDistance = MaudPreferences.getDouble("plotExpPF.minimumDistanceDeg", 10.0) / 180.0;
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




