/*
 * @(#)Texture.java created 16/07/1998 ILL, Grenoble
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.render3d.*;
import it.unitn.ing.jgraph.ColorMap;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;

/*import fr.ensicaen.odfplot.engine.Controller;
import fr.ensicaen.odfplot.functionSelector.FunctionSelector;
import fr.ensicaen.odfplot.sectionsVisualizer.SectionsFrame;
import fr.ensicaen.odfplot.functionVisualizer.OptionsFrame;
import fr.ensicaen.odfplot.isometricVisualizer.IsometricFrame;*/

/**
 * The Texture is a basic class for all texture computation models, all texture
 * models must extend this class and overwrite the necessary methods. This general
 * class is more like an abstract class and should not be instantiated. Some of
 * the methods provided are already general and does not need to be overwrited.
 * <p/>
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.22 $, $Date: 2006/01/19 14:45:55 $
 * @since JDK1.1
 */


public class Texture extends XRDcat {

  public static String[] prefs = {"texture.ODFdefaultResolution", "texture.PFintegrationStep",
                                  "texture.fiberTextureGenStep", "texture.defaultTubeRadius",
                                  "texture.defaultTubeProjectionStatus", "texture.minimumPFIntensity",
                                  "texture.storeConversionInMemory", "texture.startingExponent"
  };
  public static String[] prefVal = {"15.0", "0.0",
                                    "5.0", "10",
                                    "true", "0.02",
                                    "true", "0.01"
  };

  public int numberPoleFigures = 0;

  public static String NONE = "none";
  public static String TWO_FOLD = "2-fold";
  public static String THREE_FOLD = "3-fold";
  public static String FOUR_FOLD = "4-fold";
  public static String SIX_FOLD = "6-fold";
  public static String MIRROR = "mirror";
  public static String ORTHOROMBIC = "orthorhombic";
  public static String FIBER = "fiber";
  public static String[] symmetrychoice = {NONE, TWO_FOLD, THREE_FOLD, FOUR_FOLD,
                                           SIX_FOLD, MIRROR, ORTHOROMBIC,
                                           FIBER};

  public Texture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Texture(XRDcat aobj) {
    this(aobj, "Texture model x");
  }

  public Texture() {
  }

  public void saveTextureFactor(Phase aphase, Sample asample) {
    FilePar aparFile = getFilePar();
    if (!aparFile.isTextureComputationPermitted() || !Constants.textureOutput)
      return;

    String filename = new String(getFilePar().getDirectory() + aphase.toXRDcatString() + ".apf");
    BufferedWriter PFwriter = Misc.getWriter(filename);
    if (PFwriter != null) {
      try {

        PFwriter.write("Texture factors extracted for phase: " + aphase.toXRDcatString());
        PFwriter.write(Constants.lineSeparator);

        int hkln = aphase.gethklNumber();
        int numberPoleFigures = 0;
        for (int j = 0; j < hkln; j++) {
          Reflection refl = aphase.getReflectionVector().elementAt(j);

          if (refl.isGoodforTexture())
            numberPoleFigures++;
        }
        PFwriter.write(numberPoleFigures + "                  IZPOL");
        PFwriter.write(Constants.lineSeparator);
        for (int j = 0; j < hkln; j++) {
          Reflection refl = aphase.getReflectionVector().elementAt(j);
          if (refl.isGoodforTexture()) {
            PFwriter.write(refl.getH() + "   " + refl.getK() + "   " + refl.getL() + "            H K L");
            PFwriter.write(Constants.lineSeparator);

            int numberDatasets = asample.activeDatasetsNumber();
            int numberDataPoints = 0;
            for (int i = 0; i < numberDatasets; i++) {
	            DataFileSet dataset = asample.getActiveDataSet(i);
	            int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
              for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
	              DiffrDataFile datafile = dataset.getActiveDataFile(k);
	              for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
		              if (datafile.isInsideRange(datafile.getPositions(aphase)[j][ppp][0])) {
//			              for (int l = 0; l < radCount; l++) {
				              double pf = datafile.getExperimentalTextureFactors(aphase, j)[ppp][0 /*l*/];
				              if (!Double.isNaN(pf)) numberDataPoints++;
//			              }
		              }
	              }
              }
            }
            PFwriter.write(numberDataPoints + " <-  MEPSUM");
            PFwriter.write(Constants.lineSeparator);
            double wgt = Math.sqrt(refl.getWeight());

	          numberDataPoints = 0;
	          for (int i = 0; i < numberDatasets; i++) {
		          DataFileSet dataset = asample.getActiveDataSet(i);
		          int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
		          for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
			          DiffrDataFile datafile = dataset.getActiveDataFile(k);
			          for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
				          double position = datafile.getPositions(aphase)[j][ppp][0 /*l*/];
				          if (datafile.isInsideRange(position)) {
//				          for (int l = 0; l < radCount; l++) {
					          double pf = datafile.getExperimentalTextureFactors(aphase, j)[ppp][0 /*l*/];
					          double pfc = datafile.getTextureFactors(aphase, j)[ppp][0 /*l*/];
					          if (!Double.isNaN(pf)) {
						          numberDataPoints++;
						          double[] angles = datafile.getTextureAngles(position);
						          double[] mAngles = datafile.getTiltingAngle();
						          int bankNumber = datafile.getBankNumber() + 1;
						          double chi = angles[0];
						          double phi = angles[1];
						          PFwriter.write(chi + " " + phi + " " + pf + " " + pfc + " " + numberDataPoints + " " + wgt
								          + " " + mAngles[0] + " " + mAngles[1] + " " + mAngles[2] + " " + mAngles[3]
								          + " " + bankNumber);
						          PFwriter.write(Constants.lineSeparator);
					          }
				          }
			          }
              }
            }
          }
        }
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
        try {
          PFwriter.flush();
          PFwriter.close();
        } catch (IOException ieo) {
        }
      }
    }
  }

  public Phase getPhase() {
    return (Phase) getParent();
  }

  public int getPoleFigureNumberAll() {
    return getPhase().gethklNumber();
  }

  public int getPoleFigureNumber() {
    return numberPoleFigures;
  }

/*  public Reflection getReflectionAll(int pole) {
    return getPhase().reflectionv.elementAt(pole);
  }*/

  public double getMinimumIntensityD() {
    return 0.0;
  }

  public void initializeReflexes(Sample asample) {
  }

  public boolean getWIMVstatus() {
    return true;
  }

  public void computeTextureFactor(Phase aphase, Sample asample) {
    if (!refreshComputation)
      return;
	  for (int j = 0; j < asample.activeDatasetsNumber(); j++)
		  for (int k = 0; k < asample.getActiveDataSet(j).activedatafilesnumber(); k++)
			  asample.getActiveDataSet(j).getActiveDataFile(k).randomToTextureFactors(aphase);
    refreshComputation = false;
  }

  public double[] computeTextureFactor(Phase aphase, double[][] alphabeta,
                                       Reflection reflex) {

    int numberOfPoints = alphabeta.length / 2;
    double[] random = new double[numberOfPoints];
    for (int i = 0; i < numberOfPoints; i++)
      random[i] = 1.0;
    return random;
  }

  public double getODF(double alpha, double beta, double gamma) {
    // in radiant
    return 1.0;
  }

  public void initializeAll() {
  }

  public double getResolutionD() {
    return Constants.ODFresolution;
  }

  public double computeSharpness() {

    initializeAll();

    double va = 0.0, b = 0.0, fn = 0.0;

    double fnorm = 0.0;
    double resolutionR = getResolutionD() * Constants.DEGTOPI;

    b = resolutionR / 4.0;
    double min = 10.0, max = 0.0;

    for (double gamma = b; gamma <= Constants.PI2; gamma += resolutionR) {

      for (double beta = b; beta <= Constants.PI; beta += resolutionR) {
        double dvc = 2.0 * b;
        if (beta == b || Constants.PI - beta < b * 3.0)
          dvc *= 2.0;
        va = resolutionR * resolutionR * (Math.cos(beta - dvc) - Math.cos(beta + dvc));

        for (double alpha = b; alpha <= Constants.PI2; alpha += resolutionR) {
          fn = getODF(alpha, beta, gamma);
          min = Math.min(min, fn);
          max = Math.max(max, fn);
          fnorm += fn * fn * va;
        }
      }
    }
    fnorm /= (8.0 * Constants.PI * Constants.PI);
    System.out.println("F2 = " + fnorm + " , min = " + min + " , max = " + max);
    return fnorm;
  }

  public String computeAndGetSharpness() {
    return Double.toString(computeSharpness());
  }

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    return new double[numberofPoints][numberofPoints];
  }

  public double[] getPoleFigureGrid(Reflection refl, double[] x, double[] y) {

    return y;
  }

  public double[][] getInversePoleFigureGrid(double[] texture_angles,
                                             double maxPhi, int phiPointNumber,
                                             double maxBeta, int betaPointNumber) {

    return null;
  }

  public double[] getInversePoleFigureGrid(double[] texture_angles,
                                           double[][] phibeta) {
    return null;
  }

  public static String[] odfPlotMode = {"2D", "3D", "Sections", "Isometric view", "old 3D"};

  public void plotODFMap(double alphaStart1, double alphaEnd1, double alphaStep1,
                         double betaStart1, double betaEnd1, double betaStep1, int do3DplotIndex) {

    double alphaStart = alphaStart1 * Constants.DEGTOPI;
    double alphaEnd = alphaEnd1 * Constants.DEGTOPI;
    double alphaStep = alphaStep1 * Constants.DEGTOPI;
    double betaStart = betaStart1 * Constants.DEGTOPI;
    double betaEnd = betaEnd1 * Constants.DEGTOPI;
    double betaStep = betaStep1 * Constants.DEGTOPI;

    initializeAll();

    int alphaSlices = ((int) ((alphaEnd - alphaStart) / alphaStep)) + 1;
    int betaSlices = ((int) ((betaEnd - betaStart) / betaStep)) + 1;
    int gammaSlices = ((int) ((alphaEnd - alphaStart) / alphaStep)) + 1;
    double[][][] map3ToPlot = new double[alphaSlices][betaSlices][gammaSlices];
    for (int i = 0; i < alphaSlices; i++)
      for (int n = 0; n < betaSlices; n++)
        for (int j = 0; j < gammaSlices; j++)
          map3ToPlot[i][n][j] = ColorMap.DUMMY_VALUE;
    for (int n = 0; n < alphaSlices; n++) {
      double alpha = alphaStart + alphaStep * n;
      for (int i = 0; i < betaSlices; i++) {
        double beta = betaStart + betaStep * i;
        for (int j = 0; j < gammaSlices; j++) {
          double gamma = alphaStart + alphaStep * j;
          map3ToPlot[n][i][j] = getODF(alpha, beta, gamma); //atomMap[n][m][i];
        }
      }
    }

    switch (do3DplotIndex) {
      case 0:
        int is15 = (int) (Math.sqrt(betaSlices) * 800.0 / 600.0);
        int columns = is15;
        int rows = 0;
        rows = betaSlices / is15 + 1;
        if (betaSlices < is15)
          columns = betaSlices;
        int width = columns * alphaSlices + columns - 1;
        int height = rows * alphaSlices + rows - 1;
        double[][] mapToPlot = new double[width][height];
        for (int i = 0; i < width; i++)
          for (int n = 0; n < height; n++)
            mapToPlot[i][n] = ColorMap.DUMMY_VALUE;
        width = 0;
        height = 0;
        int row = 0;
        int column = 0;
        double IntensityMin = 0.0f;
        double IntensityMax = 0.0f;
        for (int i = 0; i < betaSlices; i++) {
//          double beta = betaStart + betaStep * i;
          for (int n = 0; n < alphaSlices; n++) {
//            double alpha = alphaStart + alphaStep * n;
            for (int m = 0; m < alphaSlices; m++) {
//              double gamma = alphaStart + alphaStep * m;
              mapToPlot[width + n][height + m] = map3ToPlot[n][i][m];//getODF(alpha, beta, gamma); //atomMap[n][m][i];
              if (IntensityMax < mapToPlot[width + n][height + m])
                IntensityMax = (double) mapToPlot[width + n][height + m];
            }
          }
          column++;
          if (column >= is15) {
            row++;
            column = 0;
          }
          width = column * (alphaSlices + 1);
          height = row * (alphaSlices + 1);
        }
        String title = "ODF map for " + getParent().toXRDcatString();
        new ODFMapPlot(new Frame(), mapToPlot, title,
            IntensityMin, IntensityMax, (double) (alphaStart * Constants.PITODEG), (double) (alphaEnd * Constants.PITODEG));

        break;
/*      case 1:
        prepare3Dplot(alphaStart1, alphaEnd1, alphaStep1,
                         betaStart1, betaEnd1, betaStep1, map3ToPlot);
        OptionsFrame frame = new OptionsFrame(controller, "ODF 3D");
        frame.setVisible(true);
        break;
      case 2:
        prepare3Dplot(alphaStart1, alphaEnd1, alphaStep1,
                         betaStart1, betaEnd1, betaStep1, map3ToPlot);
        SectionsFrame frame1 = new SectionsFrame(controller, "ODF sections");
        frame1.setVisible(true);
        break;
      case 3:
        prepare3Dplot(alphaStart1, alphaEnd1, alphaStep1,
                         betaStart1, betaEnd1, betaStep1, map3ToPlot);
        IsometricFrame frame2 = new IsometricFrame(controller, "ODF isometric");
        frame2.setVisible(true);
        break;*/
      case 4:
        Utilities3DRendering.show3DODF(new Frame(), map3ToPlot, alphaSlices, betaSlices, gammaSlices, 1, false, 256);
        break;
      default: {}
    }
  }

// 3D plot by Daniel

/*  protected Controller controller = null;

  protected FunctionSelector selectionneurFonction = null;

  public void prepare3Dplot(double alphaStart1, double alphaEnd1, double alphaStep1,
                         double betaStart1, double betaEnd1, double betaStep1, double[][][] map3ToPlot) {

    controller = new Controller(null);
    selectionneurFonction = new FunctionSelector(controller);
    controller.identifierFichier(alphaStart1, alphaEnd1, alphaStep1,
                         betaStart1, betaEnd1, betaStep1, map3ToPlot, getPhase().getLabel());
  }*/

// End 3D plot

	static String gridResString = "texturePlot.gridResolution";
	static String zoomString = "texturePlot.zoomFactor";
	static String maxAngleString = "texturePlot.maxAzimuthalAngle";
	static String logTexturePlotString = "texturePlot.logScale";
	static String numberofColors = "texturePlot.colorsNumber";
	public static int lastResolution = MaudPreferences.getInteger(gridResString, 101);
	public static double zoom = MaudPreferences.getDouble(zoomString, 1); // must be a power of 2
	public static double filterWidth = MaudPreferences.getDouble("texturePlot.gaussFilterWidth", 0.0);


	public void savePoleFiguresToFile(BufferedImage concatImage, String filename) {
		try {
			ImageIO.write(concatImage, "png", new File(filename));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public BufferedImage getPoleFigureBufferedImage(int w, int h, gov.noaa.pmel.sgt.ColorMap colorMap, Reflection pole,
	                                                int mode, int resolutionPoints, double maxAngle, int zoom) {
		BufferedImage bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);

		Graphics2D g = bi.createGraphics();

		PlotPoleFigure.createGrid(getFilePar().getSample(0), pole, mode, resolutionPoints,
			maxAngle, zoom, filterWidth);


		// drawing the circle around
		Stroke stroke = g.getStroke();
		g.setColor(Color.black);
		g.setStroke(new BasicStroke(1));
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g.drawOval(0, 0, w - 1, h - 1);
		g.setStroke(stroke);

		g.dispose();

		return bi;
	}

	public BufferedImage concatenateAllPoleFiguresImages(BufferedImage[] pfImages) {

		int imagesNumber = pfImages.length;
		int rowsNumber = 1;
		int colsNumber = imagesNumber;
		if (imagesNumber > 4) { // put in more rows
			rowsNumber = (int) Math.sqrt(imagesNumber);
			colsNumber = (imagesNumber + rowsNumber - 1) / rowsNumber;
		}

		int heightTotal = 0;
		for(int j = 0; j < imagesNumber; j += colsNumber) {
			heightTotal += pfImages[j].getHeight();
		}
		int widthTotal = 0;
		for(int j = 0; j < colsNumber; j++) {
			widthTotal += pfImages[j].getWidth();
		}

		BufferedImage concatImage = new BufferedImage(widthTotal, heightTotal, BufferedImage.TYPE_INT_RGB);
		Graphics2D g2d = concatImage.createGraphics();
		int heightCurr = 0;
		int widthCurr = 0;
		int index = 0;
		for(int j = 0; j < pfImages.length; j++) {
			if (index >= colsNumber) {
				index = 0;
				widthCurr = 0;
				heightCurr += pfImages[j - 1].getHeight();
			}
			g2d.drawImage(pfImages[j], widthCurr, heightCurr, null);
			widthCurr += pfImages[j].getWidth();
			index++;
		}
		g2d.dispose();
		return concatImage;
	}

  public boolean needIntensityExtractor() {
    return false;
  }

  public void notifyParameterChanged(Parameter source) {
    notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
  }

  public void notifyStringChanged(String source) {
    notifyStringChanged(source, Constants.TEXTURE_CHANGED);
  }

  public void notifyObjectChanged(XRDcat source) {
    notifyUpObjectChanged(source, Constants.TEXTURE_CHANGED);
  }

  public void refreshForNotificationUp(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || source == this || reason == Constants.TEXTURE_CHANGED) {
	    update(false);
	    refreshComputation = true;
    }
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || source == this || reason == Constants.TEXTURE_CHANGED) {
	    update(false);
	    refreshComputation = true;
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTextureOptionsD(parent, this);
    return adialog;
  }

  public class JTextureOptionsD extends JOptionsDialog {

    public JTextureOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Texture options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }
}
