/*
 * @(#)PlotFitting.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import com.amd.aparapi.*;
import com.amd.aparapi.device.Device;
import it.unitn.ing.jgraph.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.sdpd.BasicIndexingUtilities;
import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import java.awt.*;
import java.awt.event.*;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Vector;

/**
 * The PlotFitting is a class
 *
 * @version $Revision: 1.11 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PlotFitting extends PlotDataFile {

//      URL markerURL;
//  DataSet dataFit = null;
//  DataSet datar = null;
//  PeakSet[] datap = null;
//  Graph2D residuals = null;
//  G2Dint positions = null;
//  Axis yaxisp = null;
//  Axis xaxisr = null;
//  Axis yaxisr = null;
  JLiveOptionsD livedialog = null;
  double[][] peaksList = null;
  double[] secondDerivative = null;
  SpectrumPlotPanel thePlotPanel = null;
  DiffrDataFile[] datafile;

  boolean toolUsed = true;

  public PlotFitting(Frame parent) {

    super(parent);

  }

  public PlotFitting(Frame parent, DiffrDataFile[] afile) {

    this(parent, afile, null, null);

  }

  public PlotFitting(Frame parent, DiffrDataFile[] afile, double[][] peaks,
                     double[] derivative2) {

    this(parent);

    createDefaultMenuBar();

    Container p1 = getContentPane();
    p1.setLayout(new BorderLayout());
    p1.setBackground(Color.white);
    peaksList = peaks;
    secondDerivative = derivative2;
    datafile = afile;
    thePlotPanel = new SpectrumPlotPanel(afile, peaks, derivative2);
    if (thePlotPanel != null) {
	    p1.add(thePlotPanel, BorderLayout.CENTER);
	    setComponentToPrint(thePlotPanel.getComponentToPrint());
    }
//    thePlotPanel.getComponentToPrint().setBackground(Color.white);

  }

  public void showOptionsDialog() {
    thePlotPanel.showOptionsDialog();
  }

  public JMenu createToolsMenu() {

    JMenu toolsMenu = super.createToolsMenu();

    JMenuItem menuitem = null;

    toolsMenu.add(new JSeparator());

    toolsMenu.add(menuitem = new JMenuItem("Change parameter"));
    menuitem.addActionListener(new ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		    showLiveDialog();
	    }
    });

		  if (Constants.testing) {
			  toolsMenu.add(menuitem = new JMenuItem("Plot Integral Bessel Function"));
			  menuitem.addActionListener(new ActionListener() {
				  public void actionPerformed(ActionEvent e) {
					  plotIntegralBessel();
				  }

				  private void plotIntegralBessel() {
					  int max = 10000;
					  double stepX = 500.0 / max;
					  double[] x = new double[max];
					  double[] y = new double[max];
					  double[] z = new double[max];
					  double[] v = new double[max];
					  for (int i = 0; i < max; i++) {
						  x[i] = stepX * i;
						  y[i] = IntegratedBesselJ0.integralBesselUpTo(x[i]);
						  if (i == 0)
							  z[i] = 1;
						  else
						    z[i] = y[i] / x[i];
					  }
					  (new PlotSimpleData(new Frame(), x, y)).setVisible(true);
					  (new PlotSimpleData(new Frame(), x, z)).setVisible(true);
					  for (int i = 0; i < max; i++) {
						  x[i] = stepX * i;
						  v[i] = IntegratedBesselJ0.averageIntegralBesselUpTo(x[i]);
					  }
					  (new PlotSimpleData(new Frame(), x, v)).setVisible(true);
				  }
			  });
		  }

	  return toolsMenu;
  }


	boolean startingPoint = false;

  public void prepareForTools(boolean setData) {
    if (!startingPoint)
      thePlotPanel.datafile[0].setStartingPointForTools(setData);
    startingPoint = true;
  }

  public void resetStartingPoint() {
    startingPoint = false;
    prepareForTools(true);
    updatePlotForTools();
  }

	public void inverseFourierTransformFFT() {
		prepareForTools(true);
		int multiplier = MaudPreferences.getInteger("inversepdf_FFT.multiplier_scale", 16);
		double bkg = MaudPreferences.getDouble("inversepdf_FFT.background_Gr", 0.0);
		int dtanumber = thePlotPanel.datafile[0].computeDataNumber();
		int fftlength = MoreMath.getNextPowerof2(dtanumber * multiplier);
	//	int shift = fftlength / multiplier - dtanumber;
	//	System.out.println(shift);
		double[] realfft = new double[fftlength];
		double[] imgfft = new double[fftlength];
		int i = 0;
		for (int j = 0; j < dtanumber; j++, i++)
			imgfft[i] = thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex) + bkg;
		for (; i < fftlength; i++)
			imgfft[i] = 0.0;
		for (i = 0; i < fftlength; i++)
			realfft[i] = 0.0;

		FFT.fft(fftlength, realfft, imgfft);

		double stepQ = Constants.PI2 * dtanumber / (fftlength *
				thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].finalindex - 1));
		thePlotPanel.datafile[0].setXData(thePlotPanel.datafile[0].startingindex, 0);
		thePlotPanel.datafile[0].setYData(thePlotPanel.datafile[0].startingindex, 0);
		thePlotPanel.datafile[0].setPhasesFit(thePlotPanel.datafile[0].startingindex, 0);
		double lover4pi = thePlotPanel.datafile[0].getMeanRadiationWavelength() / (Constants.PI2 * 2.0);
		for (i = 1; i < dtanumber; i++) {
			double Q = stepQ * i;
//			double theta2 = 2.0 * Constants.PITODEG * Math.asin(lover4pi * Q);
//			double sqrt = Math.sqrt((-imgfft[i + shift] * imgfft[i + shift] + realfft[i] * realfft[i]) / 2.0);
			thePlotPanel.datafile[0].setXData(i + thePlotPanel.datafile[0].startingindex, Q);
//			thePlotPanel.datafile[0].setYData(i + thePlotPanel.datafile[0].startingindex, imgfft[i] / Q);
			thePlotPanel.datafile[0].setPhasesFit(i + thePlotPanel.datafile[0].startingindex, realfft[i] / Q + 1);
		}

		if (realfft != null)
			updatePlotForTools();

	}

	final static class InverseFourierTransform extends Kernel{

		private int dtanumber;

		private float stepQ;

		private float[] sq, gr, Q, r;

		@Override public void run() {
			int i = getGlobalId(0);
			float qq = stepQ * i;
			Q[i] = qq;
			sq[i] = 0;
			for (int j = 0; j < dtanumber; j++)
				sq[i] += gr[j] * sin(qq * r[j]);
			if (qq > 0)
				sq[i] = sq[i] / qq + 1;
			else
				sq[i] = 0;
		}

		public void applyTransform(float[] _sq, float[] _gr, float[] _r, float[] _Q, int _dtanumber, float _stepQ, int fftlength) {
			sq = _sq;
			gr = _gr;
			Q = _Q;
			r = _r;
			dtanumber = _dtanumber;
			stepQ = _stepQ;
			Device device = Constants.openclDevice;
//			System.out.println("Using openCL device: " + Constants.openclDevice.getDeviceId());
			Range range = device.createRange(fftlength);
			execute(range);
		}

	}

	public void inverseFourierTransform() {
		prepareForTools(true);
		int multiplier = MaudPreferences.getInteger("pdf_inverse_FFT.multiplier_scale", 1);
//		double bkg = MaudPreferences.getDouble("pdf_FFT.background_Gr", 0.0);
		int dtanumber = thePlotPanel.datafile[0].computeDataNumber();
		int fftlength = multiplier * dtanumber;
		float stepQ = (float) (Constants.PI / (multiplier *
				thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].finalindex - 1)));
//		double normalization = 2.0 / Constants.PI;
		//	System.out.println(shift);
		float[] sq = new float[fftlength];
		float[] Q = new float[fftlength];
		float[] gr = new float[dtanumber];
		float[] r = new float[dtanumber];
		float prevr, nextr;
		int j = 0;
		r[j] = (float) thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].startingindex);
		nextr = (float) thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].startingindex + 1);
		prevr = Q[j] + Q[j] - nextr;
		gr[j] = (float) thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex);
		gr[j] *= Math.abs(nextr - prevr) / 2;
		prevr = r[j];
		r[j + 1] = nextr;
		for (j = 1; j < dtanumber - 1; j++) {
			nextr = (float) thePlotPanel.datafile[0].getXData(j + thePlotPanel.datafile[0].startingindex + 1);
			gr[j] = (float) thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex);
			gr[j] *= Math.abs(nextr - prevr) / 2;
			prevr = r[j];
			r[j + 1] = nextr;
		}
		nextr = r[j] + r[j] - prevr;
		gr[j] = (float) thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex);
		gr[j] *= Math.abs(nextr - prevr) / 2;

		long previousTime = System.currentTimeMillis();

		if (Constants.useOpenCL)
			(new InverseFourierTransform()).applyTransform(sq, gr, r, Q, dtanumber, stepQ, fftlength);
		else {
			for (int i = 0; i < fftlength; i++) {
				sq[i] = 0;
				Q[i] = stepQ * i;
//			System.out.println(r[i]);
				for (j = 0; j < dtanumber; j++)
					sq[i] += gr[j] * Math.sin(Q[i] * r[j]);
				if (Q[i] > 0)
					sq[i] = sq[i] / Q[i] + 1;
				else
					sq[i] = 0;
			}
		}

		System.out.println("Fourier computation time (opencl = " + Constants.useOpenCL + "): " +
				(-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

//		exportComputedPDF(Q, sq);

		thePlotPanel.datafile[0].setXData(thePlotPanel.datafile[0].startingindex, 0);
		thePlotPanel.datafile[0].setPhasesFit(thePlotPanel.datafile[0].startingindex, 0.0);
		for (int i = 1; i < dtanumber; i++) {
			thePlotPanel.datafile[0].setXData(i + thePlotPanel.datafile[0].startingindex, Q[i]);
			thePlotPanel.datafile[0].setPhasesFit(i + thePlotPanel.datafile[0].startingindex, sq[i]);
		}

		updatePlotForTools();

	}

	public void inverseReflectivityFourierTransform() {
		prepareForTools(true);
		int multiplier = MaudPreferences.getInteger("inverse_FFT.multiplier_scale", 8);
		double bkg = MaudPreferences.getDouble("inverse_FFT.background", 0.0);
		boolean log = MaudPreferences.getBoolean("inverse_FFT.useLog", false);
		int dtanumber = thePlotPanel.datafile[0].computeDataNumber();
		int fftlength = MoreMath.getNextPowerof2(dtanumber * multiplier);
		//	int shift = fftlength / multiplier - dtanumber;
		//	System.out.println(shift);
		double[] realfft = new double[fftlength];
		int i = 0;
		double sur_wave = 2.0 / thePlotPanel.datafile[0].getMeanRadiationWavelength();
		double cx = thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].startingindex);
		cx = Math.cos(cx * Constants.DEGTOPI * 0.5);
		cx *= cx;
//		double max = 0;
		for (int j = 0; j < dtanumber; j++, i++) {
			realfft[j] = thePlotPanel.datafile[0].getYData(j + thePlotPanel.datafile[0].startingindex) - bkg;
			if (log)
				realfft[j] = Math.log(Math.abs(realfft[j]));
//			if (realfft[j] > max)
//				max = realfft[j];
		}
		for (int j = 0; j < dtanumber; j++, i++) {
			double sx = thePlotPanel.datafile[0].getXData(j + thePlotPanel.datafile[0].startingindex);
			sx = Math.cos(sx * Constants.DEGTOPI * 0.5);
			sx *= sx;
			double s_corr = sur_wave * Math.sqrt(cx - sx);
			realfft[j] = MoreMath.pow(s_corr, 4) * realfft[j];// / max;
			thePlotPanel.datafile[0].setXData(j + thePlotPanel.datafile[0].startingindex, s_corr);
		}
		for (; i < fftlength; i++)
			realfft[i] = 0.0;

		FFT.realfft(fftlength, realfft);

		for (i = 0; i < dtanumber; i++) {
			thePlotPanel.datafile[0].setYData(i + thePlotPanel.datafile[0].startingindex, realfft[i] * realfft[i]);
		}

//		if (realfft != null)
			updatePlotForTools();

	}

	public void fourierTransformFFT() {
		prepareForTools(true);
		int multiplier = MaudPreferences.getInteger("pdf_FFT.multiplier_scale", 16);
		double bkg = MaudPreferences.getDouble("pdf_FFT.background_Gr", 0.0);
		int dtanumber = thePlotPanel.datafile[0].computeDataNumber();
		int fftlength = MoreMath.getNextPowerof2(dtanumber * multiplier);
		//	int shift = fftlength / multiplier - dtanumber;
		//	System.out.println(shift);
		double lover4pi = thePlotPanel.datafile[0].getMeanRadiationWavelength() / (Constants.PI2 * 2.0);
		double[] realfft = new double[fftlength];
		double[] imgfft = new double[fftlength];
		int i = 0;
		for (int j = 0; j < dtanumber; j++, i++) {
			double Q = thePlotPanel.datafile[0].getXData(j + thePlotPanel.datafile[0].startingindex);
			imgfft[i] = (thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex) - 1) * Q + bkg;
		}
		for (; i < fftlength; i++)
			imgfft[i] = 0.0;
		for (i = 0; i < fftlength; i++)
			realfft[i] = 0.0;

		FFT.ifft(fftlength, realfft, imgfft);

		double stepR = Constants.PI2 * dtanumber / (fftlength *
				thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].finalindex - 1));
		thePlotPanel.datafile[0].setXData(thePlotPanel.datafile[0].startingindex, 0);
		thePlotPanel.datafile[0].setYData(thePlotPanel.datafile[0].startingindex, 0);
		thePlotPanel.datafile[0].setPhasesFit(thePlotPanel.datafile[0].startingindex, 0);
		for (i = 1; i < dtanumber; i++) {
			double R = stepR * i;
//			double sqrt = Math.sqrt((-imgfft[i + shift] * imgfft[i + shift] + realfft[i] * realfft[i]) / 2.0);
			thePlotPanel.datafile[0].setXData(i + thePlotPanel.datafile[0].startingindex, R);
//			thePlotPanel.datafile[0].setYData(i + thePlotPanel.datafile[0].startingindex, imgfft[i] / Q);
			thePlotPanel.datafile[0].setPhasesFit(i + thePlotPanel.datafile[0].startingindex, 2.0 / Constants.PI * realfft[i]);
		}

		if (realfft != null)
			updatePlotForTools();

	}

	public void fourierTransform() {
		// should be in equal steps of Q for fft
		prepareForTools(MaudPreferences.getBoolean("pdf_FFT.useData", true));
		int multiplier = MaudPreferences.getInteger("pdf_FFT.multiplier_scale", 1);
//		double bkg = MaudPreferences.getDouble("pdf_FFT.background_Gr", 0.0);
		int dtanumber = thePlotPanel.datafile[0].computeDataNumber();
		double stepR = Constants.PI / (multiplier *
				thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].finalindex - 1));
		double normalization = 2.0 / Constants.PI;
		//	System.out.println(shift);
//		if (multiplier > 8)
//			multiplier /= 2;
		int fftlength = multiplier * dtanumber;
		double[] gr = new double[fftlength];
		double[] r = new double[fftlength];
		double[] sq = new double[dtanumber];
		double[] Q = new double[dtanumber];
		double prevQ, nextQ;
		int j = 0;
		Q[j] = thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].startingindex);
		nextQ = thePlotPanel.datafile[0].getXData(thePlotPanel.datafile[0].startingindex + 1);
		prevQ = Q[j] + Q[j] - nextQ;
		sq[j] = (thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex) - 1) * Q[j];
		sq[j] *= Math.abs(nextQ - prevQ) / 2.0;
		prevQ = Q[j];
		Q[j + 1] = nextQ;
		for (j = 1; j < dtanumber - 1; j++) {
			nextQ = thePlotPanel.datafile[0].getXData(j + thePlotPanel.datafile[0].startingindex + 1);
			sq[j] = (thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex) - 1) * Q[j];
			sq[j] *= Math.abs(nextQ - prevQ) / 2.0;
			prevQ = Q[j];
			Q[j + 1] = nextQ;
		}
		nextQ = Q[j] + Q[j] - prevQ;
		sq[j] = (thePlotPanel.datafile[0].getFit(j + thePlotPanel.datafile[0].startingindex) - 1) * Q[j];
		sq[j] *= Math.abs(nextQ - prevQ) / 2.0;

		for (int i = 0; i < fftlength; i++) {
			gr[i] = 0.0;
			r[i] = stepR * i;
//			System.out.println(r[i]);
			for (j = 0; j < dtanumber; j++) {
				gr[i] += sq[j] * Math.sin(Q[j] * r[i]);
			}
			gr[i] *= normalization;
		}

//		exportComputedPDF(r, gr);
/*		fftlength = MoreMath.getNextPowerof2(dtanumber);
		double[] fft = new double[fftlength];
		int i = 0;
		for (; i < dtanumber; i++)
			fft[i] = sq[i];
		for (i = dtanumber; i < fftlength; i++)
			fft[i] = 1.0;

		FFT.realfft(fftlength, fft);

		for (i = 0; i < dtanumber; i++)
			thePlotPanel.datafile[0].setPhasesFit(i + thePlotPanel.datafile[0].startingindex, fft[i]);
*/

		for (int i = 0; i < dtanumber; i++) {
//			thePlotPanel.datafile[0].setXData(i + thePlotPanel.datafile[0].startingindex, r[i]);
			thePlotPanel.datafile[0].setPhasesFit(i + thePlotPanel.datafile[0].startingindex, gr[i]);
		}

		updatePlotForTools();

	}

	public void fourierSmoothing() {
    prepareForTools(true);

    int dtanumber = thePlotPanel.datafile[0].computeDataNumber();
    int fftlength = MoreMath.getNextPowerof2(dtanumber);
    double[] fft = new double[fftlength];
    int i = 0;
    for (; i < dtanumber; i++)
      fft[i] = thePlotPanel.datafile[0].getFit(i + thePlotPanel.datafile[0].startingindex);
    for (i = dtanumber; i < fftlength; i++)
      fft[i] = 0.0;

    FFT.realfft(fftlength, fft);

    for (i = 0; i < dtanumber; i++)
      thePlotPanel.datafile[0].setPhasesFit(i + thePlotPanel.datafile[0].startingindex, fft[i]);

    if (fft != null)
      updatePlotForTools();

    showLiveSmoothing(fft, thePlotPanel.datafile[0]);
  }

  public void inverseFourierSmoothing(double[] fft, DiffrDataFile adatafile) {
    prepareForTools(true);
    int dtanumber = adatafile.computeDataNumber();
    int fftlength = fft.length;
    FFT.realifft(fftlength, fft);
    double scale = 1.0 / fftlength;
    for (int i = 0; i < dtanumber; i++)
      fft[i] *= scale;
// this is temporary
    for (int i = 0; i < dtanumber; i++)
      adatafile.setPhasesFit(i + adatafile.startingindex, fft[i]);
    if (fft != null)
      updatePlotForTools();
  }

	public void smoothing() {
		prepareForTools(true);
		if (thePlotPanel.datafile[0].smoothing())
			updatePlotForTools();
	}

	public void backgroundSubtraction() {
    prepareForTools(true);
    if (thePlotPanel.datafile[0].backgroundSubtraction())
      updatePlotForTools();
  }

  public void kalpha2Stripping() {
    prepareForTools(true);
    if (thePlotPanel.datafile[0].kalpha2Stripping())
      updatePlotForTools();
  }

  public void peaksLocationFrame() {
    showPeakSmoothing(thePlotPanel.datafile[0]);
  }

  public void peaksLocation() {
    prepareForTools(true);
    int dtanumber = thePlotPanel.datafile[0].computeDataNumber();
    double[] derivative2 = new double[dtanumber];

    double[][] peaks = thePlotPanel.datafile[0].peaksLocation(derivative2);
    if (peaks != null) {
      showNewFrame(peaks, derivative2);
  //      thePlotPanel.peaksList = peaks;
    }
  }

  public void setCustomPeakList(Phase aphase) {
    if (thePlotPanel.peaksList == null)
      return;
    double[] peakList = new double[thePlotPanel.peaksList[0].length];
    for (int i = 0; i < thePlotPanel.peaksList[0].length; i++)
      peakList[i] = thePlotPanel.peaksList[0][i];
    if (!thePlotPanel.datafile[0].dspacingbase) {
      double wave = thePlotPanel.datafile[0].getMeanRadiationWavelength();
      for (int i = 0; i < peakList.length; i++)
        peakList[i] = wave / (2.0 * MoreMath.sind(peakList[i] / 2.0));
    }
    aphase.setCustomPeakList(peakList);
  }

  public void exportPeaksDicvol91() {
    if (thePlotPanel.peaksList == null)
      return;

    int mode = 2;
    if (thePlotPanel.datafile[0].dspacingbase || checkScaleModeX() == 1)
      mode = 3;
    boolean[] symmetrySwitch = new boolean[6];
    for (int i = 0; i < 5; i++)
      symmetrySwitch[i] = true;
    symmetrySwitch[5] = false;
    double wave = thePlotPanel.datafile[0].getDataFileSet().getInstrument().getRadiationType().getMeanRadiationWavelength();
    double error = 0.05;

    int numberOfPeaks = thePlotPanel.peaksList[0].length;

    String symmetry = "";
    for (int i = 0; i < 6; i++)
      if (symmetrySwitch[i])
        symmetry += " 1";
      else
        symmetry += " 0";
    String filename = Utility.openFileDialog(this, "Save peak file as", FileDialog.SAVE,
            "",
            null, "untitled.dat");
    BufferedWriter output = null;
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        output.write("Peaks from: " + thePlotPanel.datafile[0].getTitle());
        output.newLine();
        output.write(numberOfPeaks + " " + mode + symmetry);
        output.newLine();
        output.write("30. 30. 30. 0. 4000. 0. 0.");
        output.newLine();
        output.write(Fmt.format(wave) + " 0. 0. 0.");
        output.newLine();
        output.write("1. 0. 0.");
        output.newLine();
        for (int i = 0; i < numberOfPeaks; i++) {
          output.write(Fmt.format(thePlotPanel.peaksList[0][i]) + " " + Fmt.format(error));
          output.newLine();
        }
      } catch (IOException io) {
      }
      try {
        output.flush();
        output.close();
      } catch (IOException io) {
      }
    }
  }

	public void exportOriginalDataFPSM() {

		if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null)
			return;

		String filename = Utility.openFileDialog(this, "Save as CIF...",
				FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
		if (filename == null)
			return;

		String[] folderAndName = Misc.getFolderandName(filename);

		String folder = folderAndName[0];
		filename = folderAndName[1];

		if (Constants.sandboxEnabled && !filename.endsWith(".cif"))
			filename = filename + ".cif";

		if (filename != null) {

			BufferedWriter output = Misc.getWriter(folder, filename);
			try {
				int nPoints = thePlotPanel.datafile[0].computeDataNumber();
				output.write("data_" + thePlotPanel.datafile[0]);
				output.newLine();
				datafile[0].getDataFileSet().getInstrument().exportInstrumentDataForFPSM(output);

				output.write("_pd_meas_angle_omega " + Fmt.format(datafile[0].getOmegaValue()));
				output.newLine();
				output.write("_pd_meas_angle_chi " + Fmt.format(datafile[0].getChiValue()));
				output.newLine();
				output.write("_pd_meas_angle_phi " + Fmt.format(datafile[0].getPhiValue()));
				output.newLine();
				output.write("_pd_meas_angle_eta " + Fmt.format(datafile[0].getEtaValue()));
				output.newLine();
				output.write("_pd_meas_angle_2theta " + Fmt.format(datafile[0].get2ThetaValue()));
				output.newLine();

				output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
				output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
				output.write("_riet_meas_datafile_calibrated true");
				output.newLine();
				output.write("loop_");
				output.newLine();
				output.write("_pd_meas_2theta_scan");
				output.newLine();
				output.write("_pd_meas_intensity_total");
				output.newLine();
				int starting = thePlotPanel.datafile[0].startingindex;
				int ending = thePlotPanel.datafile[0].finalindex;
				int step = 1;
				int mode = checkScaleModeX();
				if (thePlotPanel.datafile[0].getXData(ending - 1) < thePlotPanel.datafile[0].getXData(starting)) {
					starting = thePlotPanel.datafile[0].finalindex - 1;
					ending = thePlotPanel.datafile[0].startingindex - 1;
					step = -1;
				}
				boolean from_Plot = MaudPreferences.getBoolean("exportData.useXcoordinateFromPlot", false);
				for (int i = starting; i != ending; i+=step) {
					double intens = thePlotPanel.datafile[0].getYData(i);
					double xcoorddata;
					if (!from_Plot)
						xcoorddata = thePlotPanel.datafile[0].getXData(i);
					else
						xcoorddata = thePlotPanel.datafile[0].getXDataForPlot(i, mode);
					output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
					output.newLine();
				}
			} catch (IOException io) {
			}
			try {
				output.close();
			} catch (IOException io) {
			}
		}
	}

	public void phaseIdentificationByFPSM() {

	}

	public void exportOriginalData() {

    if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null)
      return;

    String filename = Utility.openFileDialog(this, "Save as CIF...",
		    FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (Constants.sandboxEnabled && !filename.endsWith(".cif"))
      filename = filename + ".cif";

    if (filename != null) {

      BufferedWriter output = Misc.getWriter(folder, filename);
      try {
        int nPoints = thePlotPanel.datafile[0].computeDataNumber();
	      output.write("data_" + thePlotPanel.datafile[0]);
        output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
        output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
        output.write("_riet_meas_datafile_calibrated true");
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write(DiffrDataFile.CIFXcoord2T);
        output.newLine();
        output.write("_pd_meas_intensity_total");
        output.newLine();
        int starting = thePlotPanel.datafile[0].startingindex;
        int ending = thePlotPanel.datafile[0].finalindex;
        int step = 1;
	      int mode = checkScaleModeX();
        if (thePlotPanel.datafile[0].getXData(ending - 1) < thePlotPanel.datafile[0].getXData(starting)) {
          starting = thePlotPanel.datafile[0].finalindex - 1;
          ending = thePlotPanel.datafile[0].startingindex - 1;
          step = -1;
        }
	      boolean from_Plot = MaudPreferences.getBoolean("exportData.useXcoordinateFromPlot", false);
        for (int i = starting; i != ending; i+=step) {
          double intens = thePlotPanel.datafile[0].getYData(i);
          double xcoorddata;
	        if (!from_Plot)
	          xcoorddata = thePlotPanel.datafile[0].getXData(i);
	        else
	          xcoorddata = thePlotPanel.datafile[0].getXDataForPlot(i, mode);
          output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
          output.newLine();
        }
      } catch (IOException io) {
      }
      try {
        output.close();
      } catch (IOException io) {
      }
    }
  }

	public void exportComputedPDF(double[] r, double[] gr) {

		if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null)
			return;

		String filename = Utility.openFileDialog(this, "Save as prn",
				FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.prn");
		if (filename == null)
			return;

		String[] folderAndName = Misc.getFolderandName(filename);

		String folder = folderAndName[0];
		filename = folderAndName[1];

		if (Constants.sandboxEnabled && !filename.endsWith(".prn"))
			filename = filename + ".prn";

		if (filename != null) {

			BufferedWriter output = Misc.getWriter(folder, filename);
			try {
				int nPoints = r.length;
				for (int i = 0; i < nPoints; i++) {
					output.write(" " + Fmt.format(r[i]) + " " + Fmt.format(gr[i]));
					output.newLine();
				}
			} catch (IOException io) {
			}
			try {
				output.close();
			} catch (IOException io) {
			}
		}
	}

  public void exportComputedData() {

//	  System.out.println(thePlotPanel);
//	  System.out.println(thePlotPanel.datafile);
    if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null) {
	    return;
    }
	  System.out.println("Length " + thePlotPanel.datafile.length);
	  if (thePlotPanel.datafile.length <= 0) {
		  return;
	  }

    String filename = Utility.openFileDialog(this, "Save as CIF...",
            FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (Constants.sandboxEnabled && !filename.endsWith(".cif"))
      filename = filename + ".cif";

    if (filename != null) {

	    boolean addStatisticalError = thePlotPanel.datafile[0].getFilePar().addStatisticalError;

      BufferedWriter output = Misc.getWriter(folder, filename);
      try {
        int nPoints = thePlotPanel.datafile[0].computeDataNumber();
        output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
        output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
        output.write("_riet_meas_datafile_calibrated true");
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write(DiffrDataFile.CIFXcoord2T);
        output.newLine();
        output.write(DiffrDataFile.intensityCalcCIFstring);
        output.newLine();
        for (int i = thePlotPanel.datafile[0].startingindex; i < thePlotPanel.datafile[0].finalindex; i++) {
          double intens = thePlotPanel.datafile[0].getFit(i);
	        if (addStatisticalError)
		        intens += MoreMath.getGaussianNoise(intens); //getIntensityRandomError(intens);
//	        if (intens < 0)
//		        intens = 0.0;
          double xcoorddata = 0.0;
//          if (datafile[0].originalNotCalibrated)
//            xcoorddata = datafile[0].getXDataOriginal(i);
//          else
          xcoorddata = thePlotPanel.datafile[0].getXData(i);
          output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
          output.newLine();
        }
      } catch (IOException io) {
      }
      try {
        output.close();
      } catch (IOException io) {
      }
    }
  }

  public void exportExperimentalComputedData() {

    if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null)
      return;

    String filename = Utility.openFileDialog(this, "Save as CIF...",
            FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];
    int numberphases = thePlotPanel.datafile[0].getFilePar().getActiveSample().phasesNumber();

    if (Constants.sandboxEnabled && !filename.endsWith(".cif"))
      filename = filename + ".cif";

    if (filename != null) {

      BufferedWriter output = Misc.getWriter(folder, filename);

      try {
	      for (int datj = 0; datj < thePlotPanel.datafile.length; datj++) {
		      output.write("data_" + thePlotPanel.datafile[datj].getLabel());
		      output.newLine();
		      output.newLine();
		      int nPoints = thePlotPanel.datafile[datj].computeDataNumber();
		      output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
		      output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
		      output.write("_riet_meas_datafile_calibrated true");
		      output.newLine();
		      output.newLine();
		      output.write("# On the following loop you will have:");
		      output.newLine();
		      output.write("#  2theta/d coordinate   experimental intensity  calculated intensity  background");
		      for (int j = 0; j < numberphases; j++)
			      output.write("  intensity " + thePlotPanel.datafile[datj].getFilePar().getActiveSample().getPhase(j));
		      output.newLine();
		      output.newLine();
		      output.write("loop_");
		      output.newLine();
		      output.write(DiffrDataFile.CIFXcoord2T);
		      output.newLine();
		      output.write("_pd_proc_intensity_total");
		      output.newLine();
		      output.write(DiffrDataFile.intensityCalcCIFstring);
		      output.newLine();
		      output.write("_pd_proc_intensity_bkg_calc");
		      output.newLine();
		      output.write("_pd_proc_intensity_weight");
		      output.newLine();
		      for (int j = 0; j < numberphases; j++) {
			      output.write(DiffrDataFile.intensityCalcCIFstring);
			      output.newLine();
		      }
		      for (int i = thePlotPanel.datafile[datj].startingindex; i < thePlotPanel.datafile[datj].finalindex; i++) {
			      double intensE = thePlotPanel.datafile[datj].getYData(i);
			      double intens = thePlotPanel.datafile[datj].getFit(i);
			      double xcoorddata = 0.0;
//          if (datafile[0].originalNotCalibrated)
//            xcoorddata = datafile[0].getXDataOriginal(i);
//          else
			      xcoorddata = thePlotPanel.datafile[datj].getXData(i);
			      output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intensE) + " " + Fmt.format(intens));
			      output.write(" " + Fmt.format(thePlotPanel.datafile[datj].getBkgFit(i)));
			      output.write(" " + Fmt.format(thePlotPanel.datafile[datj].getWeight(i)));
			      for (int j = 0; j < numberphases; j++)
				      output.write(" " + Fmt.format(thePlotPanel.datafile[datj].getPhaseFit(i, j)));
			      output.newLine();
		      }
		      output.newLine();
		      output.write("#end of datafile: " + thePlotPanel.datafile[datj].getLabel());
		      output.newLine();
		      output.newLine();
	      }
      } catch (IOException io) {
      }
      try {
        output.close();
      } catch (IOException io) {
      }
    }
  }

  public void editInterpolatedBackgroundPoints() {
    thePlotPanel.editInterpolatedBackgroundPoints();
  }

  public void addPeak(double dToRemove) {
/*    int numberOfPeaks = peaksList[0].length;
    double[][] newPeaksList = new double[2][numberOfPeaks + 1];
    if (numberOfPeaks <= 1) {
      newPeaksList[0][numberOfPeaks] = dToRemove;
      newPeaksList[1][numberOfPeaks] = 1.0;
    } else {
      int indexToAdd = 0;
      int sign = (int) ((peaksList[0][numberOfPeaks - 1] - peaksList[0][0]) /
              Math.abs(peaksList[0][numberOfPeaks - 1] - peaksList[0][0]));
      for (int i = 0; i < numberOfPeaks; i++) {
        if (sign * peaksList[0][i] < dToRemove) {
          indexToAdd = i + 1;
        } else
          break;
      }
      int j = 0;
      for (int i = 0; i < numberOfPeaks + 1; i++) {
        if (i != indexToAdd) {
          newPeaksList[0][i] = peaksList[0][j];
          newPeaksList[1][i] = peaksList[1][j++];
        } else {
          newPeaksList[0][i] = dToRemove;
          newPeaksList[1][i] = 1.0;
        }
      }
    }
    peaksList = newPeaksList;
    updatePlotForPeaks();*/
  }

  public void removePeak(double dToRemove) {
/*    int numberOfPeaks = peaksList[0].length;
    double[][] newPeaksList = new double[2][numberOfPeaks - 1];
    int indexToRemove = 0;
    double dist = Math.abs(peaksList[0][0] - dToRemove);
    for (int i = 1; i < numberOfPeaks; i++) {
      double dist1 = Math.abs(peaksList[0][i] - dToRemove);
      if (dist1 < dist) {
        dist = dist1;
        indexToRemove = i;
      }
    }
    int j = 0;
    for (int i = 0; i < numberOfPeaks; i++) {
      if (i != indexToRemove) {
        newPeaksList[0][j] = peaksList[0][i];
        newPeaksList[1][j++] = peaksList[1][i];
      }
    }
    peaksList = newPeaksList;
    updatePlotForPeaks();*/
  }

  public void showNewFrame() {
    setVisible(false);
    getContentPane().removeAll();

    createDefaultMenuBar();
    thePlotPanel = new SpectrumPlotPanel(thePlotPanel.datafile, thePlotPanel.peaksList,
        thePlotPanel.secondDerivative);
    if (thePlotPanel != null)
      getContentPane().add(thePlotPanel, BorderLayout.CENTER);
	  setComponentToPrint(thePlotPanel.getComponentToPrint());

    getContentPane().invalidate();
    getContentPane().validate();
    setVisible(true);
  }

  public void showNewFrame(double[][] peaks, double[] derivative2) {
    setVisible(false);
    getContentPane().removeAll();
    createDefaultMenuBar();
    thePlotPanel = new SpectrumPlotPanel(thePlotPanel.datafile, peaks, derivative2);
    if (thePlotPanel != null)
      getContentPane().add(thePlotPanel, BorderLayout.CENTER);
	  setComponentToPrint(thePlotPanel.getComponentToPrint());

    getContentPane().invalidate();
    getContentPane().validate();
    setVisible(true);

  }

  public void updatePlot() {

    int i, j;

    G2Dint lgraph = thePlotPanel.graph;

    double[] trange = lgraph.getRanges();
//    double[] prange = positions.getRanges();

    thePlotPanel.np = thePlotPanel.datafile[0].finalindex - thePlotPanel.datafile[0].startingindex;

    double data[] = new double[2 * thePlotPanel.np];

    FilePar filepar = thePlotPanel.datafile[0].getFilePar();
    boolean markExcludedRegion = MaudPreferences.getBoolean("excludedRegion.setZeroForPlot", true);

    int numberphases = filepar.getActiveSample().phasesNumber();
    Phase[] phaselist = new Phase[numberphases];
    for (i = 0; i < numberphases; i++)
      phaselist[i] = filepar.getActiveSample().getPhase(i);

    if (thePlotPanel.datafile[0].hasfit()) {
      try {

        int mode = checkScaleModeX();
        for (i = j = 0; i < thePlotPanel.np; i++, j += 2) {
          data[j] = thePlotPanel.datafile[0].getXDataForPlot(i +
              thePlotPanel.datafile[0].startingindex, mode);
          if (datafile[0].xInsideRange(thePlotPanel.datafile[0].getXData(i +
              thePlotPanel.datafile[0].startingindex)) || !markExcludedRegion)
            data[j + 1] = thePlotPanel.datafile[0].getFitSqrtData(i +
              thePlotPanel.datafile[0].startingindex);
          else
            data[j + 1] = Double.NaN;
        }
        thePlotPanel.dataFit.deleteData();
        thePlotPanel.dataFit.append(data, thePlotPanel.np);

        for (i = j = 0; i < thePlotPanel.np; i++, j += 2) {
          if (datafile[0].xInsideRange(thePlotPanel.datafile[0].getXData(i + thePlotPanel.datafile[0].startingindex))
              || !markExcludedRegion)
            data[j + 1] = thePlotPanel.datafile[0].getFitSqrtData(i + thePlotPanel.datafile[0].startingindex) -
                  thePlotPanel.datafile[0].getYSqrtData(i + thePlotPanel.datafile[0].startingindex);
          else
            data[j + 1] = Double.NaN;
        }
        thePlotPanel.datar.deleteData();
        thePlotPanel.datar.append(data, thePlotPanel.np);

        DataFileSet adataset = thePlotPanel.datafile[0].getDataFileSet();
        int numberofPeaks = adataset.getNumberofPeaks();
        Vector<Peak> peaklist = adataset.getPeakList();

        int numberradiation = adataset.getInstrument().getRadiationType().getLinesCountForPlot();

        if (numberradiation == 0)
          numberradiation = 1;
        int numberofRefl = numberofPeaks / numberradiation;
        double[] datapeak = new double[2 * numberofRefl];

        if (numberofRefl > 0) {
          for (int ijn = 0; ijn < numberradiation; ijn++) {
            double wave = adataset.getInstrument().getRadiationType().getRadiationWavelength(ijn);
            for (i = j = 0; i < numberofPeaks; i++, j += 2) {
              Phase tmpphase = peaklist.elementAt(i).getPhase();
              int phaseindex = 0;
              for (int ij = 0; ij < numberphases; ij++)
                if (tmpphase == phaselist[ij])
                  phaseindex = ij;
	            // todo modify for more peaks par pattern
	            double pos = adataset.getActiveDataFile(0).getPositions(tmpphase)[ijn][peaklist.elementAt(i).getOrderPosition()][0];
	            datapeak[j] = thePlotPanel.datafile[0].convertXDataForPlot(pos, wave, mode);

              datapeak[j + 1] = (double) (phaseindex + 1);
            }
            thePlotPanel.datap[ijn].deleteData();
            thePlotPanel.datap[ijn].linestyle = 0;
            thePlotPanel.datap[ijn].marker = 9;
            double mscale = 2.0 - ijn;
            if (mscale <= 0.01)
              mscale = 0.5;
            thePlotPanel.datap[ijn].markerscale = mscale;
            thePlotPanel.datap[ijn].append(datapeak, numberofRefl);
          }
          datapeak = new double[2];
          for (int ijn = numberradiation; ijn < numberphases; ijn++) {
            datapeak[0] = 0.0;
            datapeak[1] = 1.0;
            thePlotPanel.datap[ijn].linestyle = 0;
            thePlotPanel.datap[ijn].marker = 9;
            thePlotPanel.datap[ijn].markerscale = 0.1;
            thePlotPanel.datap[ijn].markercolor = getPastelColor(ijn);
            thePlotPanel.datap[ijn].append(datapeak, 1);
          }
        }
      } catch (Exception e) {
      }

      if (thePlotPanel.yaxisp != null) {
        thePlotPanel.yaxisp.minimum = 0.5;
        thePlotPanel.yaxisp.maximum = ((double) numberphases) + 0.5;
      }
    }

//    positions.updateDataAndPaint(prange);
    lgraph.updateDataAndPaint(trange);

//    residuals.updateDataAndPaint();

  }

  public void updatePlotForPeaks() {

    if (thePlotPanel.peaksList == null)
      return;

    double[] trange = thePlotPanel.positions.getRanges();

    try {

      int mode = checkScaleModeX();
      thePlotPanel.datap[0].deleteData();

      int numberofRefl = thePlotPanel.peaksList[0].length;
      double[] datapeak = new double[2 * numberofRefl];

      DataFileSet adataset = thePlotPanel.datafile[0].getDataFileSet();
      double wave = adataset.getInstrument().getRadiationType().getMeanRadiationWavelength();
      for (int i = 0; i < numberofRefl; i++) {
        datapeak[2 * i] = thePlotPanel.datafile[0].convertXDataForPlot(thePlotPanel.peaksList[0][i], wave, mode);
//          System.out.println(datapeak[2*i]);
        datapeak[2 * i + 1] = 1.0;
      }
      thePlotPanel.datap[0].append(datapeak, numberofRefl);

    } catch (Exception e) {
    }

    thePlotPanel.positions.updateDataAndPaint(trange);

  }

  public void updatePlotForTools() {

    int i, j;

//    System.out.println("Updating: " + graph);
    G2Dint lgraph = (G2Dint) thePlotPanel.graph;
    boolean markExcludedRegion = MaudPreferences.getBoolean("excludedRegion.setZeroForPlot", true);

    double[] trange = lgraph.getRanges();
    thePlotPanel.np = thePlotPanel.datafile[0].finalindex -
        thePlotPanel.datafile[0].startingindex;
    double data[] = new double[2 * thePlotPanel.np];
//    System.out.println("numberOfPoints " + np);
//    FilePar filepar = (FilePar) datafile[0].getFilePar();

    try {
      int mode = checkScaleModeX();
      for (i = j = 0; i < thePlotPanel.np; i++, j += 2) {
//        System.out.println("j+1 " + (j + 1));
        data[j] = (double) thePlotPanel.datafile[0].getXDataForPlot(i +
            thePlotPanel.datafile[0].startingindex, mode);
        if (datafile[0].xInsideRange(thePlotPanel.datafile[0].getXData(i +
            thePlotPanel.datafile[0].startingindex)) || !markExcludedRegion)
          data[j + 1] = thePlotPanel.datafile[0].getFitSqrtData(i +
            thePlotPanel.datafile[0].startingindex);
        else
          data[j + 1] = Double.NaN;
      }
      if (thePlotPanel.dataFit == null) {
        thePlotPanel.dataFit = lgraph.loadDataSet(data, thePlotPanel.np);
        thePlotPanel.xaxis.attachDataSet(thePlotPanel.dataFit);
        thePlotPanel.yaxis.attachDataSet(thePlotPanel.dataFit);
      } else {
        thePlotPanel.dataFit.deleteData();
        thePlotPanel.dataFit.append(data, thePlotPanel.np);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    lgraph.updateDataAndPaint(trange);
    lgraph.resetRangeFull();
  }

  public static Color[] PastelColor = {Color.red, new Color(0, 102, 0), Color.blue,
                                       Color.magenta, Color.green, Color.orange,
                                       Color.cyan, Color.pink, Color.black,
                                       new Color(102, 51, 0), new Color(153, 0, 153), Color.yellow,
                                       new Color(102, 102, 0), new Color(0, 102, 102)
  };
  public static int maxNumber = 14;

  public static Color getPastelColor(int index) {
    while (index >= maxNumber)
      index -= maxNumber;
    return PastelColor[index];
  }

  public void dispose() {
    if (livedialog != null) {
      livedialog.setVisible(false);
      livedialog.dispose();
    }
    livedialog = null;
    super.dispose();
  }

  public void showLiveDialog() {
    if (livedialog == null)
      livedialog = new JLiveOptionsD(this, thePlotPanel.datafile[0].getFilePar());
    livedialog.setVisible(true);
  }

  class JLiveOptionsD extends myJFrame {

    JButton decreaseB = null;
    JButton increaseB = null;
    JTextField valueTF;
    JTextField stepTF;
    JTree parameterTree;
    Parameter selectedPar = null;
    FilePar filepar = null;

    public JLiveOptionsD(Frame parent, FilePar filepar) {

      super(parent);

      JLiveOptionsD.this.initializeSizeAndPosition(
              true, "liveFrame.frameWidth", "liveFrame.frameHeight", 550, 350,
              true, "liveFrame.framePositionX", "liveFrame.framePositionY", 100, 100);

      this.filepar = filepar;

      Container principalPanel = JLiveOptionsD.this.getContentPane();

      principalPanel.setLayout(new FlowLayout());

      JPanel jPanel3 = new JPanel();
      jPanel3.setLayout(new BorderLayout(6, 6));
      principalPanel.add(jPanel3);
      JPanel jPanel4 = new JPanel();
      jPanel4.setLayout(new BorderLayout(6, 6));
      principalPanel.add(jPanel4);
      jPanel3.add(BorderLayout.NORTH, new JLabel("Parameter:"));
      JPanel jPanel2 = new JPanel();
      jPanel2.setLayout(new DownFlowLayout(DownFlowLayout.CENTER, 6, 2));
      jPanel4.add(BorderLayout.CENTER, jPanel2);
      jPanel2.add(new JLabel("Value: "));

      decreaseB = new JIconButton("Up.gif");
      jPanel2.add(decreaseB);
      decreaseB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          increaseValue();
        }
      });

      valueTF = new JTextField(Constants.FLOAT_FIELD);
      valueTF.setText("0");
      valueTF.addKeyListener(new java.awt.event.KeyListener() {
        public void keyPressed(java.awt.event.KeyEvent event) {
        }

        public void keyReleased(java.awt.event.KeyEvent event) {
          if (event.getKeyCode() == event.VK_ENTER)
            update();
        }

        public void keyTyped(java.awt.event.KeyEvent event) {
        }
      });
      valueTF.addFocusListener(new FocusListener() {
        public void focusGained(FocusEvent fe) {
          JLiveOptionsD.this.getRootPane().setDefaultButton(null);
        }

        public void focusLost(FocusEvent fe) {
        }
      });
      jPanel2.add(valueTF);

      increaseB = new JIconButton("Down.gif");
      jPanel2.add(increaseB);
      increaseB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          decreaseValue();
        }
      });

      jPanel2.add(new JLabel("    Increment: "));
      stepTF = new JTextField(Constants.FLOAT_FIELD);
      stepTF.setText("0");
      jPanel2.add(stepTF);

      DefaultMutableTreeNode top = new DefaultMutableTreeNode(filepar);
      basicObj tmpPar[] = filepar.getChildren(null);
      if (tmpPar != null) {
        for (int i = 0; i < tmpPar.length; i++) {
          DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(tmpPar[i]);
          addChildren(top, newnode, tmpPar[i]);
        }
      }

      parameterTree = new JTree(top);
//		parameterTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
      parameterTree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
        public void valueChanged(javax.swing.event.TreeSelectionEvent event) {
          retrieveParameter();
        }
      });

      JScrollPane scrollpane = new JScrollPane(parameterTree);
//		scrollpane.setBorder(new LineBorder(Color.black));
      scrollpane.setPreferredSize(new Dimension(350, 200));
      jPanel3.add(BorderLayout.CENTER, scrollpane);

      JButton jb = new JCloseButton();
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          JLiveOptionsD.this.setVisible(false);
          JLiveOptionsD.this.dispose();
        }
      });
      JPanel jptemp = new JPanel();
      jptemp.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      jptemp.add(jb);
      jPanel4.add(BorderLayout.SOUTH, jptemp);
      JLiveOptionsD.this.getRootPane().setDefaultButton(jb);
      expandAllRows();

      JLiveOptionsD.this.setTitle("Live parameters");
      JLiveOptionsD.this.pack();

    }

    public void addChildren(DefaultMutableTreeNode parent, DefaultMutableTreeNode children, basicObj child) {
      parent.add(children);
      basicObj tmpPar[] = child.getChildren(null);
      if (tmpPar != null) {
        for (int i = 0; i < tmpPar.length; i++) {
          DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(tmpPar[i]);
          addChildren(children, newnode, tmpPar[i]);
        }
      }
    }

    public void expandAllRows() {
      int rownumbers = 0;
      int i = 0;
      rownumbers = parameterTree.getRowCount();
      while (i <= rownumbers) {
        parameterTree.expandRow(i++);
        rownumbers = parameterTree.getRowCount();
      }
    }

    public void increaseValue() {
//			retrieveParameter();
      setValue(getValue() + getStep());
      update();
    }

    public void decreaseValue() {
//			retrieveParameter();
      setValue(getValue() - getStep());
      update();
    }

    public void retrieveParameter() {

      Object par = parameterTree.getLastSelectedPathComponent();
      if (par != null && par instanceof DefaultMutableTreeNode) {
        par = ((DefaultMutableTreeNode) par).getUserObject();
        if (par != null && par instanceof Parameter) {
          if (par != selectedPar) {
            selectedPar = (Parameter) par;
            double value = selectedPar.getValueD();
            double step = 1.0E-3;
            if (value != 0.0)
              step = Math.abs(value / 10);
            setValue(value);
            setStep(step);
          }
        }
      }
    }

    public void setValue(double value) {
      valueTF.setText(Fmt.format(value));
    }

    public void setStep(double value) {
      stepTF.setText(Fmt.format(value));
    }

    public double getStep() {
      return Double.valueOf(stepTF.getText()).doubleValue();
    }

    public double getValue() {
      return Double.valueOf(valueTF.getText()).doubleValue();
    }

    public void update() {
      if (selectedPar != null) {
        JLiveOptionsD.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
        selectedPar.setValue(valueTF.getText());
        filepar.prepareComputation();
        filepar.mainfunction(false, false);
        updatePlot();
        JLiveOptionsD.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
      }
    }

  }

  public void showPeakSmoothing(DiffrDataFile adatafile) {
    JLivePeaksSearchD livedialog = new JLivePeaksSearchD(this, adatafile);
    livedialog.setVisible(true);
  }

  public void showLiveSmoothing(double[] fft, DiffrDataFile adatafile) {
    JLiveSmoothingD livedialog = new JLiveSmoothingD(this, fft, adatafile);
    livedialog.setVisible(true);
  }

  class JLiveSmoothingD extends myJFrame {

    JButton decreaseB = null;
    JButton increaseB = null;
    JTextField valueTF;
    JTextField stepTF;
    JButton decreaseFB = null;
    JButton increaseFB = null;
    JTextField valueFTF;
    JTextField stepFTF;
    DiffrDataFile adatafile;
    double[] fft = null;
    double[] newfft = null;
    int dtanumber = 0;

    public JLiveSmoothingD(Frame parent, double[] afft, DiffrDataFile tdatafile) {

      super(parent);

      fft = afft;
      adatafile = tdatafile;
      dtanumber = adatafile.computeDataNumber();

      JLiveSmoothingD.this.initializeSizeAndPosition(
              false, "liveFrame.frameWidth", "liveFrame.frameHeight", 450, 250,
              true, "liveFrame.framePositionX", "liveFrame.framePositionY", 100, 100);

      Container principalPanel = JLiveSmoothingD.this.getContentPane();

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jpanel1 = new JPanel();
      jpanel1.setLayout(new FlowLayout());
      principalPanel.add(BorderLayout.CENTER, jpanel1);

      JPanel jPanel3 = new JPanel();
      jPanel3.setLayout(new BorderLayout(6, 6));
      jpanel1.add(jPanel3);

      jPanel3.add(BorderLayout.NORTH, new JLabel("Position of the cut:"));
      JPanel jPanel2 = new JPanel();
      jPanel2.setLayout(new DownFlowLayout(DownFlowLayout.CENTER, 6, 2));
      jPanel3.add(BorderLayout.CENTER, jPanel2);
      decreaseB = new JIconButton("Up.gif");
      jPanel2.add(decreaseB);
      decreaseB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          increaseValue();
        }
      });
      valueTF = new JTextField(Constants.FLOAT_FIELD);
      valueTF.setText(MaudPreferences.getPref("smoothing.fourierZeros", "0.6"));
      valueTF.addKeyListener(new java.awt.event.KeyListener() {
        public void keyPressed(java.awt.event.KeyEvent event) {
        }

        public void keyReleased(java.awt.event.KeyEvent event) {
          if (event.getKeyCode() == event.VK_ENTER)
            update();
        }

        public void keyTyped(java.awt.event.KeyEvent event) {
        }
      });
      valueTF.addFocusListener(new FocusListener() {
        public void focusGained(FocusEvent fe) {
          JLiveSmoothingD.this.getRootPane().setDefaultButton(null);
        }

        public void focusLost(FocusEvent fe) {
        }
      });
      jPanel2.add(valueTF);
      increaseB = new JIconButton("Down.gif");
      jPanel2.add(increaseB);
      increaseB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          decreaseValue();
        }
      });
      jPanel2.add(new JLabel("    Increment: "));
      stepTF = new JTextField(Constants.FLOAT_FIELD);
      stepTF.setText("0.05");
      jPanel2.add(stepTF);

      JPanel jPanel4 = new JPanel();
      jPanel4.setLayout(new BorderLayout(6, 6));
      jpanel1.add(jPanel4);

      jPanel4.add(BorderLayout.NORTH, new JLabel("Slope of the cut:"));
      jPanel2 = new JPanel();
      jPanel2.setLayout(new DownFlowLayout(DownFlowLayout.CENTER, 6, 2));
      jPanel4.add(BorderLayout.CENTER, jPanel2);
      decreaseFB = new JIconButton("Up.gif");
      jPanel2.add(decreaseFB);
      decreaseFB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          increaseValueF();
        }
      });
      valueFTF = new JTextField(Constants.FLOAT_FIELD);
      valueFTF.setText(MaudPreferences.getPref("smoothing.multiplyingFactor", "3.9"));
      valueFTF.addKeyListener(new java.awt.event.KeyListener() {
        public void keyPressed(java.awt.event.KeyEvent event) {
        }

        public void keyReleased(java.awt.event.KeyEvent event) {
          if (event.getKeyCode() == event.VK_ENTER)
            update();
        }

        public void keyTyped(java.awt.event.KeyEvent event) {
        }
      });
      valueFTF.addFocusListener(new FocusListener() {
        public void focusGained(FocusEvent fe) {
          JLiveSmoothingD.this.getRootPane().setDefaultButton(null);
        }

        public void focusLost(FocusEvent fe) {
        }
      });
      jPanel2.add(valueFTF);
      increaseFB = new JIconButton("Down.gif");
      jPanel2.add(increaseFB);
      increaseFB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          decreaseValueF();
        }
      });
      jPanel2.add(new JLabel("    Increment: "));
      stepFTF = new JTextField(Constants.FLOAT_FIELD);
      stepFTF.setText("0.1");
      jPanel2.add(stepFTF);

      JButton jb = new JIconButton("Calculator.gif", "Inverse trasform");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          update();
          inverseFourierSmoothing(newfft, adatafile);
          JLiveSmoothingD.this.setVisible(false);
          JLiveSmoothingD.this.dispose();
        }
      });
      JPanel jptemp = new JPanel();
      jptemp.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      jptemp.add(jb);
      principalPanel.add(BorderLayout.SOUTH, jptemp);

      JLiveSmoothingD.this.setTitle("Smoothing control live");
      JLiveSmoothingD.this.pack();
      update();
    }

    public void increaseValue() {
      setValue(getValue() + getStep());
      update();
    }

    public void decreaseValue() {
      double res = getValue() - getStep();
      if (res >= 0.0) {
        if (res <= 1.0)
          setValue(res);
        else
          setValue(1.0);
      } else
        setValue(0.0);
      update();
    }

    public void setValue(double value) {
      valueTF.setText(Fmt.format(value));
    }

    public void setStep(double value) {
      stepTF.setText(Fmt.format(value));
    }

    public double getStep() {
      return Double.valueOf(stepTF.getText()).doubleValue();
    }

    public double getValue() {
      return Double.valueOf(valueTF.getText()).doubleValue();
    }

    public void increaseValueF() {
      setValueF(getValueF() + getStepF());
      update();
    }

    public void decreaseValueF() {
      double res = getValueF() - getStepF();
      if (res >= 0.0)
        setValueF(res);
      else
        setValueF(0.0);
      update();
    }

    public void setValueF(double value) {
      valueFTF.setText(Fmt.format(value));
    }

    public void setStepF(double value) {
      stepFTF.setText(Fmt.format(value));
    }

    public double getStepF() {
      return Double.valueOf(stepFTF.getText()).doubleValue();
    }

    public double getValueF() {
      return Double.valueOf(valueFTF.getText()).doubleValue();
    }

    public void update() {
      JLiveSmoothingD.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
      MaudPreferences.setPref("smoothing.fourierZeros", valueTF.getText());
      MaudPreferences.setPref("smoothing.multiplyingFactor", valueFTF.getText());
      newfft = BasicIndexingUtilities.modifyFFT(fft);

      for (int i = 0; i < dtanumber; i++)
        adatafile.setPhasesFit(i + adatafile.startingindex, newfft[i]);

      updatePlotForTools();
      JLiveSmoothingD.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

  }

  class JLivePeaksSearchD extends myJFrame {

    JButton decreaseB = null;
    JButton increaseB = null;
    JTextField valueIntTF;
    JTextField valueTF;
    JTextField stepTF;
    JButton decreaseFB = null;
    JButton increaseFB = null;
    JTextField valueFTF;
    JTextField stepFTF;
    DiffrDataFile adatafile;

    public JLivePeaksSearchD(Frame parent, DiffrDataFile tdatafile) {

      super(parent);

      adatafile = tdatafile;

      JLivePeaksSearchD.this.initializeSizeAndPosition(
              false, "liveFrame.frameWidth", "liveFrame.frameHeight", 450, 250,
              true, "liveFrame.framePositionX", "liveFrame.framePositionY", 100, 100);

      Container principalPanel = JLivePeaksSearchD.this.getContentPane();

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jpanel1 = new JPanel();
      jpanel1.setLayout(new FlowLayout());
      principalPanel.add(BorderLayout.CENTER, jpanel1);

      JPanel jPanel3 = new JPanel();
      jPanel3.setLayout(new BorderLayout(6, 6));
      jpanel1.add(jPanel3);

      jPanel3.add(BorderLayout.NORTH, new JLabel("Noise factor:"));
      JPanel jPanel2 = new JPanel();
      jPanel2.setLayout(new DownFlowLayout(DownFlowLayout.CENTER, 6, 2));
      jPanel3.add(BorderLayout.CENTER, jPanel2);
      decreaseB = new JIconButton("Up.gif");
      jPanel2.add(decreaseB);
      decreaseB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          increaseValue();
        }
      });
      valueTF = new JTextField(Constants.FLOAT_FIELD);
      valueTF.setText(MaudPreferences.getPref("peaksLocation.noiseFactor", "10"));
      valueTF.addKeyListener(new java.awt.event.KeyListener() {
        public void keyPressed(java.awt.event.KeyEvent event) {
        }

        public void keyReleased(java.awt.event.KeyEvent event) {
          if (event.getKeyCode() == event.VK_ENTER)
            update();
        }

        public void keyTyped(java.awt.event.KeyEvent event) {
        }
      });
      valueTF.addFocusListener(new FocusListener() {
        public void focusGained(FocusEvent fe) {
          JLivePeaksSearchD.this.getRootPane().setDefaultButton(null);
        }

        public void focusLost(FocusEvent fe) {
        }
      });
      jPanel2.add(valueTF);
      increaseB = new JIconButton("Down.gif");
      jPanel2.add(increaseB);
      increaseB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          decreaseValue();
        }
      });
      jPanel2.add(new JLabel("    Increment: "));
      stepTF = new JTextField(Constants.FLOAT_FIELD);
      stepTF.setText("5");
      jPanel2.add(stepTF);

      JPanel jPanel4 = new JPanel();
      jPanel4.setLayout(new BorderLayout(6, 6));
      jpanel1.add(jPanel4);

      jPanel4.add(BorderLayout.NORTH, new JLabel("Minimum 2nd derivative:"));
      jPanel2 = new JPanel();
      jPanel2.setLayout(new DownFlowLayout(DownFlowLayout.CENTER, 6, 2));
      jPanel4.add(BorderLayout.CENTER, jPanel2);
      decreaseFB = new JIconButton("Up.gif");
      jPanel2.add(decreaseFB);
      decreaseFB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          increaseValueF();
        }
      });
      valueFTF = new JTextField(Constants.FLOAT_FIELD);
      valueFTF.setText(MaudPreferences.getPref("peaksLocation.minimum2derivative%", "1"));
      valueFTF.addKeyListener(new java.awt.event.KeyListener() {
        public void keyPressed(java.awt.event.KeyEvent event) {
        }

        public void keyReleased(java.awt.event.KeyEvent event) {
          if (event.getKeyCode() == event.VK_ENTER)
            update();
        }

        public void keyTyped(java.awt.event.KeyEvent event) {
        }
      });
      valueFTF.addFocusListener(new FocusListener() {
        public void focusGained(FocusEvent fe) {
          JLivePeaksSearchD.this.getRootPane().setDefaultButton(null);
        }

        public void focusLost(FocusEvent fe) {
        }
      });
      jPanel2.add(valueFTF);
      increaseFB = new JIconButton("Down.gif");
      jPanel2.add(increaseFB);
      increaseFB.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent event) {
          decreaseValueF();
        }
      });
      jPanel2.add(new JLabel("    Increment: "));
      stepFTF = new JTextField(Constants.FLOAT_FIELD);
      stepFTF.setText("0.1");
      jPanel2.add(stepFTF);

      JLivePeaksSearchD.this.setTitle("Peak location live");
      JLivePeaksSearchD.this.pack();
      update();
    }

    public void increaseValue() {
      setValue(getValue() + getStep());
      update();
    }

    public void decreaseValue() {
      double res = getValue() - getStep();
      if (res >= 0.0) {
        setValue(res);
      } else
        setValue(0.0);
      update();
    }

    public void setValue(double value) {
      valueTF.setText(Fmt.format(value));
    }

    public void setStep(double value) {
      stepTF.setText(Fmt.format(value));
    }

    public double getStep() {
      return Double.valueOf(stepTF.getText()).doubleValue();
    }

    public double getValue() {
      return Double.valueOf(valueTF.getText()).doubleValue();
    }

    public void increaseValueF() {
      setValueF(getValueF() + getStepF());
      update();
    }

    public void decreaseValueF() {
      double res = getValueF() - getStepF();
      if (res >= 0.0)
        setValueF(res);
      else
        setValueF(0.0);
      update();
    }

    public void setValueF(double value) {
      valueFTF.setText(Fmt.format(value));
    }

    public void setStepF(double value) {
      stepFTF.setText(Fmt.format(value));
    }

    public double getStepF() {
      return Double.valueOf(stepFTF.getText()).doubleValue();
    }

    public double getValueF() {
      return Double.valueOf(valueFTF.getText()).doubleValue();
    }

    public void update() {
      JLivePeaksSearchD.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
//      MaudPreferences.setPref("peaksLocation.interpolatedPoints", valueTF.getText());
      MaudPreferences.setPref("peaksLocation.minimum2derivative%", valueFTF.getText());
      MaudPreferences.setPref("peaksLocation.noiseFactor", valueTF.getText());
      peaksLocation();

      updatePlotForTools();
      updatePlotForPeaks();
      JLivePeaksSearchD.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

  }

  class g2DPmouse extends MouseAdapter {
    G2Dint positionBox = null;
    int x1 = 0;
    int y1 = 0;

    public void mousePressed(MouseEvent e) {
      if (e.isPopupTrigger() /*G2Dint.isRightMouseButton(e)*/ && e.getComponent() instanceof G2Dint) {

        x1 = e.getX();
        y1 = e.getY();

        JPopupMenu popup = new JPopupMenu("Options");
        popup.setLightWeightPopupEnabled(false);

        positionBox = (G2Dint) e.getComponent();

        JMenuItem mi = new JMenuItem("Add peak");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = positionBox.getPoint(x1, y1);
            PlotFitting.this.addPeak(d[0]);
          }
        });
        popup.add(mi);

        mi = new JMenuItem("Remove peak");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = positionBox.getClosestPoint(x1, y1);
            PlotFitting.this.removePeak(d[0]);
          }
        });
        popup.add(mi);

        popup.show(e.getComponent(), e.getX(), e.getY());
      }
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseClicked(MouseEvent e) {
    }

  }

}
