/*
 * @(#)Strain.java created 15/11/1999 Pergine Vals.
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.*;
import java.util.Vector;
import java.util.concurrent.TimeUnit;
import javax.swing.*;

import org.apache.commons.math3.linear.*;

/**
 * The Strain is a general class to obtain strain maps from spectra.
 * This class does nothing, and applied theories must be implemented in subclasses
 * of this class.
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/07/20 13:39:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Strain extends XRDcat {

  public Strain(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Strain(XRDcat aobj) {
    this(aobj, "Strain model x");
  }

  public Strain() {
  }

  public Phase getPhase() {
    return (Phase) getParent();
  }

/*  public void computeStrain(Sample asample) {
    Phase aphase = getPhase();
    computeStrain(aphase, asample);
  }*/

  public void computeStrain(Phase aphase, Sample asample) { // you don't need to modify this unless
    if (refreshComputation) {  // the computation is done only when needed
      refreshComputation = false;  // first we set it false, so this method will not be called again if not needed

      prepareComputation(aphase, asample); // if something is needed before the real computation

      aphase.sghklcompute(false);  // will refresh the peak list if necessary
	    for (int i = 0; i < asample.activeDatasetsNumber(); i++) { // the loop goes only for active datasets
		    DataFileSet adataset = asample.getActiveDataSet(i);

		    int datafilenumber = adataset.activedatafilesnumber();  // the number of spectra in the dataset
		    for (int i1 = 0; i1 < datafilenumber; i1++) { // now the loop over the number of spectra in the dataset
			    DiffrDataFile adatafile = adataset.getActiveDataFile(i1);
			    adatafile.computeStrain(aphase); // datafile will call computeStrain(refl, strain_angles), see below
		    }
	    }
    }

  }

	public double computeStrain(Reflection refl, double[] strain_angles) { // you don't need to modify this unless
			return computeStrain(refl.phi[0], refl.beta[0],
									strain_angles[0] * Constants.DEGTOPI,
									strain_angles[1] * Constants.DEGTOPI);
	}

  public void prepareComputation(Phase aphase, Sample asample) {
  }

  public double computeStrain(double psi, double beta, double chi, double phi) {
    // Angles must be in radiants
    // psi and beta are the polar and azimuthal angles for the crystal setting
    // phi and chi for the sample

    return 0.0;
  }

/*  public double computeStrain(Phase aphase, double strain_angles[],
                              int h, int k, int l) {
    Reflection refl = aphase.getReflectionByhkl(h, k, l);
    return computeStrain(refl, strain_angles);

  }

  public double[] computeStrain(Phase aphase, double alpha[], double beta[],
                                Reflection reflex) {

    int numberOfPoints = alpha.length;
    double[] strainValues = new double[numberOfPoints];
		double[] strain_angles = new double[2];
    for (int i = 0; i < numberOfPoints; i++) {
	    strain_angles[0] = alpha[i];
	    strain_angles[1] = beta[i];
	    strainValues[i] = computeStrain(reflex, strain_angles);
    }

    return strainValues;
  }*/

	public void saveStrainValues(Phase aphase, Sample asample) {
		FilePar aparFile = getFilePar();
		if (!aparFile.isStrainComputationPermitted() || !Constants.strainOutput)
			return;

		String filename = new String(getFilePar().getDirectory() +
				aphase.toXRDcatString() + ".spf");
		BufferedWriter PFwriter = Misc.getWriter(filename);
		if (PFwriter != null) {
			try {

				PFwriter.write("Strain values for phase: " + aphase.toXRDcatString());
				PFwriter.write(Constants.lineSeparator);
				PFwriter.write("For each reflection and measured point:");
				PFwriter.write(Constants.lineSeparator);
				PFwriter.write("chi(pole fig), phi(pole fig), strain, reflection weight, omega, chi, phi, eta, bank number");
				PFwriter.write(Constants.lineSeparator);

				int hkln = aphase.gethklNumber();
				int numberPoleFigures = 0;
				for (int j = 0; j < hkln; j++) {
					Reflection refl = aphase.getReflectionVector().elementAt(j);

					if (refl.isGoodforStrain())
						numberPoleFigures++;
				}
				PFwriter.write(numberPoleFigures + "             <- Reflections number");
				PFwriter.write(Constants.lineSeparator);
				for (int j = 0; j < hkln; j++) {
					Reflection refl = aphase.getReflectionVector().elementAt(j);
					if (refl.isGoodforStrain()) {
						PFwriter.write(refl.getH() + "   " + refl.getK() + "   " + refl.getL() + "            h k l");
						PFwriter.write(Constants.lineSeparator);

						int numberDatasets = asample.activeDatasetsNumber();
						int numberDataPoints = 0;
						for (int i = 0; i < numberDatasets; i++) {
							DataFileSet dataset = asample.getActiveDataSet(i);
							int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
							for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
								DiffrDataFile datafile = dataset.getActiveDataFile(k);
								for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
									for (int l = 0; l < radCount; l++) {
										double pf = datafile.getStrains(aphase, j)[ppp][l];
										if (!Double.isNaN(pf)) numberDataPoints++;
									}
								}
							}
						}
						PFwriter.write(numberDataPoints + " <- number measured points");
						PFwriter.write(Constants.lineSeparator);
						double wgt = Math.sqrt(refl.getWeight());

						numberDataPoints = 0;
						for (int i = 0; i < numberDatasets; i++) {
							DataFileSet dataset = asample.getActiveDataSet(i);
							int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
							for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
								DiffrDataFile datafile = dataset.getActiveDataFile(k);
								for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
									for (int l = 0; l < radCount; l++) {
										double pf = datafile.getStrains(aphase, j)[ppp][l];
										double position = datafile.getPositions(aphase)[j][ppp][l];
										if (!Double.isNaN(pf)) {
											numberDataPoints++;
											double[] angles = datafile.getTextureAngles(position);
											double[] mAngles = datafile.getTiltingAngle();
											int bankNumber = datafile.getBankNumber() + 1;
											double chi = angles[0];
											double phi = angles[1];
											PFwriter.write(chi + " " + phi + " " + pf + " " + wgt
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

	public void outputStressTensor(BufferedWriter writer) throws IOException {
	}

	public void outputStressTensor2x2(BufferedWriter writer) throws IOException {
	}

	public void computeStressTensorAndOutput(Phase aphase, Vector<Reflection> reflList, String filename) {

		BufferedWriter PFwriter = Misc.getWriterForAppend(filename + ".txt");
		Sample asample = getFilePar().getActiveSample();
		if (PFwriter != null) {
			try {

				System.out.println("Strain values for phase: " + aphase.toXRDcatString());
				PFwriter.write("Strain values for phase: " + aphase.toXRDcatString());
				PFwriter.write(Constants.lineSeparator);

				outputStressTensor(PFwriter);

				int hkln = reflList.size();
				for (int j = 0; j < hkln; j++) {
					Reflection refl = reflList.elementAt(j);
					String hkl = refl.getH() + "_" + refl.getK() + "_" + refl.getL();
					int reflIndex = aphase.getReflectionIndexByAnyhkl(refl.getH(), refl.getK(), refl.getL());
					Reflection arefl = aphase.getReflex(reflIndex);
//					System.out.println(refl.getH() + "   " + refl.getK() + "   " + refl.getL() + "   " + arefl);
					if (refl.isGoodforStrain() && arefl != null) {
						PFwriter.write(refl.getH() + "   " + refl.getK() + "   " + refl.getL() + "   " + arefl.d_space + "            h k l d0");
						PFwriter.write(Constants.lineSeparator);
						PFwriter.write("psi       phi        strain       position      d      sin2(psi)");
						PFwriter.write(Constants.lineSeparator);

						int numberDatasets = asample.activeDatasetsNumber();
/*						int numberDataPoints = 0;
						for (int i = 0; i < numberDatasets; i++) {
							DataFileSet dataset = asample.getActiveDataSet(i);
							int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
							for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
								DiffrDataFile datafile = dataset.getActiveDataFile(k);
								for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
									for (int l = 0; l < radCount; l++) {
										double pf = datafile.getStrains(aphase, j)[ppp][l];
										if (!Double.isNaN(pf)) numberDataPoints++;
									}
								}
							}
						}
						PFwriter.write(numberDataPoints + " <- number measured points");
						PFwriter.write(Constants.lineSeparator);
						double wgt = Math.sqrt(refl.getWeight());

						numberDataPoints = 0;*/
						for (int i = 0; i < numberDatasets; i++) {
							DataFileSet dataset = asample.getActiveDataSet(i);
							PFwriter.write("Dataset " + i + " " + dataset.toString());
							PFwriter.write(Constants.lineSeparator);
							int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
							Vector<double[]> data = new Vector(10);
							for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
								DiffrDataFile datafile = dataset.getActiveDataFile(k);
								for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
										double pf = datafile.getStrains(aphase, reflIndex)[ppp][0];
										double position = datafile.getPositions(aphase)[reflIndex][ppp][0];
										if (!Double.isNaN(pf)) {
//											numberDataPoints++;
											double[] angles = datafile.getTextureAngles(position);
											double chi = angles[0];
											double phi = angles[1];
											double sinpsi = MoreMath.sind(chi);
											sinpsi *= sinpsi;
											double ds = arefl.d_space * Math.exp(pf);
											PFwriter.write(chi + " " + phi + " " + pf + " " + position + " " + ds + " " + sinpsi);
											PFwriter.write(Constants.lineSeparator);
											double[] one = new double[2];
											one[0] = sinpsi;
											one[1] = ds;
											data.add(one);
										}
								}
							}
							int dataNumber = data.size();
							double[] x = new double[dataNumber];
							double[] y = new double[dataNumber];
							for (int k = 0; k < dataNumber; k++) {
								x[k] = data.elementAt(k)[0];
								y[k] = data.elementAt(k)[1];
							}
//							if (plotOutputFileName != null && plotOutputFileName.length() > 0)
								createPlotFileOutput(x, y, aphase.toXRDcatString() + "-" + dataset.toXRDcatString() + "-" + hkl, filename);

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

	public void createPlotFileOutput(double[] x, double[] function, String title, String plotOutputFileName) {

		PlotSimpleData plot = new PlotSimpleData(null, x, function, 1, true,
				"[Sin(psi)]^2", "d (Å)", title);
		plot.setSize(800, 600);
		plot.setVisible(true);
		(new PersistentThread() {
			@Override
			public void executeJob() {

				try {
					TimeUnit.MILLISECONDS.sleep(3000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				Component comp = plot.componentToPrint;
				if (comp != null) {
					Rectangle rect = comp.getBounds();
					Image fileImage =
							plot.createImage(rect.width, rect.height);
					Graphics g = fileImage.getGraphics();

					//write to the image
					g.clearRect(0, 0, comp.getWidth(), comp.getHeight());
					comp.paint(g);
					// write it out in the format you want
					BeartexPFPlot.savePic(fileImage, "png", plotOutputFileName + title + ".png", comp);

					try {
						TimeUnit.MILLISECONDS.sleep(3000);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}

					//dispose of the graphics content
					g.dispose();
					plot.setVisible(false);
					plot.dispose();
				}
			}
		}).start();
	}

	public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    return null;
  }

  public double[][] getExpPoleFigureGrid(Reflection reflex, int numberofPoints, double maxAngle) {

    return null;
  }

  public boolean needPositionExtractor() {
    return false;
  }

  public void notifyStringChanged(String source) {
    notifyStringChanged(source, Constants.STRAIN_CHANGED);
  }

  public void notifyObjectChanged(XRDcat source) {
    notifyUpObjectChanged(source, Constants.STRAIN_CHANGED);
  }

  public void refreshForNotificationUp(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || source == this || reason == Constants.STRAIN_CHANGED)
      refreshComputation = true;
  }

	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			if (parameterField != null)
				for (int i = 0; i < parameterField.length; i++) {
					if (parameterField[i] == source) {
						notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
						notifyParameterChanged(source, Constants.STRAIN_CHANGED);
						return;
					}
				}
			if (parameterloopField != null)
				for (int j = 0; j < parameterloopField.length; j++)
					for (int i = 0; i < parameterloopField[j].size(); i++)
						if (source == parameterloopField[j].elementAt(i)) {
							notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
							notifyParameterChanged(source, Constants.STRAIN_CHANGED);
							return;
						}

			super.notifyParameterChanged(source);
		}
	}

	public void refreshForNotificationDown(XRDcat source, int reason) {
		if (!getFilePar().isComputingDerivate() || (source == this ||
				(reason == Constants.SAMPLE_ORIENTATION_CHANGED || reason == Constants.TEXTURE_CHANGED
						|| (source == getParent() &&
						(reason == Constants.STRAIN_CHANGED || reason == Constants.CELL_CHANGED))))) {
			refreshComputation = true;
			//     System.out.println("Reason " + reason + " Source " + source.toXRDcatString());
		}
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JStrainOptionsD(parent, this);
  }

  public class JStrainOptionsD extends JOptionsDialog {

    public JStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Strain options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }
}
