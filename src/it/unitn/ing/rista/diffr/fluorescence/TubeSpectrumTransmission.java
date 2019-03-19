/*
 * @(#)TubeSpectrumTransmission.java created January 15, 2014 Caen-Carpiquet
 *
 * Copyright (c) 2014 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.fluorescence;

import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.detector.XRFDetector;
import it.unitn.ing.rista.diffr.radiation.XrayEbelTubeRadiation;
import it.unitn.ing.rista.util.*;
import java.util.*;

import static java.lang.System.out;

/**
 * The FluorescenceBase is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: January 15, 2014 6:16:03 AM $
 * @since JDK1.1
 */
public class TubeSpectrumTransmission extends FluorescenceBase {

	public static String modelID = "Tube Spectrum";
	public static String descriptionID = "Perform quantitative fitting of tube spectrum";

	public TubeSpectrumTransmission(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

	public TubeSpectrumTransmission(XRDcat aobj) {
		this(aobj, "Tube spectrum");
	}

	public TubeSpectrumTransmission() {
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

	public void computeFluorescence(Sample asample, DataFileSet adataset) {

		int datafilenumber = adataset.activedatafilesnumber();

		final Sample theSample = asample;
		final DataFileSet theDataset = adataset;

		final int maxThreads = Math.min(Constants.maxNumberOfThreads, datafilenumber);
		if (maxThreads > 1 && Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
			if (Constants.debugThreads)
				out.println("Thread datafileset " + getLabel());
			int i;
			PersistentThread[] threads = new PersistentThread[maxThreads];
			for (i = 0; i < maxThreads; i++) {
				threads[i] = new PersistentThread(i) {
					@Override
					public void executeJob() {
						int i1 = this.getJobNumberStart();
						int i2 = this.getJobNumberEnd();

						for (int j = i1; j < i2; j++) {
							DiffrDataFile datafile = theDataset.getActiveDataFile(j);
							computeFluorescence(theSample, datafile);
							computeasymmetry(theSample, datafile);
						}
					}
				};
			}
			i = 0;
			int istep = (int) (0.9999 + datafilenumber / maxThreads);
			for (int j = 0; j < maxThreads; j++) {
				int is = i;
				if (j < maxThreads - 1)
					i = Math.min(i + istep, datafilenumber);
				else
					i = datafilenumber;
				threads[j].setJobRange(is, i);
				threads[j].start();
			}
			boolean running;
			do {
				running = false;
				try {
					Thread.sleep(Constants.timeToWaitThreadsEnding);
				} catch (InterruptedException r) {
				}
				for (int h = 0; h < maxThreads; h++) {
					if (!threads[h].isEnded())
						running = true;
				}
			} while (running);

		} else
			for (int k = 0; k < datafilenumber; k++) {
				DiffrDataFile datafile = theDataset.getActiveDataFile(k);
				computeFluorescence(theSample, datafile);
				computeasymmetry(theSample, datafile);
			}

	}

	/**
	 * The method compute the fluorescence pattern by a simple
	 * transfer of the tube spactrum. Can be used to fit the tube spectrum
	 * in connection with the Ebel tube description in the source.
	 *
	 * @param adatafile
	 * @see it.unitn.ing.rista.diffr.DiffrDataFile#addtoFit
	 */

	public void computeFluorescence(Sample asample, DiffrDataFile adatafile) {

		XRayDataSqLite.checkMinimumEnergy();

		Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
		double incidentIntensity = ainstrument.getIntensityValue();

		double[] xEnergy = adatafile.getXrangeInEnergy();
		int numberOfPoints = xEnergy.length;

		double[] fluorescence = new double[numberOfPoints];

		RadiationType radType = ainstrument.getRadiationType();
		int rad_lines = radType.getLinesCount();
		double maxEnergyInKeV = xEnergy[numberOfPoints - 1] * 0.001 * 1.1;

		Vector<FluorescenceLine> fluorescenceLines = new Vector<FluorescenceLine>(rad_lines, 10);

		for (int ej = 0; ej < rad_lines; ej++) {
			double lambda = radType.getRadiationWavelengthForFluorescence(ej);
			double energy_intensity = radType.getRadiationWeightForFluorescence(ej);
//			System.out.println(ej + " " + lambda + " " + energy_intensity);
			double energy = Constants.ENERGY_LAMBDA / lambda;    // in eV
			double energyInKeV = energy * 0.001;
			FluorescenceLine transfertLine = new FluorescenceLine(energyInKeV, -1, 0, "source");
			transfertLine.setIntensity(transfertLine.getIntensity() * energy_intensity);
			fluorescenceLines.add(transfertLine);

			if (((XRFDetector) ainstrument.getDetector()).getFiltersFluorescenceIntensityTotal() > 0) {

				Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV);
				for (int si = 0; si < filtersFluorescenceLines.size(); si++) {
					FluorescenceLine line = filtersFluorescenceLines.get(si);
					boolean addLine = true;
					for (int i = 0; i < fluorescenceLines.size(); i++) {
						FluorescenceLine lineExisting = fluorescenceLines.get(i);
						lineExisting.setIntensity(lineExisting.getIntensity());
						if (lineExisting.getEnergy() == line.getEnergy()) {
							addLine = false;
							lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity);
						}
					}
					if (addLine) {
						line.setIntensity(line.getIntensity() * energy_intensity);
						fluorescenceLines.add(line);
					}
				}
			}
			if (ainstrument.getRadiationType() instanceof XrayEbelTubeRadiation) {
				XrayEbelTubeRadiation source = (XrayEbelTubeRadiation) ainstrument.getRadiationType();
				if (source.getFiltersFluorescenceIntensityTotal() > 0) {

					Vector<FluorescenceLine> filtersFluorescenceLines = source.getFluorescenceLines(energyInKeV);
					for (int si = 0; si < filtersFluorescenceLines.size(); si++) {
						FluorescenceLine line = filtersFluorescenceLines.get(si);
						boolean addLine = true;
						for (int i = 0; i < fluorescenceLines.size(); i++) {
							FluorescenceLine lineExisting = fluorescenceLines.get(i);
							lineExisting.setIntensity(lineExisting.getIntensity());
							if (lineExisting.getEnergy() == line.getEnergy()) {
								addLine = false;
								lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity);
							}
						}
						if (addLine) {
							line.setIntensity(line.getIntensity() * energy_intensity);
							fluorescenceLines.add(line);
						}
					}
				}
			}

		}

		Vector<FluorescenceLine> sumLines = null;
		if (((XRFDetector) ainstrument.getDetector()).getSumPeaksIntensity() > 0)
			sumLines = ((XRFDetector) ainstrument.getDetector()).getPileUpPeaks(maxEnergyInKeV, fluorescenceLines);
		Vector<FluorescenceLine> escapeLines = null;
		if (((XRFDetector) ainstrument.getDetector()).getEscapePeaksIntensity() > 0)
			escapeLines = ((XRFDetector) ainstrument.getDetector()).getEscapePeaks(maxEnergyInKeV, fluorescenceLines);

		if (sumLines != null) {
			for (int si = 0; si < sumLines.size(); si++) {
				FluorescenceLine line = sumLines.get(si);
				boolean addLine = true;
				for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
					FluorescenceLine lineExisting = fluorescenceLines.get(i);
					if (lineExisting.getEnergy() == line.getEnergy()) {
						addLine = false;
						lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity());
					}
				}
				if (addLine) {
					line.setIntensity(line.getIntensity());
					fluorescenceLines.add(line);
				}
			}
		}

		if (escapeLines != null) {
			for (int si = 0; si < escapeLines.size(); si++) {
				FluorescenceLine line = escapeLines.get(si);
				boolean addLine = true;
				for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
					FluorescenceLine lineExisting = fluorescenceLines.get(i);
					if (lineExisting.getEnergy() == line.getEnergy()) {
						addLine = false;
						lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity());
					}
				}
				if (addLine) {
					line.setIntensity(line.getIntensity());
					fluorescenceLines.add(line);
				}
			}
		}

		for (int k = 0; k < fluorescenceLines.size(); k++) {
			FluorescenceLine line = fluorescenceLines.get(k);
			double[][] broad = ainstrument.getInstrumentalBroadeningAt(line.getEnergy(), adatafile);
			line.setEnergy(line.getEnergy() * 1000.0); // in eV
			line.setShape(broad);
//        System.out.print(/*line.getEnergy() + " " + */line.getIntensity() + " ");
			for (int i = 0; i < numberOfPoints; i++)
				fluorescence[i] += line.getIntensity(xEnergy[i]);
		}

		for (int i = 0; i < numberOfPoints; i++) {
			fluorescence[i] *= incidentIntensity;
			adatafile.addtoFit(i, fluorescence[i]);
//        System.out.println("Point: " + xEnergy[i] + ", intensity: " + fluorescence[i]);
		}
	}

}
