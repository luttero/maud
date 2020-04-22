/*
 * @(#)DiffractionBase.java created 29/8/2018 Povo
 *
 * Copyright (c) 2018 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.diffraction;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.PersistentThread;

import java.io.*;
import java.lang.*;
import java.util.*;

import static java.lang.System.*;

/**
 *  The DiffractionBase is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2018/08/29 11:20:12 $
 * @author Luca Lutterotti
 * @since JDK1.8
 */

public class DiffractionBase extends Diffraction {

	public DiffractionBase(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "Basic diffraction";
		IDlabel = identifier;
		description = identifier;
	}

	public DiffractionBase(XRDcat aobj) {
		this(aobj, "Basic diffraction");
	}

	public DiffractionBase() {
		identifier = "Basic diffraction";
		IDlabel = identifier;
		description = identifier;
	}

	public void computeDiffraction(Sample asample) {

		final Sample theSample = asample;
		final DataFileSet theDataset = getDataFileSet();
    
    int datafilenumber = theDataset.activedatafilesnumber();
    
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
							computeDiffraction(theSample, datafile);
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
				computeDiffraction(theSample, datafile);
				computeasymmetry(theSample, datafile);
			}
	}

	public void computeDiffraction(Sample asample, DiffrDataFile datafile) {
//		DataFileSet datafileset = datafile.getDataFileSet();
		if (getFilePar().isComputingDerivate()) {
//      System.out.println("refreshing derivative: " + this.toXRDcatString());
			for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
				double expfit[] = new double[datafile.getTotalNumberOfData()];
				int minmaxindex[] = computeReflectionIntensity(asample, datafile.getDataFileSet().getPeakList(), true,
						expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
						Constants.COMPUTED, Constants.COMPUTED, false,
						getFilePar().getActiveSample().getPhase(ij), datafile);
				for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
					datafile.addtoPhasesFit(j, expfit[j]);
			}
		} else {
//      System.out.println("refreshing: " + this.toXRDcatString());
			for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
//        System.out.println("Phase: " + getFilePar().getActiveSample().getPhase(ij).toXRDcatString());
				double expfit[] = new double[datafile.getTotalNumberOfData()];
				int minmaxindex[] = computeReflectionIntensity(asample, datafile.getDataFileSet().getPeakList(), true,
						expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
						Constants.COMPUTED, Constants.COMPUTED, false,
						getFilePar().getActiveSample().getPhase(ij), datafile);
//        System.out.println("indices: " + minmaxindex[0] + " " + minmaxindex[1] + " " + expfit[1000]);
        datafile.addtoPhasesFit(expfit, minmaxindex, ij);
			}
		}
	}

	public int[] computeReflectionIntensity(Sample asample, Vector<Peak> peaklist, boolean computeBroadening,
	                                        double[] expfit, double rangefactor, int computeTexture,
	                                        int computeStrain, int computeFhkl, boolean leBailExtraction,
	                                        Phase phase, DiffrDataFile datafile) {

		Instrument ainstrument = datafile.getDataFileSet().getInstrument();
		FilePar filepar = getFilePar();
		OutputStream out = null;
		boolean logOutput = false;
		PrintStream printStream = null;
		ByteArrayOutputStream baos = null;
		if (filepar.logOutput() && filepar.fullResults() && !leBailExtraction) {
			out = getFilePar().getResultStream();
			logOutput = true;

			try {
				baos = new ByteArrayOutputStream();
				printStream = new PrintStream(baos);
				printStream.println("             Diffraction spectrum : " + datafile.toXRDcatString());
				printStream.println("Peaks list : ");
				printStream.print(" peak n,"
						+ " rad. n,"
						+ " phase, "
						+ " h,  "
						+ " k,  "
						+ " l,  "
						+ "  d_space,   "
						+ "  Fhkl_calc,"
						+ "  Fhkl_exp, "
						+ " position,  "
						+ " strain,    "
						+ " planar def,"
						+ " intensity, "
						+ " hwhm,      "
						+ " gaussian,  "
						+ " |Fhkl|^2*m,"
						+ " incident I,"
						+ " LP,        "
						+ " texture,   "
						+ " Abs*Vol/Vc,"
						+ " rad. wt,   "
						+ " phase scale"
						+ " detector absorption"
				);
				printStream.print(Constants.lineSeparator);
				printStream.flush();
//						System.out.println("String length " + toPrint.length());
			} catch (Exception io) {
				io.printStackTrace();
			}
		}

//    Instrument ainstrument = getDataFileSet().getInstrument();
		double cutoff = datafile.getDataFileSet().getPeakCutoffD() * rangefactor;
		if (!datafile.increasingX()) {
			cutoff = -cutoff;
		}
		int[] tmpminmax = new int[2];
		int[] minmaxindex = new int[2];
		minmaxindex[0] = datafile.finalindex - 1;
		minmaxindex[1] = datafile.startingindex;
		arraycopy(minmaxindex, 0, tmpminmax, 0, 2);

//    System.out.println(peaklist.length);  // todo
		for (int i = 0; i < peaklist.size(); i++) {
			if (phase == null || peaklist.elementAt(i).getPhase() == phase) {
				peaklist.elementAt(i).computePeak(datafile, expfit, asample, ainstrument, printStream, logOutput, cutoff,
						computeTexture, computeStrain, computeFhkl, leBailExtraction, tmpminmax,
						computeBroadening, !datafile.increasingX());
				if (i == 0)
					arraycopy(tmpminmax, 0, minmaxindex, 0, 2);
				else if (!leBailExtraction) {
					if (minmaxindex[0] > tmpminmax[0])
						minmaxindex[0] = tmpminmax[0];
					if (minmaxindex[1] < tmpminmax[1])
						minmaxindex[1] = tmpminmax[1];
				}
			}
		}

		if (logOutput && baos != null) {
			try {
				synchronized (out) {
					printLine(out, baos.toString());
					newLine(out);
					out.flush();
				}
			} catch (Exception io) {
				io.printStackTrace();
			}
		}

		return minmaxindex;

	}

}
