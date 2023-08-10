/*
 * @(#)GeneratePatternAndPDF.java created June 3, 2023 White Rock
 *
 * Copyright (c) 2023 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.pdf;

import it.unitn.ing.rista.awt.ProgressFrame;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningPVCaglioti;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.PersistentThread;

import java.util.Vector;

/**
 * The GeneratePatternAndPDF is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 2023/06/03 11:56:35 $
 * @since JDK1.1
 */
public class GeneratePatternAndPDF {

	public static String[] radiationType = {"X-ray", "Neutron"};
	private Vector listData;
	int actualRunningThreads = 0;

	Phase thePhase;
	double qmin = 0;
	double qmax = 0;
	double qstep = 0;

	boolean computing = true;

	public GeneratePatternAndPDF(Phase aPhase, double amin, double amax, double astep) {
		thePhase = aPhase;
		qmin = amin;
		qmax = amax;
		qstep = astep;
	}

	public void generateData(String radiation) {
		computing = true;
		int i;
		double radWave = 0.1;

		Sample theSample = thePhase.getSample();

		DataFileSet adatafileset = new DataFileSet(theSample);
		theSample.addDataFileSet(adatafileset);
		adatafileset.setBackgroundInterpolated(false);

		// we define the instrument
		Instrument instrument = new DefaultInstrument(adatafileset);
		adatafileset.setInstrument(instrument);
		instrument.setIntensity("1.0");
		instrument.setMeasurement("Theta-2Theta");
		instrument.setRadiationType(radiation);
		instrument.getRadiationType().addRadiation("Dummy");
		instrument.getRadiationType().getRadiation(0).setRadiation("Dummy", radWave, 1.0);

		// instrument broadening
		InstrumentBroadening instBroad = instrument.getInstrumentBroadening();
		if (instBroad instanceof InstrumentBroadeningPVCaglioti) {
			InstrumentBroadeningPVCaglioti inb = (InstrumentBroadeningPVCaglioti) instBroad;
			inb.setCagliotiTanDependent(false);
			inb.setAsymmetryTanDependent(false);
			inb.setBroadeningConvoluted(false);
			inb.setTruncationAngle("0.8");
			inb.getAsymmetryList().removeAllItems();
/*			inb.getAsymmetryList().addItem(new Parameter(inb, "Asymmetry coeff 0", 135.185345));
			inb.getAsymmetryList().addItem(new Parameter(inb, "Asymmetry coeff 1", -0.2044876));
			inb.getAsymmetryList().addItem(new Parameter(inb, "Asymmetry coeff 2", -2.3630368E-5));*/
			inb.getCagliotiList().removeAllItems();
/*			inb.getCagliotiList().addItem(new Parameter(inb, "Caglioti coeff W", 0.0103287235));
			inb.getCagliotiList().addItem(new Parameter(inb, "Caglioti coeff V", -0.035830247));
			inb.getCagliotiList().addItem(new Parameter(inb, "Caglioti coeff U", 0.08134942));*/
			inb.getGaussianList().removeAllItems();
/*			inb.getGaussianList().addItem(new Parameter(inb, "gaussian coeff 0", 1.1103061));
			inb.getGaussianList().addItem(new Parameter(inb, "gaussian coeff 1", -0.029315038));
			inb.getGaussianList().addItem(new Parameter(inb, "gaussian coeff 2", 3.079934E-4));*/
		}

		// now we add the data
		adatafileset.addDatafiles(Constants.generateDummyDatafile); //addDataFileforName(folderAndName[0] + folderAndName[1], true);
//		adatafileset.addBackgroudCoeff();
//		adatafileset.addBackgroudCoeff();
//		adatafileset.addBackgroudCoeff();
//		adatafileset.addAdditionalBackgroundToAll();


		actualRunningThreads = 0;
		listData = new Vector(100, 100);

		int maxRunning = 1; //Constants.maxNumberOfThreads;
		if (Constants.debugThreads)
			System.out.println("Thread to generate G(r)");

		Vector data = new Vector(1, 1);

		if (maxRunning > data.size())
			maxRunning = data.size();

		PersistentThread[] threads = new PersistentThread[maxRunning];
		for (i = 0; i < maxRunning; i++) {
			threads[i] = new PersistentThread(i) {
				public void executeJob() {
					actualRunningThreads++;
					int index = this.threadNumber;
					int number = this.data.size();
					ProgressFrame prF = null;
					if (!Constants.textonly && Constants.showProgressFrame)
						try {
							prF = new ProgressFrame(number, 2);
							prF.setTitle("Generating G(r) and PDF");
						} catch (NullPointerException npe) {
							System.out.println("Not able to create frame, MacOSX display sleep bug?");
						}

					for (int i = 0; i < number; i++) {
						if (!Constants.textonly && prF != null) {
							prF.setProgressText("......" );
						}
						Vector dataVector = generateData(thePhase, adatafileset, prF);
						addData(dataVector);
					}
					if (!Constants.textonly && prF != null) {
						prF.setVisible(false);
						prF.dispose();
//            prF = null;
					}
					actualRunningThreads--;
				}
			};
		}
		i = 0;
		int istep = data.size() / maxRunning;
		for (int j = 0; j < maxRunning; j++) {
			int is = i;
			if (j < maxRunning - 1)
				i = Math.min(i + istep, data.size());
			else
				i = data.size();
			threads[j].setJobRange(is, i);
			threads[j].data = new Vector(istep, 10);
			for (int k = is; k < i; k++)
				threads[j].data.add(data.elementAt(k));
			threads[j].start();
		}
		do {
			try {
				Thread.sleep(Constants.timeToWaitThreadsEnding);
			} catch (InterruptedException r) {
				r.printStackTrace();
			}
		} while (actualRunningThreads > 0);

		for (int j = 0; j < maxRunning; j++) {
			theSample.removeData(adatafileset);
			threads[j] = null;
		}
//    threads = null;

		computing = false;

	}

	final Object datalock = new Object();

	public void addData(Vector data) {
		synchronized (datalock) {
			listData.add(data);
		}
	}

	public Vector getGr() {
		while (computing) {
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		return listData;
	}

	final Object lock = new Object();

	public Vector generateData(Phase phase, DataFileSet dataset, ProgressFrame prF) {

		int numberOfJobs = 1;

		Vector dataVector = new Vector(0, 1);

		if (prF != null)
			prF.setProgressBarValue(numberOfJobs, 1);



//		if (!Constants.textonly && prF != null)
//			prF.increaseProgressBarValue(1);
//		dataVector.add(getLinesAndCell(phase));

		return dataVector;
	}

}
