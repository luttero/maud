/*
 * @(#)Reflectivity.java created 25/03/2000 Le Mans
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.PersistentThread;

import javax.swing.*;

import static java.lang.System.out;


/**
 *  The Reflectivity is a general class to perform reflectivity computation
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Reflectivity extends XRDcat {

  public Reflectivity(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Reflectivity(XRDcat aobj) {
    this(aobj, "Reflectivity x");
  }

  public Reflectivity() {
  }

	public void computeReflectivity(Sample asample, DataFileSet adataset) {

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
							computeReflectivity(theSample, theDataset.getActiveDataFile(j));
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
			for (int k = 0; k < datafilenumber; k++)
				computeReflectivity(theSample, theDataset.getActiveDataFile(k));

	}


	/**
	 * The method here do nothing, subclasses should overwrite it to compute the reflectivity pattern
	 * for the <code>DiffrDataFile</code>. When the pattern is computed it should be add to the
	 * <code>DiffrDataFile</code> using one of the addtoFit methods.
	 *
	 *
	 * @param adatafile
	 * @see DiffrDataFile#addtoFit
	 */
  public void computeReflectivity(Sample asample, DiffrDataFile adatafile) {

  }

	/**
	 * To be used to overwrite the default method that uses the model set by the analysis to
	 * compute the quantity to be minimize during refinement. Some models may need a specific
	 * way to compute it disregarding what the user will set for the analysis.
	 *
	 * @return true if the class needs to overwrite the statistical model for computing
	 * the quantity to minimize (example: reflectivity minimize always the difference in the Log(I)
	 */

	public boolean needReflectivityStatistic() {
    return true;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JReflectivityOptionsD(parent, this);
    return adialog;
  }

  public class JReflectivityOptionsD extends JOptionsDialog {

    public JReflectivityOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this reflectivity model"));

      setTitle("Options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}
