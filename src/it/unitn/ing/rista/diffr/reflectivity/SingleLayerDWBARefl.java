/*
 * @(#)SingleLayerDWBARefl.java created 17/07/2002 Mesiano
 *
 * Copyright (c) 2002 Sangam Banerjee All Rights Reserved.
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

package it.unitn.ing.rista.diffr.reflectivity;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.util.Vector;

import static java.lang.System.out;

/**
 *  The SingleLayerDWBARefl is a class to compute reflectivity using the
 *  DWBA method for single layer.
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti, Sangam Banerjee
 * @since JDK1.1
 */

public class SingleLayerDWBARefl extends Reflectivity {

  public double lastFilmQc2 = 0.0;

	protected static String[] diclistc = {
			"_maud_reflectivity_scale_factor",
			"_reflectivity_dwba_boxsize",
			"_refine_diff_density"
	};
	protected static String[] diclistcrm = {
			"_maud_reflectivity_scale_factor",
			"cell size (angstrom)",
			"electron density for cell "
	};

	protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

	public SingleLayerDWBARefl(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = "DWBA Single Layer";
		IDlabel = "DWBA Single Layer";
		description = "Use the DBWA method for single layer reflectivity computation";
	}

	public SingleLayerDWBARefl(XRDcat aobj) {
    this(aobj, "DWBA Single Layer");
  }

  public SingleLayerDWBARefl() {
    identifier = "DWBA Single Layer";
    IDlabel = "DWBA Single Layer";
    description = "Use the DBWA method for single layer reflectivity computation";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 2;
    Nparameterloop = 1;
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
    // BoxSize
	  parameterField[0] = new Parameter(this, getParameterString(0), 1.0,
			  ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
			  ParameterPreferences.getDouble(getParameterString(0) + ".max", 1E9));
    parameterField[1] = new Parameter(this, getParameterString(0), 1.0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 10));
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

	public void computeReflectivity(Sample asample, DiffrDataFile adatafile) {
	  RadiationType rad = adatafile.getDataFileSet().getInstrument().getRadiationType();
	  double lambda = rad.getMeanRadiationWavelength();
    double[] q = adatafile.getXrangeInQ();
    int numberOfPoints = q.length;

    Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
    double incidentIntensity = ainstrument.getIntensityValue() * parameterValues[0];

    Layer substrate = asample.getSubstrateLayer();
    int numberOfLayers = asample.layersnumber();
    while (numberOfLayers <= 1)
      asample.addLayer(0);
    Layer film = substrate.getLayerOver();
    double filmQc2 = film.getCriticalQcValue();
    filmQc2 *= filmQc2;
    lastFilmQc2 = filmQc2;
    double substrateQc2 = substrate.getCriticalQcValue();
    substrateQc2 *= substrateQc2;

    double lon = 8.0 * Math.PI / lambda;
    double filmAbsorption = film.getAbsorptionValue() * lon;
// beta should be computed from f''
    double substrateAbsorption = substrate.getAbsorptionValue() * lon;

    double boxSize = parameterValues[1];
    int numberOfBoxes = numberOfLoopParameters[0];
    double thickness = numberOfBoxes * boxSize;

    double reflectivity = 0.0;

    double[] parameterLoopValues = (double[]) parameterLoopValuesVector.elementAt(0);
    for (int i = 0; i < numberOfPoints; i++) {
      double qs = q[i] * q[i];
      double[] q2 = {qs - filmQc2, filmAbsorption};
      q2 = MoreMath.complexSqrt(q2);
      double[] q3 = {qs - substrateQc2, substrateAbsorption};
      q3 = MoreMath.complexSqrt(q3);
      double[] r12 = MoreMath.complexDivide(q[i] - q2[0], -q2[1], q[i] + q2[0], q2[1]);

//      r12 = MoreMath.complexMultiply(r12, MoreMath.complexExp(roughness));
//      r12 = r12*cdexp(-q*q2*sig*sig),  sig = roughness of the film

      double[] r23 = MoreMath.complexDivide(q2[0] - q3[0], q2[1] - q3[1], q2[0] + q3[0], q2[1] + q3[1]);
      r23 = MoreMath.complexMultiply(r23, MoreMath.complexExp(MoreMath.complexMultiply(MoreMath.multiply(MoreMath.i, thickness), q2)));

//      r23 = ((q2-q3)/(q2+q3))*cdexp(rio*q2*d)
//      r23 = r23*cdexp(-q2*q3*sig*sig) // to do

      double[] tmpC = MoreMath.complexMultiply(r12, r23);
      double[] a = MoreMath.complexDivide(1.0 + r12[0], r12[1], 1.0 + tmpC[0], tmpC[1]);
//      a = (1.0*rrz + r12)/(1.0*rrz + r12*r23)
      double[] b = MoreMath.complexMultiply(a, r23);
//      b = a*r23
      tmpC[0] = 1.0;
      tmpC[1] = 0.0;
      double[] r0 = MoreMath.complexDivide(MoreMath.complexAdd(r12, r23),
              MoreMath.complexAdd(tmpC, MoreMath.complexMultiply(r12, r23)));
//      r0 = (r12+r23)/(1.0*rrz+r12*r23)
      double[] sum = {0.0, 0.0};
      double sum2 = 0.0;
      for (int j = 2; j <= numberOfBoxes; j++) {
        sum = MoreMath.complexAdd(sum, MoreMath.multiply(MoreMath.complexExp(MoreMath.complexMultiply(MoreMath.i, MoreMath.complexMultiply(boxSize * (j - 1), 0.0, q2[0], q2[1]))), parameterLoopValues[j - 1]));
//        sum+x(j)*cdexp(rio*q2*(j-1)*bsize)
        sum2 = sum2 + parameterLoopValues[j - 1];
//        sum2 = sum2+x(j)
      }
      sum2 += parameterLoopValues[0];
      double[] tmpC1 = {parameterLoopValues[0], 0.0};
      sum = MoreMath.complexAdd(MoreMath.complexAdd(sum, tmpC1), MoreMath.multiply(MoreMath.complexExp(MoreMath.complexMultiply(MoreMath.i, MoreMath.multiply(q2, thickness))), sum2));
//      sum = sum + x(1) - (x(1)+sum2)*cdexp(rio*q2*nbox*bsize)
      sum = MoreMath.complexMultiply(MoreMath.complexDivide(MoreMath.i, q2), sum);
//      sum = (rio/q2)*sum
      double[] csum = MoreMath.complexConjugate(sum);
//      csum = dconjg(sum)
      double[] aa = MoreMath.complexMultiply(a, a);
      double[] bb = MoreMath.complexMultiply(b, b);
      double[] aas = MoreMath.complexMultiply(aa, sum);
      double[] bbs = MoreMath.complexMultiply(bb, csum);
      aas = MoreMath.multiply(MoreMath.complexAdd(aas, bbs), 4.0 * Constants.PI / q[i]);
      double[] rq = MoreMath.complexMultiply(MoreMath.i, MoreMath.multiply(r0, 1.0 / Constants.E_RADIUS));
      rq = MoreMath.complexAdd(rq, aas);
//      rq = rio*r0/b0+(4*pi/q)*(a*a*sum + b*b*csum)
      double[] crq = MoreMath.complexConjugate(rq);
//      crq = dconjg(rq)
      double[] rr = MoreMath.complexMultiply(rq, crq);
      rr[0] *= Constants.E_RADIUS * Constants.E_RADIUS;
//      rr = rq*crq*b0*b0
//      rint(i)=rr+bkg

      rr[0] *= incidentIntensity;

      if (Double.isNaN(rr[0]))
        rr[0] = 0.0;
      adatafile.addtoFit(i, rr[0]);
//      System.out.println(i + " " + rr[0]);
    }
    adatafile.computeReflectivityBroadening(asample);

  }

  public int getBoxNumber(double x, double boxSize) {
    int index = 0;
    double actualX = 0.0;
    do {
      actualX += boxSize;
      index++;
    } while (actualX < x);
    return index;
  }

  /*
      do 100 i=1,nn
      q=xx(i)
      q2 = (q*q-qc2s+8.0*rio*rmu1*pl)*rrz
      q2 = cdsqrt(q2)
      q3 = (q*q-qc3s+8.0*rio*rmu2*pl)*rrz
      q3 = cdsqrt(q3)
      r12 = (q-q2)/(q+q2)
      r12 = r12*cdexp(-q*q2*sig*sig)
      r23 = ((q2-q3)/(q2+q3))*cdexp(rio*q2*d)
      r23 = r23*cdexp(-q2*q3*sig*sig)
      a = (1.0*rrz + r12)/(1.0*rrz + r12*r23)
      b = a*r23
      r0 = (r12+r23)/(1.0*rrz+r12*r23)
      sum = (0.0,0.0)
      sum2 = 0.0
      do 110 j = 2,nbox
      sum = sum+x(j)*cdexp(rio*q2*(j-1)*bsize)
      sum2 = sum2+x(j)
110   continue
      sum = sum + x(1) - (x(1)+sum2)*cdexp(rio*q2*nbox*bsize)
      sum = (rio/q2)*sum
      csum = dconjg(sum)
      rq = rio*r0/b0+(4*pi/q)*(a*a*sum + b*b*csum)
      crq = dconjg(rq)
      rr = rq*crq*b0*b0
      rint(i)=rr+bkg
100   continue
  */

  public void plotFunction(Frame theframe, int index) {
    DensityFunction function = new DensityFunction(parameterloopField[index]);
    (new PlotParameterFunction(theframe, function)).setVisible(true);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JDWBAOptionsD(parent, this);
    return adialog;
  }

  public class JDWBAOptionsD extends JOptionsDialog {

    JTextField scaleFactorTF;
	  JTextField boxsizeTF;
    JParameterListPane DWBAPanel;

    public JDWBAOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(3, 3));

      JPanel jp1 = new JPanel(new GridLayout(0, 2));
      principalPanel.add(jp1, BorderLayout.NORTH);

	    JPanel jp2 = new JPanel(new FlowLayout());
      jp2.add(new JLabel("Scale factor: "));
	    scaleFactorTF = new JTextField(Constants.FLOAT_FIELD);
	    scaleFactorTF.setToolTipText("Use the scale factor to balance with the diffraction intensity");
      jp2.add(scaleFactorTF);
	    jp1.add(jp2);
//      boxsizeTF.setText("1");

	    jp2 = new JPanel(new FlowLayout());
	    jp2.add(new JLabel("Box size (Angstrom): "));
	    boxsizeTF = new JTextField(Constants.FLOAT_FIELD);
	    jp2.add(boxsizeTF);
	    jp1.add(jp2);

	    jp1 = new JPanel(new FlowLayout());
      principalPanel.add(jp1, BorderLayout.CENTER);
      DWBAPanel = new JParameterListPane(this, false, true);
      jp1.add(DWBAPanel);

      setTitle("DWBA single layer");
      initParameters();
      pack();
    }

    public void initParameters() {
      scaleFactorTF.setText(parameterField[0].getValue());
      addComponenttolist(scaleFactorTF, parameterField[0]);
	    boxsizeTF.setText(parameterField[1].getValue());
	    addComponenttolist(boxsizeTF, parameterField[1]);
      DWBAPanel.setList(SingleLayerDWBARefl.this, 0);
    }

    public void retrieveParameters() {
      DWBAPanel.retrieveparlist();
      parameterField[0].setValue(scaleFactorTF.getText());
	    parameterField[1].setValue(boxsizeTF.getText());
    }

    public void dispose() {
      DWBAPanel.dispose();
      super.dispose();
    }

  }

  class DensityFunction extends it.unitn.ing.rista.util.function.ParameterFunction {

    double boxSize;
    int numberOfBoxes;
    double thickness;

    public DensityFunction(Vector parameterlist) {
      super(parameterlist);
      boxSize = parameterValues[1];
      numberOfBoxes = numberOfLoopParameters[0];
      thickness = numberOfBoxes * boxSize;
      setMin(0.0);
      setMax(thickness);

      System.out.println("Qc profile");
      double f = lastFilmQc2 * 711.1111111111;
      for (int i = 0; i < numberOfBoxes; i++) {
        f += getParameterLoopValues(0, i);
        System.out.println(0.0375 * Math.sqrt(f));
      }

    }

    public double f(double x) {

      int index = getBoxNumber(x, boxSize);
      if (index > numberOfBoxes)
        index = numberOfBoxes;
      double f = lastFilmQc2 * 711.1111111111;
      for (int i = 0; i < index; i++) {
        f += getParameterLoopValues(0, i);
      }
      return f;
    }

  }
}
