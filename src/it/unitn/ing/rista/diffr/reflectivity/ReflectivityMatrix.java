/*
 * @(#)ReflectivityMatrix.java created 27/03/2000 Le Mans
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

package it.unitn.ing.rista.diffr.reflectivity;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;

/**
 *  The ReflectivityMatrix is a class to compute reflectivity using the
 *  Matrix method. See A. Gibaud book .....
 *
 *
 * @version $Revision: 1.5 $, $Date: 2004/11/18 09:30:50 $
 * @author Luca Lutterotti, Alain Gibaud
 * @since JDK1.1
 */

public class ReflectivityMatrix extends Reflectivity {

	static String idString = "Matrix method";
	static String idDescription = "Use the matrix method for reflectivity computation";

	protected static String[] diclistc = {
			"_maud_reflectivity_scale_factor"
	};
	protected static String[] diclistcrm = {
			"_maud_reflectivity_scale_factor"
	};

	protected static String[] classlistc = {};

	protected static String[] classlistcs = {};

	public ReflectivityMatrix(XRDcat aobj, String alabel) {
    super(aobj, alabel);
		initBaseObject();
    identifier = idString;
    IDlabel = idString;
    description = idDescription;
  }

  public ReflectivityMatrix(XRDcat aobj) {
    this(aobj, idString);
  }

  public ReflectivityMatrix() {
    identifier = idString;
    IDlabel = idString;
    description = idDescription;
  }

	public void initConstant() {
		Nstring = 0;
		Nstringloop = 0;
		Nparameter = 1;
		Nparameterloop = 0;
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
		parameterField[0] = new Parameter(this, getParameterString(0), 1.0,
				ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
				ParameterPreferences.getDouble(getParameterString(0) + ".max", 1E9));
	}

	public void computeReflectivity(Sample asample, DiffrDataFile adatafile) {
	  RadiationType rad = adatafile.getDataFileSet().getInstrument().getRadiationType();
//	  System.out.println(rad.getMeanRadiationWavelength());
    double lambda = rad.getMeanRadiationWavelength();
    Layer lastLayer = asample.getTopLayer();
    double[] q2 = adatafile.getXrangeInQ();
    int numberOfPoints = q2.length;
    for (int i = 0; i < numberOfPoints; i++)
      q2[i] *= q2[i];

	  double[][][] nm;
	  double t = lastLayer.getThicknessInAngstrom();
		if (t == 0)
	    nm = getReflectivityTransferMatrixD(q2, lambda, lastLayer);
	  else {
			// need to add air on top as it is missing
			nm = getReflectivityTransferMatrixDAddAir(q2, lambda, lastLayer);
		}

    Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
    double incidentIntensity = ainstrument.getIntensityValue() * parameterValues[0];

    double reflectivity;
    double[] acr;
    for (int i = 0; i < numberOfPoints; i++) {
      acr = MoreMath.complexDivide(nm[0][1][i], nm[1][1][i], nm[0][3][i], nm[1][3][i]);
      reflectivity = incidentIntensity * (acr[0] * acr[0] + acr[1] * acr[1]);
      adatafile.addtoFit(i, reflectivity);
    }
    adatafile.computeReflectivityBroadening(asample);

  }

  public double[][][] getReflectivityTransferMatrixD(double[] q2, double lambda, Layer aLayer) {
    int numberOfPoints = q2.length;
    Layer layerBelow = aLayer.getLayerBelow();

    double lon = 4 * Math.PI / lambda;
    lon *= lon;
    double nm[][][];
    double qc = aLayer.getCriticalQcValue();
    qc *= qc;
    double absorption = 0.5 * aLayer.getAbsorptionValue() * lon;
    double re, im, re1, im1, re2, im2, re3, im3;

    double[] complex;

    if (layerBelow == null) {
//		System.out.println("last layer");
//		System.out.println(toXRDcatString());
      nm = new double[2][5][numberOfPoints];
      for (int j = 0; j < numberOfPoints; j++) {
        nm[0][0][j] = 1.0;
        nm[1][0][j] = 0.0;
        nm[0][1][j] = 0.0;
        nm[1][1][j] = 0.0;
        nm[0][2][j] = 0.0;
        nm[1][2][j] = 0.0;
        nm[0][3][j] = 1.0;
        nm[1][3][j] = 0.0;

        // nm[4] contains the Kz
        complex = MoreMath.complexSqrt((q2[j] - qc) / 4, absorption);

        nm[0][4][j] = complex[0];
        nm[1][4][j] = complex[1];

      }
      return nm;
    }

    double zn = aLayer.getzPosition();
//	  System.out.println("Z: " + zn);

    nm = getReflectivityTransferMatrixD(q2, lambda, layerBelow);

    double roughness = MoreMath.pow(layerBelow.getRoughnessValue(), 2) / 2.0;

//		System.out.println(toXRDcatString());

    double[] kz, pkz = new double[2], mkz = new double[2],
        r1, m = new double[2], p = new double[2], tm, tmm,
        rug2 = new double[2], rug1 = new double[2], tp;
    double[][] am = new double[2][4];

    for (int j = 0; j < numberOfPoints; j++) {
//		System.out.println(Math.sqrt(q2[j]));

      kz = MoreMath.complexSqrt((q2[j] - qc) / 4, absorption);

      pkz[0] = kz[0] + nm[0][4][j];
      pkz[1] = kz[1] + nm[1][4][j];
      mkz[0] = kz[0] - nm[0][4][j];
      mkz[1] = kz[1] - nm[1][4][j];

      r1 = MoreMath.complexDivide(nm[0][4][j], nm[1][4][j], kz[0], kz[1]);

      m[0] = 1.0 - r1[0];
      m[1] = -r1[1];
      p[0] = 1.0 + r1[0];
      p[1] = r1[1];

      tm = MoreMath.complexExp(-mkz[1] * zn, mkz[0] * zn);
      tmm = MoreMath.complexExp(mkz[1] * zn, -mkz[0] * zn);
      tp = MoreMath.complexExp(-pkz[1] * zn, pkz[0] * zn);

      rug1[0] = (-pkz[0] * pkz[0] + pkz[1] * pkz[1]) * roughness;
      rug1[1] = (-2.0 * pkz[0] * pkz[1]) * roughness;
      rug2[0] = (-mkz[0] * mkz[0] + mkz[1] * mkz[1]) * roughness;
      rug2[1] = (-2.0 * mkz[0] * mkz[1]) * roughness;

      rug1 = MoreMath.complexExp(rug1[0], rug1[1]);
      re = m[0] * rug1[0] - m[1] * rug1[1];
      im = m[0] * rug1[1] + m[1] * rug1[0];
      m[0] = re;
      m[1] = im;
//      m = m.multiply(Complex.exp(rug1));

      rug2 = MoreMath.complexExp(rug2[0], rug2[1]);
      re = p[0] * rug2[0] - p[1] * rug2[1];
      im = p[0] * rug2[1] + p[1] * rug2[0];
      p[0] = re;
      p[1] = im;
//      p = p.multiply(Complex.exp(rug2));

      am[0][0] = (p[0] * tmm[0] - p[1] * tmm[1]) * 0.5;
      am[1][0] = (p[0] * tmm[1] + p[1] * tmm[0]) * 0.5;
//	 		am[0] = p.multiply(tmm).multiply(0.5);

      r1 = MoreMath.complexDivide(m[0], m[1], tp[0], tp[1]);
      am[0][1] = r1[0] * 0.5;
      am[1][1] = r1[1] * 0.5;
//	 		am[1] = m.divide(tp).multiply(0.5);

      am[0][2] = (m[0] * tp[0] - m[1] * tp[1]) * 0.5;
      am[1][2] = (m[0] * tp[1] + m[1] * tp[0]) * 0.5;
//	 		am[2] = m.multiply(tp).multiply(0.5);

      am[0][3] = (p[0] * tm[0] - p[1] * tm[1]) * 0.5;
      am[1][3] = (p[0] * tm[1] + p[1] * tm[0]) * 0.5;
//	 		am[3] = p.multiply(tm).multiply(0.5);

      re = am[0][0] * nm[0][0][j] - am[1][0] * nm[1][0][j]
          + am[0][1] * nm[0][2][j] - am[1][1] * nm[1][2][j];
      im = am[0][0] * nm[1][0][j] + am[1][0] * nm[0][0][j]
          + am[0][1] * nm[1][2][j] + am[1][1] * nm[0][2][j];

      re1 = am[0][0] * nm[0][1][j] - am[1][0] * nm[1][1][j]
          + am[0][1] * nm[0][3][j] - am[1][1] * nm[1][3][j];
      im1 = am[0][0] * nm[1][1][j] + am[1][0] * nm[0][1][j]
          + am[0][1] * nm[1][3][j] + am[1][1] * nm[0][3][j];

      re2 = am[0][2] * nm[0][0][j] - am[1][2] * nm[1][0][j]
          + am[0][3] * nm[0][2][j] - am[1][3] * nm[1][2][j];
      im2 = am[0][2] * nm[1][0][j] + am[1][2] * nm[0][0][j]
          + am[0][3] * nm[1][2][j] + am[1][3] * nm[0][2][j];

      re3 = am[0][2] * nm[0][1][j] - am[1][2] * nm[1][1][j]
          + am[0][3] * nm[0][3][j] - am[1][3] * nm[1][3][j];
      im3 = am[0][2] * nm[1][1][j] + am[1][2] * nm[0][1][j]
          + am[0][3] * nm[1][3][j] + am[1][3] * nm[0][3][j];

      nm[0][0][j] = re;
      nm[1][0][j] = im;
      nm[0][1][j] = re1;
      nm[1][1][j] = im1;
      nm[0][2][j] = re2;
      nm[1][2][j] = im2;
      nm[0][3][j] = re3;
      nm[1][3][j] = im3;

      nm[0][4][j] = kz[0];
      nm[1][4][j] = kz[1];

    }

    return nm;
  }

	public double[][][] getReflectivityTransferMatrixDAddAir(double[] q2, double lambda, Layer aLayer) {
		int numberOfPoints = q2.length;
		Layer layerBelow = aLayer;

		double lon = 4 * Math.PI / lambda;
		lon *= lon;
		double nm[][][];
		double qc = 0; // air
		qc *= qc;
		double absorption = 0; // air
		double re, im, re1, im1, re2, im2, re3, im3;

		double zn = layerBelow.getzPosition() + layerBelow.getThicknessInAngstrom();
//	  System.out.println("Z: " + zn);

		nm = getReflectivityTransferMatrixD(q2, lambda, layerBelow);

		double roughness = MoreMath.pow(layerBelow.getRoughnessValue(), 2) / 2.0;

//		System.out.println(toXRDcatString());

		double[] kz, pkz = new double[2], mkz = new double[2],
				r1, m = new double[2], p = new double[2], tm, tmm,
				rug2 = new double[2], rug1 = new double[2], tp;
		double[][] am = new double[2][4];

		for (int j = 0; j < numberOfPoints; j++) {
//		System.out.println(Math.sqrt(q2[j]));

			kz = MoreMath.complexSqrt((q2[j] - qc) / 4, absorption);

			pkz[0] = kz[0] + nm[0][4][j];
			pkz[1] = kz[1] + nm[1][4][j];
			mkz[0] = kz[0] - nm[0][4][j];
			mkz[1] = kz[1] - nm[1][4][j];

			r1 = MoreMath.complexDivide(nm[0][4][j], nm[1][4][j], kz[0], kz[1]);

			m[0] = 1.0 - r1[0];
			m[1] = -r1[1];
			p[0] = 1.0 + r1[0];
			p[1] = r1[1];

			tm = MoreMath.complexExp(-mkz[1] * zn, mkz[0] * zn);
			tmm = MoreMath.complexExp(mkz[1] * zn, -mkz[0] * zn);
			tp = MoreMath.complexExp(-pkz[1] * zn, pkz[0] * zn);

			rug1[0] = (-pkz[0] * pkz[0] + pkz[1] * pkz[1]) * roughness;
			rug1[1] = (-2.0 * pkz[0] * pkz[1]) * roughness;
			rug2[0] = (-mkz[0] * mkz[0] + mkz[1] * mkz[1]) * roughness;
			rug2[1] = (-2.0 * mkz[0] * mkz[1]) * roughness;

			rug1 = MoreMath.complexExp(rug1[0], rug1[1]);
			re = m[0] * rug1[0] - m[1] * rug1[1];
			im = m[0] * rug1[1] + m[1] * rug1[0];
			m[0] = re;
			m[1] = im;
//      m = m.multiply(Complex.exp(rug1));

			rug2 = MoreMath.complexExp(rug2[0], rug2[1]);
			re = p[0] * rug2[0] - p[1] * rug2[1];
			im = p[0] * rug2[1] + p[1] * rug2[0];
			p[0] = re;
			p[1] = im;
//      p = p.multiply(Complex.exp(rug2));

			am[0][0] = (p[0] * tmm[0] - p[1] * tmm[1]) * 0.5;
			am[1][0] = (p[0] * tmm[1] + p[1] * tmm[0]) * 0.5;
//	 		am[0] = p.multiply(tmm).multiply(0.5);

			r1 = MoreMath.complexDivide(m[0], m[1], tp[0], tp[1]);
			am[0][1] = r1[0] * 0.5;
			am[1][1] = r1[1] * 0.5;
//	 		am[1] = m.divide(tp).multiply(0.5);

			am[0][2] = (m[0] * tp[0] - m[1] * tp[1]) * 0.5;
			am[1][2] = (m[0] * tp[1] + m[1] * tp[0]) * 0.5;
//	 		am[2] = m.multiply(tp).multiply(0.5);

			am[0][3] = (p[0] * tm[0] - p[1] * tm[1]) * 0.5;
			am[1][3] = (p[0] * tm[1] + p[1] * tm[0]) * 0.5;
//	 		am[3] = p.multiply(tm).multiply(0.5);

			re = am[0][0] * nm[0][0][j] - am[1][0] * nm[1][0][j]
					+ am[0][1] * nm[0][2][j] - am[1][1] * nm[1][2][j];
			im = am[0][0] * nm[1][0][j] + am[1][0] * nm[0][0][j]
					+ am[0][1] * nm[1][2][j] + am[1][1] * nm[0][2][j];

			re1 = am[0][0] * nm[0][1][j] - am[1][0] * nm[1][1][j]
					+ am[0][1] * nm[0][3][j] - am[1][1] * nm[1][3][j];
			im1 = am[0][0] * nm[1][1][j] + am[1][0] * nm[0][1][j]
					+ am[0][1] * nm[1][3][j] + am[1][1] * nm[0][3][j];

			re2 = am[0][2] * nm[0][0][j] - am[1][2] * nm[1][0][j]
					+ am[0][3] * nm[0][2][j] - am[1][3] * nm[1][2][j];
			im2 = am[0][2] * nm[1][0][j] + am[1][2] * nm[0][0][j]
					+ am[0][3] * nm[1][2][j] + am[1][3] * nm[0][2][j];

			re3 = am[0][2] * nm[0][1][j] - am[1][2] * nm[1][1][j]
					+ am[0][3] * nm[0][3][j] - am[1][3] * nm[1][3][j];
			im3 = am[0][2] * nm[1][1][j] + am[1][2] * nm[0][1][j]
					+ am[0][3] * nm[1][3][j] + am[1][3] * nm[0][3][j];

			nm[0][0][j] = re;
			nm[1][0][j] = im;
			nm[0][1][j] = re1;
			nm[1][1][j] = im1;
			nm[0][2][j] = re2;
			nm[1][2][j] = im2;
			nm[0][3][j] = re3;
			nm[1][3][j] = im3;

			nm[0][4][j] = kz[0];
			nm[1][4][j] = kz[1];

		}

		return nm;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JReflMatrixOptionsD(parent, this);
		return adialog;
	}

	public class JReflMatrixOptionsD extends JOptionsDialog {

		JTextField scaleFactorTF;

		public JReflMatrixOptionsD(Frame parent, XRDcat obj) {

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

			setTitle("Matrix reflectivity options");
			initParameters();
			pack();
		}

		public void initParameters() {
			scaleFactorTF.setText(parameterField[0].getValue());
			addComponenttolist(scaleFactorTF, parameterField[0]);
		}

		public void retrieveParameters() {
			parameterField[0].setValue(scaleFactorTF.getText());
		}

		public void dispose() {
			super.dispose();
		}

	}

}











































  /* rrdmfit by A. Gibaud, 03/2000

  function u=rrdmfit(pa);
load a2.par
lambda=1.19056;
sigres=pa(14);
dq1=0;
load rrfil.dat
q1=rrfil(:,1);
int=rrfil(:,2);
int=int/max(int);
nlim=length(int);
nlim=500;
ndep=1;
q1=q1(ndep:nlim);
int=int(ndep:nlim);
iresol=1;
if iresol==1
st=q1(2)-q1(1);
q2=-5*sigres:st:5*sigres;
resol=gauss(1,0,sigres,0,q2);
else
end
%qc=sqrt(a2(:,1)*1e-5*16*pi);
qc=a2(:,1);
sig=a2(:,2);
mu=a2(:,3);
ep=a2(:,4);
bg=a2(1,5);
nc=a2(length(qc),6);
nbuf=a2(1,6);
nlay=a2(2,6);
ncap=a2(3,6);
ntot=nbuf+nlay;
%t=a2(2,5);
ls=a2(3,5);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%qc de la tete  de la queue en fonction de A
%Il faut definir qhead et qtail ainsi que
%Nh, Nt le nb d'electron dans la tete et la queue et
%Lt et Lh leur longueur
%Nh=41;
%Nt=161;
%Lh=ep(3);
%Lt=ep(4);
%qc(4)=sqrt(Nt/(711*(19.4+da)*Lt));
%qc(3)=sqrt(Nh/(711*(19.4+da)*Lh));
%qc(5)=qc(3);
%qc(6)=qc(4);
%qc(6)=x*qc(3);
%qc(7)=x*qc(4);

% Si
sig(1)=pa(1);

%SiO2
ep(2) = pa(2);
sig(2)= pa(3);
qc(2)=  pa(4);

%NTSOH
%   partie silane  3, 4,0.031;
ep(3) = pa(5);
sig(3)= pa(6);
qc(3) = pa(7);

%  partie queue
ep(4)= pa(8);
sig(4)=pa(9);


%OTS
ep(5)=pa(10);
sig(5)=pa(11);
qc(5)=qc(3);


ep(6)=pa(12);
sig(6)=pa(13);


t=pa(15);
sca=1;
qc(1)=pa(16);

%!!!! decalage dq1 !!!!!!!!!!!

u1=((q1+dq1).^2);
lon=(4*pi/lambda)^2;
kz(:,1)=0.5*sqrt(u1-(qc(1)^2)+(2*i*mu(1)*lon));
npt=length(q1);
zn=0;
te=zeros(npt,1);
m1=ones(npt,1);
nm=[m1 te te m1];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   cette boucle permet d aller du substrat
%   a la derniere couchedu motif  Substrat-buffer1,2,3...-couche1,2,3...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for n=2:1:1+ntot;
u=u1-(qc(n)^2)+(2*i*mu(n)*lon);
kz(:,n)=sqrt(u/4);
pkz=kz(:,n)+kz(:,n-1);
mkz=kz(:,n)-kz(:,n-1);
r1=kz(:,n-1)./kz(:,n);
m=(1-r1);
p=(1+r1);
zn=zn+ep(n-1);
tm=exp(i*zn*mkz);
   tmm=1./tm;
tp=exp(i*zn*pkz);
rug1=(pkz).^2*sig(n-1)^2/2;
rug2=(mkz).^2*sig(n-1)^2/2;
m=m.*exp(-rug1);
p=p.*exp(-rug2);
   am=0.5*[p.*tmm m./tp m.*tp p.*tm];
nm=product(am,nm);
end
nm1=nm;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   il faut maintennat parcourir nc-1 fois la
%   multicouche 1,2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nm=[m1 te te m1];

for n=1:1:nc-1,

pkz=kz(:,nbuf+2)+kz(:,ntot+1);
mkz=kz(:,nbuf+2)-kz(:,ntot+1);
r1=kz(:,ntot+1)./kz(:,nbuf+2);
m=(1-r1);
p=(1+r1);
zn=zn+ep(ntot+1);
tm=exp(i*zn*mkz);
   tmm=1./tm;
tp=exp(i*zn*pkz);
rug1=(pkz).^2*sig(ntot+1)^2/2;
rug2=(mkz).^2*sig(ntot+1)^2/2;
m=m.*exp(-rug1);
p=p.*exp(-rug2);
   am=0.5*[p.*tmm m./tp m.*tp p.*tm];
nm=product(am,nm);

for s=nbuf+3:1:ntot+1,
pkz=kz(:,s)+kz(:,s-1);
mkz=kz(:,s)-kz(:,s-1);
r1=kz(:,s-1)./kz(:,s);
m=(1-r1);
p=(1+r1);
zn=zn+ep(s-1);
tm=exp(i*zn*mkz);
   tmm=1./tm;
tp=exp(i*zn*pkz);
rug1=(pkz).^2*sig(s-1)^2/2;
rug2=(mkz).^2*sig(s-1)^2/2;
m=m.*exp(-rug1);
p=p.*exp(-rug2);
   am=0.5*[p.*tmm m./tp m.*tp p.*tm];
nm=product(am,nm);

end
end
nm=product(nm,nm1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    fin du calcul avec cap/2 + air/cap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for n=ntot+2:1:length(qc),
u=u1-(qc(n)^2)+(2*i*mu(n)*lon);
kz(:,n)=sqrt(u/4);
pkz=kz(:,n)+kz(:,n-1);
mkz=kz(:,n)-kz(:,n-1);
r1=kz(:,n-1)./kz(:,n);
m=(1-r1);
p=(1+r1);
zn=zn+ep(n-1);
tm=exp(i*zn*mkz);
   tmm=1./tm;
tp=exp(i*zn*pkz);
rug1=(pkz).^2*sig(n-1)^2/2;
rug2=(mkz).^2*sig(n-1)^2/2;
m=m.*exp(-rug1);
p=p.*exp(-rug2);
   am=0.5*[p.*tmm m./tp m.*tp p.*tm];
nm=product(am,nm);
end
acr=nm(:,2)./nm(:,4);
cr=sca*(acr.*conj(acr))+bg;
npt=length(q1);
u=4*pi ./ (lambda*q1);
dum=t.*u;
cr=(cr.*(dum>ls).*ls ./ dum)+cr.*(dum<ls);
%cr=cr/max(cr);
if iresol==1
y1=resol;
u1=trapz(y1);
ncr=conv(cr,y1);
nn=length(cr);
nptc=length(ncr);
nptr=round(length(y1)/2);
ncr1=ncr(nptr:nptc-nptr);
if length(ncr1)<npt
ncr1=[ncr1;ncr(nptc-nptr+1)];
else
end
else
nptr=1;
nlim=length(q1)+1;
end
%ncr1=cr;
nlim=nlim-ndep+1;
cr=ncr1(nptr:nlim-nptr)/max(ncr1);
ifit=1;
if ifit==1
q2=q1(nptr:nlim-nptr);
af=log10(cr);
int1=int(nptr:nlim-nptr);
u=norm(log10(int1)-af);
h=plot(q2,log10(int1),q2,af);
%set(h,'MarkerSize',0.12);
else
q2=q1(nptr:nlim-nptr);
q24=16*q2.^4/qc(1)^4;
fa=log10(cr.*q24);
int2=int(nptr:nlim-nptr);
rq4=log10(int2.*q24);
u=norm(rq4-fa);
plot(q2,rq4,q2,fa)
end
m=[qc sig mu ep a2(:,5) a2(:,6)];
save na2.par m /ascii
save pa.par pa /ascii
%plot(q1(1:20),log10(int(1:20)),q1(1:20),af(1:20));
%plot(q1(nptr:nlim-nptr),log10(int),q1(nptr:nlim-nptr),af);
m=[q2 int1 cr];
%save jac5ntc.dat m /ascii
%save sil8ntc.dat m /ascii
%m=p2;
%save p2.par m /ascii
drawnow

*/

