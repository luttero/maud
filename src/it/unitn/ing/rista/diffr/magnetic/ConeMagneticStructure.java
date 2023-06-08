/*
 * @(#)ConeMagneticStructure.java created 17/04/1999 Riva del Garda
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.magnetic;

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;


/**
 *  The ConeMagneticStructure is a class to represent a magnetic structure
 *  and to compute the magnetic structure factors
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class ConeMagneticStructure extends MagneticStructure {

  protected static String[] diclistc = {
    "_riet_par_magnetic_structure_phi",
    "_riet_par_magnetic_structure_theta",
    "_riet_par_magnetic_structure_cone_angle"
  };
  protected static String[] diclistcrm = {
    "phi (deg)",
    "theta (deg)",
    "cone angle (deg)"
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  public ConeMagneticStructure(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Cone model";
    IDlabel = "Cone model";
    description = "select this for a simple magnetic cone model";
  }

  public ConeMagneticStructure(XRDcat aobj) {
    this(aobj, "Magnetic structure x");
  }

  public ConeMagneticStructure() {
    identifier = "Cone model";
    IDlabel = "Cone model";
    description = "select this for a simple magnetic cone model";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 3;
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
    setPhi(0.0);
    setTheta(0.0);
    setConeAngle(5.0);
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < parameterField.length; i++) {
        if (source == parameterField[i]) {
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
        }
      }
      super.notifyParameterChanged(source);
    }
  }

  public Parameter getPhi() {
    return parameterField[0];
  }

  public void setPhi(String value) {
    refreshMagneticStructure = false;
    getPhi().setValue(value);
  }

  public void setPhi(double value) {
    setPhi(Double.toString(value));
  }

  public double getPhiD() {
    return getPhi().getValueD();
  }

  public Parameter getTheta() {
    return parameterField[1];
  }

  public void setTheta(String value) {
    refreshMagneticStructure = false;
    getTheta().setValue(value);
  }

  public void setTheta(double value) {
    setTheta(Double.toString(value));
  }

  public double getThetaD() {
    return getTheta().getValueD();
  }

  public Parameter getConeAngle() {
    return parameterField[2];
  }

  public void setConeAngle(String value) {
    refreshMagneticStructure = false;
    getConeAngle().setValue(value);
  }

  public void setConeAngle(double value) {
    setConeAngle(Double.toString(value));
  }

  public double getConeAngleD() {
    return getConeAngle().getValueD();
  }

  public double getMagneticMomentD() {
    return 1.0;
  }

  public double getMagneticStructureFactor(int h, int k, int l, int multiplicity, double dspacing,
                                           int radType, int tubeNumber) {

    Phase aphase = (Phase) getParent();


//  Calculate the crystallographic components of the cone axis ONLY the first
//  time the subroutine is called in the current cycle.

    double cosgamma = MoreMath.cosd(aphase.getFullCellValue(5));
    double singamma = MoreMath.sind(aphase.getFullCellValue(5));

    double phi = getPhiD();
    double theta = getThetaD();
    double sintheta = MoreMath.sind(theta);
    double cmx = MoreMath.cosd(phi) * sintheta;
    double cmy = MoreMath.sind(phi) * sintheta;

    double coneh = (cmx - cmy * cosgamma / singamma) / aphase.getFullCellValue(0);
    double conek = cmy / singamma / aphase.getFullCellValue(1);
    double conel = MoreMath.cosd(theta) / aphase.getFullCellValue(2);

//  Calculation of the orientation factor for the magnetic intensity

    double cw = coneh * h + conek * k + conel * l;
    double ss = 2.0 * Math.sqrt(1.0 / (4.0 * dspacing * dspacing));
    cw /= ss;
    double q2 = Math.abs(1.0 - cw * cw);

//  Trigonometric part of structure factor

    double a1 = 0.0;
    double a2 = 0.0;
    boolean satellite = false;  // to be completed for satellite peaks

    double i_dspace = 0.25 / (dspacing * dspacing);
    for (int j = 0; j < aphase.getFullAtomList().size(); j++) {   // to be ported to the new model
      AtomSite ato = (AtomSite) aphase.getFullAtomList().get(j);
      if (ato.useThisAtom) {
      double magscatf = ato.magneticScatfactor(dspacing, radType);

      double cosbi = MoreMath.cosd(getConeAngleD());

      double DWfactor = ato.DebyeWaller(h, k, l, i_dspace);
            //ato.DebyeWaller(h, k, l, dspacing);
      double scatFactor = DWfactor * ato.getOccupancy().getValueD();
      magscatf *= 0.2696 * scatFactor * getMagneticMomentD() * cosbi;

      for (int ix = 0; ix < ato.getSiteMultiplicity(); ix++) {
        double[] x = ato.getCoordinates(ix);
        double arg = 2.0 * Constants.PI * (h * x[0] + k * x[1] + l * x[2]);
        double w1 = Math.cos(arg);
        double w2 = Math.sin(arg);
        a1 += magscatf * w1;
        a2 += magscatf * w2;
      }
      }
    }
    return (a1 * a1 + a2 * a2) * multiplicity * q2;

/*
      DATA RAD/0.0174532925/,TPI/6.28318530718/
C      Change the sign of the magnetic phase when the current scattering
c      vector correspond to -k.

      isig = -1;
      IF((IHKL(NN).GT.NVK(IPH)/2).AND.(GENK(IPH).EQ.'Y')) ISIG=1
      DO 1 I=1,3                   !Cell parameters a,b,c
      SIDE(I)=SAVEC(IPH,I)
      COSA(I)=COS(SAVEC(IPH,I+3)*RAD)
      SINA(I)=SQRT(1.0-COSA(I)*COSA(I))
    1 continue
      IF(NSCAT.LT.0) RSTAR=2000.0*SQRT(SSNN)
c
c  Calculate the crystallographic components of the cone axis ONLY the first
c  time the subroutine is called in the current cycle. The derivatives of the
c  cone components w.r.t. the polar angles are also calculated.
c
      if(ifirst(IPH,2).eq.0) then
       Phi_h(IPH)=xl(iof+1,9)*rad
       Theta_h(iph)=xl(iof+1,10)*rad
       CMX=cos(phi_h(iph))*sin(theta_h(iph))
       CMY=sin(phi_h(iph))*sin(theta_h(iph))
       cone(iph,1)=(CMx-CMy*COSA(3)/SINA(3))/SIDE(1)
       cone(iph,2)=CMy/SINA(3)/SIDE(2)
       cone(iph,3)=cos(theta_h(iph))/SIDE(3)
       do i=1,n
        ipiof=iof+i
        if(lp(ipiof,8).ne.0) then
         XL(ipiof,8)=AMOD(XL(ipiof,8),1.0)
        endif
       end do
       ifirst(IPH,2)=1
       if(icent.ne.1) icent=1
       if(irl.ne.0) irl=0
      endif
c
c Calculation of the orientation factor for the magnetic intensity
c
      CW=0.
      ss=2.0*sqrt(SSNN)
      DO  I=1,3
       CW=CW+CONE(IPH,I)*HNN(I)
      END DO
      cw=cw/ss
      CW2=CW*CW

      if(IHKL(NN).eq.0) then
       q2=ABS(1.-CW2)
      else
       q2=(1.0+cw2)/4.0
      endif
C
C  Trigonometric part of structure factor
C
C  COSAR(i,j)=Cos{2pi*[H.r(i)+Phas(i)*isig]}
C  SINAR(i,j)=Sin{2pi*[H.r(i)+Phas(i)*isig]}
C
      AF=0.0
      BF=0.0
      DO 2 I=1,N       !Loop over all atoms
      IPIOF=I+IOF
      IF(IHKL(NN).eq.0) then
        arg=0.0
        ccs(i)=cos(xl(ipiof,7)*rad)
       else
        ARG=XL(ipiof,8)*isig     !Magnetic phase added for satellites
        ccs(i)=sin(xl(ipiof,7)*rad)
      end if
        DO J=1,3
         ARG=IH(J)*XL(IPIOF,J)+ARG
        end do
         ARG=TPI*ARG     ! ((hkl)+k){R,t}(xyz)
         COSAR(I)=COS(ARG)
         SINAR(I)=SIN(ARG)
         TEMP(I)=EXP(-XL(IPIOF,4)*SSNN)
         NI=PTR(IPIOF,2)
       IF(NSCAT.LT.0.AND.NI.LE.IABS(NSCAT)) THEN
        CALL SFINT(RSTAR,NI,FFX)
       ELSE
      FFX=AC(7,NI)
      DO  II=1,5,2
       FFX=FFX+AC(II,NI)*EXP(-AC(II+1,NI)*SSNN)  !Form factor value for Q=H+k
      end do
       ENDIF
      temp(i)=0.2696*FFX*TEMP(I)*XL(IPIOF,5)     ! 0.2696.f(Q).Temp(i).Occ
      ca(i)=temp(i)*cosar(i)*xl(ipiof,6)*ccs(i)
      cb(i)=temp(i)*sinar(i)*xl(ipiof,6)*ccs(i)
      AF=AF+ca(i)
      BF=BF+cb(i)
    2 CONTINUE                       !End loop over atoms I-index
      GFF=AF*AF+BF*BF
      FNN=Q2*GFF
*/
  }

  public double getSatelliteMagneticStructureFactor(int h, int k, int l, int multiplicity, int dspacing,
                                           int radType, int tubeNumber) {

    return 0.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JConeMagneticStructureOptionsD(parent, this);
    return adialog;
  }

  public class JConeMagneticStructureOptionsD extends JOptionsDialog {

    JTextField phiTF;
    JTextField thetaTF;
    JTextField coneangleTF;

    public JConeMagneticStructureOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());

      JPanel subpanel = new JPanel();
      subpanel.setLayout(new GridLayout(3, 1, 6, 6));
      principalPanel.add(subpanel, BorderLayout.CENTER);
      JPanel jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Phi angle: "));
      phiTF = new JTextField(Constants.FLOAT_FIELD);
      phiTF.setToolTipText("Phi angle (degrees) for the model");
      jPanel9.add(phiTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Theta angle: "));
      thetaTF = new JTextField(Constants.FLOAT_FIELD);
      thetaTF.setToolTipText("Theta angle (degrees) for the model");
      jPanel9.add(thetaTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Cone angle: "));
      coneangleTF = new JTextField(Constants.FLOAT_FIELD);
      coneangleTF.setToolTipText("Cone angle (degrees) for the model");
      jPanel9.add(coneangleTF);

      setTitle("Magnetic structure");
      initParameters();
      pack();
    }

    public void initParameters() {
      phiTF.setText(getPhi().getValue());
      addComponenttolist(phiTF, getPhi());
      thetaTF.setText(getTheta().getValue());
      addComponenttolist(thetaTF, getTheta());
      coneangleTF.setText(getConeAngle().getValue());
      addComponenttolist(coneangleTF, getConeAngle());
    }

    public void retrieveParameters() {
      setPhi(phiTF.getText());
      setTheta(thetaTF.getText());
      setConeAngle(coneangleTF.getText());
    }
  }

}
