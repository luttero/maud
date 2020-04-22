/*
 * @(#)GeometryDiffractometer.java created 10/01/1999 Pergine Vals.
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

package it.unitn.ing.rista.diffr.geometry;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 *  The GeometryDiffractometer is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2005/05/06 18:07:26 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryDiffractometer extends Geometry {

  public static String[] diclistc = {
    "_diffrn_radiation_monochromator", "_pd_instr_2theta_monochr_post",
    "_pd_instr_dist_src/samp", "_pd_instr_monochr_pre_spec",
    "_pd_instr_2theta_monochr_pre", "_pd_instr_divg_eq_src/samp",
    "_pd_instr_divg_slit_auto", "_pd_instr_divg_ax_src/samp",
    "_diffrn_radiation_polarisn_norm", "_diffrn_radiation_polarisn_ratio"

  };
  public static String[] diclistcrm = {
    "_diffrn_radiation_monochromator", "_pd_instr_2theta_monochr_post",
    "_pd_instr_dist_src/samp", "_pd_instr_monochr_pre_spec",
    "_pd_instr_2theta_monochr_pre", "_pd_instr_divg_eq_src/samp",
    "_pd_instr_divg_slit_auto", "_pd_instr_divg_ax_src/samp",
    "polarization normal (deg)", "polarization ratio"

  };

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  double slitaperture = 0.0;
	double slitaperturey = 0.0;
  double monochromatorCorrection = 1.0;

  public GeometryDiffractometer(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "General diffractometer";
    IDlabel = "General diffractometer";
    description = "General diffractometer instrument geometry";
  }

  public GeometryDiffractometer(XRDcat aobj) {
    this(aobj, "General diffractometer");
  }

  public GeometryDiffractometer() {
    identifier = "Disabled General diffractometer";
    IDlabel = "General diffractometer";
    description = "General diffractometer instrument geometry";
  }

  public void initConstant() {
    Nstring = 8;
    Nstringloop = 0;
    Nparameter = 2;
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
    setGoniometerRadius("175.0");
    radius = 175.0;
    stringField[0] = new String("filtered");
    setPostMonochromatorAngle("0");
    stringField[3] = new String("filtered");
    stringField[4] = new String("0");
	  setEquatorialSlitAperture("0.0");
	  setAxialSlitAperture("0.0");
    setAutomaticSlit(false);
    parameterField[0] = new Parameter(this, getParameterString(0), 0.0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 360.0));
    parameterField[1] = new Parameter(this, getParameterString(1), 0.0,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 1.0));
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < parameterField.length; i++) {
        if (source == parameterField[i]) {
            notifyParameterChanged(source, Constants.LORENTZ_POLARIZATION_CHANGED);
            return;
        }
      }
      super.notifyParameterChanged(source);
    }
  }

  public void setGoniometerRadius(String astring) {
    stringField[2] = new String(astring);
  }

  public String getGoniometerRadius() {
    return stringField[2];
  }

  public void setEquatorialSlitAperture(String astring) {
    stringField[5] = new String(astring);
  }

  public String getEquatorialSlitAperture() {
    return stringField[5];
  }

	public void setAxialSlitAperture(String astring) {
		stringField[7] = new String(astring);
	}

	public String getAxialSlitAperture() {
		return stringField[7];
	}

	public void setPostMonochromatorAngle(String value) {
    stringField[1] = new String(value);
  }

  public String getPostMonochromatorAngle() {
    if (stringField[1] != null)
      return stringField[1];
    else
      return "0.0";
  }

  public void setPreMonochromatorAngle(String value) {
    stringField[4] = new String(value);
  }

  public String getPreMonochromatorAngle() {
    if (stringField[4] != null)
      return stringField[4];
    else
      return "0.0";
  }

  public boolean getAutomaticSlit() {
    return stringField[6].equalsIgnoreCase("true");
  }

  public void setAutomaticSlit(boolean status) {
    if (status)
      stringField[6] = new String("true");
    else
      stringField[6] = new String("false");
  }

  public void setAutomaticSlit(String value) {
    stringField[6] = new String(value);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    // we cache the values here in doubles, so we don't need to transform them from Strings every time
    slitaperture = Double.valueOf(getEquatorialSlitAperture()).doubleValue();
	  slitaperturey = Double.valueOf(getAxialSlitAperture()).doubleValue();
    radius = Double.valueOf(getGoniometerRadius()).doubleValue();
    monochromatorCorrection =
            Math.cos(Double.valueOf(getPostMonochromatorAngle()).doubleValue() * Constants.DEGTOPI);// *
        //    Math.cos(Double.valueOf(getPreMonochromatorAngle()).doubleValue() * Constants.DEGTOPI / 2.0);
    monochromatorCorrection *= monochromatorCorrection;
  }

  double polarization = 0.0;
	double polarizationStar = 0.0;
	double polarizationStarRec = 0.0;
	double polarizationStarRecSqrt = 0.0;
  double polarizationAngle = 0.0;

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);
    polarizationAngle = getParameterValue(0) * Constants.DEGTOPI;
    polarization = getParameterValue(1);
    if (polarization < 0.0)
      polarization = 0.0;
    else if (polarization > 1.0)
      polarization = 1.0;
	  // we repeat to be sure
	  monochromatorCorrection =
			  Math.cos(Double.valueOf(getPostMonochromatorAngle()).doubleValue() * Constants.DEGTOPI);// *
	  //    Math.cos(Double.valueOf(getPreMonochromatorAngle()).doubleValue() * Constants.DEGTOPI / 2.0);
	  monochromatorCorrection *= monochromatorCorrection;
	  polarizationStar = (1.0 - polarization) / (1.0 + polarization);
/*	  if (polarizationStar > 0)
	    polarizationStarRec = (1.0 - polarizationStar) / polarizationStar;
	  polarizationStarRecSqrt = Math.sqrt(polarizationStarRec);*/

  }

	public double getPolarizationAmount() {
		return polarization;
	}

	public double getPolarizationAngle() {
		return polarizationAngle;
	}

	public double polarization(DiffrDataFile adatafile, double position) {
		if (((Instrument) getParent()).isNeutron() || ((Instrument) getParent()).isElectron())
			return 1.0;

		// X-ray
		double Ph = 0.0;
		if (position < Constants.PI) {
			double cos2theta = Math.cos(position);
			cos2theta *= cos2theta;
			double eta = adatafile.getEtaValue() * Constants.DEGTOPI + polarizationAngle;
			double coseta = Math.cos(eta);
			double sineta = Math.sin(eta);
			double cos2eta = coseta * coseta;
			double sin2eta = sineta * sineta;
			double Pm = getMonochromatorCorrection();
			Ph = 2.0 * ((polarizationStar * cos2eta + sin2eta) * cos2theta * Pm + polarizationStar * sin2eta + cos2eta)
					/ (1.0 + polarizationStar);
		}
		return Ph;
	}

	public double getMonochromatorCorrection() {
		return monochromatorCorrection;
	}

  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles,
                                     DiffrDataFile adatafile) {
    // we delegate this computation to the measurement object, because it could be different for
    // a position sensistive detector or a point one.
    double[] angles = getTrueTiltingAngles(adatafile, tilting_angles, x);
    return getMeasurement().getCorrectedPosition(asample, x, angles, getRadius(null), adatafile);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JGeometryGDOptionsD(parent, this);
    return adialog;
  }

  public class JGeometryGDOptionsD extends JOptionsDialog {

    JTextField goniometerradiusTF;
    JTextField incidentslitTF;
	  JTextField incidentslitAxialTF;
    JArmPanel firstArmP;
    JArmPanel secondArmP;
    JRadioButton progDivgRB;
    JRadioButton fixedDivgRB;

    public JGeometryGDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(3, 3));


      JTabbedPane tabPanel3 = new JTabbedPane();
      String tempString2[] = {"First arm", "Second arm"};
      principalPanel.add(BorderLayout.CENTER, tabPanel3);

      JPanel firstarmPanel = new JPanel();
      firstarmPanel.setLayout(new BorderLayout(6, 6));
      tabPanel3.addTab(tempString2[0], null, firstarmPanel);
      firstArmP = new JArmPanel(this);
      firstArmP.setBorder(new BevelBorder(BevelBorder.LOWERED));
      firstarmPanel.add("Center", firstArmP);

      JPanel secondarmPanel = new JPanel();
      secondarmPanel.setLayout(new BorderLayout(6, 6));
      tabPanel3.addTab(tempString2[1], null, secondarmPanel);
      secondArmP = new JArmPanel(this);
      secondArmP.setBorder(new BevelBorder(BevelBorder.LOWERED));
      secondarmPanel.add("Center", secondArmP);

      JPanel jp1 = new JPanel();
      jp1.setLayout(new GridLayout(0, 2, 6, 6));
      jp1.add(new JLabel("Goniometer radius (mm):"));
      goniometerradiusTF = new JTextField(Constants.FLOAT_FIELD);
      goniometerradiusTF.setToolTipText("Insert the goniometer radius in mm");
      jp1.add(goniometerradiusTF);
      jp1.add(new JLabel("Equatorial slit (degrees):"));
      incidentslitTF = new JTextField(Constants.FLOAT_FIELD);
      incidentslitTF.setToolTipText("Insert the equatorial incident slit angle in degrees");
      jp1.add(incidentslitTF);
	    jp1.add(new JLabel("Axial slit (degrees):"));
	    incidentslitAxialTF = new JTextField(Constants.FLOAT_FIELD);
	    incidentslitAxialTF.setToolTipText("Insert the Axial incident slit angle in degrees");
	    jp1.add(incidentslitAxialTF);
      ButtonGroup rbg = new ButtonGroup();
      fixedDivgRB = new JRadioButton("Fixed slit aperture");
      fixedDivgRB.setSelected(true);
      fixedDivgRB.setToolTipText("Select this if the slits have a fixed aperture");
      jp1.add(fixedDivgRB);
      rbg.add(fixedDivgRB);
      progDivgRB = new JRadioButton("Programmable slits");
      progDivgRB.setToolTipText("Select this if the sample illumination area is fixed");
      jp1.add(progDivgRB);
      rbg.add(progDivgRB);
      principalPanel.add(BorderLayout.SOUTH, jp1);

      setTitle("Diffractometer geometry");
      initParameters();
      pack();
    }

    public void initParameters() {
      goniometerradiusTF.setText(getGoniometerRadius());
      incidentslitTF.setText(getEquatorialSlitAperture());
	    incidentslitAxialTF.setText(getAxialSlitAperture());
      progDivgRB.setSelected(getAutomaticSlit());

      if (getString(0).equalsIgnoreCase("filtered") ||
              getString(0).equalsIgnoreCase("none")) {
        secondArmP.mon = true;
        secondArmP.initFiltered();
      } else {
        secondArmP.mon = false;
        secondArmP.initMonochromator(getString(1));
      }
      secondArmP.initListener();
      if (getString(3).equalsIgnoreCase("filtered") ||
              getString(3).equalsIgnoreCase("none")) {
        firstArmP.mon = true;
        firstArmP.initFiltered();
      } else {
        firstArmP.mon = false;
        firstArmP.initMonochromator(getString(4));
      }
      firstArmP.initListener();
    }

    /**
     * This method is automatically called when the user press the close button on the dialog.
     */
    public void retrieveParameters() {

      setGoniometerRadius(new String(goniometerradiusTF.getText()));
      setEquatorialSlitAperture(new String(incidentslitTF.getText()));
	    setAxialSlitAperture(new String(incidentslitAxialTF.getText()));
      setAutomaticSlit(progDivgRB.isSelected());

      if (secondArmP.mon) {
        setString(0, "Monochromator");
        setString(1, secondArmP.getAngle());
      } else {
        setString(0, "Filtered");
        setString(1, secondArmP.getAngle());
      }
      if (firstArmP.mon) {
        setString(3, "Monochromator");
        setString(4, firstArmP.getAngle());
      } else {
        setString(3, "none");
        setString(4, firstArmP.getAngle());
      }
    }
  }

}
