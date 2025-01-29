/*
 * @(#)EPSCmodel.java created 13/12/2024 Povo
 *
 * Copyright (c) 2024 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rsa;

    import java.awt.*;
    import javax.swing.*;

    import it.unitn.ing.rista.diffr.*;
    import it.unitn.ing.rista.awt.*;
    import it.unitn.ing.rista.util.*;
    import it.unitn.ing.rista.diffr.rta.*;

    import javax.swing.border.*;
    import java.io.*;

/**
 *  The EPSCmodel is a class to compute the diffraction shift from
 *  the EPSC model of C. N. Tomè and R. A. Lebenshon, LANL.
 *  Based on EPSC4 version
 *
 * @version $Revision: 1.0 $, $Date: 2024/12/13 12:44:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class EPSCmodel extends Strain {

  public static String[] diclistc = {
      "_rista_epsc_grain_shape_ctrl",
      "_rista_epsc_ellipsoid_ratio_x",
      "_rista_epsc_ellipsoid_ratio_y",
      "_rista_epsc_ellipsoid_ratio_z",
      "_rista_residual_stress_use_texture",

      "_rista_stiffness_11", // 1
      "_rista_stiffness_12", // 2
      "_rista_stiffness_13", // 3
      "_rista_stiffness_14", // 4
      "_rista_stiffness_15", // 5
      "_rista_stiffness_16", // 6
//                                      "_rista_stiffness_21",
      "_rista_stiffness_22", // 7
      "_rista_stiffness_23", // 8
      "_rista_stiffness_24", // 9
      "_rista_stiffness_25", // 10
      "_rista_stiffness_26", // 11
//                                      "_rista_stiffness_31",
//                                      "_rista_stiffness_32",
      "_rista_stiffness_33", // 12
      "_rista_stiffness_34", // 13
      "_rista_stiffness_35", // 14
      "_rista_stiffness_36", // 15
//                                      "_rista_stiffness_41",
//                                      "_rista_stiffness_42",
//                                      "_rista_stiffness_43",
      "_rista_stiffness_44", // 16
      "_rista_stiffness_45", // 17
      "_rista_stiffness_46", // 18
//                                      "_rista_stiffness_51",
//                                      "_rista_stiffness_52",
//                                      "_rista_stiffness_53",
//                                      "_rista_stiffness_54",
      "_rista_stiffness_55", // 19
      "_rista_stiffness_56", // 20
//                                      "_rista_stiffness_61",
//                                      "_rista_stiffness_62",
//                                      "_rista_stiffness_63",
//                                      "_rista_stiffness_64",
//                                      "_rista_stiffness_65",
      "_rista_stiffness_66", // 21
      "_rista_macrostress_11",
      "_rista_macrostress_22",
      "_rista_macrostress_33",
      "_rista_macrostress_23",
      "_rista_macrostress_13",
      "_rista_macrostress_12"
  };
  public static String[] diclistcrm = {
      "_rista_residual_stress_model",
      "_rista_residual_stress_use_texture",
      "voigt-reuss weight",
      "stiffness_11 (arb)", // 1
      "stiffness_12 (arb)", // 2
      "stiffness_13 (arb)", // 3
      "stiffness_14 (arb)", // 4
      "stiffness_15 (arb)", // 5
      "stiffness_16 (arb)", // 6
//                                      "_rista_stiffness_21",
      "stiffness_22 (arb)", // 7
      "stiffness_23 (arb)", // 8
      "stiffness_24 (arb)", // 9
      "stiffness_25 (arb)", // 10
      "stiffness_26 (arb)", // 11
//                                      "_rista_stiffness_31",
//                                      "_rista_stiffness_32",
      "stiffness_33 (arb)", // 12
      "stiffness_34 (arb)", // 13
      "stiffness_35 (arb)", // 14
      "stiffness_36 (arb)", // 15
//                                      "_rista_stiffness_41",
//                                      "_rista_stiffness_42",
//                                      "_rista_stiffness_43",
      "stiffness_44 (arb)", // 16
      "stiffness_45 (arb)", // 17
      "stiffness_46 (arb)", // 18
//                                      "_rista_stiffness_51",
//                                      "_rista_stiffness_52",
//                                      "_rista_stiffness_53",
//                                      "_rista_stiffness_54",
      "stiffness_55 (arb)", // 19
      "stiffness_56 (arb)", // 20
//                                      "_rista_stiffness_61",
//                                      "_rista_stiffness_62",
//                                      "_rista_stiffness_63",
//                                      "_rista_stiffness_64",
//                                      "_rista_stiffness_65",
      "stiffness_66 (arb)", // 21
      "macrostress_11 (arb)",
      "macrostress_22 (arb)",
      "macrostress_33 (arb)",
      "macrostress_23 (arb)",
      "macrostress_13 (arb)",
      "macrostress_12 (arb)"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  Sample actualsample = null;

  public static String[] stressModels = {"Voigt", "Reuss", "Hill", "PathGEO", "BulkPathGEO"};
//	int actuallayer = 0;

  int irandom = 1;
  double pfthreshold_tmp = 0.05;
  double[] hj = new double[9], hjm = new double[9];
  //  int[][] mij = new int[3][3];
  double pi, pif, p2i, pi5, pi25, pisim;
  double[][] facun = new double[6][6];
  int[][] mij = new int[6][2];
  int[][] mik = new int[3][3];
  int[] ifiw = new int[73];
  double[] cr2 = new double[181], sr2 = new double[181], cr4 = new double[181], sr4 = new double[181],
      cr6 = new double[181], sr6 = new double[181], cr8 = new double[181], sr8 = new double[181];
  double[][] trigs = new double[9][73];
  double[][] e0 = new double[6][6];
  double[][] s0 = new double[6][6];
  double[][] c0 = new double[6][6];
  double[][] egeom = new double[6][6], ea = new double[6][6];
  double[][] svoigt = new double[6][6];
  double s12l, c12l, s23l, c23l, s31l, c31l;
  double cda, cdb;
  double[] spmas = new double[9], spmasm = new double[9];
  double[][][] sshi0 = null, sshi0m = null;
  double[][][] hs = new double[36][36][9];
  //  double fio[73][37][73];
  double[][][][] sgeofull = new double[3][3][3][3];
  //	double f[73][37][73];
  double[][] wwarir = new double[36][36];
  double[][] wwarim = new double[36][36];
  //  int iadd[] = new int[9885];
//  int c00 = 0;
  public static int c6 = 6;
  int c73 = 73;
  int c37 = 37;
  double phon = 0.;

  boolean debug_output = MaudPreferences.getBoolean("momentPoleStress.debug", false);
  boolean log_output = false;
  boolean siegfried_strict = false;

  public static final int[] mi = {1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 2, 3, 4,
      5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6};
  public static final int[] mj = {1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6, 1, 1, 1,
      1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5};
  public static final int[] mivoigt = {1, 2, 3, 2, 3, 1, 3, 1, 2};
  public static final int[] mjvoigt = {1, 2, 3, 3, 1, 2, 2, 3, 1};

  private int actualReflexIndex = 0;

  public EPSCmodel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Moment Pole Stress";
    IDlabel = "Moment Pole Stress";
    description = "select this to apply the Moment Pole Stress method of Siegfried Matthies";
  }

  public EPSCmodel(XRDcat aobj) {
    this(aobj, "Moment Pole Stress");
  }

  public EPSCmodel() {
    identifier = "Moment Pole Stress";
    IDlabel = "Moment Pole Stress";
    description = "select this to apply the Moment Pole Stress method of Siegfried Matthies";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 28;
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

    double s11 = 168.4;
    double s12 = 121.4;
    parameterField[0] = new Parameter(this, getParameterString(0), 0.5,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));
    for (int i = 1; i < 22; i++) {
      if (i == 1 || i == 7 || i == 12) // 11, 22, 33
        parameterField[i] = new Parameter(this, getParameterString(i), s11,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 1000));
      else if (i == 16 || i == 19 || i == 21)  // 44, 55, 66
        parameterField[i] = new Parameter(this, getParameterString(i), 2.0 * (s11 - s12),
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 500));
      else if (i == 2 || i == 3 || i == 8)  // 12, 13, 23
        parameterField[i] = new Parameter(this, getParameterString(i), s12,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 300));
      else
        parameterField[i] = new Parameter(this, getParameterString(i), 0,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", -100),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 100));
    }
    for (int i = 22; i < 28; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", -1),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 1));
    }

    refreshComputation = true;
  }

  double[] macrostress = new double[6];
  int imodel = 0;

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);

    imodel = getStressModelValue();
    fio_tmp = null;

    int k = 1;
    for (int i = 0; i < 6; i++)
      for (int j = 0; j < 6; j++) {
        if (i <= j)
          e0[i][j] = parameterValues[k++];
        else
          e0[i][j] = e0[j][i];
      }
    int factor = 1;
    for (int i = 0; i < 6; i++) {
      if (i == 3) factor++;
      macrostress[i] = parameterValues[k++] * factor;
    }
    checkForSymmetries(e0);
  }

  public void checkForSymmetries(double[][] e0) {
  }

  public String getStressModelID() {
    return stringField[0];
  }

  public int getStressModelValue() {

    String modelID = getStressModelID();

    for (int i = 0; i < stressModels.length; i++) {
      if (modelID.equals(stressModels[i]))
        return i;
    }
    return 0;
  }

  public void setStressModel(int i) {
    setStressModel(stressModels[i]);
  }

  public void setStressModel(String value) {
    stringField[0] = value;
  }

  public boolean useTexture() {
    return stringField[1].equalsIgnoreCase("true");
  }

  public void useTexture(boolean status) {
    if (status)
      stringField[1] = "true";
    else
      stringField[1] = "false";
  }

  public void useTexture(String value) {
    stringField[1] = value;
  }

/*  public void computeStrain(Sample asample) {

    Phase aphase = getPhase();
    computeStrain(aphase, asample);

  }*/

  double[][][] fio_tmp = null;
  double bk0_tmp = 0.0;
  OutputStream out = null;

  public void prepareComputation(Phase aphase, Sample asample) {
    log_output = /* getFilePar().isStrainComputationPermitted() && */getFilePar().logOutput();
    if (log_output)
      out = getFilePar().getResultStream();
    update(false);
    int igb = SpaceGroups.getLGNumberSiegfriedConv(aphase.getPointGroup());
    int iga = 1;
    phon = 0.0;

    if (useTexture())
      irandom = 0;
    else
      irandom = 1;

//    double fmin = 10.0;
    Texture atexture = aphase.getActiveTexture();
    atexture.initializeAll();

    double resolution = 5.0; //atexture.getResolutionD(); // for the moment to be changed to variable in the future

    int alphaMaxIndex = (int) (360.0 / resolution + 1.00001);
    int betaMaxIndex = (int) (180.0 / resolution + 1.00001);
    int gammaMaxIndex = (int) (360.0 / resolution + 1.00001);

    fio_tmp = new double[alphaMaxIndex][betaMaxIndex][gammaMaxIndex];
    double odf_min = 1E30;
    double odf_max = -1E30;
    for (int ia = 0; ia < alphaMaxIndex; ia++)
      for (int ib = 0; ib < betaMaxIndex; ib++)
        for (int ig = 0; ig < gammaMaxIndex; ig++) {
          double alpha = resolution * (.25 + ia);
          if (alpha > 360.0)
            alpha -= 360.0;
          double beta = resolution * (.25 + ib);
          if (beta > 180.0)
            beta -= 180.0;
          double gamma = resolution * (.25 + ig);
          if (gamma > 360.0)
            gamma -= 360.0;
          fio_tmp[ia][ib][ig] = atexture.getODF(alpha * Constants.DEGTOPI,
              beta * Constants.DEGTOPI, gamma * Constants.DEGTOPI);
          if (fio_tmp[ia][ib][ig] < odf_min)
            odf_min = fio_tmp[ia][ib][ig];
          if (fio_tmp[ia][ib][ig] > odf_max)
            odf_max = fio_tmp[ia][ib][ig];
//          System.out.println("fio "+fio_tmp[ia][ib][ig]);
        }
//    if (fmin == 1.0)
//      irandom = 1;
//		System.out.println("ODF min/max: " + odf_min + " - " + odf_max);
    if (log_output) {
      try {
        printString(out, "Use texture in moment pole stress computation : " );
        if (irandom == 1)
          printLine(out, "no" );
        else
          printLine(out, "yes");
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
//    System.out.println("irandom "+irandom);

/*    prepare1();
    if (imodel != 0)
      subhkwkw();
    if (irandom != 1 && MoreMath.pow_ii(imodel) > 0)
      prepare3();
    if (pfthreshold_tmp < .001)
      pfthreshold_tmp = .001;
    bk0_tmp = e0prep(imodel);
    igaigbprep(iga, igb, irandom);
    double[][][] fio_corrected = null;
    if (irandom != 1)
      fio_corrected = odfprep(iga, igb, fio_tmp);
    if (MoreMath.pow_ii(imodel) > 0)
      bulkprep(imodel, irandom, fio_corrected);
*/
    int hkln = aphase.gethklNumber();
    sshi0 = new double[hkln][6][6];
    sshi0m = new double[hkln][6][6];
    double[] cdsc = aphase.lattice();
    for (int j = 0; j < hkln; j++) {
      Reflection refl = aphase.getReflectionVector().elementAt(j);

      double[] sctf = Uwimvuo.tfhkl(refl.getH(), refl.getK(), refl.getL(),
          cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
/*      subshi0(j, sctf[0], sctf[1], sctf[2], sctf[3]); */
    }
  }

  public double computeStrain(Reflection refl, double[] strain_angles) { // you don't need to modify this unless
    actualReflexIndex = getPhase().getReflexIndex(refl);
    return super.computeStrain(refl, strain_angles);
  }

  public double computeStrain(double psi, double beta, double chi, double phi) {
    // Angles must be in radiants
    // psi and beta are the polar and azimuthal angles for the crystal setting
    // phi and chi for the sample

    double cfhi = Math.cos(beta);
    double sfhi = Math.sin(beta);
    double cthi = Math.cos(psi);
    double sthi = Math.sin(psi);
    double ctyj = Math.cos(chi);
    double styj = Math.sin(chi);
    double cfyj = Math.cos(phi);
    double sfyj = Math.sin(phi);
    if (debug_output) {
      try {
        printLine(out, "fhi " + beta * Constants.PITODEG + ", " +
            "thi " + psi * Constants.PITODEG + ", " +
            "tyj " + chi * Constants.PITODEG + ", " +
            "fyj " + phi * Constants.PITODEG
        );
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
/*    double[] sla33 = subsla33(imodel, irandom, bk0_tmp,
        sthi, cthi, sfhi, cfhi, 1,
        styj, ctyj,
        sfyj, cfyj, pfthreshold_tmp, fio_tmp);*/
    double strain33 = 0.0;

/*    for (int i = 0; i < 6; i++)
      strain33 += sla33[i] * macrostress[i];*/
    return strain33;
  }


  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTSStrainOptionsD(parent, this);
    return adialog;
  }

  class JTSStrainOptionsD extends JOptionsDialog {

    JComboBox ssmodelCB = null;
    JCheckBox textureCB;
    JTextField[] pars = null;
    JTextField[] cijTF = null;
    String[] labels = {
        "Grain shape ctrl (0-2): ",
        "    Ellipsoid ratio x : ",
        "    Ellipsoid ratio y : ",
        "    Ellipsoid ratio z : ",
        "    Orientation alpha : ",
        "    Orientation beta  : ",
        "    Orientation gamma : ",
        "    Max iterations (100) : ",
        "    Orientation gamma : ",
        "    Orientation gamma : ",
    };

    public JTSStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      pars = new JTextField[labels.length];

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPaneln = new JPanel();
      jPaneln.setLayout(new BorderLayout(6, 6));
      principalPanel.add(BorderLayout.NORTH, jPaneln);
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      jPaneln.add(BorderLayout.WEST, jPanel8);

      for (int i = 1; i < labels.length; i++) {
        JPanel jpl = new JPanel();
        jpl.setLayout(new FlowLayout(FlowLayout.LEFT));
        jPanel8.add(jpl);
        jpl.add(new JLabel(labels[i]));
        pars[i] = new JTextField(Constants.FLOAT_FIELD);
        pars[i].setText("0");
        jpl.add(pars[i]);
      }

      JPanel jPanel6 = new JPanel();
      jPanel6.setLayout(new GridLayout(0, 1, 3, 3));
      jPaneln.add(BorderLayout.CENTER, jPanel6);
      JPanel jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      jPanel7.add(new JLabel("Stress/strain model: "));
      ssmodelCB = new JComboBox();
      for (int i = 0; i < stressModels.length; i++)
        ssmodelCB.addItem(stressModels[i]);
      ssmodelCB.setToolTipText("Select the micromechanical model for strain computation from macrostresses");
      jPanel7.add(ssmodelCB);

      jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      jPanel7.add(new JLabel(labels[0]));
      pars[0] = new JTextField(Constants.FLOAT_FIELD);
      pars[0].setText("0");
      jPanel7.add(pars[0]);

      jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      textureCB = new JCheckBox("Use texture ODF");
      textureCB.setToolTipText("Check the box to use the ODF for strain computation from stress and stiffness tensors");
      jPanel7.add(textureCB);

      jPanel8 = new JPanel();
      jPanel8.setBorder(new TitledBorder(
          new BevelBorder(BevelBorder.LOWERED), "Stiffness matrix"));
      jPanel8.setLayout(new GridLayout(0, 6, 1, 1));
      principalPanel.add(BorderLayout.CENTER, jPanel8);

      cijTF = new JTextField[21];
      int ij = 0;
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          if (i <= j) {
            cijTF[ij] = new JTextField(Constants.FLOAT_FIELD);
            cijTF[ij].setText("0");
            jPanel8.add(cijTF[ij++]);
          } else
            jPanel8.add(new JLabel("-"));
        }
      }

      setTitle("Moment pole figures options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
//      pars[0].setText(parameterField[0].getValue());
      addComponenttolist(pars[0], parameterField[0]);
      for (int i = 1; i < labels.length; i++) {
//        pars[i].setText(parameterField[i + 21].getValue());
        addComponenttolist(pars[i], parameterField[i + 21]);
      }
      for (int i = 0; i < 21; i++) {
//        System.out.println(i + " " + cijTF[i] + " " + parameterField[i+1]);
//        cijTF[i].setText(parameterField[i + 1].getValue());
        addComponenttolist(cijTF[i], parameterField[i + 1]);
      }
      ssmodelCB.setSelectedItem(getStressModelID());
      textureCB.setSelected(useTexture());
    }

    public void retrieveParameters() {
      parameterField[0].setValue(pars[0].getText());
      for (int i = 1; i < labels.length; i++) {
        parameterField[i + 21].setValue(pars[i].getText());
      }
      for (int i = 0; i < 21; i++) {
        parameterField[i + 1].setValue(cijTF[i].getText());
      }
      setStressModel(ssmodelCB.getSelectedItem().toString());
      useTexture(textureCB.isSelected());
    }

  }

}

