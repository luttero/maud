/*
 * @(#)TriaxialStress.java created 25/03/2025 Casalino
 *
 * Copyright (c) 2025 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.io.BufferedWriter;

/**
 *  The ThermoMechanicalProcess is a class used in EPSC4
 *
 * @version $Revision: 1.00 $, $Date: 2025/03/25 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class ThermoMechanicalProcess extends XRDcat {
  // Dictionary, here we define the CIF string for each parameter and option in the model, will be used for
  // saving and loading files
  public static String[] diclistc = {
      "_rista_epsc_tm_process_filename", // 0
      "_rista_epsc_tm_process_title",
      "_rista_epsc_tm_process_nsteps",
      "_rista_epsc_tm_process_ctrl_var",
      "_rista_epsc_tm_process_boundary_cond", // 4

      "_rista_epsc_tm_process_ietbc_11", // 5
      "_rista_epsc_tm_process_ietbc_12",
      "_rista_epsc_tm_process_ietbc_13",
      "_rista_epsc_tm_process_ietbc_21",
      "_rista_epsc_tm_process_ietbc_22",
      "_rista_epsc_tm_process_ietbc_23",
      "_rista_epsc_tm_process_ietbc_31",
      "_rista_epsc_tm_process_ietbc_32",
      "_rista_epsc_tm_process_ietbc_33",
      "_rista_epsc_tm_process_istbc_11", // 14
      "_rista_epsc_tm_process_istbc_12",
      "_rista_epsc_tm_process_istbc_13",
      "_rista_epsc_tm_process_istbc_22",
      "_rista_epsc_tm_process_istbc_23",
      "_rista_epsc_tm_process_istbc_33",

      "_rista_epsc_tm_process_temp_start", // 20
      "_rista_epsc_tm_process_temp_delta",
      "_rista_epsc_tm_process_elastic_t_dep",
      "_rista_epsc_tm_process_iref_et",
      "_rista_epsc_tm_process_iref_st", // 24

      "_rista_epsc_tm_process_etbc_11", // 0
      "_rista_epsc_tm_process_etbc_12",
      "_rista_epsc_tm_process_etbc_13",
      "_rista_epsc_tm_process_etbc_21",
      "_rista_epsc_tm_process_etbc_22",
      "_rista_epsc_tm_process_etbc_23",
      "_rista_epsc_tm_process_etbc_31",
      "_rista_epsc_tm_process_etbc_32",
      "_rista_epsc_tm_process_etbc_33", // 8
      "_rista_epsc_tm_process_stbc_11", // 9
      "_rista_epsc_tm_process_stbc_12",
      "_rista_epsc_tm_process_stbc_13",
      "_rista_epsc_tm_process_stbc_22",
      "_rista_epsc_tm_process_stbc_23",
      "_rista_epsc_tm_process_stbc_33" // 14

  };

  // these are the corresponding labels that will appear in the GUI in the parameter list window etc.
  public static String[] diclistcrm = {
      "_rista_epsc_tm_process_filename",
      "_rista_epsc_tm_process_title",
      "_rista_epsc_tm_process_nsteps",
      "_rista_epsc_tm_process_ctrl_var",
      "_rista_epsc_tm_process_boundary_cond",
      "_rista_epsc_tm_process_",
      "_rista_epsc_tm_process_",
      "_rista_epsc_tm_process_",
      "_rista_epsc_tm_process_",
      "_rista_epsc_tm_process_"
  };

  // this model does not have subobjects, so the class list for subobjects is empty
  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  // here we define the model string to appear in the GUI and the description string
  // change it accordingly to your method, it should be an unique identifier
  final static String id = "Thermo Mechanical Process";
  final static String desc = "Describe a Thermo Mechanical Process to be used in EPSC4";

  // Constructors, and init methods do not change the code between the two stars lines
  // **********************************************************************************
  public ThermoMechanicalProcess(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = id;
    IDlabel = id;
    description = desc;
  }

  public ThermoMechanicalProcess(XRDcat aobj) {
    this(aobj, id);
  }

  public ThermoMechanicalProcess() {
    identifier = id;
    IDlabel = id;
    description = desc;
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

  public void initConstant() {
    Nstring = 25;    // number of options, treated as strings only, in this case only the first
    Nstringloop = 0;  // no vectors of strings for options
    Nparameter = 15;   // 0 parameters refinables in the model
    Nparameterloop = 0;  // no parameter vectors in this model, to be used when the number of parameters may
    // change in the model and/or is defined by other options
    Nsubordinate = 0;    // no subobjects or subordinate objects
    Nsubordinateloop = 0;  // no vectors of subobjects
  }

  public void initParameters() {
    super.initParameters();

    stringField[0] = "";
    stringField[1] = "Describe the thermomechanical process";
    stringField[2] = "100";
    stringField[3] = "3";
    stringField[4] = "1";
    for (int i = 5; i < 13; i++)
      stringField[i] = "";
    stringField[13] = "1";
    for (int i = 14; i < 19; i++)
      stringField[i] = "1";
    stringField[19] = "0";

    stringField[20] = "298";
    stringField[21] = "0.0";
    stringField[22] = "0";
    stringField[23] = "0";
    stringField[24] = "0";

    for (int i = 0; i < 9; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 0,
        ParameterPreferences.getDouble(getParameterString(i) + ".min", -0.1),
        ParameterPreferences.getDouble(getParameterString(i) + ".max", 0.1));
    for (int i = 9; i < 15; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
        ParameterPreferences.getDouble(getParameterString(i) + ".min", -1),
        ParameterPreferences.getDouble(getParameterString(i) + ".max", 1));

    refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
  }

  // this is the main method that perform the actual computation of the strain based on the crystallographic
  // angles of the hkl reflection (psi, beta), and angles of orientation of the sample respect to the diffraction
  // vector (chi and phi)

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
        for (int i = 0; i < parameterField.length; i++) {
          if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
            return;
          }
        }
      super.notifyParameterChanged(source);
    }
  }

  public String getFilename() {
    if (stringField[0] == "") {
      XRDcat parent = getParent();
      for (int i = 0; i < parent.numberofelementSubL(EPSCmodel.THERMO_MECHANICAL_ID); i++) {
        if (parent.subordinateloopField[EPSCmodel.THERMO_MECHANICAL_ID].elementAt(i) == this) {
          stringField[0] = ((Phase) getParent().getParent()).getPhaseName() + "_" + + i + ".pro";
          break;
        }
      }
    }
    return stringField[0];
  }

  public String getTitle() {
    return stringField[1];
  }

  public String getStrainsBoundaryCondition(int i, int j) {
    return stringField[4 + (i - 1) * 3 + j];
  }

  public void setStrainsBoundaryCondition(int i, int j, String value) {
    setString(4 + (i - 1) * 3 + j, value);
  }

  public String getStrain(int i, int j) {
    return parameterField[(i - 1) * 3 + j - 1].getValue();
  }

  public void setStrain(int i, int j, String value) {
    getParameter((i - 1) * 3 + j - 1).setValue(value);
  }

  public String getStressesBoundaryCondition(int i) {
    return stringField[14 + i];
  }

  public void setStressesBoundaryCondition(int i, String value) {
    setString(14 + i, value);
  }

  public String getStress(int i) {
    return parameterField[9 + i].getValue();
  }

  public void setStress(int i, String value) {
    getParameter(9 + i).setValue(value);
  }

  public void writeInputFile() {
    String filename = getFilename();
    BufferedWriter output = null;
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);

        output.write("EPSC Process Input File");
        output.newLine();
        output.write(getTitle() + ":");
        output.newLine();
        output.write("__________________________________");
        output.newLine();
        output.write(stringField[2] + "     Number of steps in process, nsteps");
        output.newLine();
        output.write(stringField[3] + "      Process Control Variable, i_control_var  [0=temp, 1-6=corresponding known etbc(1-6) or stbc(1-6) component");
        output.newLine();
        output.write(stringField[4] + "      Relative or absolute boundary conditions, i_bc_mode [0 for relative BC, 1 for absolute BC]");
        output.newLine();
        output.write("__________________________________");
        output.newLine();

        output.write("STRAINS");
        output.newLine();
        output.write("Boundary Conditions of Deformation Step, ietbc");
        output.newLine();
        for (int i = 1; i < 4; i++) {
          for (int j = 1; j < 4; j++)
            output.write(getStrainsBoundaryCondition(i, j) + " ");
          output.newLine();
        }
        output.write("Total Deformation Tensor for the Process, etbc");
        output.newLine();
        for (int i = 1; i < 4; i++) {
          for (int j = 1; j < 4; j++)
            output.write(getStrain(i, j) + "  ");
          output.newLine();
        }
        output.write("__________________________________");
        output.newLine();

        output.write("STRESSES");
        output.newLine();
        output.write("Boundary Conditions of Stress, istbc");
        output.newLine();
        output.write(stringField[14] + " " + stringField[15] + " " + stringField[16]);
        output.newLine();
        output.write("  " + stringField[17] + " " + stringField[18]);
        output.newLine();
        output.write("    " + stringField[19]);
        output.newLine();
        output.write("Total Stress Tensor for the Process, stbc");
        output.newLine();
        output.write(parameterField[9].getValue() + "  " + parameterField[10].getValue() + "  " + parameterField[11].getValue());
        output.newLine();
        output.write("             " + parameterField[12].getValue() + "  " + parameterField[13].getValue());
        output.newLine();
        output.write("                               " + parameterField[14].getValue());
        output.newLine();
        output.write("__________________________________");
        output.newLine();

        output.write("TEMPERATURE");
        output.newLine();
        output.write("Starting Temperature, temp_s");
        output.newLine();
        output.write(stringField[20]);
        output.newLine();
        output.write("Temperature Increment, deltemp (change in temperature per step - positive or negative)");
        output.newLine();
        output.write(stringField[21]);
        output.newLine();
        output.write("Enforced Temperature Dependence on Elastic Constants (1=Zirconium or 0=Not Zirconium)");
        output.newLine();
        output.write(stringField[22]);
        output.newLine();
        output.write("Define reference values for Macro and Grain Strain Prior to Beginning Process (1=YES, 0=NO)");
        output.newLine();
        output.write(stringField[23] + "              i_ref_et");
        output.newLine();
        output.write("Define reference values for Grain Stress Prior to Beginning Process (1=YES, 0=NO)");
        output.newLine();
        output.write(stringField[24] + "              i_ref_st");
        output.newLine();

        output.flush();
        output.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }


  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new ThermoMechanicalProcess.JTSProcessOptionsD(parent, this);
  }

  class JTSProcessOptionsD extends JOptionsDialog {

    JTextField[] generalTF = new JTextField[5];
    JTextField[] temperatureTF = new JTextField[5];
    JTextField[][] ietbcTF = new JTextField[3][3];
    JTextField[] istbcTF = new JTextField[6];
    JTextField[][] parsETBC = new JTextField[3][3];
    JTextField[] parsSTBC = new JTextField[6];

    String[] generalLabels = {
        "Process filename:        ",
        "Process title/info:      ",
        "Number of process steps: ",
        "Process control flag:    ",
        "Boundary conditions type:"};
    String[] generalHelp = {
        "The filename used for input in epsc4 (should be unique, .pro extension)",
        "Info or title for this process",
        "Number of steps in the process, nsteps",
        "i_control_var  [0=temp, 1-6=corresponding known etbc(1-6) or stbc(1-6) component",
        "Relative or absolute boundary conditions, i_bc_mode [0 for relative BC, 1 for absolute BC]"};

    String[] temperatureLabels = {
        "Start. temperature (K): ",
        "Temperature increment:  ",
        "E temperature dependent:",
        "Strain reference values:",
        "Stress reference values:"};
    String[] temperatureHelp = {
        "Starting temperature in Kelvin for the process, temp_s",
        "Temperature increment in Kelvin (positive or negative), deltatemp",
        "Specify if the elastic constants are temperature dependent (1=yes, 0=No)",
        "Define a strain reference value before the process (1=yes, 0=No), i_ref_et",
        "Define a stress reference value before the process (1=yes, 0=No), i_ref_st"};

    public JTSProcessOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, jPanel8);
      for (int i = 0; i < generalLabels.length; i++) {
        jPanel8.add(new JLabel(generalLabels[i]));
        generalTF[i] = new JTextField(50);
        jPanel8.add(generalTF[i]);
        generalTF[i].setToolTipText(generalHelp[i]);
      }

      JTabbedPane tabPanel = new JTabbedPane();
      principalPanel.add(tabPanel, BorderLayout.CENTER);

      JPanel tensorPanel = new JPanel(new BorderLayout(3, 3));
      tabPanel.addTab("Strain BC", tensorPanel);
      JPanel labelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
      tensorPanel.add(labelPanel, BorderLayout.NORTH);
      labelPanel.add(new JLabel("Boundary conditions on deformation step"));
      JPanel centerPanel = new JPanel(new GridLayout(4, 4));
      tensorPanel.add(centerPanel, BorderLayout.CENTER);
      for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              centerPanel.add(new Label("Strains B.C."));
            else if (i == 0)
              centerPanel.add(new Label(Integer.toString(j)));
            else
              centerPanel.add(new Label(Integer.toString(i)));
          } else {
            centerPanel.add(ietbcTF[i-1][j-1]);
            ietbcTF[i-1][j-1].setText(getStrainsBoundaryCondition(i, j));
            ietbcTF[i-1][j-1].setToolTipText("0=no strain BC, 1=strain BC");
          }
        }
      }
      centerPanel = new JPanel(new GridLayout(4, 4));
      tensorPanel.add(centerPanel, BorderLayout.EAST);
      for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              centerPanel.add(new Label("Total deformation"));
            else if (i == 0)
              centerPanel.add(new Label(Integer.toString(j)));
            else
              centerPanel.add(new Label(Integer.toString(i)));
          } else {
            centerPanel.add(parsETBC[i-1][j-1]);
            parsETBC[i-1][j-1].setToolTipText("Applied strain");
          }
        }
      }

      JPanel tensorSPanel = new JPanel(new BorderLayout(3, 3));
      tabPanel.addTab("Stress BC", tensorSPanel);
      JPanel labelSPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
      tensorSPanel.add(labelSPanel, BorderLayout.NORTH);
      labelSPanel.add(new JLabel("Boundary conditions on stresses"));
      JPanel centerSPanel = new JPanel(new GridLayout(4, 4));
      tensorSPanel.add(centerSPanel, BorderLayout.CENTER);
      int index = 0;
      for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              centerSPanel.add(new Label("Stresses B.C."));
            else if (i == 0)
              centerSPanel.add(new Label(Integer.toString(j)));
            else
              centerSPanel.add(new Label(Integer.toString(i)));
          } else {
            if (i > j)
              centerSPanel.add(new JLabel(" "));
            else {
              centerSPanel.add(istbcTF[index]);
              istbcTF[index].setText(getStressesBoundaryCondition(index));
              istbcTF[index].setToolTipText("0=no stress BC, 1=stress BC");
              index++;
            }
          }
        }
      }
      centerSPanel = new JPanel(new GridLayout(4, 4));
      tensorSPanel.add(centerSPanel, BorderLayout.EAST);
      index = 0;
      for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              centerSPanel.add(new Label("Applied stress"));
            else if (i == 0)
              centerSPanel.add(new Label(Integer.toString(j)));
            else
              centerSPanel.add(new Label(Integer.toString(i)));
          } else {
            if (i > j)
              centerSPanel.add(new JLabel(" "));
            else {
              centerSPanel.add(parsSTBC[index]);
              parsSTBC[index].setToolTipText("Applied stress (same unit as elastic tensor)");
              index++;
            }
          }
        }
      }

      JPanel tensorTPanel = new JPanel(new BorderLayout(3, 3));
      tabPanel.addTab("Temperature", tensorTPanel);
      JPanel labelTPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
      tensorTPanel.add(labelTPanel, BorderLayout.NORTH);
      labelTPanel.add(new JLabel("Temperature process settings"));
      JPanel centerTPanel = new JPanel(new GridLayout(0, 2));
      tensorTPanel.add(centerTPanel, BorderLayout.CENTER);
      for (int i = 0; i < temperatureLabels.length; i++) {
        centerTPanel.add(new JLabel(temperatureLabels[i]));
        temperatureTF[i] = new JTextField(Constants.FLOAT_FIELD);
        centerTPanel.add(temperatureTF[i]);
        temperatureTF[i].setToolTipText(temperatureHelp[i]);
      }

      setTitle("Process parameters");
      initParameters();
      pack();

    }

    public void initParameters() {
      for (int i = 0; i < generalTF.length; i++)
        generalTF[i].setText(stringField[i]);
      int index = 0;
      for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++) {
          parsETBC[i][j].setText(parameterField[index].getValue());
          addComponenttolist(parsETBC[i][j], parameterField[index++]);
        }
      for (int i = 0; i < 6; i++) {
        parsSTBC[i].setText(parameterField[i + 9].getValue());
        addComponenttolist(parsSTBC[i], parameterField[i + 9]);
      }
      for (int i = 0; i < temperatureLabels.length; i++)
        temperatureTF[i].setText(stringField[20 + i]);
    }

    public void retrieveParameters() {
      for (int i = 0; i < generalTF.length; i++)
        stringField[i] = generalTF[i].getText();
      for (int i = 1; i < 4; i++)
        for (int j = 1; j < 4; j++)
          setStrainsBoundaryCondition(i, j, ietbcTF[i-1][j-1].getText());
      int index = 0;
      for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++)
          parameterField[index++].setValue(parsETBC[i][j].getText());
      for (int i = 0; i < 6; i++)
        parameterField[i + 9].setValue(parsSTBC[i].getText());
      for (int i = 0; i < temperatureLabels.length; i++)
        stringField[20 + i] = temperatureTF[i].getText();
    }

  }

}
