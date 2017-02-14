package it.unitn.ing.esqui.client;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** ClientAnalysis.java
 * <br>
 * Title:			<b>ESQUI Client Analysis</b>
 * </br>
 * Description:	Abstract class for instancing different kind of
 analyses for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		January 2001
 * @comment:		none
 */

abstract class ClientAnalysis {

  Client client;

//	Define the ESQUIGO software commands (11)
  String[] esquigoCommands = {
    "KV",
    "MA",
    "MT",
    "MF",
    "CT",
    "NC",
    "SP",
    "EP",
    "ST",
    "SA",
    "OF"
  };

//	Define the information strings for the textarea
  String[] infoString = {"kV",
                         "mA",
                         "Measure type",
                         "Filter",
                         "Counting Time",
                         "Channels",
                         "2-theta",
                         "omega",
                         "chi",
                         "phi",
                         "x",
                         "y",
                         "z"
  };
  
  String typeOfAnalysis = "not defined";

//	Define the max index for movements
  public static final int maxIndex = 3;


//	Define the value for no motors movement
  public static final int noMove = 999;

/*
	Arrays containing the MIN value, MAX value and STEP
	for the parameters. Values are ordered same way as in the
	ESQUIGO software.
*/
  float[] z = new float[maxIndex];
  float[] y = new float[maxIndex];
  float[] x = new float[maxIndex];
  float[] phi = new float[maxIndex];
  float[] chi = new float[maxIndex];
  float[] omega = new float[maxIndex];
  float[] twotheta = new float[maxIndex];
  int ct = 0;
  float kV = 0;
  float mA = 0;
  int channels = 0;
  int movefilter = 0;
  String measuretype = null;

  String title = null;
  AnalysisFrame analysisFrame = null;
  JButton confirmSettings = null;

  JTextField[] zFields = null;
  JTextField[] yFields = null;
  JTextField[] xFields = null;
  JTextField[] phiFields = null;
  JTextField[] chiFields = null;
  JTextField[] omegaFields = null;
  JTextField[] twothetaFields = null;
//	This array shoulkd be created for each analysis, considering the temporary
//	options number available for the analysis. I will override the method
//	'createAdvancedSettingsPanel' adding the new options panel.
  JTextField[] optionsFields = new JTextField[3];
  String[] channelValues = {"8192",
                            "4096",
                            "2048",
                            "1024",
                            "512",
                            "256",
                            "128"
  };
  JComboBox channelBox = new JComboBox((String[]) channelValues);
  JTextField analysisTimeField = null;

  Vector allOptionsVector = new Vector(5, 1);
  Vector allMovementsVector = new Vector(20, 1);
  int separator = 0;

  public ClientAnalysis(String title) {
    this.title = title;
  }

//		Create the new frame
  void showFrame(String title) {
    if (analysisFrame != null) {
      analysisFrame.toFront();
      return;
    }
    analysisFrame = new AnalysisFrame(this);
    analysisFrame.addAdvSettingsPanel(createAdvancedSettingsPanel(analysisFrame));
    setAnalysisTime();
    analysisFrame.displayInformation(getAnalysisInformation());
    analysisFrame.pack();
    analysisFrame.setResizable(false);
    ClientMisc.locateOnScreen(analysisFrame, ClientMisc.X_WINDOW_RELPOS, 8);
    analysisFrame.setVisible(true);
  }

  public void updateParameters() {
    setTextFields(zFields, z);
    setTextFields(yFields, y);
    setTextFields(xFields, x);
    setTextFields(phiFields, phi);
    setTextFields(chiFields, chi);
    setTextFields(omegaFields, omega);
    setTextFields(twothetaFields, twotheta);
    optionsFields[0].setText(String.valueOf(ct));
    optionsFields[1].setText(String.valueOf(kV));
    optionsFields[2].setText(String.valueOf(mA));
    channelBox.setSelectedItem(String.valueOf(channels));
  }

  public void setDefaultParameters() {
  }

  public void setActualParameters() {
    z = getParameters(zFields);
    y = getParameters(yFields);
    x = getParameters(xFields);
    phi = getParameters(phiFields);
    chi = getParameters(chiFields);
    omega = getParameters(omegaFields);
    twotheta = getParameters(twothetaFields);
    ct = getIntParameter(optionsFields[0]);
    kV = getFloatParameter(optionsFields[1]);
    mA = getFloatParameter(optionsFields[2]);
    channels = Integer.parseInt((String) channelBox.getSelectedItem());
    movefilter = 0;
  }

  JPanel createAdvancedSettingsPanel(JFrame tmpFrame) {
    JPanel centralPanel = new JPanel(new KappaLayout());
//		Create the movement textfield panel
    JPanel movementsPanel = new JPanel(new GridLayout(8, 4, 2, 2));
    movementsPanel.setBorder(ClientMisc.newBorder("Movement settings", 3));
    String[] advSettingsLabels = {"", "min", "max", "step"};
    for (int i = 0; i < advSettingsLabels.length; i++) {
      JLabel tmpMovLabel = new JLabel(advSettingsLabels[i]);
      tmpMovLabel.setHorizontalAlignment(JLabel.CENTER);
      movementsPanel.add(tmpMovLabel);
    }
//		Create the textfields
    FieldListener tmpListener = new ParameterFieldListener(tmpFrame);

    zFields = createArrayTextFields(movementsPanel, "z", tmpListener);
    yFields = createArrayTextFields(movementsPanel, "y", tmpListener);
    xFields = createArrayTextFields(movementsPanel, "x", tmpListener);
    phiFields = createArrayTextFields(movementsPanel, "phi", tmpListener);
    chiFields = createArrayTextFields(movementsPanel, "chi", tmpListener);
    omegaFields = createArrayTextFields(movementsPanel, "omega", tmpListener);
    twothetaFields = createArrayTextFields(movementsPanel, "twotheta", tmpListener);

    centralPanel.add(movementsPanel, "0,0,,,1,,");

//		Create the general options textfield panel
    JPanel rightPanel = new JPanel(new BorderLayout());
    JPanel optionsPanel = new JPanel(new KappaLayout());
    optionsPanel.setBorder(ClientMisc.newBorder("Acquisition settings", 3));
    String[] optionsLabels = {"Counting time (s)",
                              "Current (mA)",
                              "Tube Voltage (kV)"
    };
    int i;
    for (i = 0; i < optionsLabels.length; i++) {
      optionsPanel.add(new JLabel(optionsLabels[i]), "0," + i + ",1,1,7,,2");
      optionsFields[i] = createJTextField(tmpListener);
      optionsFields[i].setColumns(7);
      optionsPanel.add(optionsFields[i], "1," + i + ",1,1,,,2");
    }
    JLabel channelBoxLabel = new JLabel("Channels (number)");
    optionsPanel.add(channelBoxLabel, "0," + (i + 1) + ",1,1,7,,2");
    optionsPanel.add(channelBox, "1," + (i + 1) + ",1,1,,,2");
    channelBox.setEditable(false);
    channelBox.setSelectedIndex(1);
    JLabel analysisTimeLabel = new JLabel("Analysis required time");
    optionsPanel.add(analysisTimeLabel, "0," + (i + 2) + ",2,1,,,2");
    analysisTimeField = new JTextField(10);
    optionsPanel.add(analysisTimeField, "0," + (i + 3) + ",2,1,,,2");

    rightPanel.add(optionsPanel, BorderLayout.NORTH);
//		Create some buttons to perform useful actions
    JPanel usefulButtonPanel = new JPanel();
    JButton infoAreaClearButton = ClientMisc.createButton("CleanUp.gif", "clear analysis info area");
    infoAreaClearButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        analysisFrame.displayInformation(null);
      }
    });
    usefulButtonPanel.add(infoAreaClearButton);
    JButton setDefaultButton = ClientMisc.createButton("RotCWRight.gif", "set default parameters");
    setDefaultButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setDefaultParameters();
        updateParameters();
        setAnalysisTime();
        analysisFrame.displayInformation(getAnalysisInformation());
      }
    });
    usefulButtonPanel.add(setDefaultButton);
    confirmSettings = ClientMisc.createButton("Check.gif", "confirm the new analysis settings");
    confirmSettings.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          setActualParameters();
        } catch (NumberFormatException numexc) {
          ClientMisc.messageBox(analysisFrame, "Alert", "Some (highlighted) values are wrong!", JOptionPane.WARNING_MESSAGE);
          return;
        }
        setAnalysisTime();
        analysisFrame.displayInformation(getAnalysisInformation());
      }
    });
    usefulButtonPanel.add(confirmSettings);
    rightPanel.add(usefulButtonPanel, BorderLayout.SOUTH);
    centralPanel.add(rightPanel, "1,0,,,1,,");

    return centralPanel;
  }

  void addToAllMovementsVector(float[] tmpFloat) {
    for (int i = 0; i < tmpFloat.length; i++)
      allMovementsVector.addElement(new Float(tmpFloat[i]));
  }

  JTextField createJTextField(FieldListener tmpListener) {
    JTextField tmpTextField = new JTextField();
    tmpTextField.setHorizontalAlignment(JTextField.CENTER);
    tmpTextField.addFocusListener(tmpListener);
    return tmpTextField;
  }

  JTextField[] createArrayTextFields(JPanel tmpPanel, String tmpString,
                                     FieldListener tmpListener) {
    JTextField[] fields = new JTextField[maxIndex];
    for (int i = 0; i < maxIndex; i++) {
      fields[i] = createJTextField(tmpListener);
    }
    tmpPanel.add(new JLabel(tmpString));
    for (int i = 0; i < maxIndex; i++)
      tmpPanel.add(fields[i]);
    return fields;
  }

  public String getTitle() {
    return title + " analysis";
  }

  public String getMenuTitle() {
    return title;
  }

  public void getAnalysisVector() {
//	Initialize the vector
    allOptionsVector.clear();
    allMovementsVector.clear();
//	Create a vector containg alll the analysis parameters
    allOptionsVector.addElement(new Float(kV));
    allOptionsVector.addElement(new Float(mA));
    allOptionsVector.addElement(measuretype);
    allOptionsVector.addElement(new Integer(movefilter));
    allOptionsVector.addElement(new Integer(ct));
    allOptionsVector.addElement(new Integer(channels));
    separator = allOptionsVector.size();
    addToAllMovementsVector(twotheta);
    addToAllMovementsVector(omega);
    addToAllMovementsVector(chi);
    addToAllMovementsVector(phi);
    addToAllMovementsVector(x);
    addToAllMovementsVector(y);
    addToAllMovementsVector(z);
  }

  public String getAnalysisFile() {
    return null;
  }

  public void setNewLine(StringBuffer tmpBuffer) {
    tmpBuffer.append("\n");
  }

  public void setBlankSpace(StringBuffer tmpBuffer) {
    tmpBuffer.append(" ");
  }

  public void setParameters(float[] parameters, float[] values) {
    int arrayLength = parameters.length;
    for (int i = 0; i < arrayLength; i++) {
      parameters[i] = values[i];
    }
  }

  public void setTextFields(JTextField[] movTextField, float[] parameters) {
    int arrayLength = parameters.length;
    for (int i = 0; i < arrayLength; i++) {
      if ((int) parameters[i] == noMove) {
        movTextField[i].setText("-");
      } else {
        movTextField[i].setText(String.valueOf(parameters[i]));
      }
    }
  }

  public float[] getParameters(JTextField[] movParFields) throws NumberFormatException {
    int arrayLength = movParFields.length;
    String tmpString = null;
    float tmpArray[] = new float[arrayLength];
    for (int i = 0; i < arrayLength; i++) {
      tmpString = movParFields[i].getText().trim();
      if (tmpString.equals("-"))
        tmpString = "999";
      tmpArray[i] = Float.parseFloat(tmpString);
    }
    return tmpArray;
  }

  public float getFloatParameter(JTextField optParField) {
    float tmpValue = 0;
    String tmpString = optParField.getText().trim();
    try {
      tmpValue = Float.parseFloat(tmpString);
    } catch (NumberFormatException numexc) {
      numexc.printStackTrace();
    }
    return tmpValue;
  }

  public int getIntParameter(JTextField optParField) {
    int tmpValue = (int) getFloatParameter(optParField);
    return tmpValue;
  }

  public String getAnalysisInformation() {
    StringBuffer tmpBuffer = new StringBuffer();
    for (int i = 0; i < separator; i++)
      tmpBuffer.append(infoString[i] + "\t" + allOptionsVector.elementAt(i) + "\n\r");
    tmpBuffer.append("\tmin\tmax\tstep\n\r");
    int i = 0;
    int j = 0;
    while ((separator + j) < infoString.length) {
      tmpBuffer.append(infoString[separator + j]);
      j++;
      for (; i < (j * 3); i++) {
        if (((Float) allMovementsVector.elementAt(i)).intValue() == noMove) {
          tmpBuffer.append("\t" + "-");
        } else {
          tmpBuffer.append("\t" + allMovementsVector.elementAt(i));
        }
      }
      tmpBuffer.append("\n\r");

    }
    return tmpBuffer.toString();
  }

  public void sendAnalysis(String analysisString) throws Exception {
    if (Connection.getConnectionActive()) {
      Connection.sendString(ServerConnection.analysisCommands[0]);
      Connection.sendString(analysisString);
      Connection.sendString(ServerConnection.analysisCommands[1]);
    } else {
      ClientMisc.messageBox(analysisFrame, "Alert", "Connection to server is down!", JOptionPane.WARNING_MESSAGE);
      throw new Exception();
    }
  }

  String analysisTime() {
    try {
      long numberOfMovements = 1;
      for (int i = 0; i < allMovementsVector.size(); i++) {
        float tmpValue1 = ((Float) allMovementsVector.elementAt(i++)).floatValue();
        float tmpValue2 = ((Float) allMovementsVector.elementAt(i++)).floatValue();
        float tmpValue3 = ((Float) allMovementsVector.elementAt(i)).floatValue();
        if (tmpValue3 != (float) noMove)
          numberOfMovements = numberOfMovements * (long) ((tmpValue2 - tmpValue1) / tmpValue3 + 1);
      }
      float totalSeconds = numberOfMovements * ct;
      float remainder = (totalSeconds % 3600) / 3600;
      long hours = (long) (totalSeconds / 3600);
      long minutes = (long) (remainder * 60);
      long seconds = (long) (((remainder * 60) - minutes) * 60);
      return new String(String.valueOf(hours) + "h " +
              String.valueOf(minutes) + "m " +
              String.valueOf(seconds) + "s");
    } catch (Exception exc) {
      return new String("********");
    }
  }

  public void setAnalysisTime() {
    getAnalysisVector();
    analysisTimeField.setBackground(Color.yellow);
    analysisTimeField.setFont(new Font("Arial", Font.BOLD, 12));
    analysisTimeField.setHorizontalAlignment(JTextField.CENTER);
    analysisTimeField.setText(analysisTime());
  }

  public String getAnalysisTime() {
    try {
      return analysisTimeField.getText();
    } catch (NullPointerException nullexc) {
      return analysisTime();
    }
  }
}
