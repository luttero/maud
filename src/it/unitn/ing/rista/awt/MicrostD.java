/*
 * @(#)MicrostD.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.awt;

import java.awt.*;
import java.awt.event.*;

import it.unitn.ing.rista.awt.myJFrame;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;

import it.unitn.ing.rista.util.*;

/**
 * The MicrostD is a class  No more in use.....
 *
 * @version $Revision: 1.5 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MicrostD extends myJFrame {

  Phase thephase;
  JComboBox[] modelCB = null;
  JTextField grainsize;

  public MicrostD(Frame parent, Phase aphase) {
    super(parent);

    setOwnPosition = true;

    createDefaultMenuBar();

    thephase = aphase;

    Container principalpane = getContentPane();

    principalpane.setLayout(new BorderLayout());

    JPanel linebroadmethod = new JPanel();
    principalpane.add(BorderLayout.NORTH, linebroadmethod);
    linebroadmethod.setLayout(new GridLayout(4, 1, 6, 6));
    linebroadmethod.setBorder(new TitledBorder(
            new BevelBorder(BevelBorder.LOWERED), "Line Broadening"));

    String[] labelstring = {"Line Broadening model:",
                            "Size-Strain model: ",
                            "Antiphase boundary model: ",
                            "Planar defects model: "
    };


    String[] tooltipString = {"Select the Line Broadening model to be used by this phase",
                              "Select the method to symmetrize anisotropic crystallites and microstrains",
                              "Select the antiphase boundary broadening model of super-reflections (only for fcc intermetallics)",
                              "Select the planar defect broadening model (only for fcc/bcc/hcp)"
    };

    String[] tooltipButton = {"No options for this",
                              "Edit the crystallite and microstrain values",
                              "Edit the antiphase probability",
                              "Edit deformation/growth and twin faults"
    };

    modelCB = new JComboBox[4];

    for (int j = 1; j < 5; j++) {

      final int index = j - 1;
      JPanel jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 2, 2));
      linebroadmethod.add(jp1);
      jp1.add(new JLabel(labelstring[index]));
      modelCB[index] = new JComboBox();
      modelCB[index].setToolTipText(tooltipString[index]);
      for (int i = 0; i < thephase.getsubordClassNumber(j); i++)
        modelCB[index].addItem(thephase.getsubordIdentifier(j, i));
      jp1.add(modelCB[index]);
      JButton jb = new JIconButton("Eyeball.gif", "Options");
      jb.setToolTipText(tooltipButton[index]);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          broadeningOptions(index + 1);
        }
      });
      jp1.add(jb);
    }

    JPanel jp1 = new JPanel();
    jp1.setLayout(new BorderLayout());
    principalpane.add(BorderLayout.SOUTH, jp1);
    JPanel microabsPanel = new JPanel();
    microabsPanel.setBorder(new TitledBorder(
            new BevelBorder(BevelBorder.LOWERED), "Microabsorption correction"));
    microabsPanel.setLayout(new BorderLayout());
    jp1.add(BorderLayout.CENTER, microabsPanel);
    JPanel firstP = new JPanel(new FlowLayout());
    microabsPanel.add(firstP, BorderLayout.NORTH);
    firstP.add(new JLabel("Grain size (microns):"));
    grainsize = new JTextField(Constants.FLOAT_FIELD);
    grainsize.setText("0");
    firstP.add(grainsize);
    JSubordinatePanel subordinatePanels = new JSubordinatePanel(this, thephase, thephase.microAbsorptionID,
            "Microabsorption ",
            "Choose a microabsorption model");
    microabsPanel.add(subordinatePanels, BorderLayout.CENTER);

    JPanel jp2 = new JPanel();
    jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    jp1.add(BorderLayout.SOUTH, jp2);

    JButton jb1 = new JCancelButton();
    jb1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    jp2.add(jb1);

    jb1 = new JCloseButton();
    jb1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        retrieveParameters();
        setVisible(false);
        dispose();
      }
    });
    jp2.add(jb1);

    getRootPane().setDefaultButton(jb1);

    setTitle("Microstructure");

    grainsize.setText(new String(thephase.getAbsorptionCrystSize().getValue()));
    addComponenttolist(grainsize, thephase.getAbsorptionCrystSize());

    for (int j = 1; j < 5; j++)
      modelCB[j - 1].setSelectedItem(((XRDcat) thephase.subordinateField[j]).identifier);

    pack();

  }

  public MicrostD(Frame parent, Phase aphase, String title) {
    this(parent, aphase);
    setTitle(title);
  }

  public void retrieveParameters() {
    thephase.setAbsorptionCrystSize(grainsize.getText());

    for (int i = 1; i < 5; i++) {
      String value = modelCB[i - 1].getSelectedItem().toString();
      if (thephase.subordinateField[i] == null ||
              !value.equals(((XRDcat) thephase.subordinateField[i]).identifier)) {
        if (i == 4 && thephase.getClosePackedType() == -1)
          thephase.setsubordinateField(i, "none pd");
        else
          thephase.setsubordinateField(i, value);
      }
    }
  }

  public void broadeningOptions(int index) {
    String value = modelCB[index - 1].getSelectedItem().toString();
    if (thephase.subordinateField[index] == null ||
            !value.equals(((XRDcat) thephase.subordinateField[index]).identifier))
      if (index == 4 && thephase.getClosePackedType() == -1) {
        thephase.setsubordinateField(index, "none pd");
        modelCB[index - 1].setSelectedItem("none pd");
      } else
        thephase.setsubordinateField(index, value);

    ((XRDcat) thephase.subordinateField[index]).getOptionsDialog(this).setVisible(true);
  }

  public void dispose() {
    thephase = null;
    super.dispose();
  }

}
