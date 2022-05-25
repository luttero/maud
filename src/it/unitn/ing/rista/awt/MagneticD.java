/*
 * @(#)MagneticD.java created 01/01/1997 Mesiano
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
import javax.swing.*;
import java.net.*;
import java.awt.event.*;
import java.lang.*;

import it.unitn.ing.rista.awt.myJFrame;
import it.unitn.ing.rista.diffr.*;

/**
 * The MagneticD is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MagneticD extends myJFrame {

  JComboBox magneticModelsCB = null;

  Phase thephase;

  public MagneticD(Frame parent, Phase aphase) {
    super(parent);

    setOwnPosition = true;

    createDefaultMenuBar();

    thephase = aphase;

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));
    JPanel jPanel2 = new JPanel();
    jPanel2.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
    c1.add("South", jPanel2);
    JButton jb = new JCloseButton();
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        Ok_Clicked();
      }
    });
    jPanel2.add(jb);
    getRootPane().setDefaultButton(jb);
    jb = new JCancelButton();
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        Cancel_Clicked();
      }
    });
    jPanel2.add(jb);

    JPanel magneticPanel = new JPanel();
    c1.add("Center", magneticPanel);
    magneticPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    magneticPanel.add(new JLabel("Magnetic structure model: "));
    magneticModelsCB = new JComboBox();
    magneticModelsCB.setToolTipText("Select the magnetic structure model to be used by this phase");
    for (int i = 0; i < thephase.magneticStructureModelsNumber(); i++)
      magneticModelsCB.addItem(thephase.getsubordIdentifier(thephase.getMagneticStructureID(), i));
    magneticPanel.add(magneticModelsCB);
    jb = new JButton("Magnetic structure options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        magneticOptions();
      }
    });
    magneticPanel.add(jb);

    setTitle("Magnetic Structure");
    pack();

    initParameters();
  }

  public MagneticD(Frame parent, Phase aphase, String title) {
    this(parent, aphase);
    setTitle(title);
  }

  void Cancel_Clicked() {
    setVisible(false);
    dispose();
  }

  void Ok_Clicked() {
    retrieveParameters();
    Cancel_Clicked();
  }

  public void retrieveParameters() {
    thephase.setMagneticStructureModel(magneticModelsCB.getSelectedItem().toString());
  }

  public void initParameters() {
    magneticModelsCB.setSelectedItem(thephase.getMagneticStructureModel());
  }

  public void magneticOptions() {
    String selectedMagnetic = magneticModelsCB.getSelectedItem().toString();
    if (!thephase.getMagneticStructureModel().equals(selectedMagnetic))
      thephase.setMagneticStructureModel(selectedMagnetic);
    thephase.getActiveMagneticStructure().getOptionsDialog(this).setVisible(true);
  }

  public void dispose() {
    thephase = null;
    super.dispose();
  }
}
