/*
 * @(#)MicromechanicD.java created 16/11/1999 Pergine Vals.
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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
import java.awt.event.*;
import java.lang.*;

import it.unitn.ing.rista.awt.myJFrame;
import it.unitn.ing.rista.diffr.*;

/**
 *  The MicromechanicD implements a Frame for visual editing of the characteristics of
 * an object of classes Stress and Strain.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MicromechanicD extends myJFrame {

  JComboBox strainModelsCB = null;
//	JComboBox stressModelsCB = null;

  Phase thephase;

  public MicromechanicD(Frame parent, Phase aphase) {
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

    JPanel strainPanel = new JPanel();
    c1.add("North", strainPanel);
    strainPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    strainPanel.add(new JLabel("Stress/Strain model: "));
    strainModelsCB = new JComboBox();
    strainModelsCB.setToolTipText("Select the stress/strain model to be used by this phase");
    for (int i = 0; i < thephase.strainModelsNumber(); i++)
      strainModelsCB.addItem(thephase.getsubordIdentifier(thephase.getStrainID(), i));
    strainPanel.add(strainModelsCB);
    jb = new JButton("Stress/Strain options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        strainOptions();
      }
    });
    strainPanel.add(jb);

/*		JPanel stressPanel = new JPanel();
		c1.add("Center", stressPanel);
		stressPanel.setLayout(new FlowLayout(FlowLayout.CENTER,6,6));
		stressPanel.add(new JLabel("Stress model: "));
		stressModelsCB = new JComboBox();
		stressModelsCB.setToolTipText("Select the stress model to be used by this phase");
		for (int i = 0; i < thephase.stressModelsNumber(); i++)
			stressModelsCB.addItem(thephase.getsubordIdentifier(thephase.getStressID(), i));
		stressPanel.add(stressModelsCB);
		jb = new JButton("Stress model options");
		jb.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				stressOptions();
			}
		});
		stressPanel.add(jb);*/

    initParameters();

    setTitle("Strain and Stress models");

    pack();

  }

  public MicromechanicD(Frame parent, Phase aphase, String title) {
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
    thephase.setStrainModel(strainModelsCB.getSelectedItem().toString());
//		thephase.setStressModel(stressModelsCB.getSelectedItem().toXRDcatString());
  }

  public void initParameters() {
    strainModelsCB.setSelectedItem(thephase.getStrainModel());
//		stressModelsCB.setSelectedItem(thephase.getStressModel());
  }

  public void strainOptions() {
    String selectedStrain = strainModelsCB.getSelectedItem().toString();
    if (!thephase.getStrainModel().equals(selectedStrain))
      thephase.setStrainModel(selectedStrain);
    thephase.getActiveStrain().getOptionsDialog(this).setVisible(true);
  }

/*	public void stressOptions() {
		String selectedStress = stressModelsCB.getSelectedItem().toXRDcatString();
		if (!thephase.getStressModel().equals(selectedStress))
			thephase.setStressModel(selectedStress);
		thephase.getActiveStress().getOptionsDialog(this).setVisible(true);
	}*/

  public void dispose() {
    thephase = null;
    super.dispose();
  }
}
