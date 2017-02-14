/*
 * @(#)ChooseAtomD.java created 17/09/2000 Casalino
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.FlowLayout;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.chemistry.*;

/**
 * The ChooseAtomD display a dialog to select the atom type from a periodic table.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ChooseAtomD extends myJDialog {

  JComboBox oxidationchoice;
  JComboBox isotopechoice;

  public ChooseAtomD(Frame parent, String atomLabel) {
    super(parent, "Choose atom type and oxidation", true);

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout());

    JPanel p1 = new JPanel();
    p1.setLayout(new FlowLayout());
    c1.add("North", p1);
    PeriodicTable periodicTableP = new PeriodicTable(this,
            AtomSite.stripIsotopeNumber(AtomSite.stripOxidation(atomLabel)));
    p1.add(periodicTableP);

    p1 = new JPanel();
    p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 12, 3));
    c1.add("Center", p1);

    p1.add(new JLabel("Isotope:"));
    isotopechoice = new JComboBox();
    rebuildIsotopeList(AtomSite.stripIsotopeNumber(AtomSite.stripOxidation(atomLabel)),
            AtomInfo.getIsotopicNumber(atomLabel));
//		oxidationchoice.setSelectedIndex(AtomInfo.getOxidationNumber(atomLabel));
    p1.add(isotopechoice);

    p1.add(new JLabel("Oxidation state:"));
    oxidationchoice = new JComboBox();
    rebuildOxidationList(AtomSite.stripIsotopeNumber(AtomSite.stripOxidation(atomLabel)),
            AtomInfo.getOxidationNumber(atomLabel));
//		oxidationchoice.setSelectedIndex(AtomInfo.getOxidationNumber(atomLabel));
    p1.add(oxidationchoice);

    selectedAtomLabel = AtomSite.stripIsotopeNumber(AtomSite.stripOxidation(atomLabel));

//		p1 = new JPanel();
//		p1.setLayout(new FlowLayout(FlowLayout.RIGHT,5,5));
//		c1.add("South", p1);
    JButton closeButton = new JCloseButton();
    p1.add(closeButton);
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
      }
    });

    getRootPane().setDefaultButton(closeButton);
//		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
//		setTitle("Choose atom type and oxidation");

//		createDefaultMenuBar();

    pack();
  }

  void rebuildOxidationList(String atomLabel, String oxidationState) {
    Vector oxList = AtomInfo.getOxidationStates(atomLabel);
    int oxNumber = oxList.size();
    if (oxidationchoice.getItemCount() > 0)
      oxidationchoice.removeAllItems();
    for (int i = 0; i < oxNumber; i++) {
      String label = (String) oxList.elementAt(i);
      if (label.equals(""))
        label = "none";
      oxidationchoice.addItem(label);
//			System.out.println(oxList.elementAt(i));
    }
    if (oxidationState.equals(""))
      oxidationState = "none";
    oxidationchoice.setSelectedItem(oxidationState);
  }

  void rebuildIsotopeList(String atomLabel, String isotopeNumber) {
    Vector isoList = AtomInfo.getIsotopeNumbers(atomLabel);
    int isoNumber = isoList.size();
    if (isotopechoice.getItemCount() > 0)
      isotopechoice.removeAllItems();
    for (int i = 0; i < isoNumber; i++) {
      String label = (String) isoList.elementAt(i);
      if (label.equals(""))
        label = "natural";
      isotopechoice.addItem(label);
//			System.out.println(oxList.elementAt(i));
    }
    if (isotopeNumber.equals(""))
      isotopeNumber = "natural";
    isotopechoice.setSelectedItem(isotopeNumber);
  }

  String selectedAtomLabel = "";

  public void setAtomLabel(String atomLabel) {
    selectedAtomLabel = atomLabel;
    rebuildOxidationList(atomLabel, "");
    rebuildIsotopeList(atomLabel, "");
  }

  public String getSelectedAtom() {
    return selectedAtomLabel;
  }

  public String getSelectedOxidation() {
    String oxidationState = oxidationchoice.getSelectedItem().toString();
    if (oxidationState.equals("none"))
      oxidationState = "";
    return oxidationState;
  }

  public String getSelectedIsotope() {
    String isotopeState = isotopechoice.getSelectedItem().toString();
    if (isotopeState.equals("natural"))
      isotopeState = "";
    return isotopeState;
  }

  public String getSelectedAtomLabel() {
    return getSelectedIsotope() + getSelectedAtom() + getSelectedOxidation();
  }

  public static void getAtomType(Frame aframe, AtomScatterer anatom) {
    ChooseAtomD chooseAtomD = new ChooseAtomD(aframe, anatom.getAtomSymbol());
    chooseAtomD.setVisible(true);
    while (chooseAtomD.isVisible()) {
      try {
        Thread.currentThread().sleep(100);
      } catch (InterruptedException e) {
      }
    }
	 anatom.setAtomSymbol(chooseAtomD.getSelectedAtomLabel());
    chooseAtomD.dispose();
    chooseAtomD = null;
  }

	public static String getAtomType(Frame aframe, String anatom) {
		ChooseAtomD chooseAtomD = new ChooseAtomD(aframe, anatom);
		chooseAtomD.setVisible(true);
		while (chooseAtomD.isVisible()) {
			try {
				Thread.currentThread().sleep(100);
			} catch (InterruptedException e) {
			}
		}
		anatom = chooseAtomD.getSelectedAtomLabel();
		return anatom;
	}

}

