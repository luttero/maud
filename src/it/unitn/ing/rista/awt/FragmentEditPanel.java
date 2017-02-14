/*
 * @(#)FragmentEditPanel.java created Aug 29, 2004 Pergine Valsugana
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.Fragment;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.Coordinates;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The FragmentEditPanel is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2004/08/30 09:41:27 $
 * @since JDK1.1
 */

public class FragmentEditPanel extends JPanel {
  private Fragment m_Fragment;
  private myJFrame parentFrame;
  protected JTextField textXCoord;
  protected JTextField textYCoord;
  protected JTextField textZCoord;
  protected JButton btnResetCoords;

  protected JTextField textAAngle;
  protected JTextField textBAngle;
  protected JTextField textCAngle;
  protected JButton btnResetAngles;
  AtomPanel atomPanel;


  public FragmentEditPanel(Fragment m_Fragment, myJFrame parentFrame, AtomPanel atomPanel) {
    super();
    this.m_Fragment = m_Fragment;
    this.parentFrame = parentFrame;
    this.atomPanel = atomPanel;
    initComponents();
    initParameters();
  }

  public void initParameters() {
    parentFrame.addComponenttolist(textXCoord, m_Fragment.getX());
    parentFrame.addComponenttolist(textYCoord, m_Fragment.getY());
    parentFrame.addComponenttolist(textZCoord, m_Fragment.getZ());
    parentFrame.addComponenttolist(textAAngle, m_Fragment.getA());
    parentFrame.addComponenttolist(textBAngle, m_Fragment.getB());
    parentFrame.addComponenttolist(textCAngle, m_Fragment.getC());
  }

  public void retrieveParameters() {
    m_Fragment.getX().setValue(textXCoord.getText());
    m_Fragment.getY().setValue(textYCoord.getText());
    m_Fragment.getZ().setValue(textZCoord.getText());
    m_Fragment.getA().setValue(textAAngle.getText());
    m_Fragment.getB().setValue(textBAngle.getText());
    m_Fragment.getC().setValue(textCAngle.getText());
  }

  private void initComponents() {
    setLayout(new FlowLayout());
    setBorder(new TitledBorder("Fragment coordinates"));
    JPanel panelCoords = new JPanel(new GridLayout(0, 1));
    textXCoord = new JTextField("0", Constants.FLOAT_FIELD);
    textYCoord = new JTextField("0", Constants.FLOAT_FIELD);
    textZCoord = new JTextField("0", Constants.FLOAT_FIELD);
    btnResetCoords = new JButton("Reset center");
    JPanel panelAngles = new JPanel(new GridLayout(0, 1));
    textAAngle = new JTextField("0", Constants.FLOAT_FIELD);
    textBAngle = new JTextField("0", Constants.FLOAT_FIELD);
    textCAngle = new JTextField("0", Constants.FLOAT_FIELD);
    btnResetAngles = new JButton("Reset angles");

    JPanel panelXCoord = new JPanel(new FlowLayout());
    panelXCoord.add(new JLabel("X"));
//    textXCoord.setPreferredSize(new java.awt.Dimension(64, 20));
    panelXCoord.add(textXCoord);
    panelCoords.add(panelXCoord);

    JPanel panelYCoord = new JPanel(new FlowLayout());
    panelYCoord.add(new JLabel("Y"));
//    textYCoord.setPreferredSize(new java.awt.Dimension(64, 20));
    panelYCoord.add(textYCoord);
    panelCoords.add(panelYCoord);

    JPanel panelZCoord = new JPanel(new FlowLayout());
    panelZCoord.add(new JLabel("Z"));
//    textZCoord.setPreferredSize(new java.awt.Dimension(64, 20));
    panelZCoord.add(textZCoord);
    panelCoords.add(panelZCoord);

    btnResetCoords.setToolTipText("");
    btnResetCoords.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        resetCoords();
      }
    });
    panelCoords.add(btnResetCoords);

    add(panelCoords);

    JPanel panelAAngle = new JPanel(new FlowLayout());
    panelAAngle.add(new JLabel("Roll"));
//    textAAngle.setPreferredSize(new java.awt.Dimension(64, 20));
    panelAAngle.add(textAAngle);
    panelAngles.add(panelAAngle);

    JPanel panelBAngle = new JPanel(new FlowLayout());
    panelBAngle.add(new JLabel("Pitch"));
//    textBAngle.setPreferredSize(new java.awt.Dimension(64, 20));
    panelBAngle.add(textBAngle);
    panelAngles.add(panelBAngle);

    JPanel panelCAngle = new JPanel(new FlowLayout());
    panelCAngle.add(new JLabel("Yaw"));
//    textCAngle.setPreferredSize(new java.awt.Dimension(64, 20));
    panelCAngle.add(textCAngle);
    panelAngles.add(panelCAngle);

    btnResetAngles.setToolTipText("");
    btnResetAngles.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        resetAngles();
      }
    });

    panelAngles.add(btnResetAngles);

    add(panelAngles);
  }

  public void resetCoords() {
    m_Fragment.setPivotCoords(atomPanel.getSelectedAtom().getLocalCoordinates());
//    updateCoordinates();
  }

  public void resetAngles() {
    m_Fragment.setOrientation(new Coordinates(0, 0, 0));
//    updateCoordinates();
  }

}
