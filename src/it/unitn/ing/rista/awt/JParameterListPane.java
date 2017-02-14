/*
 * @(#)JParameterListPane.java created 1/01/1997 xxx
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
import java.awt.event.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;

/**
 * The JParameterListPane is a class
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JParameterListPane extends JParListPane {
  //insert class definition here

  JButton plotB, addB;

  public JParameterListPane(Frame parent) {
    this(parent, false, false);
  }

  public JParameterListPane(Frame parent, boolean showTotal) {
    this(parent, showTotal, false);
  }

  public JParameterListPane(Frame parent, boolean showTotal, boolean showPlot) {
    this(parent, showTotal, showPlot, true);
  }

  public JParameterListPane(Frame parent, boolean showTotal, boolean showPlot,
                            boolean changeNumber) {
    super(parent);

    JPanel jp1, jp2, jp3;

    setLayout(new BorderLayout(6, 6));
    jp1 = new JPanel();
    jp1.setLayout(new BorderLayout(6, 6));
    add("Center", jp1);
    if (showTotal) {
      jp2 = new JPanel();
      jp2.setLayout(new BorderLayout(6, 6));
      jp1.add("North", jp2);
      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
      jp2.add("East", jp3);
      jp3.add(new JLabel("Total parameter:"));
      totTF = new JTextField(4);
      totTF.setEditable(false);
      totTF.setText("0");
      jp3.add(totTF);
    }
    thelist = new JList();
    thelist.setVisibleRowCount(4);
    thelist.setPrototypeCellValue("123456789012345678901234567890");
    thelist.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    JScrollPane sp1 = new JScrollPane();
//		sp1.setBorder(new LineBorder(Color.black));
    sp1.getViewport().add(thelist);
    jp1.add("Center", sp1);
    jp2 = new JPanel();
    jp2.setLayout(new BorderLayout(6, 6));
    jp1.add("West", jp2);

    jp3 = new JPanel();
    jp3.setLayout(new GridLayout(0, 1, 6, 6));
    jp2.add(BorderLayout.NORTH, jp3);
    if (showPlot) {
      plotB = new JIconButton("LineGraph.gif", "Plot function");
      jp3.add(plotB);
    }
    if (changeNumber) {
      addB = new JIconButton("Plus.gif", "add parameter");
      jp3.add(addB);
      final JRemoveButton removeB = new JRemoveButton("Minus.gif", "remove parameter");
      removeB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected parameter?"))
            removeB_Clicked();
        }
      });
      jp3.add(removeB);
    }
    jp2 = new JPanel();
    jp2.setLayout(new BorderLayout(6, 6));
    add("South", jp2);
    jp3 = new JPanel();
    jp3.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
    jp2.add("Center", jp3);
    jp3.add(new JLabel("Value:"));
    firstvalueTF = new JTextField(Constants.FLOAT_FIELD);
    firstvalueTF.setText("0");
    jp3.add(firstvalueTF);

  }

  public JButton getaddB() {
    return addB;
  }
//	public JButton getremoveB() {return removeB;}

  public void initListener() {
    super.initListener();

    if (plotB != null)
      plotB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          plotFunction();
        }
      });
    if (addB != null)
      addB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          addB_Clicked();
        }
      });
  }

  /**
   * Create and display the plot of the parameters as a
   * linear function of a generic coordinate x.
   */

  protected void plotFunction() {
    retrieveparlist(selected);
    itsparent.plotFunction(getFrameParent(), theindex);
  }

  void addB_Clicked() {
    // add a new parameter
    if (itsparent != null) {
      retrieveparlist(selected);
      selected = -1;
      int numb = itsparent.numberofelementPL(theindex);
      itsparent.addparameterloopField(theindex, new Parameter(itsparent, itsparent.getParameterString(theindex, numb), 0));
      numb = itsparent.numberofelementPL(theindex);
      if (totTF != null)
        totTF.setText(String.valueOf(numb));
    }
  }

  void removeB_Clicked() {
    // remove selected parameter

//	  System.out.println("List selection model: " + ListSelectionModel.MULTIPLE_INTERVAL_SELECTION + " == " + thelist.getSelectionMode());
    if (itsparent != null) {
      int[] selectedIndices = thelist.getSelectedIndices();
      if (selectedIndices != null) {
        ((myJFrame) getFrameParent()).removeComponentfromlist(firstvalueTF);
        if (itsparent.removeselPLField(theindex)) {
          int numb = itsparent.numberofelementPL(theindex);
          if (totTF != null)
            totTF.setText(String.valueOf(numb));
          selected = -1;
        }
      }
    }
  }

}
