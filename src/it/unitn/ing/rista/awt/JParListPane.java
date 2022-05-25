/*
 * @(#)JParListPane.java	created 01/01/1997 Mesiano
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
import javax.swing.event.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.FilePar;

import javax.swing.border.*;


/**
 * The JParListPane is a class
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JParListPane extends JPanel {
  //insert class definition here

  JTextField totTF = null;
  JList thelist;
  JTextField firstvalueTF;
  XRDcat itsparent;
  public int theindex = 0, selected = -1;
  Frame theparent = null;
  boolean retrievePermitted = false;

  public JParListPane(Frame parent) {
    super();
    setFrameParent(parent);
  }

  public JParListPane(Frame parent, boolean showTotal) {
    super();
    setFrameParent(parent);

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
    jp3.setLayout(new GridLayout(2, 1, 6, 6));
    jp2.add("South", jp3);
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

  public void setFrameParent(Frame parent) {
    theparent = parent;
  }

  public Frame getFrameParent() {
    return theparent;
  }

  public FilePar getFileParent() {
    Frame aparent = getFrameParent();
    while (aparent != null && !(aparent instanceof principalJFrame)) {
      aparent = ((ParentFrame) aparent).getFrameParent();
    }
    if (aparent != null)
      return ((ParentFrame) aparent).getFileParent();
    else
      return null;
  }

  public JTextField gettotTF() {
    return totTF;
  }

  public JList getthelist() {
    return thelist;
  }

  public JTextField getvalueTF() {
    return firstvalueTF;
  }

  public void initListener() {
    if (thelist != null) {
      thelist.addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(ListSelectionEvent event) {
          thelist_ListSelect();
        }
      });
      retrievePermitted = true;
    }
  }

  public void changeList(XRDcat aparent, int index) {
    retrieveparlist();
    if (itsparent != null && theindex >= 0)
      itsparent.parameterloopField[theindex].removeList();
    itsparent = aparent;
    theindex = index;
    if (itsparent != null) {
      int numb = itsparent.parameterloopField[theindex].setList(thelist);
      if (totTF != null)
        totTF.setText(String.valueOf(numb));
      retrievePermitted = false;
      if (numb > 0)
        thelist.setSelectedIndex(0);
      retrievePermitted = true;
    }
  }

  public void setList(XRDcat aparent, int index) {
    itsparent = aparent;
    theindex = index;
    if (itsparent != null) {
      int numb = itsparent.parameterloopField[theindex].setList(thelist);
      if (totTF != null)
        totTF.setText(String.valueOf(numb));
      if (numb > 0) {
	      thelist.setSelectedIndex(0);
        setparameterlist(0);
	      selected = 0;
      }
    }
    initListener();
  }

/*  public void setparameterlist(int numb) {
//    thelist.setSelectedIndex(numb);
    setparameterlist(numb);
//    selected = thelist.getSelectedIndex();
  }*/

  public void setparameterlist(int numb) {
    if (itsparent != null) {
	    if (numb == -2) {
		    ((myJFrame) getFrameParent()).removeComponentfromlist(firstvalueTF);
		    selected = -1;
	    } else if (numb == -1) {
		    ((myJFrame) getFrameParent()).removeComponentfromlist(firstvalueTF);
		    thelist.setSelectedIndex(0);
		    setparameterlist(0);
		    selected = 0;
	    } else {
		    Parameter apar = (Parameter) itsparent.parameterloopField[theindex].selectedElement();
		    if (apar != null) {
			    firstvalueTF.setText(apar.getValue());
			    ((myJFrame) getFrameParent()).addComponenttolist(firstvalueTF, apar);
		    } else {
			    ((myJFrame) getFrameParent()).removeComponentfromlist(firstvalueTF);
			    firstvalueTF.setText("0");
		    }
	    }
    }
  }

  public void retrieveparlist(int numb) {
    if (!retrievePermitted)
      return;
    if (numb >= 0 && itsparent != null) {
      if (numb < itsparent.parameterloopField[theindex].size()) {
        Parameter apar = (Parameter) itsparent.parameterloopField[theindex].elementAt(numb);
        if (apar != null)
          apar.setValue(firstvalueTF.getText());
      }
    }
  }

  public void retrieveparlist() {
    retrieveparlist(selected);
  }

  public void setparameterField(String label) {
    if (itsparent != null)
      itsparent.parameterloopField[theindex].setLabelAt(label, thelist.getSelectedIndex());
  }

  public String gettheLabel() {
    return "Parameter label:";
  }

  void thelist_ListSelect() {
    if (thelist != null) {
      retrieveparlist(selected);
	    int[] mselected = thelist.getSelectedIndices();
	    if (mselected != null) {
		    if (mselected.length == 1) {
          selected = mselected[0];
          setparameterlist(selected);
		    } else {
			    selected = -2;
			    setparameterlist(selected);
		    }
	    }
    }
  }

  public void dispose() {
    thelist = null;
    itsparent = null;
  }

}
