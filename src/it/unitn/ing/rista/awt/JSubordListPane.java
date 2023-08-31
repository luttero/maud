/*
 * @(#)JSubordListPane.java created 01/01/1997 Mesiano
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

/**
 * The JSubordListPane is a class
 *
 * @version $Revision: 1.8 $, $Date: 2006/07/20 13:39:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JSubordListPane extends JPanel {
  //insert class definition here

  JTextField totTF = null;
  JList thelist;
  JButton addB;
  JTextField[] valueTF = null;
  XRDcat itsparent = null;
  int theindex = 0, selected = -1;
  JPanel fieldsPanel;
  int fieldNumber;
  Frame theparent = null;

  public JSubordListPane(Frame parent, boolean showTotal) {
    super();
    setFrameParent(parent);

    JPanel jp1, jp2, jp3;

    setLayout(new BorderLayout(3, 3));
    jp1 = new JPanel();
    jp1.setLayout(new BorderLayout(3, 3));
    add("North", jp1);
    if (showTotal) {
      jp2 = new JPanel();
      jp2.setLayout(new BorderLayout(3, 3));
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
    thelist.setPrototypeCellValue("123456789012");
    thelist.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    JScrollPane sp1 = new JScrollPane();
//		sp1.setBorder(new LineBorder(Color.black));
    sp1.getViewport().add(thelist);
    jp1.add(BorderLayout.CENTER, sp1);

    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new FlowLayout());
    jp1.add(BorderLayout.WEST, buttonPanel);
    jp3 = new JPanel();
    jp3.setLayout(new GridLayout(0, 1, 3, 3));
    buttonPanel.add(jp3);
    jp3.add(addB = new JIconButton("Plus.gif", "add term"));
    final JRemoveButton removeB = new JRemoveButton("Minus.gif", "remove term");
    jp3.add(removeB);
    removeB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected object?"))
          removeB_Clicked();
      }
    });
/*	  final JButton optionsB = new JButton("Options");
	  jp3.add(optionsB);
	  optionsB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  optionsB_Clicked();
		  }
	  });*/
    jp2 = new JPanel();
    jp2.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    add("Center", jp2);
    fieldsPanel = new JPanel();
    fieldsPanel.setLayout(new BorderLayout(6, 6));
    jp2.add(fieldsPanel);
	  initListener();

	  addCustomControlsToFieldsPanel();

  }

	public void addCustomControlsToFieldsPanel() {

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

	ActionListener buttonListener = null;
	ListSelectionListener listSelection = null;

  public void initListener() {
	  if (buttonListener == null)
    addB.addActionListener(buttonListener = new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        addB_Clicked();
      }
    });
	  if (listSelection == null)
    thelist.addListSelectionListener(listSelection = new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent event) {
        thelist_ListSelect();
      }
    });
  }

	public void removeListener() {
		addB.removeActionListener(buttonListener);
		thelist.removeListSelectionListener(listSelection);
		listSelection = null;
		buttonListener = null;
	}

	public void setFields(String[] labels) {
		if (valueTF != null) {
			for (int i = 0; i < valueTF.length; i++) {
//				System.out.println("Removing: " + i + " " + valueTF[i].getText() + " " + valueTF[i].toString());
				((myJFrame) getFrameParent()).removeComponentfromlist(valueTF[i]);
				if (valueTF[i] != null) {
					valueTF[i].removeAll();
//					valueTF[i].revalidate();
//					valueTF[i].repaint();
				}

//				valueTF[i].removeAll();
				valueTF[i] = null;
			}
		}
		fieldsPanel.removeAll();
		addCustomControlsToFieldsPanel();
//		System.out.println("New JTextField");
    valueTF = new JTextField[fieldNumber];
    JPanel jp1 = new JPanel();
    if (fieldNumber > 8)
      jp1.setLayout(new GridLayout(0, 2, 3, 3));
    else
      jp1.setLayout(new GridLayout(0, 1, 3, 3));
    fieldsPanel.add(BorderLayout.CENTER, jp1);
    for (int i = 0; i < fieldNumber; i++) {
      JPanel jp2 = new JPanel();
      jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      jp2.add(new JLabel(labels[i]));
      valueTF[i] = new JTextField(Constants.FLOAT_FIELD);
      valueTF[i].setText("0");
      jp2.add(valueTF[i]);
//	    System.out.println("Added: " + i + " " + valueTF[i].getText() + " " + valueTF[i].toString());
//	    System.out.println("Adding focus: " + i);
      valueTF[i].addFocusListener(new FocusListener() {
        public void focusLost(FocusEvent fe) {
          retrieveparlist(selected);
        }
        public void focusGained(FocusEvent fe) {
        }
      });
      jp1.add(jp2);
    }
		fieldsPanel.revalidate();
		fieldsPanel.repaint();
  }

  public void setList(XRDcat aparent, int index, int number, String[] labels) {
	  if (itsparent != null) {
//		  System.out.println("Retrieving");
		  retrieveparlist();
	  }
	  if (itsparent == aparent && theindex == index)
		  return;
//	  System.out.println("Changing list: " + aparent.getLabel() + " " + index + " " + number + " " + labels[0]);
    itsparent = aparent;
    theindex = index;
    fieldNumber = number;
    setFields(labels);
    if (itsparent != null) {
	    selected = -1;
      int numb = itsparent.subordinateloopField[theindex].setList(thelist);
      if (totTF != null)
        totTF.setText(String.valueOf(numb));
      if (numb > 0) {
        setparameterlist(0);
	      selected = 0;
      }
    }
//    initListener();
  }

  public void setparameterlist(int numb) {
    int totnumb = itsparent.numberofelementSubL(theindex);
    if (totnumb > numb) {
      thelist.setSelectedIndex(numb);
      setparameterlist();
    }
  }

	public XRDcat selectedObject = null;

  public void setparameterlist() {
    if (itsparent != null) {
      XRDcat obj = (XRDcat) itsparent.subordinateloopField[theindex].selectedElement();
      if (obj != null && obj != selectedObject)
	      selectedObject = obj;
        for (int i = 0; i < fieldNumber; i++) {
          Parameter apar = obj.parameterField[i];
          if (apar != null) {
	          ((myJFrame) getFrameParent()).removeComponentfromlist(valueTF[i]);
            ((myJFrame) getFrameParent()).addComponenttolist(valueTF[i], apar);
	          valueTF[i].setText(apar.getValue());
          } /*else {
            ((myJFrame) getFrameParent()).removeComponentfromlist(valueTF[i]);
          }*/
        }
    }
  }

  public void retrieveparlist(int numb) {
    if (numb >= 0 && itsparent != null) {
      XRDcat obj = (XRDcat) itsparent.subordinateloopField[theindex].elementAt(numb);
      if (obj != null)
        for (int i = 0; i < fieldNumber; i++)
          obj.parameterField[i].setValue(valueTF[i].getText());
    }
  }

  public void retrieveparlist() {
    retrieveparlist(selected);
  }

  public void setparameterField(String label) {
    if (itsparent != null)
      itsparent.subordinateloopField[theindex].setLabelAt(label, thelist.getSelectedIndex());
  }

  public String gettheLabel() {
    return "Parameter label:";
  }

  void addB_Clicked() {
    // add a new parameter
    if (itsparent != null && thelist != null) {
      retrieveparlist(selected);
      selected = -1;
      itsparent.addsubordinateloopField(theindex);
      int numb = itsparent.numberofelementSubL(theindex);
      if (totTF != null)
        totTF.setText(String.valueOf(numb));
      setparameterlist(numb - 1);
    }
  }

  void removeB_Clicked() {
    // remove selected parameter
    if (itsparent != null && thelist != null)
      if (thelist.getSelectedIndex() >= 0) {
        selected = -1;
        if (itsparent.removeselSubLField(theindex)) {
          int numb = itsparent.numberofelementSubL(theindex);
          if (totTF != null)
            totTF.setText(String.valueOf(numb));
        }
        setparameterlist(0);
      }
  }

/*	void optionsB_Clicked() {
		// open the editing panel for the object
		if (itsparent != null && thelist != null)
			if (thelist.getSelectedIndex() >= 0) {
				itsparent.subordinateloopField[theindex].elementAt(thelist.getSelectedIndex()).getOptionsDialog(new Frame()).setVisible(true);
			}
	}*/

	void thelist_ListSelect() {
    if (thelist != null) {
      retrieveparlist(selected);
      selected = thelist.getSelectedIndex();
      setparameterlist(selected);
    }
  }

  public void dispose() {
    thelist = null;
    itsparent = null;
  }

}
