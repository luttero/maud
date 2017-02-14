/*
 * @(#)JSubordinateLoopListPane.java created Jul 24, 2004 Braila
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.border.BevelBorder;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The JSubordinateLoopListPane is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:52 $
 * @since JDK1.1
 */

public class JSubordinateLoopListPane extends JPanel {

  //insert class definition here

  JList thelist;
  JButton addB;
  JButton optionsB;
  public XRDcat itsparent;
  int theindex = 0, selected = -1;
  JPanel fieldsPanel;
  Frame theparent = null;
  JPanel additionalButtonPanel = null;

  public JSubordinateLoopListPane(Frame parent, String title) {
    super();
    setFrameParent(parent);
    setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), title));

    JPanel jp1;

    setLayout(new BorderLayout(3, 3));
    jp1 = new JPanel();
    jp1.setLayout(new BorderLayout(3, 3));
    add("North", jp1);
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
    jp1.add(BorderLayout.EAST, buttonPanel);
    additionalButtonPanel = new JPanel();
    additionalButtonPanel.setLayout(new GridLayout(0, 1, 3, 3));
    buttonPanel.add(additionalButtonPanel);
    additionalButtonPanel.add(addB = new JIconButton("Plus.gif", "add object"));
    final JRemoveButton removeB = new JRemoveButton("Minus.gif", "remove object");
    additionalButtonPanel.add(removeB);
    removeB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected object?"))
          removeB_Clicked();
      }
    });
    additionalButtonPanel.add(optionsB = new JIconButton("Eyeball.gif", "Object options"));

  }

  public void addButton(JButton button) {
    additionalButtonPanel.add(button);
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

  public void initListener() {
    addB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        addB_Clicked();
      }
    });
    thelist.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent event) {
        thelist_ListSelect();
      }
    });
    optionsB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        optionsB_Clicked();
      }
    });
  }

  public void setList(XRDcat aparent, int index) {
    itsparent = aparent;
    theindex = index;
    if (itsparent != null) {
      int numb = itsparent.subordinateloopField[theindex].setList(thelist);
      if (numb > 0) {
        setparameterlist(0);
        selected = 0;
      }
    }
    initListener();
  }

  public void setparameterlist(int numb) {
    int totnumb = itsparent.numberofelementSubL(theindex);
    if (totnumb > numb) {
      thelist.setSelectedIndex(numb);
    }
  }

  void addB_Clicked() {
    // add a new parameter
    if (itsparent != null && thelist != null) {
      selected = -1;
      itsparent.addsubordinateloopField(theindex);
      int numb = itsparent.numberofelementSubL(theindex);
      setparameterlist(numb - 1);
    }
  }

  void removeB_Clicked() {
    // remove selected parameter
    if (itsparent != null && thelist != null)
      if (thelist.getSelectedIndex() >= 0) {
        selected = -1;
        itsparent.removeselSubLField(theindex);
        setparameterlist(0);
      }
  }

  void optionsB_Clicked() {
    // remove selected parameter
    if (itsparent != null && thelist != null)
      if (thelist.getSelectedIndex() >= 0) {
        XRDcat obj = (XRDcat) itsparent.subordinateloopField[theindex].selectedElement();
        obj.getOptionsDialog(getFrameParent()).setVisible(true);
      }
  }

  public XRDcat getSelectedObject() {
    if (itsparent != null && thelist != null)
      if (thelist.getSelectedIndex() >= 0) {
    return (XRDcat) itsparent.subordinateloopField[theindex].selectedElement();
      }
    return null;
  }

  void thelist_ListSelect() {
    if (thelist != null) {
      selected = thelist.getSelectedIndex();
      setparameterlist(selected);
    }
  }

  public void dispose() {
    thelist = null;
    itsparent = null;
  }

}
