/*
 * @(#)ListSelection.java created Sep 11, 2004 Location
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

package it.unitn.ing.rista.util;

import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The ListSelection is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2004/11/18 09:38:50 $
 * @since JDK1.1
 */

public class ListSelection {

  int result = -1;
  int[] theindex = null;

  public void setResult(int value) {
    result = value;
  }

  public int getResult() {
    return result;
  }

  public void setMultipleResult(int[] value) {
    theindex = value;
  }

  public int[] getMultipleResult() {
    return theindex;
  }

  public int getSelection(Vector namelist, String title, boolean sortTheList) {
    setResult(-1);
    new ListJDialog(new Frame(), title, true, namelist, false, sortTheList);
    return getResult();
  }

  public int[] getMultipleSelection(Vector namelist, String title, boolean sortTheList) {
    setMultipleResult(null);
    new ListJDialog(new Frame(), title, true, namelist, true, sortTheList);
    return getMultipleResult();
  }

  class ListJDialog extends myJDialog {

    JList thelist;
    Hashtable originalindexes;
    boolean multipleSelection = false;

    public ListJDialog(Frame parent, String title, boolean modal, Vector alist, boolean multipleSelection,
                       boolean sortTheList) {

      super(parent, title, modal);

      this.multipleSelection = multipleSelection;

      Container pane = getContentPane();
      pane.setLayout(new BorderLayout(6, 6));
      thelist = new JList(sortedList(alist, sortTheList));
      thelist.setVisibleRowCount(20);
      thelist.setPrototypeCellValue("1234567890123456789012345678901234567890");
      if (multipleSelection)
        thelist.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
      else
        thelist.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
      JScrollPane sp1 = new JScrollPane();
//			sp1.setBorder(new LineBorder(Color.black));
      sp1.getViewport().add(thelist);
      pane.add("Center", sp1);

      JPanel panel1 = new JPanel();
      panel1.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      pane.add("South", panel1);

      JButton jb;

      panel1.add(jb = new JIconButton("Check.gif", "Choose"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          setVisible(false);
          dispose();
        }
      });
      panel1.add(jb = new JCancelButton());
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
          dispose();
        }
      });
      if (!modal)
        setHelpButton(panel1);

      pack();
      setVisible(true);

    }

    public void retrieveParameters() {
//			Integer indexseletecteditem = (Integer)originalindexes.get(thelist.getSelectedValue());
//			setResult(indexseletecteditem.intValue());
      if (multipleSelection) {
        Object[] newIndices = thelist.getSelectedValues();
        int[] index = new int[newIndices.length];
        for (int i = 0; i < newIndices.length; i++) {
          index[i] = ((IndexedObject) newIndices[i]).index;
        }
        setMultipleResult(index);
      } else {
        IndexedObject indexseletecteditem = (IndexedObject) thelist.getSelectedValue();
        setResult(indexseletecteditem.index);
      }
    }

    Vector sortedList(Vector unsortedvector, boolean sort) {
      Vector newsortedvector = new Vector(0, 1);
      for (int i = 0; i < unsortedvector.size(); i++) {
        newsortedvector.addElement(new IndexedObject(i, (String) unsortedvector.elementAt(i)));
      }
      if (sort)
        Collections.sort(newsortedvector, new phComparer());
      return newsortedvector;
    }

    class phComparer implements Comparator {
      public int compare(Object obj1, Object obj2) {
        String phase1 = ((IndexedObject) obj1).object.toLowerCase();
        String phase2 = ((IndexedObject) obj2).object.toLowerCase();
        int len1 = phase1.length();
        int len2 = phase2.length();
        int minLength = Math.min(len1, len2);
        for (int i = 0; i < minLength; i++) {
          int diff = phase1.charAt(i) - phase2.charAt(i);
          if (diff > 0)
            return 1;
          else if (diff < 0)
            return -1;
        }
        if (len1 > len2)
          return 1;
        else if (len1 < len2)
          return -1;
        return 0;
      }
    }
  }

  public class IndexedObject {
    public int index = -1;
    public String object = null;

    public IndexedObject(int index, String object) {
      this.index = index;
      this.object = object;
    }

    public String toString() {
      return (String) object;
    }
  }
}

