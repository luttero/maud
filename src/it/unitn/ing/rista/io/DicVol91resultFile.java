/*
 * @(#)DicVol91resultFile.java created 3/7/2002 Barcelona
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io;

import java.io.*;
import java.util.*;
import java.awt.*;
import java.net.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.io.*;
import it.unitn.ing.rista.interfaces.*;

import javax.swing.*;
import java.awt.event.*;
import javax.swing.border.*;

/**
 *  The DicVol91resultFile is a class to import Dicvol91 results
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DicVol91resultFile {

  static final String systemNames[] = {"C U B I C",
                                       "T E T R A G O N A L",
                                       "H E X A G O N A L",
                                       "O R T H O R H O M B I C",
                                       "M O N O C L I N I C",
                                       "T R I C L I N I C"
  };

  static final String symmetryNames[] = {"cubic",
                                       "tetragonal",
                                       "hexagonal",
                                       "orthorhombic",
                                       "monoclinic",
                                       "triclinic"
  };

  Vector fileindex = null;

  Frame theparent = null;
  String filename = null;

  int theindex = -1;

  public DicVol91resultFile(String filename, Frame parent) {
    theparent = parent;
    this.filename = filename;
  }

  public DicVol91Result getSelectedResult() {
    int index = 0;
    Vector namelist = readallDataField();

    if (namelist.size() > 0) {

      index = getSelectedResult(namelist);

      if (index > -1) {
        index = ((Integer) fileindex.elementAt(index)).intValue();
      } else
        return null;
    } else
      return null;

    return readuntilobject(index);
  }

  public int getSelectedResult(Vector namelist) {
    setResult(-1);
    ListImportDV91JDialog listD = new ListImportDV91JDialog(theparent, "Choose the result to import",
            true, namelist);
/*    try {
      do {
        Thread.currentThread().sleep(100);
      } while (listD.isVisible());
    } catch (InterruptedException e) {}*/
    return getResult();
  }

  public void setResult(int value) {
    theindex = value;
  }

  public int getResult() {
    return theindex;
  }

  public Vector readallDataField() {
    Vector namelist = new Vector(0, 1);
    fileindex = new Vector(0, 1);
    int index = -1;

    int actualSystem = -1;
    String nextStringToSearch = systemNames[actualSystem + 1];
    BufferedReader reader = Misc.getReader(filename);
    if (reader != null) {
      try {

        String line = null;
        boolean endoffile = false;

        do {
          line = reader.readLine();
          if (line != null) {
            if (line.indexOf("S Y S T E M") > 0) {
            actualSystem = 0;
            nextStringToSearch = systemNames[actualSystem];
            while (actualSystem < 5 && line.indexOf(nextStringToSearch) < 0) {
              actualSystem++;
              nextStringToSearch = systemNames[actualSystem];
            }
            }
            if (line.indexOf("DIRECT PARAMETERS") > 0) {
              index++;
              StringBuffer tmp = new StringBuffer(symmetryNames[actualSystem]);
              tmp.append(" ");
              tmp.append(line.substring(line.indexOf(':') + 2));
              tmp.append(" ");
              do {
                line = reader.readLine();
                if (line != null) {
                  if (line.indexOf("FIGURES OF MERIT") > 0) {
                    line = reader.readLine();
                    tmp.append(Misc.toStringDeleteBlankAndTab(line.substring(line.indexOf('M'),
                        line.lastIndexOf('('))));
//                    tmp.append(Misc.toStringDeleteBlankAndTab(line));
                    tmp.append(" ");
                    line = reader.readLine();
                    tmp.append(Misc.toStringDeleteBlankAndTab(line.substring(line.indexOf('F'),
                        line.lastIndexOf('('))));
//                    tmp.append(Misc.toStringDeleteBlankAndTab(line));
                    namelist.addElement(tmp.toString());
                    fileindex.addElement(new Integer(index));
                  }
                } else
                  endoffile = true;
              } while (!endoffile && line.indexOf("---------") < 0);
            }
          } else
            endoffile = true;
        } while (!endoffile);
      } catch (IOException e) {
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    return namelist;
  }

  public DicVol91Result readuntilobject(int objectindex) {
    DicVol91Result result = new DicVol91Result("");
    int index = -1;
    int actualSystem = 0;
    String nextStringToSearch;
    BufferedReader reader = Misc.getReader(filename);
    boolean dspaces = false;
    result.setWavelength(-1);
    if (reader != null) {
      try {

        String token = new String("not");
        String line = null;
        StringTokenizer st = null;

        boolean endoffile = false;

        do {
          line = reader.readLine();
          if (line != null) {
            if (line.indexOf("S Y S T E M") > 0) {
              actualSystem = 0;
              nextStringToSearch = systemNames[actualSystem];
              while (actualSystem < 5 && line.indexOf(nextStringToSearch) < 0) {
                actualSystem++;
                nextStringToSearch = systemNames[actualSystem];
              }
              System.out.println("Found: " + symmetryNames[actualSystem]);
            } else if (line.indexOf("              INPUT DATA") > 0) {
              for (int i = 0; i < 4; i++) {
                line = reader.readLine();
                if (line.indexOf("D VALUES") > 0) {
                  dspaces = true;
                  result.setWavelength(-1);
                }
              }
              do {
                line = reader.readLine();
//                System.out.println(line);
                if (line == null)
                  endoffile = true;
                else if (line.indexOf("**************") < 0) {
//                  System.out.println("using: " + line);
                  st = new StringTokenizer(line, " \t\r\n");
                  if (st.hasMoreTokens()) {
                    String position = st.nextToken();
//                    System.out.println(position);
                    String error = st.nextToken();
                    result.addReflex(Double.valueOf(position).doubleValue(),
                            Double.valueOf(error).doubleValue());
                  }
                }
              } while (!endoffile && line.indexOf("**************") < 0);
            } else if (line.indexOf("D VALUES") > 0) {
              dspaces = true;
              result.setWavelength(-1);
//              System.out.println("Wavelength: " + result.getWavelength());
            } else if (!dspaces && line.indexOf("WAVELENGTH") > 0) {
              st = new StringTokenizer(line, " =\t\r\n");
              st.nextToken();
              String wave = st.nextToken();
              result.setWavelength(Double.valueOf(wave).doubleValue());
//              System.out.println("Wavelength: " + wave);
            } else if (line.indexOf("DIRECT PARAMETERS") > 0) {
              index++;
              if (index < objectindex) {
                do {
                  line = reader.readLine();
                  if (line == null)
                    endoffile = true;
                } while (!endoffile && line.indexOf("---------") < 0);
              } else {
                result.setLabel(symmetryNames[actualSystem]);
                st = new StringTokenizer(line, " ,=:\t\r\n");
                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  if (token.equalsIgnoreCase("A"))
                    result.setCellA(st.nextToken());
                  else if (token.equalsIgnoreCase("B"))
                    result.setCellB(st.nextToken());
                  else if (token.equalsIgnoreCase("C"))
                    result.setCellC(st.nextToken());
                  else if (token.equalsIgnoreCase("ALPHA"))
                    result.setCellAlpha(st.nextToken());
                  else if (token.equalsIgnoreCase("BETA"))
                    result.setCellBeta(st.nextToken());
                  else if (token.equalsIgnoreCase("GAMMA"))
                    result.setCellGamma(st.nextToken());
                }
                endoffile = true;
              }
            }
          } else
            endoffile = true;
        } while (!endoffile);
      } catch (IOException e) {
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    return result;
  }

  class ListImportDV91JDialog extends myJDialog {

    JList thelist;
//		Hashtable originalindexes;

    public ListImportDV91JDialog(Frame parent, String title, boolean modal, Vector alist) {

      super(parent, title, modal);

      Container pane = getContentPane();
      pane.setLayout(new BorderLayout(6, 6));
//	by Leonardo, 15th January, 2001
      thelist = new JList(alist); //new JList(sortedList(alist));

      thelist.setVisibleRowCount(10);
      thelist.setPrototypeCellValue("123456789012345678901234567890");
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
      setResult(thelist.getSelectedIndex());
    }

  }
}
