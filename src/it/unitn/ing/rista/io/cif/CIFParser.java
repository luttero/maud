/*
 * @(#)CIFParser.java created 8/11/1998 Riva del Garda
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io.cif;

import java.io.*;
import java.util.*;
import java.awt.*;

import it.unitn.ing.rista.diffr.instrument.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;
import java.awt.event.*;

/**
 *  The CIFParser is a class
 *
 *
 * @version $Revision: 1.13 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFParser {

  static final String CIFname[] = {
		  "_chemical_name_systematic",
		  "_chemical_name_mineral",
		  "_chemical_name_common",
		  "_chemical_formula_structural",
		  "_chemical_formula_sum",
		  "_chemical_formula_moiety",
		  "_pd_phase_name",
		  "_pd_phase_id",
		  "_pd_phase_block_id",
		  "_symmetry_space_group_name_H-M",
		  "_symmetry_cell_setting",
		  "_chemical_name_structure_type",
		  "_diffrn_measurement_device",
		  "_diffrn_measurement_device_type",
		  "_diffrn_measurement_method",
		  "_pd_instr_geometry",
		  "_pd_instr_location",
		  "_diffrn_radiation_type",
		  "_pd_meas_dataset_id",
		  "_pd_spec_id"
  };

  static final int CIFnamesStart[] = {0, 12, 18, 19};
  static final int CIFnamesStop[] = {12, 18, 19, 20};

  static final String XRDcattype[] = {"Phase", "Instrument", "DataFileSet", "Sample"};
  static final int XRDcattypeNumber = 4;

//  Vector fileindex = null;

  Frame theparent = null;
  String filename = null;
  XRDcat parentObj = null;

  int XRDtype = -1;
  boolean realCif = false;

  int[] theindex = null;

  public CIFParser(String filename, Frame parent, XRDcat parentObj, String type) {

    theparent = parent;
    realCif = false;
    this.filename = filename;
    if (filename.endsWith(".cif"))
      realCif = true;
    this.parentObj = parentObj;
    for (int i = 0; i < XRDcattypeNumber; i++)
      if (type.equalsIgnoreCase(XRDcattype[i]))
        XRDtype = i;
  }

  public XRDcat[] getMainCat() {
    int[] index = null;
    Vector namelist = readallDataField();
    String catname;

    if (namelist.size() <= 0)
      return null;

    if (theparent != null) {
      index = getMainCat(namelist);

      if (index == null)
        return null;
    } else {
      index = new int[namelist.size()];
      for (int i = 0; i < namelist.size(); i++)
        index[i] = i;
    }

    XRDcat[] xrdobject = new XRDcat[index.length];
    for (int i = 0; i < index.length; i++) {
      catname = (String) namelist.elementAt(index[i]);
      int finalIndex = catname.indexOf(", ");
      if (finalIndex <= 1)
        finalIndex = catname.length();
      catname = catname.substring(0, finalIndex);
      if (catname != null) {
        switch (XRDtype) {
          case 0:
            xrdobject[i] = new Phase(parentObj, catname);
            break;
          case 1:
          	// _diffrn_measurement_device_type
	          String instrumentType = readuntilCif(index[i], "_diffrn_measurement_device");
//          	System.out.println(instrumentType);
          	if (instrumentType.equalsIgnoreCase("EDXRF Instrument")) {
	            xrdobject[i] = new EDXRFInstrument(parentObj, catname);
            } else if (instrumentType.equalsIgnoreCase("XRF Instrument")) {
	            xrdobject[i] = new XRFInstrument(parentObj, catname);
            } else {
	            xrdobject[i] = new DefaultInstrument(parentObj, catname);
            }
            break;
          case 2:
            xrdobject[i] = new DataFileSet(parentObj, catname);
            break;
          case 3:
            xrdobject[i] = new Sample(parentObj, catname);
            break;
          default:
            {}
        }
//        System.out.println("Stop at " + index[i]);
        readuntilobject(index[i], xrdobject[i]);
      } else
        xrdobject[i] = null;
    }
    return xrdobject;
  }

  public int[] getMainCat(Vector namelist) {
    setResult(null);
    (new ListJDialog(theparent, "Choose the " + XRDcattype[XRDtype] + " to add",
            true, namelist)).setVisible(true);
    return getResult();
  }

  public void setResult(int[] value) {
    theindex = value;
  }

  public int[] getResult() {
    return theindex;
  }

  public Vector readallDataField() {
    Vector namelist = new Vector(0, 1);
    BufferedReader reader = Misc.getReader(filename);
    readallDataField(reader, namelist, -1);
    try {
      reader.close();
//        System.out.println("Closed");
    } catch (IOException e) {
      e.printStackTrace();
    }
    return namelist;
  }

  public void readallDataFieldNew(BufferedReader reader, Vector namelist, int indexToStop) {
    boolean checkInput = Constants.checkCIFinputInConsole;
    int objIndex = 0;
    boolean nextStop = false;
    if (reader != null) {
      try {

        String token = new String("not");
        String line = "";
        StringTokenizer st = null;

        boolean endoffile = false;
//        boolean lineAlreadyIn = true;

        do {
          if (objIndex == indexToStop)
            nextStop = true;
          if (checkInput)
            System.out.println(objIndex + " =? " + indexToStop);
//          if (!lineAlreadyIn)
//            line = reader.readLine();
          if (line != null) {

//            if (line.toLowerCase().startsWith("data_")) {
              do {
                line = reader.readLine();
                if (checkInput)
					        System.out.println("Line input: " + line);
                if (line != null) {
                  if (line.toLowerCase().startsWith("data_")) {
                    token = new String("data_");
                    st = new StringTokenizer(line.substring(5), "\r\n");   //llll " ,\t\r\n");
                  } else {
                    st = new StringTokenizer(line, " ,\t\r\n");
                    if (st.hasMoreTokens())
                      token = st.nextToken();
                    else
                      token = new String("not");
                  }
                } else
                  endoffile = true;
                if (checkInput)
                  System.out.println(line + ", is CIF name: " + isCIFname(token));
              } while (!endoffile && !isCIFname(token));
              if (isCIFname(token)) {
                if (checkInput)
                  System.out.println("Is CIF name to check: " + token + " " + isCIFname(token));
                if (nextStop)
                  return;
                StringBuffer tmp = new StringBuffer("");
                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  tmp.append(token);
//	                if (st.hasMoreTokens())
//                    tmp.append(" ");
                }
                boolean readingDotComma = false;
/*                while (tmp.toString().equals("") || readingDotComma) {
                  line = reader.readLine();
                  if (line.startsWith(";"))
                    readingDotComma = !readingDotComma;
                  else {
                    if (readingDotComma) {
                      tmp.append(line);
                      tmp.append(" ");
                    } else {
                      st = new StringTokenizer(line, "\t\r\n");
                      while (st.hasMoreTokens()) {
                        token = st.nextToken();
                        tmp.append(token);
                        tmp.append(" ");
                      }
                    }
                  }
                }*/
                String finalName = tmp.toString();
//                if (finalName.endsWith(" "))
//                  finalName = finalName.substring(0, finalName.length() - 1);
                st = new StringTokenizer(finalName, "\t\r\n");
                StringBuffer theName = new StringBuffer("");
                if (st.hasMoreTokens()) {
                  token = st.nextToken();
                  theName.append(token);
                }
                String nameToCheckForEnd = theName.toString();
                if (checkInput)
                  System.out.println("-----------Name to check: " + nameToCheckForEnd);
                boolean newdatafield = false;
                do {
                  line = reader.readLine();
                  if (checkInput)
					          System.out.println("Line input for the CIF object : " + line);
                  if (line != null) {
                    if (line.toLowerCase().startsWith("data_")) {
                      token = "data_";
                      newdatafield = true;
                    } else if (line.startsWith("#end_subordinateObject_"+nameToCheckForEnd)) {
                        if (checkInput)
                          System.out.println("End of object or new object: " + line);
                        token = new String("not");
                        newdatafield = true;
                    } else {
                      st = new StringTokenizer(line, " ,\t\r\n");
                      if (st.hasMoreTokens())
                        token = st.nextToken();
                      else
                        token = new String("not");
                      if (isCIFname(token)) {
                        if (checkInput)
                          System.out.println("Checking again the CIF name: " + token + " " + isCIFname(token));
                        if (nextStop)
                          return;
                        tmp = new StringBuffer("");
                        while (st.hasMoreTokens()) {
                          token = st.nextToken();
                          tmp.append(token);
                          tmp.append(" ");
                        }
                        readingDotComma = false;
                        while (tmp.toString().equals("") || readingDotComma) {
                          line = reader.readLine();
                          if (line.startsWith(";"))
                            readingDotComma = !readingDotComma;
                          else {
                            if (readingDotComma) {
                              tmp.append(line);
                              tmp.append(" ");
                            } else {
                              st = new StringTokenizer(line, "\t\r\n");
                              while (st.hasMoreTokens()) {
                                token = st.nextToken();
                                tmp.append(token);
                                tmp.append(" ");
                              }
                            }
                          }
                        }
                        finalName = tmp.toString();
                        if (finalName.endsWith(" "))
                          finalName = finalName.substring(0, finalName.length() - 1);
                        st = new StringTokenizer(finalName, "'\t\r\n");
                        if (st.hasMoreTokens()) {
                          token = st.nextToken();
                          theName.append(", " + token);
                        }
                      }
                    }
                  } else
                    endoffile = true;
                } while (!endoffile && !newdatafield);
                if (namelist != null)
                  namelist.addElement(theName.toString());
                objIndex++;
              }
//            }

          } else
            endoffile = true;

        } while (!endoffile);
      } catch (IOException e) {
        System.out.println("Error in loading the data file! Try to remove this data file");
        e.printStackTrace();
      }
    }
  }

	public String readallDataField(BufferedReader reader, Vector namelist, int indexToStop) {
		boolean checkInput = Constants.checkCIFinputInConsole;
		int objIndex = 0;
		boolean nextStop = false;
		if (reader != null) {
			try {

				String token = new String("not");
				String line = "";
				StringTokenizer st = null;

				boolean endoffile = false;
//        boolean lineAlreadyIn = true;

				do {
					if (objIndex == indexToStop)
						nextStop = true;
					if (checkInput)
						System.out.println(objIndex + " =? " + indexToStop);
//          if (!lineAlreadyIn)
//            line = reader.readLine();
					if (line != null) {

//            if (line.toLowerCase().startsWith("data_")) {
						do {
							line = reader.readLine();
							if (checkInput)
								System.out.println("Line input: " + line);
							if (line != null) {
								if (line.toLowerCase().startsWith("data_")) {
									token = new String("data_");
									st = new StringTokenizer(line.substring(5), " ,\t\r\n");
								} else {
									st = new StringTokenizer(line, " ,\t\r\n");
									if (st.hasMoreTokens())
										token = st.nextToken();
									else
										token = new String("not");
								}
							} else
								endoffile = true;
							if (checkInput)
								System.out.println(line + ", is CIF name: " + isCIFname(token));
						} while (!endoffile && !isCIFname(token));
						if (isCIFname(token)) {
							if (checkInput)
								System.out.println("Is CIF name to check: " + token + " " + isCIFname(token));
							if (nextStop) {
								return token + " " + inputCifItem(st);
							}
							StringBuffer tmp = new StringBuffer("");
							while (st.hasMoreTokens()) {
								token = st.nextToken();
								tmp.append(token);
								tmp.append(" ");
							}
							boolean readingDotComma = false;
							while (tmp.toString().equals("") || readingDotComma) {
								line = reader.readLine();
								if (line.startsWith(";"))
									readingDotComma = !readingDotComma;
								else {
									if (readingDotComma) {
										tmp.append(line);
										tmp.append(" ");
									} else {
										st = new StringTokenizer(line, "\t\r\n");
										while (st.hasMoreTokens()) {
											token = st.nextToken();
											tmp.append(token);
											tmp.append(" ");
										}
									}
								}
							}
							String finalName = tmp.toString();
							if (finalName.endsWith(" "))
								finalName = finalName.substring(0, finalName.length() - 1);
							st = new StringTokenizer(finalName, "'\t\r\n");
							StringBuffer theName = new StringBuffer("");
							if (st.hasMoreTokens()) {
								token = st.nextToken();
								theName.append(token);
							}
							String nameToCheckForEnd = theName.toString();
							if (checkInput)
								System.out.println("-----------Name to check: " + nameToCheckForEnd);
							boolean newdatafield = false;
							do {
								line = reader.readLine();
								if (checkInput)
									System.out.println("Line input for the CIF object : " + line);
								if (line != null) {
									if (line.toLowerCase().startsWith("data_")) {
										token = "data_";
										newdatafield = true;
									} else if (line.startsWith("#end_subordinateObject_"+nameToCheckForEnd)) {
										if (checkInput)
											System.out.println("End of object or new object: " + line);
										token = new String("not");
										newdatafield = true;
									} else {
										st = new StringTokenizer(line, " ,\t\r\n");
										if (st.hasMoreTokens())
											token = st.nextToken();
										else
											token = new String("not");
										if (isCIFname(token)) {
											if (checkInput)
												System.out.println("Checking again the CIF name: " + token + " " + isCIFname(token));
											if (nextStop)
												return token + " " + inputCifItem(st);
											tmp = new StringBuffer("");
											while (st.hasMoreTokens()) {
												token = st.nextToken();
												tmp.append(token);
												tmp.append(" ");
											}
											readingDotComma = false;
											while (tmp.toString().equals("") || readingDotComma) {
												line = reader.readLine();
												if (line.startsWith(";"))
													readingDotComma = !readingDotComma;
												else {
													if (readingDotComma) {
														tmp.append(line);
														tmp.append(" ");
													} else {
														st = new StringTokenizer(line, "\t\r\n");
														while (st.hasMoreTokens()) {
															token = st.nextToken();
															tmp.append(token);
															tmp.append(" ");
														}
													}
												}
											}
											finalName = tmp.toString();
											if (finalName.endsWith(" "))
												finalName = finalName.substring(0, finalName.length() - 1);
											st = new StringTokenizer(finalName, "'\t\r\n");
											if (st.hasMoreTokens()) {
												token = st.nextToken();
												theName.append(", " + token);
											}
										}
									}
								} else
									endoffile = true;
							} while (!endoffile && !newdatafield);
							if (namelist != null)
								namelist.addElement(theName.toString());
							objIndex++;
						}
//            }

					} else
						endoffile = true;

				} while (!endoffile);
			} catch (IOException e) {
				System.out.println("Error in loading the data file! Try to remove this data file");
				e.printStackTrace();
			}
		}
		return "";
	}

	private String inputCifItem(StringTokenizer st) {
  	   String result = "";
		if (st.hasMoreTokens()) {
			String partial = st.nextToken("\r\n");
			while (partial.startsWith(" ")) {
				partial = partial.substring(1);
			}
			while (partial.startsWith("\t")) {
				partial = partial.substring(1);
			}
			if (partial.length() > 0) {
				if (partial.startsWith("'")) {
					partial = partial.substring(1);
					if (st.hasMoreTokens())
						partial += st.nextToken("'\r\n");
					if (partial.endsWith("'"))
						partial = partial.substring(0, partial.length() - 1);
				} else if (partial.startsWith("\"")) {
					partial = partial.substring(1);
					if (st.hasMoreTokens())
						partial += st.nextToken("\"\r\n");
					if (partial.endsWith("'"))
						partial = partial.substring(0, partial.length() - 1);
				} else if (partial.startsWith("`")) {
					partial = partial.substring(1);
					if (st.hasMoreTokens())
						partial += st.nextToken("`\r\n");
					if (partial.endsWith("'"))
						partial = partial.substring(0, partial.length() - 1);
				} else if (partial.startsWith(";")) {
					partial = partial.substring(1);
					if (st.hasMoreTokens())
						partial += st.nextToken(";\r\n");
					if (partial.endsWith("'"))
						partial = partial.substring(0, partial.length() - 1);
				} else {
					if (st.hasMoreTokens())
						partial += st.nextToken(" \r\n");
				}
				result = partial;
			}
		}
		return result;
	}

	public boolean isCIFname(String cifstring) {
    if (realCif && cifstring.startsWith("data_"))
      return true;
    for (int i = CIFnamesStart[XRDtype]; i < CIFnamesStop[XRDtype]; i++)
      if (cifstring.toLowerCase().equals(CIFname[i]))
        return true;
    return false;
  }

  public void readuntilobject(int objectindex, XRDcat xrdobject) {

    BufferedReader in = Misc.getReader(filename);

    readallDataField(in, null, objectindex);

    CIFtoken ciffile = new CIFtoken(in);

    xrdobject.isImportingFromCIF = true;
    xrdobject.readall(ciffile);
	  xrdobject.isImportingFromCIF = false;


    try {
      in.close();
    } catch (IOException ioe) {
      System.out.println("IO exception in closing the file");
    }
  }

	public String readuntilCif(int objectindex, String cif_keyword) {

		BufferedReader in = Misc.getReader(filename);

		String result = readallDataField(in, null, objectindex);
		if (result.length() > cif_keyword.length()) {
			if (result.startsWith(cif_keyword)) {
				int index = result.indexOf(' ');
//				System.out.println("Found: " + result.substring(index + 1));
				return result.substring(index + 1);
			}
		}

		CIFtoken ciffile = new CIFtoken(in);
		ciffile.pushBack();

			String thecife;
			int newtoken, tokentype;
			boolean endofInput = false;
			try {
				do {
					tokentype = ciffile.nextToken();
//					System.out.println(tokentype);
					switch (tokentype) {
						case CIFtoken.TT_DATA:
						case CIFtoken.TT_GLOB:
						case CIFtoken.TT_DATASET:
						case CIFtoken.TT_SAMPLE:
						case CIFtoken.TT_PHASE:
						case CIFtoken.TT_SUBE:
						case CIFtoken.TT_SUBO:
						case CIFtoken.TT_CUSTOM:
						case CIFtoken.TT_CUSTOM_END:
						case CIFtoken.TT_LOOP:
							// subordinate loop
//							System.out.println("End of input");
							endofInput = true;
							break;
						case CIFtoken.TT_INST:
						case CIFtoken.TT_CIFE:
//							System.out.println("inst or cife");
							// CIF item
							thecife = ciffile.thestring;
							newtoken = ciffile.nextToken();
//							System.out.println(thecife + " : " + ciffile.thestring);
							if (thecife.startsWith(cif_keyword))
								return ciffile.thestring;
							break;
						default:
						{
						}
					}
				} while (tokentype != CIFtoken.TT_EOF && !endofInput);
			} catch (Exception ioe) {
				ioe.printStackTrace();
			}


		try {
			in.close();
		} catch (IOException ioe) {
			System.out.println("IO exception in closing the file");
		}
		return "Diffraction instrument";
	}

	class ListJDialog extends myJDialog {

    JList thelist;
    Hashtable originalindexes;

    public ListJDialog(Frame parent, String title, boolean modal, Vector alist) {

      super(parent, title, modal);

      Container pane = getContentPane();
      pane.setLayout(new BorderLayout(6, 6));
//	by Leonardo, 15th January, 2001
      thelist = new JList(sortedList(alist));

      thelist.setVisibleRowCount(20);
      thelist.setPrototypeCellValue("1234567890123456789012345678901234567890");
      thelist.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
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
    }

    public void retrieveParameters() {
//			Integer indexseletecteditem = (Integer)originalindexes.get(thelist.getSelectedValue());
//			setResult(indexseletecteditem.intValue());
      Object[] newIndices = thelist.getSelectedValues();
      int[] index = new int[newIndices.length];
      for (int i = 0; i < newIndices.length; i++) {
        index[i] = ((IndexedObject) newIndices[i]).index;
//        System.out.println(i + " - " + index[i]);
      }
      setResult(index);
    }

//	by Leonardo, 15th January, 2001
    Vector sortedList(Vector unsortedvector) {
      Vector newsortedvector = new Vector(0, 1);
//      fileindex = new Vector(0, 1);
      Vector tmpvector = new Vector(0, 1);
      for (int i = 0; i < unsortedvector.size(); i++) {
        tmpvector.addElement(new IndexedObject(i, (String) unsortedvector.elementAt(i)));
      }
      Collections.sort(tmpvector, new phComparer());
      for (int i = 0, n = unsortedvector.size(); i < n; i++) {
        newsortedvector.addElement(tmpvector.elementAt(i));
//        Integer theindex = new Integer(((IndexedObject) tmpvector.elementAt(i)).index);
//        fileindex.addElement(theindex);
      }
      return newsortedvector;
    }

//by Leonardo, 15th January, 2001
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
