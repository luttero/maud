/*
 * @(#)MultDiffrDataFile.java created 08/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;


/**
 *  The MultDiffrDataFile is a superclass for any class used to retrieve data from a file.
 *
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/02/02 16:11:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MultDiffrDataFile extends DiffrDataFile {

  private DiffrDataFile theDiffrDataFile;

  static Vector datafileCache = new Vector(0, 1);

  public MultDiffrDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public void initDatafile(String alabel) {
    String[] folderandname = Misc.getFolderandName(alabel);
    if (!folderandname[0].startsWith("//")) {
      if (getFilePar() != null && getVersion() < 1.2) {
        folderandname[0] = Misc.getAbsolutePath(folderandname[0], Misc.getUserDir() + "/");
      } else {
        if (getFilePar() != null && getFilePar().getCreatorID().startsWith("Rietquan")) {
          if (folderandname[0].length() > 1 && folderandname[0].charAt(1) == ':')
            folderandname[0] = new String("//" + folderandname[0]);
          else
            folderandname[0] = Misc.getAbsolutePath(folderandname[0], getDirectory());
        } else
          folderandname[0] = Misc.getAbsolutePath(folderandname[0], getDirectory());
      }
    }
    folder = new String(folderandname[0]);
    thelabel = new String(folderandname[1]);
    mustRemoved = true;

//    System.out.println("MultDiffrDataFile: " + alabel);
    if (!getFilePar().isLoadingFile() || !getFilePar().storeSpectraWithAnalysis()) {
      int indexFile = MultDiffrDataFile.isInCache(thelabel);
      if (indexFile >= 0) {
        addDiffrDatafile(MultDiffrDataFile.getFromCache(indexFile));
        MultDiffrDataFile.deleteFromCache(indexFile);
      } else {
        dataLoaded = loadData();
//        System.out.println("Load data " + theDiffrDataFile);
      }
    } else {
      addDiffrDatafile(Integer.toString(getFileNumber(thelabel)));
//      System.out.println("Load data 2 " + theDiffrDataFile);
    }
//    System.out.println(this + " Finally: " + getTheRealOne());
  }

  public MultDiffrDataFile() {
  }

  public int getFileNumber() {
    return getFileNumber(this.toXRDcatString());
  }

  public int getFileNumber(String label) {
    int filenumber = -1;
    String filename = label;
    if (filename.endsWith(")")) {
      int startIndex = -1;
      for (int i = filename.length() - 1; i > 0; i--) {
        if (filename.substring(i, i + 1).equals("(")) {
          startIndex = i;
          break;
        }
      }
      if (startIndex != -1) {
        String number = new String(filename.substring(startIndex + 1, filename.length() - 1));
        filenumber = Integer.valueOf(number).intValue();
      }
    }
    return filenumber;
  }

  public boolean readallSpectra() {
    // this method must be overrided by subclasses
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

//			 insert the code to read from the file
//				 example (this read the entire file):

        String token = new String("");
        Vector multicolumn = new Vector(100, 100);
        StringTokenizer st = null;
        String token1 = new String("");
        String token2 = new String("");


        boolean firstline = true;

        int columns = 1;

        String linedata = reader.readLine();      // read one line
        while (linedata != null) {

          st = new StringTokenizer(linedata, " ,\t\r\n");  	// create the tokenizer, fields are separed by
          // spaces ( ), commas (,), tabs (\t),
          // carriage return (\r) or line feed (\n)

          while (st.hasMoreTokens()) {
            token = st.nextToken();      // retrieve one by one the fields inside the linedata string
            multicolumn.addElement(token);
          }

          if (firstline) {
            columns = multicolumn.size();
          }

          linedata = reader.readLine();		// read next line

        }

        if (columns > 2)
          columns = 1;
        datanumber = multicolumn.size() / columns;

        initData(datanumber);

        int j = 0;
        for (int i = 0; i < datanumber; i++) {

          token1 = (String) multicolumn.elementAt(j);
          if (columns > 1) {
            token2 = (String) multicolumn.elementAt(j + 1);
            setXData(i, Double.valueOf(token1).doubleValue());  // old was setCalibratedXData
          } else {
            token2 = token1;
            setXData(i, (double) i);
          }
          double intensityValue = Double.valueOf(token2).doubleValue();
          setYData(i, intensityValue);
          double tmpweight = Math.sqrt(intensity[i]);
          if (tmpweight != 0.0)
            setWeight(i, 1.0 / tmpweight);
          else
            setWeight(i, 1.0);
          j += columns;
        }

      } catch (Exception e) {
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (Exception e) {
      }
    }
    else {
	    isAbilitatetoRefresh = tmpB;
	    return false;
    }
    isAbilitatetoRefresh = tmpB;
    return true;
  }

  public DiffrDataFile addDiffrDatafile(DiffrDataFile obj) {
		if (obj != null) {
    DataFileSet dataset = (DataFileSet) getParent();

    obj.setParent(dataset);
//    System.out.println("Set theDiffrDataFile " + obj);
    theDiffrDataFile = obj;
    dataset.addsubordinateloopField(2, obj);
		}
    return obj;
  }

  public DiffrDataFile addDiffrDatafile(String label) {
//  System.out.println("Add " + label + ", to: " + this + ", the real one " + theDiffrDataFile);
    DiffrDataFile obj = null;
	  if (label != null) {
    DataFileSet dataset = (DataFileSet) getParent();
		  if (dataset != null) {
    int filenumber = getFileNumber();
    int number = Integer.valueOf(label).intValue();
    String newname;

    if (filenumber == number)
      newname = this.toXRDcatString();
    else
      newname = new String(filterfilename(this.toXRDcatString()) + "(" + label + ")");

    if (filenumber == number || filenumber == -1) {
      if (theDiffrDataFile != null && filenumber == getFileNumber(theDiffrDataFile.toXRDcatString()))
        return theDiffrDataFile;
      obj = new DiffrDataFile(dataset, getFolder() + newname);
      dataset.addsubordinateloopField(2, obj);
//      System.out.println("Set theDiffrDataFile " + obj);
      theDiffrDataFile = obj;
    } else {
//      System.out.println("Add to cache theDiffrDataFile " + obj);
      obj = new DiffrDataFile(dataset, getFolder() + newname);
      addToCache(obj);
    }
		  }
	  }
    return obj;
  }

  public DiffrDataFile addDiffrDatafile() {

    DataFileSet dataset = (DataFileSet) getParent();
    String newname = this.toXRDcatString();

    DiffrDataFile obj = new DiffrDataFile(dataset, getFolder() + newname);

//    System.out.println("Set theDiffrDataFile " + obj);
    theDiffrDataFile = obj;
    dataset.addsubordinateloopField(2, obj);

    return obj;
  }

  public static void addToCache(DiffrDataFile adatafile) {
    datafileCache.addElement(adatafile);
  }

  public static void deleteFromCache(int index) {
    datafileCache.removeElementAt(index);
  }

  public static DiffrDataFile getFromCache(int index) {
    return (DiffrDataFile) datafileCache.elementAt(index);
  }

  public static int getCacheSize() {
    return datafileCache.size();
  }

  public static String getFromCacheFilename(int index) {
    return datafileCache.elementAt(index).toString();
  }

  public static void resetCache() {
    if (getCacheSize() > 0)
      datafileCache.removeAllElements();
  }

  public static int isInCache(String filename) {
//    System.out.println("Check " + filename);
    int filenumber = getCacheSize();
    for (int i = 0; i < filenumber; i++) {
      if (getFromCacheFilename(i).endsWith(filename) || filename.endsWith(getFromCacheFilename(i)))
        return i;
    }
    return -1;
  }

  public XRDcat getTheRealOne() {
//    System.out.println("The theDiffrDataFile " + theDiffrDataFile);
    return theDiffrDataFile;
  }

  public int setField(String cif, String astring, String astringerror, String min, String max, boolean free,
                      String refName, String refBound, String constant, String ratio, String expression,
                      boolean autoTrace, boolean positive) {
//    	System.out.println(this.toXRDcatString() + " " + theDiffrDataFile + " " + cif);
    if (theDiffrDataFile != null) {
      return theDiffrDataFile.setField(cif, astring, astringerror, min, max, free,
               refName, refBound, constant, ratio, expression, autoTrace, positive);
    } else
      return super.setField(cif, astring, astringerror, min, max, free,
               refName, refBound, constant, ratio, expression, autoTrace, positive);
  }

  public XRDcat setSubordinateField(String cif, String astring) {
//    System.out.println(this.toXRDcatString() + " " + theDiffrDataFile + " " + cif);
    if (theDiffrDataFile != null)
      return theDiffrDataFile.setSubordinateField(cif, astring);
    else
      return super.setSubordinateField(cif, astring);
  }

  public void setLoop(Vector avector, int element) {
    if (theDiffrDataFile != null)
      theDiffrDataFile.setLoop(avector, element);
    else
      super.setLoop(avector, element);
  }

}
