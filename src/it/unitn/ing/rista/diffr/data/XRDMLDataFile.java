/*
 * @(#)XRDMLDataFile.java created Jan 11, 2005 Riva Del Garda
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.diffr.rta.XRDMLPoleFigureReader;
import it.unitn.ing.rista.io.XMLReader;

import java.util.StringTokenizer;
import java.util.Vector;

import org.w3c.dom.*;


/**
 * The XRDMLDataFile is a class to load XRDML datfiles from Panalytical
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/07/20 13:39:05 $
 * @since JDK1.1
 */

public class XRDMLDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

  public XRDMLDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    identifier = ".xrdml";
  }

  public XRDMLDataFile() {

    identifier = ".xrdml";
  }


  public boolean readallSpectra() {

    // to be completed, not everything implemented, only principal features
    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    DiffrDataFile datafile = null;
    double omega_angle = 0.0;
    double phi_angle = 0.0;
    double chi_angle = 0.0;
    double eta_angle = 0.0;
//    Vector intValues = new Vector();

    XMLReader xreader = new XMLReader();
    if (xreader != null) {
    xreader.readFile(getBufferedInputStream());
//    xreader.checkTree();
    NodeList nodelist = xreader.getElementsByTagName("scan");
    boolean atmpB = true;
    for (int j = 0; j < nodelist.getLength(); j++) {
      datafile = addDiffrDatafile(Integer.toString(j));
      atmpB = datafile.isAbilitatetoRefresh;
      datafile.isAbilitatetoRefresh = false;
      datafile.title = title + " number " + Integer.toString(j);
      datafile.dspacingbase = false;
      datafile.constantstep = false;
//      datafile.setOmega(omega_angle);

      Element scanElement = (Element) nodelist.item(j);
      String scanAxis = scanElement.getAttribute("scanAxis");

      NodeList intensities = scanElement.getElementsByTagName("intensities");
      if (intensities.getLength() > 0) {
        // intensities
        Vector intVector = new Vector();
        for (int ij = 0; ij < intensities.getLength(); ij++) {
//              System.out.println("intensities " + ij + " : " + intensities.item(ij).toXRDcatString());
          String intensityS = xreader.getValue(intensities.item(ij));
          StringTokenizer st = new StringTokenizer(intensityS, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            intVector.add(st.nextToken());
          }
        }

        datanumber = intVector.size();
        datafile.initData(datanumber);
        // angles
        double start = 0.0;
        double end = 10.0;
        double step = 0.01;

        NodeList positions = scanElement.getElementsByTagName("positions");
        for (int ij = 0; ij < positions.getLength(); ij++) {
          Element posElement = (Element) positions.item(ij);
          String axis = posElement.getAttribute("axis");
          if (axis.equalsIgnoreCase(XRDMLPoleFigureReader.axisTag[3])) {
            NodeList startPos = posElement.getElementsByTagName("startPosition");
            NodeList endPos = posElement.getElementsByTagName("endPosition");
            if (startPos.getLength() > 0 && endPos.getLength() > 0) {
              String angleS = xreader.getValue(startPos.item(0));
              String angleE = xreader.getValue(endPos.item(0));
//              for (int n = 0; n < axisTag.length; n++) {
              if (axis.equalsIgnoreCase(XRDMLPoleFigureReader.axisTag[3])) {
                start = Double.parseDouble(angleS);
                end = Double.parseDouble(angleE);
                step = (end - start) / (datanumber - 1);
                start = start - step / 2;
              }
//              }
            }
          } else {
            NodeList commonPos = posElement.getElementsByTagName("commonPosition");
            String angleS = "0.0";
            if (commonPos.getLength() > 0) {
              angleS = xreader.getValue(commonPos.item(0));
              if (axis.equalsIgnoreCase(XRDMLPoleFigureReader.axisTag[1])) {
                chi_angle = Double.parseDouble(angleS);
              }
              if (axis.equalsIgnoreCase(XRDMLPoleFigureReader.axisTag[2])) {
                phi_angle = Double.parseDouble(angleS);
              }
            }
          }
        }
        NodeList countingTimeL = scanElement.getElementsByTagName("commonCountingTime");
        if (countingTimeL.getLength() > 0) {
          datafile.setCountTime(xreader.getValue(countingTimeL.item(0)));
        }

        datafile.setAngleValue(1, chi_angle);
        datafile.setAngleValue(2, phi_angle);
        datafile.setAngleValue(3, eta_angle);
        for (int i = 0; i < datanumber; i++) {
          datafile.setXData(i, start + i * step);
          double intens = Double.parseDouble((String) intVector.elementAt(i));
          datafile.setYData(i, intens);
//            System.out.println(xy[indexIntensity][j]);
          double tmpweight = Math.sqrt(datafile.getYData(i));
          if (tmpweight != 0.0)
            datafile.setWeight(i, 1.0 / tmpweight);
          else
            datafile.setWeight(i, 1.0);
        }
        loadSuccessfull = true;
        datafile.isAbilitatetoRefresh = atmpB;
        datafile.dataLoaded = true;
      }

    }
    }
    isAbilitatetoRefresh = tmpB;
    return loadSuccessfull;
  }

}
