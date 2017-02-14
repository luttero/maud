/*
 * @(#)XRDMLPoleFigureReader.java created Jan 11, 2005 Riva Del Garda
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.io.XMLReader;
import org.w3c.dom.*;

import java.util.Vector;
import java.util.StringTokenizer;


/**
 * The XRDMLPoleFigureReader is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:57 $
 * @since JDK1.1
 */

public class XRDMLPoleFigureReader extends XMLReader {
//  axis
  public static final String[] axisTag = {"Omega", "Psi", "Phi", "2Theta"};

  public Vector getPoleFigure() {
    Vector reflection = new Vector();
    Vector singleRefl = new Vector();
    int numbEq = 0;
    int numbDiff = 0;
    int totNumber = 0;
    Vector poleIntensity = new Vector();
    Vector anglesV = new Vector();
    NodeList nodelist = getElementsByTagName("xrdMeasurement");
    for (int i = 0; i < nodelist.getLength(); i++) {
      Node node = nodelist.item(i);
      if (node instanceof Element) {
        NodeList scanElements = ((Element) node).getElementsByTagName("scan");
        for (int j = 0; j < scanElements.getLength(); j++) {
          Element scanElement = (Element) scanElements.item(j);
          String scanAxis = scanElement.getAttribute("scanAxis");

          NodeList intensities = scanElement.getElementsByTagName("intensities");
          if (intensities.getLength() > 0) {
            // intensities
            Vector intVector = new Vector();
            for (int ij = 0; ij < intensities.getLength(); ij++) {
//              System.out.println("intensities " + ij + " : " + intensities.item(ij).toXRDcatString());
              String intensityS = getValue(intensities.item(ij));
              StringTokenizer st = new StringTokenizer(intensityS, "' ,\t\r\n");
              while (st.hasMoreTokens()) {
                intVector.add(st.nextToken());
              }
            }
            poleIntensity.add(intVector);
            // angles
            int numberOfPoints = intVector.size();
            totNumber += numberOfPoints;
            NodeList positions = scanElement.getElementsByTagName("positions");
            double[][] angles = new double[axisTag.length][numberOfPoints];
            for (int ij = 0; ij < positions.getLength(); ij++) {
              Element posElement = (Element) positions.item(ij);
              String axis = posElement.getAttribute("axis");
              if (axis.equalsIgnoreCase(scanAxis)) {
                NodeList startPos = posElement.getElementsByTagName("startPosition");
                NodeList endPos = posElement.getElementsByTagName("endPosition");
                if (startPos.getLength() > 0 && endPos.getLength() > 0) {
                  String angleS = getValue(startPos.item(0));
                  String angleE = getValue(endPos.item(0));
                  for (int n = 0; n < axisTag.length; n++) {
                    if (axis.equalsIgnoreCase(axisTag[n])) {
                      double start = Double.parseDouble(angleS);
                      double end = Double.parseDouble(angleE);
                      double step = (end - start) / (numberOfPoints - 1);
                      start = start - step / 2;
                      for (int ji = 0; ji < numberOfPoints; ji++)
                        angles[n][ji] = start + step * ji;
                      break;
                    }
                  }
                }
              } else {
                NodeList commonPos = posElement.getElementsByTagName("commonPosition");
                String angleS = "0.0";
                if (commonPos.getLength() > 0)
                  angleS = getValue(commonPos.item(0));
                for (int n = 0; n < axisTag.length; n++) {
                  if (axis.equalsIgnoreCase(axisTag[n])) {
                    angles[n][0] = Double.parseDouble(angleS);
                    for (int ji = 1; ji < numberOfPoints; ji++)
                      angles[n][ji] = angles[n][0];
                    break;
                  }
                }
              }
            }
            anglesV.add(angles);
            // hkl
            NodeList hList = scanElement.getElementsByTagName("h");
            NodeList kList = scanElement.getElementsByTagName("k");
            NodeList lList = scanElement.getElementsByTagName("l");
            int[] hkli = new int[3];
            for (int n = 0; n < hList.getLength(); n++) {
              hkli[0] = Integer.parseInt(getValue(hList.item(n)));
              hkli[1] = Integer.parseInt(getValue(kList.item(n)));
              hkli[2] = Integer.parseInt(getValue(lList.item(n)));
            }
            if (numbDiff == 0) {
              reflection.add(hkli);
              numbEq++;
              numbDiff++;
            } else {
              int[] hkl = (int[]) reflection.get(reflection.size() - 1);
              if (hkli[0] == hkl[0] && hkli[1] == hkl[1] && hkli[2] == hkl[2]) {
                numbEq++;
              } else {
                int[] numbers = new int[2];
                numbers[0] = numbEq;
                numbers[1] = totNumber;
                singleRefl.add(numbers);
                numbEq = 1;
                numbDiff++;
                reflection.add(hkli);
              }
            }
          }

        }
      }
    }

    int[] numbers = new int[2];
    numbers[0] = numbEq;
    numbers[1] = totNumber;
    singleRefl.add(numbers);

    int numberOfScan = 0;

    Vector polesAndAngles = new Vector();

    for (int i = 0; i < singleRefl.size(); i++) {
      numbers = (int[]) singleRefl.get(i);
      double[][] pfInt = new double[3][numbers[1]];
      int progN = 0;
      for (int j = 0; j < numbers[0]; j++) {
        double[][] angles = (double[][]) anglesV.elementAt(numberOfScan);
        Vector intens = (Vector) poleIntensity.elementAt(numberOfScan++);
        for (int ij = 0; ij < angles[0].length; ij++) {
          pfInt[0][progN] = angles[1][ij];
          pfInt[1][progN] = angles[2][ij];
          pfInt[2][progN++] = Double.parseDouble((String) intens.get(ij));
        }
      }
      polesAndAngles.add(reflection.elementAt(i));
      polesAndAngles.add(pfInt);
    }

    return polesAndAngles;
  }
}
