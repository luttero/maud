/*
 * @(#)ArtificialNeuralNetworkIndexing.java created July 8, 2006 Casalino
 *
 * Copyright (c) 1996-2006 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.comp.ArtificialNeuralNetwork;
import it.unitn.ing.rista.diffr.Phase;
import it.unitn.ing.rista.diffr.Sample;
import it.unitn.ing.rista.util.*;

import java.util.Vector;

/**
 * The ArtificialNeuralNetworkIndexing
 * <p/>
 * References
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */

public class ArtificialNeuralNetworkIndexing {

  double[][] peakList = null;
  int dataLength = 0;
  Phase thePhase = null;

  public ArtificialNeuralNetworkIndexing(Phase aphase, double[][] apeakList) {
    dataLength = apeakList[1].length;
    peakList = new double[1][dataLength];
    System.arraycopy(apeakList[0], 0, peakList[0], 0, dataLength);
    thePhase = aphase;
  }

  public void solve() {
    (new PersistentThread() {
        public void executeJob() {
        GenerateIndexingDataUI dataui = new GenerateIndexingDataUI((Sample) thePhase.getParent(), false);
        dataui.setVisible(true);
        while (dataui.isVisible()) {
          try {
            Thread.sleep(500);
          } catch (InterruptedException e) {
          }
        }
        Vector allData = dataui.getData();
        for (int k = 0; k < allData.size(); k++) {
          Vector listData = (Vector) allData.elementAt(k);
          int numberData = listData.size() - 1;
          if (numberData > 0) {
            int linesNumber = ((double[]) listData.elementAt(0)).length - 6;
            double[][] inputData = new double[numberData][linesNumber];
            double[][] tcellData = new double[numberData][6];
            int i = 0;
            for (; i < numberData; i++) {
              double[] row = (double[]) listData.elementAt(i);
              for (int j = 0; j < linesNumber; j++)
                inputData[i][j] = row[j];
              for (int j = 0; j < 6; j++)
                tcellData[i][j] = row[j + linesNumber];
            }
            double[][] cellData;
            boolean trial = MaudPreferences.getBoolean("ANN.fullCell", true);
            if (!trial) {
              String[] sym = (String[]) listData.elementAt(i);
              thePhase.setSymmetry(sym[0]);
              thePhase.setSpaceGroup(true, sym[1], false);
              thePhase.updateAll();
              thePhase.CellSymmetry();
              int cellNumber = thePhase.numberOfCellParameters();
              cellData = new double[numberData][cellNumber];
              for (int j = 0; j < tcellData.length; j++) {
                int l = 0;
                for (int s = 0; s < 6; s++)
                  if (thePhase.ic[s] == 1)
                    cellData[j][l++] = tcellData[j][s];

              }
            } else {
              cellData = tcellData;
            }
            double[][] reducedPeakList = new double[1][linesNumber];
            System.arraycopy(peakList[0], 0, reducedPeakList[0], 0, linesNumber);
            for (int j = 0; j < linesNumber; j++) {
              reducedPeakList[0][j] = GenerateIndexingData.normalizeLine(reducedPeakList[0][j], dataui.amin, dataui.amax);
            }
            ArtificialNeuralNetwork ann = new ArtificialNeuralNetwork();
            ann.prepareNeuralNet(inputData, cellData);
            ann.trainNeuralNet(inputData, cellData, true);
            double[] rcell = ann.evaluateSet(peakList);
            if (!trial) {
              double[] fcell = new double[6];
              int l = 0;
              for (i = 0; i < 6; i++) {
                if (thePhase.ic[i] == 1)
                  fcell[i] = rcell[l++];
                else
                  fcell[i] = 0.5;
              }
              rcell = dataui.reconstructCell(fcell);
            } else
              rcell = dataui.reconstructCell(rcell);
            System.out.println("NeuralNetwork result:");
            for (i = 0; i < 6; i++) {
              System.out.print(rcell[i] + " ");
              thePhase.setCellValue(i, rcell[i]);
            }
            System.out.println("");
          }
        }
      }
    }).start();

  }
}
