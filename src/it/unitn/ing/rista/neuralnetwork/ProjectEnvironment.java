/*
 * @(#)ProjectEnvironment.java created Mar 22, 2006 Casalino
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

package it.unitn.ing.rista.neuralnetwork;

import java.util.ArrayList;
import java.util.Vector;


/**
 * The ProjectEnvironment is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class ProjectEnvironment {

  private ArrayList numberCategories;
  private boolean isAutoThreshold;
  private boolean isNeuralNetworkRunning;
  private boolean isNeuralNetworkTrained;

  private Vector spectrumNameVector;

  /**
   * Creates a new instance of ProjectEnvironment
   */
  public ProjectEnvironment() {
    numberCategories = new ArrayList();
    isAutoThreshold = true;
    isNeuralNetworkRunning = false;
    isNeuralNetworkTrained = false;

    spectrumNameVector = null;
  }

  public void New() {
    numberCategories = new ArrayList();
    isAutoThreshold = true;
    isNeuralNetworkRunning = false;
    isNeuralNetworkTrained = false;
  }

  public ArrayList getNumberCategories() {
    return numberCategories;
  }

  public Object getNumberCategory(int index) {
    return numberCategories.get(index);
  }

  public void addNumberCategory(NumberCategory numberCategory) {
    numberCategories.add(numberCategory);
  }

  public void removeNumberCategory(NumberCategory numberCategory) {
    numberCategories.remove(numberCategory);
  }

  public int getNumberOfNumberCategory() {
    return numberCategories.size();
  }

  public int getNumberOfNumberSpectrum() {
    int numberOfNumberSpectrum = 0;

    for (int i = 0; i < numberCategories.size(); i++) {
      NumberCategory numberCategory = (NumberCategory) numberCategories.get(i);
      numberOfNumberSpectrum += numberCategory.getNumberOfSpectra();
    }

    return numberOfNumberSpectrum;
  }

  public int getNumberOfProcessedNumberSpectrum() {
    int numberOfProcessedNumberSpectrum = 0;

    for (int i = 0; i < numberCategories.size(); i++) {
      NumberCategory numberCategory = (NumberCategory) numberCategories.get(i);
      int numOfSpectra = numberCategory.getNumberOfSpectra();

      for (int j = 0; j < numOfSpectra; j++) {
        NumberSpectrum numberSpectrum = numberCategory.getSpectrum(j);

        if (numberSpectrum.getNumberOfTokens() > 0) {
          numberOfProcessedNumberSpectrum++;
        }
      }
    }

    return numberOfProcessedNumberSpectrum;
  }

  /**
   * getMaxToken()
   * <p/>
   * method that return the highest number of Tokens found for
   * one LeafSpectrum
   */
  public int getMaxToken() {
    int maxToken = 0;

    for (int i = 0; i < numberCategories.size(); i++) {
      NumberCategory category = (NumberCategory) numberCategories.get(i);

      for (int j = 0; j < category.getNumberOfSpectra(); j++) {
        NumberSpectrum numberSpectrum = category.getSpectrum(j);

        if (numberSpectrum.getNumberOfTokens() > maxToken) {
          maxToken = numberSpectrum.getNumberOfTokens();
        }
      }
    }

    return maxToken;
  }

  public boolean getIsAutoThreshold() {
    return isAutoThreshold;
  }

  public void setIsAutoThreshold(boolean isAutoThreshold) {
    this.isAutoThreshold = isAutoThreshold;
  }

  public boolean getIsNeuralNetworkRunning() {
    return isNeuralNetworkRunning;
  }

  public void setIsNeuralNetworkRunning(boolean isNeuralNetworkRunning) {
    this.isNeuralNetworkRunning = isNeuralNetworkRunning;
  }

  public boolean getIsNeuralNetworkTrained() {
    return isNeuralNetworkTrained;
  }

  public void setIsNeuralNetworkTrained(boolean isNeuralNetworkTrained) {
    this.isNeuralNetworkTrained = isNeuralNetworkTrained;
  }

  public Vector getSpectrumNameVector() {
    return spectrumNameVector;
  }


  public void setSpectrumNameVector(Vector spectrumNameVector) {
    this.spectrumNameVector = spectrumNameVector;
  }

}
