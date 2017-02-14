/*
 * @(#)NumberCategory.java created Mar 22, 2006 Casalino
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


/**
 * The NumberCategory is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class NumberCategory {

  private double[] ID;	// Category type encoding
  private String categoryName;
  private ArrayList spectra;

  /** Creates a new instance of NumberCategory */
  public NumberCategory(String categoryName) {
      this.categoryName = categoryName;
      spectra = new ArrayList();
      ID = new double[0];
  }

  public void setCategoryName(String categoryName) {
      this.categoryName = categoryName;
  }

  public String toString() {
      return categoryName+" ("+spectra.size()+")";
  }

  public double[] getID() {
      return ID;
  }

  public void setID(double[] ID) {
      this.ID = ID;
  }

  public String getCategoryName() {
      return categoryName;
  }

  public void addSpectrum(NumberSpectrum spectrum) {
      spectra.add(spectrum);
  }

  public void removeSpectrum(NumberSpectrum spectrum) {
      spectra.remove(spectrum);
  }

  public NumberSpectrum getSpectrum(int idx) {
      return (NumberSpectrum)spectra.get(idx);
  }

  public int getNumberOfSpectra() {
      return spectra.size();
  }

}
