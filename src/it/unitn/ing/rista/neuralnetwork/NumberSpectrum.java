/*
 * @(#)NumberSpectrum.java created Mar 23, 2006 Casalino
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


/**
 * The NumberSpectrum is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class NumberSpectrum {

  private double[] tokens;
  private NumberCategory category;

  /** Creates a new instance of NumberSpectrum */
  public NumberSpectrum(double[] data) {
      setSpectrum(data);
      category = null;
  }

  public void setSpectrum(double[] data) {
      tokens = data;
  }

  public void setCategory(NumberCategory category) {
      this.category = category;
  }

  public NumberCategory getCategory() {
      return category;
  }

  public int getLength() {
      return getNumberOfTokens();
  }

  public int getNumberOfTokens() {
      return tokens.length;
  }

  public double[] getTokens() {
      return tokens;
  }

  public void setTokens(double[] tokens) {
      this.tokens = tokens;
  }

  public boolean isValid() {
      return true;
  }
}
