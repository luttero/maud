/*
 * @(#)OutputFrameI.java created Jan 25, 2004 Casalino
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

package it.unitn.ing.rista.comp;

import it.unitn.ing.rista.awt.ProgressPanel;

import javax.swing.*;


/**
 * The OutputFrameI is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2004/08/12 09:36:04 $
 * @since JDK1.1
 */

public interface OutputFrameI {

  public void addComponent(JComponent aComponent);

  public void reset();

  public void append(String atext);

  public void appendnewline(String atext);

  public void newline();

  public void increaseProgressBarValue();

  public void resizeProgressBarMaximum(int maximum);

  public ProgressPanel getProgressBar();

  public void setProgressText(String value);

  public void computationStopped();

  public void iterationStopped();

  public void computationPaused();

  public void computationResumed();

  public void iterationResumed();

  public void hideprogressBar();

  public void scrollToEnd();

}
