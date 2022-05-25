/*
 * @(#)MaudetteMacOSFrame.java created Oct 12, 2004 Casalino
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

package it.unitn.ing.rista.awt;


/**
 * The MaudetteMacOSFrame is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2004/11/18 09:38:50 $
 * @since JDK1.1
 */

public class MaudetteMacOSFrame extends DiffractionMainFrame {

  boolean alreadyHandled = false;

  public MaudetteMacOSFrame() {
    super();
  }

  public void handleAbout() {
    aboutHelp_Action();
  }

  public void handleQuit() {
    if (!alreadyHandled) {
      alreadyHandled = true;
      (new Thread() {
        public void run() {
          myFrame_WindowClosing();
        }
      }).start();
    }
  }

}
