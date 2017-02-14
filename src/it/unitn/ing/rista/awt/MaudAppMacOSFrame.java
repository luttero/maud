/*
 * @(#)MaudAppMacOSFrame.java created 11/02/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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
 * The MaudAppMacOSFrame is an extension to mainFrame to register the MacOS
 * specific behaviors.
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2004/11/18 09:30:48 $
 * @since JDK1.1
 */

public class MaudAppMacOSFrame extends DiffractionMainFrame {
  boolean alreadyHandled = false;

  public MaudAppMacOSFrame() {
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

