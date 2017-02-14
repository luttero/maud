/*
 * @(#)JHelpButton.java	created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.util.*;

import javax.swing.*;

/**
 * The JHelpButton is a class
 *
 * @version $Revision: 1.4 $, $Date: 2006/11/20 21:36:24 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JHelpButton extends JIconButton {

  public JHelpButton() {
      this("Help.gif");

  }

  public JHelpButton(String icon) {
    super();
    if (icon != null) {
      if (Constants.macosx)
        icon = "info32.gif";
      setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + icon)));
    } else {
      if (Constants.macosx)
        setText("?");
      else
        setText("Help");
    }
  }

}
