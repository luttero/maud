/*
 * @(#)JIconButton.java created 17/08/1998 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

import java.net.*;
import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;

/**
 *  The JIconButton is an extension of the JButton class to include an icon
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/11/18 09:30:48 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JIconButton extends JButton {

  public JIconButton(String icon, String text) {
    super(text);
    if (icon != null &&
          (text.equals("") || MaudPreferences.getBoolean("gui.useIconsInButtons", !Constants.macosx)))
      setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + icon)));
  }

  public JIconButton(String icon) {
    this(icon, "");
  }

  public JIconButton() {
    super();
  }

  public JIconButton(String icon, String text, String actionCommand) {
    this(icon, text);
    setActionCommand(actionCommand);
  }

  public JIconButton(String icon, String text, String actionCommand, String toolTipText) {
    this(icon, text, actionCommand);
    setToolTipText(toolTipText);
  }

  public void setIcon(String icon) {
    setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + icon)));
  }

}
