/*
 * @(#)iconJFrame.java created 1/1/1997 xxx
 *
 * Copyright (c) 1997 Luca Lutterotti, All Rights Reserved.
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

import javax.swing.*;

import it.unitn.ing.rista.util.*;

import java.awt.*;

/**
 * Basic frame with icon.
 *
 * @version $Revision: 1.4 $, $Date: 2006/11/10 09:32:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class iconJFrame extends JFrame {

  public iconJFrame() {
    super();
    Image icon = getProgramIcon();
    if (icon != null)
      setIconImage(icon);
  }

  public Image getProgramIcon() {
    return (new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.imagefolder + Constants.programIcon))).getImage();
        // Toolkit.getDefaultToolkit().getImage(Misc.getResource(Constants.programIcon));
  }

  protected void finalize() throws Throwable {
//		if (Constants.testing)
//			System.out.println("DEBUG: " + this.toXRDcatString() + " finalizing");
    super.finalize();
  }
}
