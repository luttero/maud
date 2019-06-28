/*
 * @(#)StartingAboutD.java created 01/01/1997 Mesiano
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

import java.awt.*;
import javax.swing.border.*;
import javax.swing.*;
import java.net.*;

import it.unitn.ing.rista.util.*;

/**
 * The StartingAboutD is a class
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StartingAboutD extends Window {

  public StartingAboutD(Frame parent, ProgressPanel pgP) {

    super(parent);

    JPanel c1 = new JPanel();
    add(c1);
    c1.setLayout(new BorderLayout(12, 12));

    JPanel firstP = new JPanel();
    firstP.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
    JLabel abutton = new JLabel("");
//		abutton.setBorder(new BevelBorder(BevelBorder.LOWERED));
    boolean third = Misc.isThirdMinute();
    if (third && !Constants.testing)
      abutton.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.imagefolder + "Peaks.jpg")));
    else if (!third && Constants.testing) {
      abutton.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.imagefolder + "Peaksleos.jpg")));
    } else {
	    abutton.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.imagefolder + "Maud_ladybug2_full512.jpg")));
	    Constants.refineIcon = "Hammer.gif";
    }

    firstP.add(abutton);
    c1.add(firstP, BorderLayout.NORTH);

    JPanel borderPanel1 = new JPanel();
    borderPanel1.setLayout(new GridLayout(2, 1));
    Font jlfont = new Font("Dialog", Font.ITALIC, 12);
    String releaseText = null;
    String release = null;
    JLabel jl = new JLabel("by Luca Lutterotti, " + Constants.maudReleaseBuilt);
    jl.setFont(jlfont);
    borderPanel1.add(jl);
    jl = new JLabel("version " + Constants.getVersion());
    jl.setFont(jlfont);
    borderPanel1.add(jl);
    c1.add(borderPanel1, BorderLayout.CENTER);
    c1.add(pgP, BorderLayout.SOUTH);
    pack();

  }

  public void setVisible(boolean visible) {
    if (visible) {
      setCursor(new Cursor(Cursor.WAIT_CURSOR));
      centerOnScreen();
    } else {
      setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
      try {
        Thread.currentThread().sleep(2000);
      } catch (InterruptedException e) {
      }
    }
//		if (!Constants.macos)
    super.setVisible(visible);
  }

  public void centerOnScreen() {
    Dimension paneSize = getSize();
    Dimension screenSize = getToolkit().getScreenSize();

    setLocation((screenSize.width - paneSize.width) / 2,
            (screenSize.height - paneSize.height) / 2);
  }

}

