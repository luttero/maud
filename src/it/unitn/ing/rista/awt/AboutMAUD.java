/*
 * @(#)AboutMAUD.java created 1/01/1997 ?
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;


/**
 * The AboutMAUD is a dialog class providing the about information box.
 *
 * @version $Revision: 1.4 $, $Date: 2006/11/10 09:32:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AboutMAUD extends myJDialog {

  public AboutMAUD(Frame parent, boolean modal) {

    super(parent, modal);

    MouseClicked closing = new MouseClicked();

    Container c1 = getContentPane();
    c1.addMouseListener(closing);
    c1.setLayout(new BorderLayout(6, 6));

    JPanel bp1 = new JPanel(new BorderLayout(6, 6));
    bp1.addMouseListener(closing);
    bp1.setBorder(new BevelBorder(BevelBorder.LOWERED));
    c1.add(BorderLayout.CENTER, bp1);

	 JLabel imageLabel = new JLabel("");
    imageLabel.addMouseListener(closing);
	 try {
		 imageLabel.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.imagefolder + "About_Maud_512.png")));
	 } catch (Exception e) {
		 e.printStackTrace();
		 imageLabel.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.imagefolder + "maud_logo.gif")));
	 }

//	 imageLabel.setSize(512, 384);
    bp1.add(BorderLayout.CENTER, imageLabel);

    bp1 = new JPanel();
    bp1.addMouseListener(closing);
    bp1.setBorder(new BevelBorder(BevelBorder.LOWERED));
    bp1.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));

    bp1.add(new JLabel(Constants.maudReleaseBuilt));

    c1.add(BorderLayout.SOUTH, bp1);

    setTitle("About MAUD");
    pack();

  }

	public AboutMAUD(Frame parent, String title, boolean modal) {
    this(parent, modal);
    setTitle(title);
  }

  class MouseClicked extends java.awt.event.MouseAdapter {
    public void mousePressed(java.awt.event.MouseEvent event) {
      setVisible(false);
      dispose();
    }
  }


}
