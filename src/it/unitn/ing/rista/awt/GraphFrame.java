/*
 * @(#)GraphFrame.java created 28/01/2002 Mesiano.
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

import it.unitn.ing.jgraph.Graph2D;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

/**
 * The GraphFrame is an interface for printing graphs.
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GraphFrame extends myJFrame {

  Graph2D graph = null;
  CopyPrintPanel fullGraphPanel = null;

  public GraphFrame(Frame parent) {

    super(parent);

//		must be set by the subclass ?
//    setComponentToPrint(fullGraphPanel);
//    System.out.println("Printing fullGraphPanel");
  }

/*/  public JMenu createEditMenu() {
    JMenuItem menuitem = null;

    JMenu editMenu = new JMenu("Edit");

    editMenu.setMnemonic('e');
    editMenu.add(menuitem = new JMenuItem("Copy"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('c');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Clipboard clipboard = getToolkit().getSystemClipboard();
        CopyPrintPanel comp = fullGraphPanel;
        if (comp != null) {
          Rectangle rect = comp.getBounds();
          Image fileImage = createImage(rect.width, rect.height);
          Graphics g = fileImage.getGraphics();

          //write to the image
          comp.clearComponent(g);
          comp.paintComponent(g);
//          comp.paint(g);
          clipboard.setContents(new ClipImage(fileImage), GraphFrame.this);
          // write it out in the format you want

          //dispose of the graphics content
          g.dispose();
        }
      }
    });

    return editMenu;
  }    /*/

}
