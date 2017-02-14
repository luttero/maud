/*
 * @(#)CopyPrintablePanel.java created Sep 21, 2005 Casalino
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

import javax.swing.*;
import java.awt.print.*;
import java.awt.*;


/**
 * The CopyPrintablePanel is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/01/19 15:51:49 $
 * @since JDK1.1
 */

public class CopyPrintablePanel extends JPanel implements Printable {

  Component componentToPrint = null;

  public void setComponentToPrint(Component component) {
    componentToPrint = component;
//    System.out.println("Set for printing: " + componentToPrint);
  }

  /**
   * Send the content of the frame to the printer.
   */

  public void letsTryToPrint() {
    PrinterJob pjob = PrinterJob.getPrinterJob();
    if (pjob != null) {
      PageFormat graphicsPageFormat = new PageFormat();
      graphicsPageFormat = pjob.pageDialog(graphicsPageFormat);
      if (pjob.printDialog()) {
        graphicsPageFormat = pjob.validatePage(graphicsPageFormat);
        pjob.setPrintable(this, graphicsPageFormat);
        try {
          pjob.print();
        } catch (Exception PrintException) {
          PrintException.printStackTrace();
        }
      }
    }
  }

  public int print(Graphics graphics, PageFormat pageFormat, int pageIndex) /* throws PrinterException */ {
//		Graphics2D  g2 = (Graphics2D) graphics;
//    if (componentToPrint == null)
//      componentToPrint = getContentPane();
    if (pageIndex > 0)
      return NO_SUCH_PAGE;
//    System.out.println("Printing: " + componentToPrint);
    graphics.translate((int) pageFormat.getImageableX(), (int) pageFormat.getImageableY());
    if (componentToPrint instanceof CopyPrintPanel)
      ((CopyPrintPanel) componentToPrint).print(graphics, pageFormat);
    else
      componentToPrint.print(graphics);
    return PAGE_EXISTS;
  }

}
