/*
 * @(#)PrintFrame.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.util;

import java.awt.*;

/**
 * The PrintFrame is a class
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PrintFrame {
  //insert class definition here

  public PrintFrame(Object dummy) {
    Frame f = new Frame("Printer Test");
    f.setSize(300, 100);
    f.show();

    PrintJob job = f.getToolkit().getPrintJob(f, "Printer Test", null);
    Graphics page = job.getGraphics();
    Dimension pageSize = job.getPageDimension();
    int res = job.getPageResolution();
    int border = 5;

    System.out.println("page dimensions: " + pageSize.width + " x " + pageSize.height);
    System.out.println("page resolution is: " + res);

    page.drawRect(border, border, pageSize.width - 2 * border, pageSize.height - 2 * border);
    // spit out some text here
    job.end();
  }
}

/*
Component c = this.getParent();
while (c!=null && !(c instanceof Frame))
    c=c.getParent();

PrintJob pj = getToolkit().getPrintJob((Frame) c, "test", null);
Graphics pg = pj.getGraphics();
printAll(pg);
pg.dispose();
pj.end();
*/
/*
try {
    FileOutputStream fos = new FileOutputStream("LPT1");
    PrintStream ps = new PrintStream(fos);
            ps.print("Your string goes here");
            ps.print("\f");
            ps.close();
} catch (Exception e) {
    System.out.println("Exception occurred: " + e);
}
*/

/*  PrintJob properties
* awt.print.destination - can be "printer" or "file"
*	awt.print.printer - printer name
*	awt.print.fileName - name of the file to print
*	awt.print.numCopies - obvious
*	awt.print.options - options to pass to the print command
*	awt.print.orientation - can be "portrait" or "landscape"
*	awt.print.paperSize - can be "letter","legal","executive" or "a4"
*/
