/*
 * @(#)ClipImage.java created Feb 13, 2003 Mesiano
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

import java.awt.datatransfer.*;
import java.awt.*;


/**
 * The ClipImage is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ClipImage implements Transferable {
  private DataFlavor[] myFlavors;
  private Image myImage;
  static private DataFlavor imageFlavor = new DataFlavor("image/x-java-image; class=java.awt.Image", "Image");

  public ClipImage(Image theImage) {
    try {
      myFlavors = new DataFlavor[]{ imageFlavor /*DataFlavor.imageFlavor*/};
      myImage = theImage;
    } catch (Exception e) {}
  }

  public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
    if (flavor != imageFlavor /*DataFlavor.imageFlavor*/) {
      throw new UnsupportedFlavorException(flavor);
    }
    return myImage;
  }

  public DataFlavor[] getTransferDataFlavors() {
    return myFlavors;
  }

  public boolean isDataFlavorSupported(DataFlavor flavor) {
    return (flavor == imageFlavor /*DataFlavor.imageFlavor */ );
  }


}
