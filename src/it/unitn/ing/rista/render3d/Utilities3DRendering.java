/*
 * @(#)Utilities3DRendering.java created Oct 30, 2005 Casalino
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

package it.unitn.ing.rista.render3d;

import it.unitn.ing.rista.awt.myJFrame;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.interfaces.i3DCanvas;

import javax.media.opengl.*;
import javax.swing.*;
import java.awt.*;

import com.jogamp.opengl.util.Animator;

/**
 * The Utilities3DRendering is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/11/10 09:33:01 $
 * @since JDK1.1
 */

public class Utilities3DRendering {

  static myJFrame odfFrame = null;

  public static void show3DODF(Frame parent, double[][][] mapToPlot, int alphaSlices, int betaSlices, int gammaSlices,
                               int mode, boolean logScale, int colrsNumber) {

    if (odfFrame != null) {
      odfFrame.setVisible(false);
      odfFrame.dispose();
      odfFrame = null;
    }

    myJFrame odfFrame = new myJFrame(parent);

    odfFrame.createDefaultMenuBar();

    odfFrame.setVisible(false);

    Map3DPanel sampleshape = new Map3DPanel(mapToPlot, alphaSlices, betaSlices, gammaSlices,
                         mode, logScale, colrsNumber);
    odfFrame.getContentPane().add(sampleshape);
    odfFrame.setSize(400, 480);
    sampleshape.initComponents();
    odfFrame.setVisible(true);
    sampleshape.setVisible(true);
  }


}
