/*
 * @(#)launchComp.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.comp;

import it.unitn.ing.rista.interfaces.*;

import java.awt.*;

import javax.swing.*;

/**
 * The launchComp is a class
 *
 * @version $Revision: 1.7 $, $Date: 2004/11/18 09:30:49 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class launchComp extends launchBasic {

  public launchComp(Function afileparameter, OutputPanel aframe) {
    super(afileparameter, aframe);
  }

  public void stuffToRun() {

//		System.gc();
//		System.runFinalization();

//		if (Constants.speedUp)
//    	Constants.speedUp = false;
    reset();

    print("Start rita/rista function computation");

    if (parameterfile != null) {
      try {
        parameterfile.prepareComputation();
        parameterfile.mainfunction(true, true);
        print("End of function computation");
        print("Have a nice day!");

      } catch (Exception e) {

        print("Error in the computation, check the java console window for more details.");
        print("Have a nice day (if you get it working)!");
        e.printStackTrace();

      }
    }

//    Removed also from preferences
//    Constants.speedUp = MaudPreferences.getBoolean(MaudPreferences.speedupComp);

    if (parameterfile != null)
      parameterfile.fittingFileOutput();

    if (parameterfile != null)
      parameterfile.endOfComputation();

  }

}

