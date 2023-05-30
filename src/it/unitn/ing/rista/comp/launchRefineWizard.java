/*
 * @(#)launchRefineWizard.java created 24/12/1998 Riva del Garda, Iva & Stelio home
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

package it.unitn.ing.rista.comp;

import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.Function;

/**
 *  The launchRefineWizard is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class launchRefineWizard extends launchRefine {

  int wizardControl = 0;

  public launchRefineWizard(Function afileparameter, OutputPanel aframe, int wizardIndex) {
    super(afileparameter, aframe);

    wizardControl = wizardIndex;
  }

  public void stuffToRun() {

    FilePar theparameterfile = (FilePar) parameterfile;

    reset();

//    String mem = Misc.freeMemory();
//    if (Constants.testing)
//      System.out.println(mem);

    print("Start rita/rista refinement, automatic mode number: " + wizardControl);

    String message = null;

    if (theparameterfile != null)
      try {
	      switch (wizardControl) {
          case 0:
          case 1:
          case 2:
          case 3:
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, wizardControl);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 4:
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, wizardControl);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 5:
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, wizardControl);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 6:
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.prepareWizard(outputframe, wizardControl);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 7:
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.prepareWizard(outputframe, wizardControl);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 8:
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.prepareWizard(outputframe, wizardControl);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 9:
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.prepareWizard(outputframe, wizardControl);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 10:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 11:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 3);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 12:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 4);
            sol.solveGeneral(this, theparameterfile);
            // add texture
            break;
          case 13:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            message = new String("Start forth cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 4);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 5);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 14:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 6);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 15:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            message = new String("Start forth cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 6);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 7);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 16:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 8);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 17:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            message = new String("Start forth cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 8);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[1]);
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 9);
            sol.solveGeneral(this, theparameterfile);
            break;
          case 18:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);

            for (int i = 0; i < theparameterfile.samplesNumber(); i++) {
//              Sample asample = (Sample) theparameterfile.getSample(i);

              theparameterfile.prepareWizard(outputframe, 0);
              sol.solveGeneral(this, theparameterfile);
              if (shouldStop())
                break;
              message = new String("Start second cycle");
              if (outputframe != null)
                outputframe.setProgressText(message);
              print(message);
              theparameterfile.prepareWizard(outputframe, 1);
              sol.solveGeneral(this, theparameterfile);
              if (shouldStop())
                break;
              message = new String("Start last cycle");
              if (outputframe != null)
                outputframe.setProgressText(message);
              print(message);
              theparameterfile.prepareWizard(outputframe, 2);
              sol.solveGeneral(this, theparameterfile);
            }
            break;
          case 99:
            message = new String("Start first cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 0);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start second cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.prepareWizard(outputframe, 99);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start third cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 1);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            message = new String("Start forth cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 2);
            sol.solveGeneral(this, theparameterfile);
            if (shouldStop())
              break;
            theparameterfile.setTextureFactorsExtractionStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setTextureComputationStatus(FilePar.COMP_STATUS[1]);
            theparameterfile.setPositionExtractionStatus(FilePar.COMP_STATUS[0]);
            theparameterfile.setStrainComputationStatus(FilePar.COMP_STATUS[0]);
            message = new String("Start last cycle");
            if (outputframe != null)
              outputframe.setProgressText(message);
            print(message);
            theparameterfile.prepareWizard(outputframe, 4);
            sol.solveGeneral(this, theparameterfile);
            // add texture
            break;
          default:
            break;
        }

      } catch (Exception e) {
        theparameterfile.setOptimizing(false);

        print("Error in the refinement, check the java console window for more details.");
        print("Have a nice day (if you get it working)!");
        e.printStackTrace();

      }

  }

  public void endOfRun() {
	  if (parameterfile != null) {
		  ((FilePar) parameterfile).resetWizard();
	  }

	  super.endOfRun();

  }

}

