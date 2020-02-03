/*
 * @(#)SDPDutilities.java created Nov 9, 2005 Casalino
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.io.PrintStream;
import java.util.Vector;


/**
 * The SDPDutilities is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/07/20 13:39:06 $
 * @since JDK1.1
 */

public class SDPDutilities {

  public static void prepareAndSaveShelXInstruction(Phase aphase, Frame aframe) {
    String filename = Utility.browseFilenametoSave(aframe, "Save ShelX instruction file");
    if (filename != null) {
      PrintStream printStream = new PrintStream(Misc.getOutputStream(filename));

      try {
        Sample asample = aphase.getFilePar().getSample(0);
        DataFileSet dataset = asample.getSelectedDataSet();
        if (dataset == null)
          dataset = asample.getDataSet(0);
        RadiationType radType = dataset.getInstrument().getRadiationType();
/*        int nrad = -1;
        if (rad != null)
          nrad = rad.getRadiationIDNumber();*/
        double wave = radType.getMeanRadiationWavelength();
        printStream.print("TITL ");
        printStream.print(aphase.getLabel());
        printStream.print(Constants.lineSeparator);
        printStream.print("CELL ");
        printStream.print(Fmt.format(wave) + " ");
        printStream.print(Fmt.format(aphase.getFullCellValue(0)) + " ");
        printStream.print(Fmt.format(aphase.getFullCellValue(1)) + " ");
        printStream.print(Fmt.format(aphase.getFullCellValue(2)) + " ");
        printStream.print(Fmt.format(aphase.getFullCellValue(3)) + " ");
        printStream.print(Fmt.format(aphase.getFullCellValue(4)) + " ");
        printStream.print(Fmt.format(aphase.getFullCellValue(5)));
        printStream.print(Constants.lineSeparator);
        printStream.print("LATT ");
        if (!aphase.isCentrosymmetric())
          printStream.print("-");
        printStream.print(1);
        printStream.print(Constants.lineSeparator);
        int npositions = aphase.getPhaseInfo().getSitePositionNumber();
        if (aphase.isCentrosymmetric())
          npositions /= 2;
        for (int i = 1; i < npositions; i++) {
          printStream.print("SYMM ");
          printStream.print((new String((aphase.getPhaseInfo().getSitePosition(i)).getx_i())).toUpperCase() + ",");
          printStream.print((new String((aphase.getPhaseInfo().getSitePosition(i)).gety_i())).toUpperCase() + ",");
          printStream.print((new String((aphase.getPhaseInfo().getSitePosition(i)).getz_i())).toUpperCase());
          printStream.print(Constants.lineSeparator);
        }
        Vector atoms = aphase.getFullAtomList();
        for (int i = 0; i < atoms.size(); i++) {
          AtomSite atom = (AtomSite) atoms.get(i); // todo fix, now AtomSite may contain more atoms types
	        AtomScatterer atomScatterer = atom.getAtomScatterer(0);
          printStream.print("SFAC " + AtomSite.stripIsotopeNumber(AtomSite.stripOxidation(atomScatterer.getAtomSymbol())) + " ");
	        if (radType.isNeutron()) {
              for (int j = 0; j < 4; j++)
                printStream.print("0.0 0.0 ");
              printStream.print("=");
              printStream.print(Constants.lineSeparator);
              printStream.print("     ");
              printStream.print(Fmt.format(radType.getRadiation(0).neutronSF[atomScatterer.getIsotopicListNumber()]) + " ");
              printStream.print("0.0 0.0 ");
	        } else if (radType.isElectron()) {
              for (int j = 0; j < 4; j++)
                printStream.print(Fmt.format(radType.getRadiation(0).electronSF[atomScatterer.getAtomicListNumber()][j]) + " " +
                    Fmt.format(radType.getRadiation(0).electronSF[atomScatterer.getAtomicListNumber()][j + 4]) + " ");
              printStream.print("=");
              printStream.print(Constants.lineSeparator);
              printStream.print("     ");
              printStream.print(Fmt.format(radType.getRadiation(0).electronSF[atomScatterer.getAtomicListNumber()][8]) + " ");
	        } else {
		        for (int j = 0; j < 4; j++)
			        printStream.print(Fmt.format(radType.getRadiation(0).xraySF[atomScatterer.getAtomicListNumber()][j]) + " " +
					        Fmt.format(radType.getRadiation(0).xraySF[atomScatterer.getAtomicListNumber()][j + 4]) + " ");
		        printStream.print("=");
		        printStream.print(Constants.lineSeparator);
		        printStream.print("     ");
		        printStream.print(Fmt.format(radType.getRadiation(0).xraySF[atomScatterer.getAtomicListNumber()][8]) + " ");
		        printStream.print(Fmt.format(0.0) + " ");
		        printStream.print(Fmt.format(0.0) + " ");
	        }

          printStream.print(Fmt.format(atom.getSiteAbsorptionForXray(radType.getRadiationEnergy()) * 1.66043) + " ");
          printStream.print(Fmt.format(atomScatterer.getAtomRadius()));
          printStream.print(Constants.lineSeparator);
        }
        printStream.print("UNIT ");
        for (int i = 0; i < atoms.size(); i++) {
          AtomSite atom = (AtomSite) atoms.get(i);
          printStream.print(Fmt.format(atom.getQuantityD()) + " ");
        }
        printStream.print(Constants.lineSeparator);
        printStream.print("FMAP 7");
        printStream.print(Constants.lineSeparator);
        printStream.print("HKLF 3");
        printStream.print(Constants.lineSeparator);
        printStream.print("END");
        printStream.print(Constants.lineSeparator);
      } catch (Exception io) {
        io.printStackTrace();
      }
      try {
        printStream.flush();
        printStream.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }
  }

}
