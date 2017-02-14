/*
 * @(#)MolecularImporter.java created Aug 28, 2005 Casalino
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

package it.unitn.ing.rista.io;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;
import it.unitn.ing.rista.util.Coordinates;
import it.unitn.ing.rista.interfaces.AtomsStructureI;

import java.util.StringTokenizer;
import java.io.*;


/**
 * The MolecularImporter is a class that provide utilities for importing fragments
 * molecules, structures from popular formats like XYZ, PDB etc.
 * <p/>
 * Description
 * Most of the methods are statics and it is necessary to provide the structure
 * object that will finally held the atoms imported.
 * So to import a structure from a file in XYZ format to a Fragment (subclass of
 * StructureModel) you just call:
 * <code>MolecularImporter.importXYZmolecule(myFragment, XYZ_filename);</code>
 * where String XYZ_filename should contain the complete path and filename in
 * the correct format for the platform (see the it.unitn.ing.rista.util.Misc class
 * for methods to locate, open and convert automatically file names and paths for
 * the active platform.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class MolecularImporter {

  public static final int XYZformat = 0;

  /**
   * Import an atom list from a XYZ molecule file in absolute cartesian coordinates
   * and add all the atom to the specified structure.
   *
   * @param structure the parent object that will store the atoms for example a Fragment
   * @param filename the absolute path+name of the file containing the XYZ molecule
   */
  public static void importXYZmolecule(AtomsStructureI structure, String filename) {
    BufferedReader reader = Misc.getReader(filename);
    if (reader != null) {
      try {

        String line = null;
        String token = null;

        line = reader.readLine();
        if (line != null) {
          StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
          token = st.nextToken();
          int numberOfAtoms = Integer.parseInt(token);

          line = reader.readLine();
          System.out.println("Importing " + line);
          for (int i = 0; i < numberOfAtoms; i++) {
            line = reader.readLine();
            st = new StringTokenizer(line, " ,\t\r\n");
            String atomType = st.nextToken();
            String atomLabel = atomType + Integer.toString(i);
            double x = Double.parseDouble(st.nextToken());
            double y = Double.parseDouble(st.nextToken());
            double z = Double.parseDouble(st.nextToken());
            AtomSite atom = new AtomSite((XRDcat) structure, atomLabel);
            structure.addAtom(atom);
            atom.addAtomWithSymbol(atomType);
            atom.setLocalCoordinates(new Coordinates(x, y, z));
          }
        }
      } catch (IOException e) {
        e.printStackTrace();
        System.out.println("Error in loading the XYZ molecule file!");
      }
      try {
        reader.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

}
