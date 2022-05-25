/*
 * @(#)Structure.java created Feb 21, 2003 Mesiano
 *
 * Copyright (c) 2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.util.ListVector;
import it.unitn.ing.rista.awt.*;

import java.util.Vector;
/**
 *  The Structure is a 
 *
 *  
 * @version $Revision: 1.12 $, $Date: 2005/03/14 13:38:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureModel extends XRDcat {

	public StructureModel(XRDcat aobj, String alabel) {
		super(aobj, alabel);
	}

  public StructureModel(XRDcat aobj) {
		this(aobj, "Generic structure x");
	}

	public StructureModel() {
	}

	public void initParameters() {
    super.initParameters();
  }

  public Phase getPhaseParent() {
    Object aparent = getParent();
    while (aparent != null && !(aparent instanceof Phase))
      aparent = ((XRDcat) aparent).getParent();
    return (Phase) aparent;
  }

  public double getEnergyWeight() {
    return 1.0;
  }

  public ListVector getAtomList() {
    return null;
  }

  public AtomSite getAtom(int index) {
    return null;
  }

  public void addAtom() {
  }

  public void addAtom(AtomSite anatom) {
  }

  public void removeAtomAt(int number) {
  }

  public void convertAtomsForSG(double traslx, double trasly, double traslz) {
  }

  public Vector getFullAtomList() {
    return null;
  }

	public boolean isDebyeWallerModelDimensionLess() {
		return false;
	}

	public void deleteAtomFromFullList(int i) {
  }  

  public void freeAllCrystalParameters() {
  }

  public iJPanel getEditPanel(myJFrame aframe) {
    return null;
  }

  public boolean quantityFromOccupancy() {
    Phase aphase = (Phase) getParent();
    if (aphase.getStructureFactorModel() instanceof
			it.unitn.ing.rista.diffr.sfm.StructureFactorStandardModel)
      return true;
    else
      return false;
  }

  public double computeEnergy() {
    return 0.0;
  }

}
