/*
 * @(#)AtomsStructureI.java created Aug 29, 2004 Pergine Valsugana
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

package it.unitn.ing.rista.interfaces;

import it.unitn.ing.rista.diffr.AtomSite;
import it.unitn.ing.rista.util.ListVector;
import it.unitn.ing.rista.diffr.Phase;


/**
 * The AtomsStructureI is an interface
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2005/09/07 17:14:08 $
 * @since JDK1.1
 */

public interface AtomsStructureI {

  public boolean getQuantityFromOccupancy();

  public ListVector getAtomList();

  public void setQuantityFromOccupancy(boolean selected);

  public void addAtom();

  public void removeAtomAt(int i);

  public AtomSite getAtom(int i);

  public void addAtom(AtomSite newatom);

  public void removeFragmentAt(int i);

  public Phase getPhaseParent();

	public boolean isDebyeWallerModelDimensionLess();

	public void setDebyeWallerModelDimensionLess(boolean value);

}
