/*
 * @(#)Bond.java created Feb 21, 2003 Mesiano
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

import it.unitn.ing.rista.util.Constants;

/**
 *  The Bond is a 
 *
 *  
 * @version $Revision: 1.4 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Bond extends XRDcat {

	protected static String[] diclistc = {
		"_bond_atom_1", "_bond_atom_2", "_bond_distance"
	};
  protected static String[] diclistcrm = {
    "_bond_atom_1", "_bond_atom_2", "_bond_distance"
  };

	protected static String[] classlistc = {};

	protected static String[] classlistcs = {};


	public Bond(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
	}

	public Bond(XRDcat aobj) {
		this(aobj, "Bond x");
	}

	public Bond() {
		super("bond_x");
		initBaseObject();
	}

	public void initConstant() {
		Nstring = 2;
		Nstringloop = 0;
		Nparameter = 1;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		for (int i = 0; i < totsubordinateloop; i++)
			diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
			classlist[i] = classlistc[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	public void initParameters() {
		super.initParameters();
	}

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.ATOM_POSITION_CHANGED, -1);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.ATOM_POSITION_CHANGED, -1);
            return;
          }

    }
    super.notifyParameterChanged(source);
  }

	public String getAtom1() {
		return stringField[0];
	}

	public void setAtom1(String atom1) {
		stringField[0] = atom1;
	}

	public String getAtom2() {
		return stringField[1];
	}

	public void setAtom2(String atom2) {
		stringField[1] = atom2;
	}

}
