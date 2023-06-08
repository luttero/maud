/*
 * @(#)CompositionElement.java created 1/7/2013 Caen
 *
 * Copyright (c) 2013 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;

/**
 *  The CompositionElement is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2013/07/01 10:52:26 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CompositionElement extends XRDcat {


	public static String[] diclistc = {"_atom_type_symbol",
			"_atom_quantity_fraction"
	};
	public static String[] diclistcrm = {"_atom_type_symbol",
			"_atom_quantity_fraction"
	};

	public static String[] classlistc = {};
	public static String[] classlistcs = {};

	public CompositionElement(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
	}

	public CompositionElement(XRDcat aobj) {
		this(aobj, "AtomSite fraction");
	}

	public CompositionElement() {}

	public void initConstant() {
		Nstring = 1;
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
		parameterField[0] = new Parameter(this, getParameterString(0), 1,
				ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
				ParameterPreferences.getDouble(getParameterString(0) + ".max", 1.0));
	}

	public void chooseTheAtom(Frame parentD) {
		String anatom = getString(0);
		if (anatom != null) {
			String label = ChooseAtomD.getAtomType(parentD, anatom);
			if (label != null)
				setString(0, label);
		}
	}

}
