/*
 * @(#)myObj.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.interfaces;

import java.util.*;
import java.lang.*;
import it.unitn.ing.rista.io.*;
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

/**
 * The myObj is a class
 *
 * @version $Revision: 1.5 $, $Date: 2004/08/12 09:36:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public interface myObj extends basicObj {

	public int setField(String cif, String astring, String astringerror, String min, String max, boolean free,
                      String refName, String refBound, String constant, String ratio, String expression,
                      boolean autoTrace, boolean positive);
	public void setLoop(Vector item, int element);
	public void notifyParentChanged();
	public void readtheobject(CIFtoken ciffile);
	public void notifyParameterChanged(Parameter source);
	public void notifyObjectChanged(XRDcat source, int reason, boolean up);
	public boolean isObjectSupported(XRDcat source, ListVector list);
	public boolean isObjectSupported(ListVector list);

}
