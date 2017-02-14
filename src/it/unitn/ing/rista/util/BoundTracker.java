/*
 * @(#)BoundTracker.java created Apr 17, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.util;

import it.unitn.ing.rista.diffr.Parameter;

import java.util.*;


/**
 * The BoundTracker is a class
 *  
 * @version $Revision: 1.2 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class BoundTracker {
  Hashtable allReferences;
  int counter = 0;

  public BoundTracker() {
    allReferences = new Hashtable();
    initReferenceNames();
  }

  void initReferenceNames() {
    counter = 0;
  }

  String getNewReferenceString() {
    String refString = "#ref" + Integer.toString(counter);
    counter++;
    return refString;
  }

  public void addReferenceParameter(Parameter ref) {
    if (!allReferences.containsKey(ref))
      allReferences.put(ref, getNewReferenceString());
  }

  public String getReferenceName(Parameter ref) {
    return (String) allReferences.get(ref);
  }

	public Parameter[] getReferenceParameters(String expression) {
		return null;
	}

  public void removeReferenceParameter(Parameter ref) {
    if (allReferences.containsKey(ref))
      allReferences.remove(ref);
    if (allReferences.containsValue(ref)) {
      for (Iterator e = allReferences.keySet().iterator(); e.hasNext();) {
        Object key = e.next();
        if (allReferences.get(key) == ref) {
          allReferences.remove(ref);
          break;
        }
      }
    }
  }

  public void addReferenceParameter(String ref, Parameter par) {
    if (!allReferences.containsKey(ref))
      allReferences.put(ref, par);
  }

  public Parameter getReferenceParameter(String ref) {
    return (Parameter) allReferences.get(ref);
  }

  public void dispose() {
    allReferences.clear();
  }

/*  public class RefBound {
    Parameter referenceParameter;
    String referenceName;
    public RefBound(Parameter ref, String name) {
      referenceParameter = ref;
      referenceName = name;
    }
  }*/
}
