/*
 * @(#)basicObj.java created 01/01/1997 Mesiano
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

import java.lang.*;
import java.util.Enumeration;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.tree.TreeNode;

/**
 * The basicObj is a class
 *
 * @version $Revision: 1.5 $, $Date: 2005/03/10 13:50:40 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public interface basicObj {
  public String getLabel();

  public void setLabel(String alabel);

  public void merge(basicObj obj);

  public void dispose();

  public boolean isObjectSupported(basicObj source, ListVector list);

  public boolean isObjectSupported(ListVector list);

  public void setParent(XRDcat obj);

  public boolean automaticOutput();

  public void setAutomaticOutput(boolean status);

  public Object[] getObjectChildren();

  public XRDcat getParent();

  public basicObj[] getChildren(String searchString, boolean refinable);

  public boolean isLeaf();

  public boolean getAllowsChildren();

  public int getChildCount(String searchString, boolean refinable);

  public basicObj getChildAt(int childIndex);

  public Enumeration children();

  public int getIndex(basicObj node);

  public void freeAllParameters(String searchString, boolean refinable);

  public void fixAllParameters(String searchString, boolean refinable);

  }

