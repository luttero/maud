/*
 * @(#)StructureFactorList.java created 19/08/2001 Riva Del Garda
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

/**
 * The StructureFactorList is a class to store a structure factor list
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:45 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureFactorList {
  public RadiationType radiation = null;
  public StructureFactor[] structureFactor = null;

  public StructureFactorList(int size) {
    structureFactor = new StructureFactor[size];
  }
}
