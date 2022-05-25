/*
 * @(#)BaseFactoryObject.java created 16/03/2002 Casalino
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

/**
 * The BaseFactoryObject is a base class for the factory.
 *
 * @version $Revision: 1.3 $, $Date: 2003/04/23 00:20:48 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class BaseFactoryObject {

	public static String noneID = "none id";

  public String identifier = noneID;
  public String IDlabel = noneID;
  public String description = "any description";

  public BaseFactoryObject() {
  }

}
