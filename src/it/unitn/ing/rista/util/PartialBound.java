/*
 * @(#)MultiBound.java created MAy 16, 2015 Flight to Shanghai
 *
 * Copyright (c) 1996-2015 Luca Lutterotti All Rights Reserved.
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

/**
 * The PartialBound is a class to store and manage customizable bounds and
 * equations between parameters (@Parameter). The PartialBound just store
 * one of the links between a reference parameter (the one depending on the
 * others) and the registered ones (all the others which values determine
 * the value of the reference one through the equation). The link is composed
 * by the Parameter reference and the operation.
 *
 * @version $Revision: 1.0 $, $Date: 2015/05/16 21:12:35 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PartialBound {

	Parameter registered;
	String linkOperation;  // permitted operations: + - * /

}
