/*
 * @(#)AnalysisTools.java created Jul 10, 2024 Caen
 *
 * Copyright (c) 2024 Luca Lutterotti All Rights Reserved.
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
package com.radiographema.tools;
import java.util.ArrayList;

/**
 * The AnalysisTools is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jul 10, 2024 11:38:37 AM $
 * @since JDK1.1
 */
public class AnalysisTools {

  public static void main(String[] args) {
    (new QuantitativeTextureAnalysis()).performAnalysis();
  }
}
