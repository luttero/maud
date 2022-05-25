/*
 * @(#)NaturalStringSorter.java created Mar 29, 2003 Berkeley
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

import java.util.Comparator;


/**
 * The NaturalStringSorter is a class
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class NaturalStringSorter implements Comparator {
  public int compare(Object obj1, Object obj2) {
    String phase1 = (String) obj1;
    String phase2 = (String) obj2;
    int len1 = phase1.length();
    int len2 = phase2.length();
    int minLength = Math.min(len1, len2);
    for (int i = 0; i < minLength; i++) {
      int diff = phase1.charAt(i) - phase2.charAt(i);
      if (diff > 0)
        return 1;
      else if (diff < 0)
        return -1;
    }
    if (len1 > len2)
      return 1;
    else if (len1 < len2)
      return -1;
//    System.out.println("Equals: " +  phase1 + " " + phase2);
    return 0;
  }
}

