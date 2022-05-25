/*
 * @(#)HKLIntensityPeak.java created Feb 21, 2010 Caen
 *
 * Copyright (c) 2010 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.StructureFactor;
import it.unitn.ing.rista.diffr.Reflection;

/**
 * The HKLIntensityPeak is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 21, 2010 1:18:42 PM $
 * @since JDK1.1
 */
public class HKLIntensityPeak {
  public int h = 0;
  public int k = 0;
  public int l = 0;
  public double intensity = 1000.0;
  public double A = 0.0;
  public double B = 0.0;

  public HKLIntensityPeak(int h, int k, int l, double intensity) {
    this.h = h;
    this.k = k;
    this.l = l;
    this.intensity = intensity;
  }

  public HKLIntensityPeak(int h, int k, int l, double intensity, double a, double b) {
    this(h, k, l, intensity);
    this.A = a;
    this.B = b;
  }

  public void exchangeWith(HKLIntensityPeak peak) {
    int h1 = peak.h;
    int k1 = peak.k;
    int l1 = peak.l;
    double int1 = peak.intensity;
    double a = peak.A;
    double b = peak.B;
    peak.h = h;
    peak.k = k;
    peak.l = l;
    peak.intensity = intensity;
    peak.A = A;
    peak.B = B;
    h = h1;
    k = k1;
    l = l1;
    intensity = int1;
    A = a;
    B = b;
  }

  public static boolean equivalent(HKLIntensityPeak refl, StructureFactor sf) {
    if (sf.h == refl.h && sf.k == refl.k && sf.l == refl.l)
      return true;
    for (int i = 0; i < sf.reflectionhList.length; i++) {
      if (sf.reflectionhList[i] == refl.h && sf.reflectionkList[i] == refl.k && sf.reflectionlList[i] == refl.l ||
          -sf.reflectionhList[i] == refl.h && -sf.reflectionkList[i] == refl.k && -sf.reflectionlList[i] == refl.l) {
        return true;
      }
    }
    return false;
  }

  public static boolean equivalent(HKLIntensityPeak refl, Reflection sf) {
    if (sf.getH() == refl.h && sf.getK() == refl.k && sf.getL() == refl.l)
      return true;
    for (int i = 0; i < sf.hlist.length; i++) {
      if (sf.hlist[i] == refl.h && sf.klist[i] == refl.k && sf.llist[i] == refl.l ||
          -sf.hlist[i] == refl.h && -sf.klist[i] == refl.k && -sf.llist[i] == refl.l) {
        return true;
      }
    }
    return false;
  }


}
