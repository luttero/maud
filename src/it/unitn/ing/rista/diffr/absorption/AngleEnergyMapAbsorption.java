/*
 * @(#)AngleEnergyMapAbsorption.java created 17/01/2020 Pergine Vals.
 *
 * Copyright (c) 2020 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.absorption;

import java.lang.*;
import it.unitn.ing.rista.diffr.*;

/**
 *  The AngleEnergyMapAbsorption is a class to compute absorption
 *  correction in angle-energy map measurements
 *
 *
 * @version $Revision: 1.0 $, $Date: 2020/01/17 9:20:32 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AngleEnergyMapAbsorption extends Absorption {
  
  public static String modelID = "Angle-energy map abs";
  private static String descriptionID = "Angle-energy map absorption model";
  
  public static String[] diclistc = {};
  public static String[] diclistcrm = {};
  
  public static String[] classlistc = {};
  public static String[] classlistcs = {};
  
  public AngleEnergyMapAbsorption(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public AngleEnergyMapAbsorption(XRDcat aobj) {
    this(aobj, modelID);
  }
  
  public AngleEnergyMapAbsorption() {
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
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
  }
  
  public double getAbsorptionCorrection(DiffrDataFile adatafile, Phase aphase, double positionOrEnergy) {
    Sample asample = adatafile.getDataFileSet().getSample();
    return asample.getAbsorptionForXray(positionOrEnergy); // todo: * computeShapeAbsorptionCorrection(asample, inst.getRadiationType(), rad_index, angles, positionOrEnergy, toLambda);
  }
  
}
