/*
 * @(#)NoneTexture.java created 16/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

/**
 *  The NoneTexture is a class that implements a default (random) texture model
 *
 *
 * @version $Revision: 1.5 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class NoneTexture extends Texture {
  //insert class definition here

  public NoneTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "none tex";
    IDlabel = "none tex";
    description = "select this for untextured phase";
  }

  public NoneTexture(XRDcat aobj) {
    this(aobj, "none tex");
  }

  public NoneTexture() {
    identifier = "none tex";
    IDlabel = "none tex";
    description = "select this for untextured phase";
  }
  public void initializeReflexes(Sample asample) {}

	public void computeTextureFactor(final Phase aphase, final Sample asample) {

		if (!refreshComputation)
			return;

		for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
			int datafilenumber = asample.getActiveDataSet(i).activedatafilesnumber();
			for (int ij1 = 0; ij1 < datafilenumber; ij1++) {
				DiffrDataFile adatafile = asample.getActiveDataSet(i).getActiveDataFile(ij1);
				adatafile.resetForRandomTexture(aphase);
			}
		}
	}

}
