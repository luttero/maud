/*
 * @(#)HIPPOBank.java created Feb 14, 2007 Casalino
 *
 * Copyright (c) 1996-2007 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.wizard.HIPPOWizard;

import java.util.Vector;

/**
 * The HIPPOBank is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 2007/02/14 15:35:02 $
 * @since JDK1.1
 */

public class HIPPOBank {

  public String name;

  public boolean enabled = true;
  public boolean available = false;

  //public HIPPOPanel[] banks;

  public Vector/*<HIPPOPanel>*/ panels;

  public int startBank = 1;

  public String dSpacingMin = "0.0";
  public String dSpacingMax = "0.0";

	public double theta2 = -999.0;

  public int _instrument_counter_bank;
  public double _pd_proc_intensity_incident = 1;

  public HIPPOBank(String name, double theta2, int startBank, boolean available) {
    this.name = name;
    this.startBank = startBank;
    panels = new Vector/*<HIPPOPanel>*/(10,10);
    this.available = available;
	  this.theta2 = theta2;
  }

  public HIPPOBank(String name, double theta2, int bankNumber, int startBank) {
    this(name, theta2, startBank, true);
    /*banks = new HIPPOPanel[bankNumber];
    for (int bn = 0; bn < bankNumber; bn++)
      banks[bn] = new HIPPOPanel(GSASbankCalibration.bankPrefix + (startBank + bn), 4);*/
  }

}
