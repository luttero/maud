/*
 * @(#)JParameterPanel.java created 16/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.awt;

import javax.swing.*;

import it.unitn.ing.rista.diffr.*;

/**
 *  The JParameterPanel is a simple JPanel derived class to be implemented
 *  by a Texture object/model for showing/retrieving the necessary parameters
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JParameterPanel extends JPanel {
  //insert class definition here

  public XRDcat theparentObj;

  public JParameterPanel(XRDcat parent) {
    super();
    theparentObj = parent;
  }

  public void retrieveParameters() {
    // to be implemented by subclasses
  }

  public void initParameters() {
    // to be implemented by subclasses
  }
}
