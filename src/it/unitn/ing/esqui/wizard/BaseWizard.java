/*
 * @(#)BaseWizard.java created 16/03/2002 Casalino
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

package it.unitn.ing.esqui.wizard;

import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.util.*;
import java.lang.reflect.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 * The BaseWizard is a basic class to
 * provide wizard starting actions.
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:51 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class BaseWizard extends BaseFactoryObject {

  principalJFrame theMainFrame = null;
  String iconName = "DefaultAnalysis.gif";

  public BaseWizard(principalJFrame aframe, String alabel) {
    this();
    theMainFrame = aframe;
  }

  public BaseWizard() {
    identifier = "Default analysis";
    IDlabel = "Default analysis    ";
    description = "Open the default analysis file (default.par)";
  }

  public JIconButton getButton() {

    JIconButton iconButton = new JIconButton(iconName);
    iconButton.setToolTipText(description);

    return iconButton;
  }

  public JLabel getLabel() {
    return new JLabel(IDlabel);
  }

  public void startAction() {
    (new Thread() {
      public void run() {
        startWizardAction();
      }
    }).start();
  }

  public void startWizardAction() {
    theMainFrame.newFile_Action(null, null);
  }

  public static class PrototypeNotFound extends Exception {
  }

  public static class CannotCreateWizard extends Exception {
  }

  private static Vector WizardTypes = new Vector(0, 1);

  public static BaseWizard factory(principalJFrame obj, String alabel, String classname)
          throws PrototypeNotFound, CannotCreateWizard {
//		System.out.println("Creating " + classname + alabel);
    for (int i = 0; i < WizardTypes.size(); i++) {
      Class xc = (Class) WizardTypes.elementAt(i);
      if (xc.getName().indexOf(classname) != -1) {
        try {
          Constructor ctor = xc.getConstructor(
                  new Class[]{principalJFrame.class, String.class});
          return (BaseWizard) ctor.newInstance(
                  new Object[]{obj, alabel});
        } catch (Exception ex) {
          System.out.println(obj);
          System.out.println(alabel);
          System.out.println(classname);
          ex.printStackTrace();
          throw new CannotCreateWizard();
        }
      }
    }
    try {
//			System.out.println("Loading " + classname);
      WizardTypes.addElement(Class.forName(classname));
    } catch (Exception ex) {
      ex.printStackTrace();
      throw new PrototypeNotFound();
    }
    return factory(obj, alabel, classname);
  }

}
