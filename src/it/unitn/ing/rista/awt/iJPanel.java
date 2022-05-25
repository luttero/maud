/*
 * @(#)iJPanel.java created Mar 18, 2003 Berkeley
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

package it.unitn.ing.rista.awt;

import javax.swing.*;

/**
 *  The iJPanel is a
 *
 *
 * @version $Revision: 1.3 $, $Date: 2005/09/07 17:14:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public abstract class iJPanel extends JPanel {
  public abstract void retrieveParameters();
  public abstract void dispose();
  public abstract void setatomsite();
  public abstract void setatomsite(int number);
  public abstract void importStructure(int formatIndex);
}
