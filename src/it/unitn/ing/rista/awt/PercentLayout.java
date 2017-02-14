/*
 * @(#)PercentLayout.java created 01/01/2000 Mesiano
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

import java.awt.*;
import java.io.PrintStream;
import java.util.Vector;

/**
 * The PercentLayout is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PercentLayout implements LayoutManager {
  private Vector rects;
  private Vector comps;

  public PercentLayout() {
    rects = new Vector();
    comps = new Vector();
  }

  public void addLayoutComponent(String string, Component component) {
    int i1 = string.indexOf(" ");
    if (i1 == -1)
      return;
    int j1 = Integer.valueOf(string.substring(0, i1)).intValue();
    if (j1 < 0 || j1 > 100)
      return;
    int k1 = string.indexOf(" ", i1 + 1);
    if (k1 == -1)
      return;
    int i2 = Integer.valueOf(string.substring(i1 + 1, k1)).intValue();
    if (i2 < 0 || i2 > 100)
      return;
    int j2 = string.indexOf(" ", k1 + 1);
    if (j2 == -1)
      return;
    int k2 = Integer.valueOf(string.substring(k1 + 1, j2)).intValue();
    if (k2 < 0 || k2 > 100)
      return;
    int i3 = Integer.valueOf(string.substring(j2 + 1, string.length())).intValue();
    if (i3 < 0 || i3 > 100)
      return;
    rects.addElement(new Rectangle(j1, i2, k2, i3));
    comps.addElement(component);
  }

  public void removeLayoutComponent(Component component) {
    int i = comps.indexOf(component);
    try {
      comps.removeElementAt(i);
      rects.removeElementAt(i);
    } catch (ArrayIndexOutOfBoundsException e) {
      System.err.println(new StringBuffer("Internal Syncronyzation Error :").append(e).toString());
    }
    return;
  }

  public Dimension preferredLayoutSize(Container container) {
    int i = 0;
    int j = 0;
    for (int k = 0; k < comps.size(); k++) {
      Dimension dimension = ((Component) comps.elementAt(k)).getPreferredSize();
      if (dimension.width > i)
        i = dimension.width;
      if (dimension.height > j)
        j = dimension.height;
    }
    Insets insets = container.getInsets();
    return new Dimension(i + insets.left + insets.right, j + insets.top + insets.bottom);
  }

  public Dimension minimumLayoutSize(Container container) {
    int i = 0;
    int j = 0;
    for (int k = 0; k < comps.size(); k++) {
      Dimension dimension = ((Component) comps.elementAt(k)).getMinimumSize();
      if (dimension.width > i)
        i = dimension.width;
      if (dimension.height > j)
        j = dimension.height;
    }
    Insets insets = container.getInsets();
    return new Dimension(i + insets.left + insets.right, j + insets.top + insets.bottom);
  }

  public void layoutContainer(Container container) {
    Insets insets = container.getInsets();
    int i1 = container.getSize().width - (insets.left + insets.right);
    int j1 = container.getSize().height - (insets.top + insets.bottom);
    int k = insets.left;
    int i2 = insets.top;
    for (int j2 = 0; j2 < comps.size(); j2++) {
      Rectangle rectangle = (Rectangle) rects.elementAt(j2);
      ((Component) comps.elementAt(j2)).setBounds(k + i1 * rectangle.x / 100, i2 + j1 * rectangle.y / 100, k + i1 * rectangle.width / 100 - (k + i1 * rectangle.x / 100), i2 + j1 * rectangle.height / 100 - (i2 + j1 * rectangle.y / 100));
    }
  }
}
